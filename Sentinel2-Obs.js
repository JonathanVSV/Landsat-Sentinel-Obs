//Imports
var BR = ee.FeatureCollection("users/JonathanVSV/Brazil4GEE"),
    geometry = /* color: #d63000 */ee.Geometry.Polygon(
        [[[-75.138588704977, -8.103869716910255],
          [-69.074135579977, -12.512780812118697],
          [-66.437416829977, -11.136408112054964],
          [-60.460854329977, -16.595176371752615],
          [-58.439369954977, -21.90333804217225],
          [-55.011635579977, -26.077546799835154],
          [-58.439369954977, -30.108106286988637],
          [-53.253823079977, -34.48938956573482],
          [-47.453041829977, -27.878938278486466],
          [-46.398354329977, -25.126427047013646],
          [-41.212807454977, -23.202010951593326],
          [-39.454994954977, -20.674974233365308],
          [-38.048744954977, -17.09988427985822],
          [-37.345619954977, -12.855762814999657],
          [-33.038979329977, -7.668573812762633],
          [-35.060463704977, -3.8215480903200234],
          [-40.685463704977, -1.4511826780461587],
          [-49.210854329977, 2.502943805512297],
          [-51.056557454977, 5.658581576352428],
          [-56.329994954977, 3.292941568573435],
          [-60.372963704977, 6.183110262121933],
          [-65.470619954977, 4.87090928062092],
          [-71.007729329977, 1.6246162731577878]]]),
    Sentinel2_1C = ee.ImageCollection("COPERNICUS/S2");

/*
General description:
This script calculates the number of cloudless observations over different ecoregions. The number of cloudless 
observations is equal to the number of cloudless observations acquired with more than a day of difference.

Thse script contains 5 sections:

   1. Variable definition.
   2. Function definition.
   3. Construction of mosaics.
   4. Exporting mosaics.
   5. Get and export metadata information.

The script can export two types of results: 
   1. Monthly and annual images of the number of cloudless observations. 
      Two types of images are exported: annual and monthly mosaics.
      (L4-8_SR_T1_MXjDayVObs_Year_60m y L4-8_SR_T1_MXjDayVObs_Year_Month_60m)
      Monthly images contain information for every month in the whole study period.
      Due to the enormous amount of monthly images that can be exported, this version only exports
      the images of the year that is indicated as 'monthYearExport'.
      Annual images contain the information for each year in the study period.
   2. Metadata of images
      Export a single table that contain the metadata of every image included in the study period
      (Landsat4-8_SR_T1_Metadata_YYYY-YYYY)
*/

//1. 
//------------------Variables definition-----------------------------------
  //Google Drive folder where results are going to be exported
  var folder = 'Sentinel-2_exportAllMonth';
  
  //Set several variables
  var prueba = BR, //Shapefile of BR
      area = geometry, // geometry2 polygon that englobes BR shape
      resolution = 600, //Pixel size in m to export results, QA60 = 60 m res
      //Higher values tend to reduce the size of the exported image
      
      //Month counts
      start = '2015-07-01', //Start period for month counts 2015-07-01
      //Ignore 2015 images because they start in july
      end = '2020-01-01', //End period for month counts at 00:00:00
      
      //Metadata period
      mStart = 2015, //Start in year 2015 for annual evaluations
      mEnd = 2019, //End in year 2019 for annual evaluations
      yTot = mEnd - mStart, // Number of years of evaluation, used in exporting loop
      annualStart = '2015-01-01', //Start date in annual evaluations
      annualEnd = '2020-01-01',//End date un annual evaluations at 00:00:00

      yEnd = '2020-01-01',//End date for monthly evaluations
      yStart = '2015-07-01';//Start date for monthly evaluations

    
//2.
//-------------------Functions definition-----------------------------------
//2.1 Clipping function just to work with the area of interest
  //Clip to delimiting polygon
  var corte = function(image){
    return image.clip(area);
  };
  //Clip to BR polygon
  var corteMX = function(image){
    return image.clip(prueba);
  };

//2.2 Function that will return valid and total observations per pixel
  //Generates valid observations by mapping the juliandDate
  var qaCount = function(image){
    
    var qa = image.select('QA60');
    //Use NIR band to mask out areas which do not contain data
    var NIR = image.select('B4');
    
    var validObs = qa.eq(0);
    validObs = validObs.updateMask(NIR);
    validObs = validObs.rename('QA');

    var jDay = image.get('system:time_start');
    
    //Create an image containing the julian day of the date the image was 
    //acquired
    jDay = ee.Image.constant(ee.Number.parse(ee.Date(jDay).format('D')));
    
    //Apply mask
    jDay = jDay.updateMask(validObs);
    jDay = jDay.rename('julianDay');
    
    var resul = jDay;
    
    //Round value and cast it as unsigned integer
    resul = resul.floor();
    resul = resul.toUint16();
    return resul; 
  };
  
//2.3 Reduction of collections to 1 image composite
  //Reduce julianDay band to get number of distinct observations
  var reductionImage = function(collection1, extraDate){
     
      //Count different values of julian day
      var julDay = collection1.select('julianDay').reduce(ee.Reducer.countDistinct());
      //Unmask, masked values get value 0
      var julDayZero = collection1.map(function(image){
        return image.unmask({value:0});
      });
      //Get locations where there are invalid observations
      julDayZero =julDayZero.select('julianDay').reduce(ee.Reducer.min());
      
      //Subtract 1 observation to the pixels where an invalid observation was counted
      julDayZero = julDayZero.lte(0);
      julDay = julDay.subtract(ee.Image(julDayZero));
      
      //Unmaks and set to 0
      julDay = julDay.unmask({value:0});
      julDay = julDay.rename('julianDay_ValidObs');
      var resul = julDay;
      resul = corteMX(resul);
      
      //round and unsigned integer (probably this step is not necessary).
      resul = resul.floor();
      resul = resul.toUint16();
      //Set date info as Fecha
      resul = resul.set('Fecha',ee.Date(extraDate).format('Y-M-d'));
      
      return resul;  
  };

// 2.4 Date Generators
  //Transform list values into a list of dates
  //Monthly
  var dateGen = function (num){ 
    return ee.Date(yStart).advance(num,'month'); 
  };
  //Yearly
  var dateGenYear = function (num){ 
    return ee.Date(annualStart).advance(num,'year'); 
  };

// 2.5 Filtering collections
  //Monthly. Used for contructing the monthly images
  var monthFilter2 = function(date){
      //End Date is 1 month ahead of the date indicated by the entry taken from the dates list
      var endDate = ee.Date(date).advance(1,'month'); 
      var extraDate = ee.Date(date);
      
      //Filter collection to the start and end dates
      var collection1 = Sentinel2_1C
          //Date filter
          .filterDate(date,endDate)
          //Area of interest filter
          .filterBounds(area)
          .select(['QA60','B4']);
          
      //Apply crop and qaCount functions
          collection1 = collection1 
            .map(corte)
            .map(qaCount);
            
      //Create an empty collection
      var emptyColl = ee.Image.constant(0).clip(area).rename('julianDay_ValidObs');
     
      var temp = ee.ImageCollection(collection1).size();
      
      //If collection is empty, fill the infor with an empty image. This help avoid an error.
      //If collection is not empty, then apply redcutionTable function (produces monthly mosaics)
      var imRed = ee.Algorithms.If(ee.ImageCollection(collection1).size().gte(1),
                                    reductionImage(ee.ImageCollection(collection1),extraDate), 
                                    emptyColl);
      
      var resul = imRed;
      resul = ee.Image(resul).set('YearEval',extraDate.format('Y'));
      
      return resul;
    
  };
  
  //Yearly. Used for contructing the annual images
  var yearFilter2 = function(date){
     
     //End Date is 1 year ahead of the date indicated by the entry taken from the dates list
      var endDate = ee.Date(date).advance(1,'year'); 
      var extraDate = ee.Date(date);
      
      //Filter collection to start and end date
      var collection1 = Sentinel2_1C
          //Date filter
          .filterDate(date,endDate)
          //Area of interest filter
          .filterBounds(area)
          .select(['QA60','B4']);
      
          collection1 = collection1 
            .map(corte)
            .map(qaCount);
            
      //Create an empty collection
      var emptyColl = ee.Image.constant(0).clip(area).rename('julianDay_ValidObs');
      
      var temp = ee.ImageCollection(collection1).size();
      
      //If collection is empty, fill the infor with an empty image. This help avoid an error.
      //If collection is not empty, then apply redcutionTable function (produces monthly mosaics)
      var imRed = ee.Algorithms.If(ee.ImageCollection(collection1).size().gte(1),
                                    reductionImage(ee.ImageCollection(collection1),extraDate), 
                                    emptyColl);
      
      var resul = imRed;
      
      return resul;
    
  };

//2.7 Metadata extraction information
  //Extract metadata for each image
  var consulta = function(image) {
    var id = ee.String(image.get('PRODUCT_ID'));
    var CloudCover = image.get('CLOUDY_PIXEL_PERCENTAGE');
    var CloudCoverLand = image.get('CLOUD_COVERAGE_ASSESSMENT');
    var date = ee.Date(image.get('system:time_start'));
    var sensor = ee.String(image.get('SPACECRAFT_NAME'));
    var mgrsTile = ee.Number(image.get('MGRS_TILE'));
    
    
    var dictionarySummary = ee.Dictionary({
          ID: id,
          cloud_pixel_percentage: CloudCover,
          cloud_coverage_assessment: CloudCoverLand,
          date: date,
          sensor: sensor,
          mgrsTile: mgrsTile, 
          
          });
          
    var SummaryFeature = ee.Feature(null,dictionarySummary);
    return SummaryFeature;
  };

//2.8 Exporting functions
  //Export images
    //Monthly
    var exportImageMonth = function(image,month,resolution){
      Export.image.toDrive({
                  image: image,
                  description: 'S2_1C_MXjDayVObs_MonthStack'+month.toString()+'_'+resolution.toString()+'m',
                  scale: resolution,
                  folder: folder,
                  crs: 'EPSG:4326',
                  maxPixels:1e12,
                  region: area
                });
    };
    
    //Yearly
    var exportImageYear = function(image,year,resolution){
      Export.image.toDrive({
                  image: image,
                  description: 'S2_1C_MXObs_'+year.toString()+'_'+resolution.toString()+'m',
                  scale: resolution,
                  folder: folder,
                  crs: 'EPSG:4326',
                  maxPixels:1e12,
                  region: area
                });
    };

    //Metadata info
    var exportMetadata = function(table,y1,y2){
     Export.table.toDrive({
          collection: table,
          description: 'Sentinel-2_1C_Metadata_'+y1+
                      '-'+y2,
          folder: folder,
          fileFormat: 'CSV'
        });
    };

//3
//----------------Construct mosaics-----------------------
  //3.1 Monthly mosaics 

    var monthlySeq = ee.List.sequence(0,(12*(yTot+1)-7),1); 
    
    //Transform list of numbers into dates
    var datesList = monthlySeq.map(dateGen); 
    
    //Construct monthly mosaics of valid observations
    var meanFeatureCol = datesList.map(monthFilter2);
    
    //Create an image collection from the monthly mosaics
    var expImColl = ee.ImageCollection.fromImages(meanFeatureCol);
    
  //3.2 Annual mosaics
    
    var yearlySeq = ee.List.sequence(0,yTot,1); 
    
    //Transform list of numbers into dates
    var datesListYear = yearlySeq.map(dateGenYear);
    
    //Construct annual mosaics of valid observations
    var meanFeatureColYear = datesListYear.map(yearFilter2);
    
    //Create an image collection from the annual mosaics
    var expImCollYear = ee.ImageCollection.fromImages(meanFeatureColYear);

//4 
//----------------Export images--------------------------------------
  //Export annual images and monthly stacks per year using a loop
    var i = 0;
    for(;i<=yTot;i++){
        
        //Export yearly mosaics
        var currYear = mStart + i;
      
        //Select the current annual mosaic
        var yearIm = expImCollYear.filter(ee.Filter.eq('Fecha',currYear.toString()+'-1-1'));
        //Force it as ee.Image
        yearIm = yearIm.first();
        //Select julianDay_ValidObs band
        yearIm = yearIm.select('julianDay_ValidObs');
        //Floor and cast as Uint16
        yearIm = yearIm.floor().toUint16();
      
        exportImageYear(yearIm,currYear,resolution);
      
        //Export monthly stacks per year
        var monthIm = expImColl.filter(ee.Filter.eq('YearEval',currYear.toString()));
        
        var merged = monthIm.toBands();
        merged = merged.floor().toUint16();
        exportImageMonth(merged,currYear,resolution);
    }

//5
//-----------------Export metadata table----------------------
  var S2_1CMeta = Sentinel2_1C
      //Date filter
      .filterDate(mStart.toString()+'-01-01',mEnd.toString()+'-12-31')
      //Area of interest filter
      .filterBounds(prueba)
      .select('QA60');
      
  var S2_1CExp = ee.FeatureCollection(S2_1CMeta.map(consulta));
  
  exportMetadata(S2_1CExp,mStart,mEnd);
