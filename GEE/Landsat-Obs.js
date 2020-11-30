//Imports
var BR = ee.FeatureCollection("users/JonathanVSV/Brazil4GEE"),
    L4 = ee.ImageCollection("LANDSAT/LT04/C01/T1_SR"),
    L5 = ee.ImageCollection("LANDSAT/LT05/C01/T1_SR"),
    L7 = ee.ImageCollection("LANDSAT/LE07/C01/T1_SR"),
    L8 = ee.ImageCollection("LANDSAT/LC08/C01/T1_SR"),
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
          [-71.007729329977, 1.6246162731577878]]]);

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
  //Google Drive folder where the results are going to be exportes
  var folder = 'Landsat_exportAllMonth';
  //Valid dates are 2015 - 2019 (no images in 2014)
  var prueba = BR, //shapefile of BR
      area = geometry, // geometry polygon that englobes all BR 
      resolution = 600, //Pixel size in m to export results, pixel_qa = 30 m res
      //Higher values tend to reduce the size of the exported image
      
      //Month counts
      start = '1984-01-01', //Start period for month counts 
      end = '2020-01-01', //End period for month counts at 00:00:00
      
      //Metadata period
      mStart = 1984, //Start year for annual evaluations
      mEnd = 2019, //End year for annual evaluations
      yTot = mEnd - mStart, // Number of years of evaluation, used in exporting loop
      annualStart = '1984-01-01', //Start date (YYYY-MM-DD) in annual evaluations
      annualEnd = '2020-01-01', //End date (YYYY-MM-DD) in annual evaluations at 00:00:00
      yStart = '1984-01-01',//Start date for monthly evaluations
      yEnd = '2020-01-01';//End date for monthly evaluations 
    
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
  //For Landsat 4, 5, 7 images
  var qaCount = function(image){
    
    var qa = image.select('pixel_qa');
        
    var validObs = qa.eq(66)
                      .or(qa.eq(68));
    validObs = validObs.rename('QA');

    var jDay = image.get('system:time_start');
    
    //Make constat image using the julian date info
    jDay = ee.Image.constant(ee.Number.parse(ee.Date(jDay).format('D')));
    
    //Update mask (only stay with cloudless observations)
    jDay = jDay.updateMask(validObs);
    jDay = jDay.rename('julianDay');
   
    var resul = jDay;
    
    //Round values and transform to unsigned integer                  
    resul = resul.floor();
    resul = resul.toUint16();
    return resul; 
  };

  //For Landsat 8 images
  var qaCount8 = function(image){
    
    var qa = image.select('pixel_qa');
        
    var validObs = qa.eq(322)
                      .or(qa.eq(324));
    validObs = validObs.rename('QA');

    var jDay = image.get('system:time_start');
    
    //Make constat image using the julian date info
    jDay = ee.Image.constant(ee.Number.parse(ee.Date(jDay).format('D')));
    
    //Update mask (only stay with cloudless observations)
    jDay = jDay.updateMask(validObs);
    jDay = jDay.rename('julianDay');
  
    var resul = jDay;
    
    //Round values and transform to unsigned integer                      
    resul = resul.floor();
    resul = resul.toUint16();
    return resul; 
  };
  
//2.3 Reduction of collections to 1 image composite
  //Reduce julianDay band to get number of distinct observations
  var reductionImage = function(collection1, extraDate){
     
      //Count different values (of Julian day values)
      var julDay = collection1.select('julianDay').reduce(ee.Reducer.countDistinct());
      //Unmask invalid observations and set them to 0
      var julDayZero = collection1.map(function(image){
        return image.unmask({value:0});
      });
      //Reduce to obtain the minimum
      julDayZero =julDayZero.select('julianDay').reduce(ee.Reducer.min());
      
      //Pixel locations that contain 0s subtract 1 value
      julDayZero = julDayZero.lte(0);
      julDay = julDay.subtract(ee.Image(julDayZero));
      
      //Unmask and set invalid obs to 0
      julDay = julDay.unmask({value:0});
      julDay = julDay.rename('julianDay_ValidObs');
      
      var resul = corteMX(julDay);
      
      //Round and transform to unsigned integer
      resul = resul.floor();
      resul = resul.toUint16();
      //Set Fecha property
      resul = resul.set('Fecha',ee.Date(extraDate).format('Y-M-d'));
      
      return resul;  
  };

// 2.4 Date Generators
  //Transform list values into a list of dates
  //Monthly gen
  var dateGen = function (num){ 
    return ee.Date(yStart).advance(num,'month'); 
  };
  //Yearly gen
  var dateGenYear = function (num){ 
    return ee.Date(annualStart).advance(num,'year'); 
  };

// 2.5 Filtering collections
  //Monthly. Used for contructing the monthly images
  var monthFilter = function(date){
      //End Date is 1 month ahead of the date indicated by the entry taken from the dates list
      var endDate = ee.Date(date).advance(1,'month'); 
      var extraDate = ee.Date(date);
      
      //Filter Landsat collections
      //Must be done separately because pixel_qa values are different for L4-7 and L8
        var l4 = L4
            //Date filter
            .filterDate(date,endDate)
            //Area of interest filter
            .filterBounds(area)
            .select('pixel_qa');
        
            l4 = l4 
              .map(corte)
              .map(qaCount);
       
        var l5 = L5
            //Date filter
            .filterDate(date,endDate)
            //Area of interest filter
            .filterBounds(area)
            .select('pixel_qa');
        
            l5 = l5 
              .map(corte)
              .map(qaCount);
        
        var l7 = L7
            //Date filter
            .filterDate(date,endDate)
            //Area of interest filter
            .filterBounds(area)
            .select('pixel_qa');
        
            l7 = l7 
              .map(corte)
              .map(qaCount);
       
        var l8 = L8
            //Date filter
            .filterDate(date,endDate)
            //Area of interest filter
            .filterBounds(area)
            .select('pixel_qa');
        
            l8 = l8 
              .map(corte)
              .map(qaCount8);
      
      //Create an empty collection
      var emptyColl = ee.Image.constant(0).clip(area).rename('julianDay_ValidObs');
      
      //Get the year of the date
      var dateNum = ee.Date(date).format('Y');
      //Pass the year as a number
      dateNum = ee.Number.parse(dateNum);
      
      //Set LAll as emptyColl by default
      var LAll = emptyColl; 

      //Set some rules in order to merge different landsat collections according to the date year
      //This is used in order to avoid merging empty collections
      LAll = ee.Algorithms.If(dateNum.lt(1984), l4, LAll);
      LAll = ee.Algorithms.If(dateNum.gte(1984).and(dateNum.lte(1993)), l5.merge(l4), LAll);
      LAll = ee.Algorithms.If(dateNum.gt(1993).and( dateNum.lt(1999)), l5, LAll);
      LAll = ee.Algorithms.If(dateNum.gte(1999).and(dateNum.lte(2012)), l7.merge(l5), LAll);
      LAll = ee.Algorithms.If(dateNum.gte(2013), l8.merge(l7), LAll);
     
      //If collection is empty, fill the imRed object with an empty image. This help avoid an error.
      //If collection is not empty, then apply redcutionTable function (produces monthly mosaics)
      var imRed = ee.Algorithms.If(ee.ImageCollection(LAll).size().gte(1),
                                    reductionImage(ee.ImageCollection(LAll),extraDate), 
                                    emptyColl);
      
      var resul = imRed;
      //Set Year Eval property as the year of the date
      resul = ee.Image(resul).set('YearEval',extraDate.format('Y'));
      return resul;
    
  };

    
  //Yearly. Used for contructing the annual images
  var yearFilter = function(date){
     
     //End Date is 1 year ahead of the date indicated by the entry taken from the dates list
      var endDate = ee.Date(date).advance(1,'year'); 
      var extraDate = ee.Date(date);
      
      //Filter Landsat collections
      //Must be done separately because pixel_qa values are different for L4-7 and L8
        var l4 = L4
            //Date filter
            .filterDate(date,endDate)
            //Area of interest filter
            .filterBounds(area)
            .select('pixel_qa');
        
            l4 = l4 
              .map(corte)
              .map(qaCount);
       
        var l5 = L5
            //Date filter
            .filterDate(date,endDate)
            //Area of interest filter
            .filterBounds(area)
            .select('pixel_qa');
        
            l5 = l5 
              .map(corte)
              .map(qaCount);
        
        var l7 = L7
            //Date filter
            .filterDate(date,endDate)
            //Area of interest filter
            .filterBounds(area)
            .select('pixel_qa');
        
            l7 = l7 
              .map(corte)
              .map(qaCount);
       
        var l8 = L8
            //Date filter
            .filterDate(date,endDate)
            //Area of interest filter
            .filterBounds(area)
            .select('pixel_qa');
        
            l8 = l8 
              .map(corte)
              .map(qaCount8);
      
      //Create an empty collection
      var emptyColl = ee.Image.constant(0).clip(area).rename('julianDay_ValidObs');
      
      //Get the year of the date
      var dateNum = ee.Date(date).format('Y');
      //Pass the year as a number
      dateNum = ee.Number.parse(dateNum);
      
      //Set LAll as emptyColl by default
      var LAll = emptyColl; 

      //Set some rules in order to merge different landsat collections according to the date year
      //This is used in order to avoid to merge empty collections
      LAll = ee.Algorithms.If(dateNum.lt(1984), l4, LAll);
      LAll = ee.Algorithms.If(dateNum.gte(1984).and(dateNum.lte(1993)), l5.merge(l4), LAll);
      LAll = ee.Algorithms.If(dateNum.gt(1993).and( dateNum.lt(1999)), l5, LAll);
      LAll = ee.Algorithms.If(dateNum.gte(1999).and(dateNum.lte(2012)), l7.merge(l5), LAll);
      LAll = ee.Algorithms.If(dateNum.gte(2013), l8.merge(l7), LAll);

      //If collection is empty, fill the infor with an empty image. This help avoid an error.
      //If collection is not empty, then apply redcutionTable function (produces monthly mosaics)
      var imRed = ee.Algorithms.If(ee.ImageCollection(LAll).size().gte(1),
                                    reductionImage(ee.ImageCollection(LAll),extraDate), 
                                    emptyColl);
      
      var resul = imRed;
      
      return resul;
    
  };

//2.7 Metadata extraction information
  //Extract metadata for each image
  var consulta = function(image) {
    var id = ee.String(image.get('LANDSAT_PRODUCT_ID'));
    var CloudCover = image.get('CLOUD_COVER');
    var CloudCoverLand = image.get('CLOUD_COVER_LAND');
    var date = ee.Date(image.get('system:time_start'));
    var sensor = ee.String(image.get('SPACECRAFT_ID'));
    var path = ee.Number(image.get('WRS_PATH'));
    var row = ee.Number(image.get('WRS_ROW'));
    var collection = ee.String(image.get('COLLECTION_CATEGORY'));
    var coll_num = ee.Number(image.get('COLLECTION_NUMBER'));
    var dataType = ee.String(image.get('DATA_TYPE'));
    var earthSunDist = ee.Number(image.get('EARTH_SUN_DISTANCE'));
    
    
    var dictionarySummary = ee.Dictionary({
          ID: id,
          cloud_pixel_percentage: CloudCover,
          cloud_coverage_assessment: CloudCoverLand,
          date: date,
          sensor: sensor,
          path: path,
          row: row,
          collection: collection,
          collection_num: coll_num,
          dataType: dataType,
          earthSunDist: earthSunDist, 
          
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
                  description: 'L4-8_SR_T1_MXjDayVObs_MonthStack'+month.toString()+'_'+resolution.toString()+'m',
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
                  description: 'L4-8_SR_T1_MXObs_'+year.toString()+'_'+resolution.toString()+'m',
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
          description: 'Landsat4-8_SR_T1_Metadata_'+y1+
                      '-'+y2,
          folder: folder,
          fileFormat: 'CSV'
        });
    };

//3
//----------------Construct mosaics and export tables-----------------------
  //3.1 Monthly mosaic

    var monthlySeq = ee.List.sequence(0,(12*(yTot+1)-7),1); 
    
    //Transform list of numbers into dates
    var datesList = monthlySeq.map(dateGen); 
    
    //Construct monthly mosaics of valid observations
    var meanFeatureCol = datesList.map(monthFilter);
    
    //Create an image collection from the monthly mosaics
    var expImColl = ee.ImageCollection.fromImages(meanFeatureCol);
    
  //3.2 Annual mosaic (histogram)
    
    var yearlySeq = ee.List.sequence(0,yTot,1); 
    
    //Transform list of numbers into dates
    var datesListYear = yearlySeq.map(dateGenYear);
    
    //Construct annual mosaics of valid observations
    var meanFeatureColYear = datesListYear.map(yearFilter);
    
    //Create an image collection from the annual mosaics
    var expImCollYear = ee.ImageCollection.fromImages(meanFeatureColYear);
  
//4 
//----------------Export images--------------------------------------
  //Export annual images using a loop
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

  var L4L8_SRT1Meta = ee.ImageCollection(L4.merge(L5).merge(L7).merge(L8))
      //Date filter
      .filterDate(mStart.toString()+'-01-01',mEnd.toString()+'-12-31')
      //Area of interest filter
      .filterBounds(prueba)
      .select('pixel_qa');
      
  var L4L8_SRT1Exp = ee.FeatureCollection(L4L8_SRT1Meta.map(consulta));
  
  exportMetadata(L4L8_SRT1Exp,mStart,mEnd);

