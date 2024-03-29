# -*- coding: utf-8 -*-
"""
Generated by ArcGIS ModelBuilder on : 2024-03-25 13:29:01
"""
import arcpy

def Geoprocessing():  # Geoprocessing_Workflow

    # To allow overwriting outputs change overwriteOutput option to True.
    arcpy.env.overwriteOutput = False

    # Check out any necessary licenses.
    arcpy.CheckOutExtension("spatial")

    arcpy.ImportToolbox(r"c:\program files\arcgis\pro\Resources\ArcToolbox\toolboxes\Data Management Tools.tbx")
    arcpy.ImportToolbox(r"c:\program files\arcgis\pro\Resources\ArcToolbox\toolboxes\Conversion Tools.tbx")
    Subadult_Bluestriped_Grunt_Full_Dataset_csv = "E:\\BIOL398_Patch_Reef_Residency\\Tabular_Data\\Subadult_Bluestriped_Grunt_Full_Dataset.csv"
    Scratch_gdb = "E:\\BIOL398_Patch_Reef_Residency\\GIS\\Geodatabases\\Scratch.gdb"
    UnifiedReefMap = "E:\\BIOL398_Patch_Reef_Residency\\GIS\\Data\\Unified_Reef_Map\\FWC_UnifiedFloridaReefMap_v2.0.gdb\\ReefMap\\UnifiedReefMap"
    Study_Domain_shp = "E:\\BIOL398_Patch_Reef_Residency\\GIS\\Data\\Study_Domain.shp"
    Patch_Reef_Residency_gdb = "E:\\BIOL398_Patch_Reef_Residency\\GIS\\Geodatabases\\Patch_Reef_Residency.gdb"
    Reef_Dist_asc = "E:\\BIOL398_Patch_Reef_Residency\\GIS\\Data\\Reef_Dist.asc"
    Subadult_Gray_Snapper_Full_Dataset_csv = "E:\\BIOL398_Patch_Reef_Residency\\Tabular_Data\\Subadult_Gray_Snapper_Full_Dataset.csv"

    # Process: Table to Table (Table To Table) (conversion)
    Subadult_Bluestriped_Grunt_Data_CES_MSc = arcpy.conversion.TableToTable(in_rows=Subadult_Bluestriped_Grunt_Full_Dataset_csv, out_path=Scratch_gdb, out_name="Subadult_Bluestriped_Grunt_Data_CES_MSc")[0]

    # Process: Make XY Event Layer (Make XY Event Layer) (management)
    Subadult_Bluestriped_Grunt_Data_CES_MSc_Layer = "Subadult_Bluestriped_Grunt_Data_CES_MSc_Layer"
    arcpy.management.MakeXYEventLayer(table=Subadult_Bluestriped_Grunt_Data_CES_MSc, in_x_field="LON_M", in_y_field="LAT_M", out_layer=Subadult_Bluestriped_Grunt_Data_CES_MSc_Layer, spatial_reference="PROJCS[\"NAD_1983_StatePlane_Florida_East_FIPS_0901\",GEOGCS[\"GCS_North_American_1983\",DATUM[\"D_North_American_1983\",SPHEROID[\"GRS_1980\",6378137.0,298.257222101]],PRIMEM[\"Greenwich\",0.0],UNIT[\"Degree\",0.0174532925199433]],PROJECTION[\"Transverse_Mercator\"],PARAMETER[\"False_Easting\",200000.0],PARAMETER[\"False_Northing\",0.0],PARAMETER[\"Central_Meridian\",-81.0],PARAMETER[\"Scale_Factor\",0.9999411764705882],PARAMETER[\"Latitude_Of_Origin\",24.33333333333333],UNIT[\"Meter\",1.0]];-5422800 -12693600 10000;-100000 10000;-100000 10000;0.001;0.001;0.001;IsHighPrecision")

    # Process: Feature Class to Feature Class (2) (Feature Class To Feature Class) (conversion)
    Subadult_Bluestriped_Grunt_CES_MSc = arcpy.conversion.FeatureClassToFeatureClass(in_features=Subadult_Bluestriped_Grunt_Data_CES_MSc_Layer, out_path=Scratch_gdb, out_name="Subadult_Bluestriped_Grunt_CES_MSc")[0]

    # Process: Feature Class to Feature Class (4) (Feature Class To Feature Class) (conversion)
    URM_Original = arcpy.conversion.FeatureClassToFeatureClass(in_features=UnifiedReefMap, out_path=Scratch_gdb, out_name="URM_Original")[0]

    # Process: Project (Project) (management)
    URM_Projected = "E:\\BIOL398_Patch_Reef_Residency\\GIS\\Geodatabases\\Scratch.gdb\\URM_Projected"
    arcpy.management.Project(in_dataset=URM_Original, out_dataset=URM_Projected, out_coor_system="PROJCS[\"NAD_1983_StatePlane_Florida_East_FIPS_0901\",GEOGCS[\"GCS_North_American_1983\",DATUM[\"D_North_American_1983\",SPHEROID[\"GRS_1980\",6378137.0,298.257222101]],PRIMEM[\"Greenwich\",0.0],UNIT[\"Degree\",0.0174532925199433]],PROJECTION[\"Transverse_Mercator\"],PARAMETER[\"False_Easting\",200000.0],PARAMETER[\"False_Northing\",0.0],PARAMETER[\"Central_Meridian\",-81.0],PARAMETER[\"Scale_Factor\",0.9999411764705882],PARAMETER[\"Latitude_Of_Origin\",24.33333333333333],UNIT[\"Meter\",1.0]]")

    # Process: Feature Class to Feature Class (Feature Class To Feature Class) (conversion)
    Study_Domain = arcpy.conversion.FeatureClassToFeatureClass(in_features=Study_Domain_shp, out_path=Patch_Reef_Residency_gdb, out_name="Study_Domain")[0]

    # Process: Clip (Clip) (analysis)
    URM_Clipped = "E:\\BIOL398_Patch_Reef_Residency\\GIS\\Geodatabases\\Scratch.gdb\\URM_Clipped"
    arcpy.analysis.Clip(in_features=URM_Projected, clip_features=Study_Domain, out_feature_class=URM_Clipped)

    # Process: Select (Select) (analysis)
    Patch_Reefs = "E:\\BIOL398_Patch_Reef_Residency\\GIS\\Geodatabases\\Patch_Reef_Residency.gdb\\Patch_Reefs"
    arcpy.analysis.Select(in_features=URM_Clipped, out_feature_class=Patch_Reefs, where_clause="ClassLv1 = 'Individual or Aggregated Patch Reef'")

    # Process: Add Geometry Attributes (Add Geometry Attributes) (management)
    Modified_Input_Features = arcpy.management.AddGeometryAttributes(Input_Features=Patch_Reefs, Geometry_Properties=["AREA", "PERIMETER_LENGTH"], Length_Unit="METERS", Area_Unit="SQUARE_METERS", Coordinate_System="PROJCS[\"NAD_1983_StatePlane_Florida_East_FIPS_0901\",GEOGCS[\"GCS_North_American_1983\",DATUM[\"D_North_American_1983\",SPHEROID[\"GRS_1980\",6378137.0,298.257222101]],PRIMEM[\"Greenwich\",0.0],UNIT[\"Degree\",0.0174532925199433]],PROJECTION[\"Transverse_Mercator\"],PARAMETER[\"False_Easting\",200000.0],PARAMETER[\"False_Northing\",0.0],PARAMETER[\"Central_Meridian\",-81.0],PARAMETER[\"Scale_Factor\",0.9999411764705882],PARAMETER[\"Latitude_Of_Origin\",24.33333333333333],UNIT[\"Meter\",1.0]]")[0]

    # Process: Add Field (Add Field) (management)
    if Modified_Input_Features:
        Patch_Reefs_7_ = arcpy.management.AddField(in_table=Patch_Reefs, field_name="PA_Ratio", field_type="DOUBLE")[0]

    # Process: Calculate Field (2) (Calculate Field) (management)
    if Modified_Input_Features:
        Patch_Reefs_3_ = arcpy.management.CalculateField(in_table=Patch_Reefs_7_, field="PA_Ratio", expression="[PERIMETER] / [POLY_AREA]", expression_type="VB")[0]

    # Process: Near (Near) (analysis)
    if Modified_Input_Features:
        Patch_Reefs_4_ = arcpy.analysis.Near(in_features=Patch_Reefs_3_, near_features=[Patch_Reefs_3_])[0]

    # Process: Add Field (2) (Add Field) (management)
    if Modified_Input_Features:
        Patch_Reefs_5_ = arcpy.management.AddField(in_table=Patch_Reefs_4_, field_name="Patch_ID", field_type="DOUBLE")[0]

    # Process: Calculate Field (Calculate Field) (management)
    if Modified_Input_Features:
        Patch_Reefs_6_ = arcpy.management.CalculateField(in_table=Patch_Reefs_5_, field="Patch_ID", expression="[OBJECTID_1]", expression_type="VB")[0]

    # Process: Spatial Join (Spatial Join) (analysis)
    Subadult_Bluestriped_Grunt_Patch_Reefs = "E:\\BIOL398_Patch_Reef_Residency\\GIS\\Geodatabases\\Scratch.gdb\\Subadult_Bluestriped_Grunt_Patch_Reefs"
    if Modified_Input_Features:
        arcpy.analysis.SpatialJoin(target_features=Subadult_Bluestriped_Grunt_CES_MSc, join_features=Patch_Reefs_6_, out_feature_class=Subadult_Bluestriped_Grunt_Patch_Reefs, join_type="KEEP_COMMON", match_option="CLOSEST", search_radius="15 Meters", distance_field_name="Survey_to_Reef_Dist")

    # Process: Buffer (Buffer) (analysis)
    Subadult_Bluestriped_Grunt_Patch_Reefs_Buffer = "E:\\BIOL398_Patch_Reef_Residency\\GIS\\Geodatabases\\Scratch.gdb\\Subadult_Bluestriped_Grunt_Patch_Reefs_Buffer"
    if Modified_Input_Features:
        arcpy.analysis.Buffer(in_features=Subadult_Bluestriped_Grunt_Patch_Reefs, out_feature_class=Subadult_Bluestriped_Grunt_Patch_Reefs_Buffer, buffer_distance_or_field="500 Meters")

    # Process: Tabulate Intersection (Tabulate Intersection) (analysis)
    Subadult_Bluestriped_Grunt_Seascape_Type_Long = "E:\\BIOL398_Patch_Reef_Residency\\GIS\\Geodatabases\\Scratch.gdb\\Subadult_Bluestriped_Grunt_Seascape_Type_Long"
    if Modified_Input_Features:
        arcpy.analysis.TabulateIntersection(in_zone_features=Subadult_Bluestriped_Grunt_Patch_Reefs_Buffer, zone_fields=["OBJECTID"], in_class_features=URM_Clipped, out_table=Subadult_Bluestriped_Grunt_Seascape_Type_Long, class_fields=["ClassLv0"], sum_fields=["Shape_Area"], out_units="SQUARE_METERS")

    # Process: Pivot Table (Pivot Table) (management)
    Subadult_Bluestriped_Grunt_Seascape_Type = "E:\\BIOL398_Patch_Reef_Residency\\GIS\\Geodatabases\\Patch_Reef_Residency.gdb\\Subadult_Bluestriped_Grunt_Seascape_Type"
    if Modified_Input_Features:
        arcpy.management.PivotTable(in_table=Subadult_Bluestriped_Grunt_Seascape_Type_Long, fields=["OBJECTID_1"], pivot_field="ClassLv0", value_field="Shape_Area", out_table=Subadult_Bluestriped_Grunt_Seascape_Type)

    # Process: Join Field (Join Field) (management)
    if Modified_Input_Features:
        Subadult_Bluestriped_Grunt_Patch_Reefs_2_ = arcpy.management.JoinField(in_data=Subadult_Bluestriped_Grunt_Patch_Reefs, in_field="OBJECTID", join_table=Subadult_Bluestriped_Grunt_Seascape_Type, join_field="OBJECTID_1", fields=["Coral_Reef_and_Hardbottom", "Not_Classified", "Seagrass", "Unconsolidated_Sediment"])[0]

    # Process: ASCII to Raster (ASCII To Raster) (conversion)
    Reef_Dist = "E:\\BIOL398_Patch_Reef_Residency\\GIS\\Geodatabases\\Scratch.gdb\\Reef_Dist"
    arcpy.conversion.ASCIIToRaster(in_ascii_file=Reef_Dist_asc, out_raster=Reef_Dist, data_type="FLOAT")
    Reef_Dist = arcpy.Raster(Reef_Dist)

    # Process: Extract Values to Points (Extract Values to Points) (sa)
    Subadult_Bluestriped_Grunt_Patch_Reefs_3_ = "E:\\BIOL398_Patch_Reef_Residency\\GIS\\Geodatabases\\Patch_Reef_Residency.gdb\\Subadult_Bluestriped_Grunt_Patch_Reefs"
    if Modified_Input_Features:
        arcpy.sa.ExtractValuesToPoints(Subadult_Bluestriped_Grunt_Patch_Reefs_2_, Reef_Dist, Subadult_Bluestriped_Grunt_Patch_Reefs_3_, "NONE", "VALUE_ONLY")

    # Process: Alter Field (Alter Field) (management)
    if Modified_Input_Features:
        Subadult_Bluestriped_Grunt_Patch_Reefs_4_ = arcpy.management.AlterField(in_table=Subadult_Bluestriped_Grunt_Patch_Reefs_3_, field="RASTERVALU", new_field_name="Reef_Dist")[0]

    # Process: Table To Excel (Table To Excel) (conversion)
    Subadult_Bluestriped_Grunt_Patch_Reef_Data_xls = "E:\\BIOL398_Patch_Reef_Residency\\Tabular_Data\\Subadult_Bluestriped_Grunt_Patch_Reef_Data.xls"
    if Modified_Input_Features:
        arcpy.conversion.TableToExcel(Input_Table=Subadult_Bluestriped_Grunt_Patch_Reefs_4_, Output_Excel_File=Subadult_Bluestriped_Grunt_Patch_Reef_Data_xls)

    # Process: Table to Table (2) (Table To Table) (conversion)
    Subadult_Gray_Snapper_Data_CES_MSc = arcpy.conversion.TableToTable(in_rows=Subadult_Gray_Snapper_Full_Dataset_csv, out_path=Scratch_gdb, out_name="Subadult_Gray_Snapper_Data_CES_MSc")[0]

    # Process: Make XY Event Layer (2) (Make XY Event Layer) (management)
    Subadult_Gray_Snapper_Data_CES_MSc_Layer = "Subadult_Gray_Snapper_Data_CES_MSc_Layer"
    arcpy.management.MakeXYEventLayer(table=Subadult_Gray_Snapper_Data_CES_MSc, in_x_field="LON_M", in_y_field="LAT_M", out_layer=Subadult_Gray_Snapper_Data_CES_MSc_Layer, spatial_reference="PROJCS[\"NAD_1983_StatePlane_Florida_East_FIPS_0901\",GEOGCS[\"GCS_North_American_1983\",DATUM[\"D_North_American_1983\",SPHEROID[\"GRS_1980\",6378137.0,298.257222101]],PRIMEM[\"Greenwich\",0.0],UNIT[\"Degree\",0.0174532925199433]],PROJECTION[\"Transverse_Mercator\"],PARAMETER[\"False_Easting\",200000.0],PARAMETER[\"False_Northing\",0.0],PARAMETER[\"Central_Meridian\",-81.0],PARAMETER[\"Scale_Factor\",0.9999411764705882],PARAMETER[\"Latitude_Of_Origin\",24.33333333333333],UNIT[\"Meter\",1.0]];-5422800 -12693600 10000;-100000 10000;-100000 10000;0.001;0.001;0.001;IsHighPrecision")

    # Process: Feature Class to Feature Class (3) (Feature Class To Feature Class) (conversion)
    Subadult_Gray_Snapper_CES_MSc = arcpy.conversion.FeatureClassToFeatureClass(in_features=Subadult_Gray_Snapper_Data_CES_MSc_Layer, out_path=Scratch_gdb, out_name="Subadult_Gray_Snapper_CES_MSc")[0]

    # Process: Spatial Join (2) (Spatial Join) (analysis)
    Subadult_Gray_Snapper_Patch_Reefs = "E:\\BIOL398_Patch_Reef_Residency\\GIS\\Geodatabases\\Scratch.gdb\\Subadult_Gray_Snapper_Patch_Reefs"
    if Modified_Input_Features:
        arcpy.analysis.SpatialJoin(target_features=Subadult_Gray_Snapper_CES_MSc, join_features=Patch_Reefs_6_, out_feature_class=Subadult_Gray_Snapper_Patch_Reefs, join_type="KEEP_COMMON", field_mapping="SPECIES_CD \"SPECIES_CD\" true true false 8000 Text 0 0,First,#,E:\\BIOL398_Patch_Reef_Residency\\GIS\\Geodatabases\\Scratch.gdb\\Subadult_Gray_Snapper,SPECIES_CD,0,7999;LIFE_STAGE \"LIFE_STAGE\" true true false 8000 Text 0 0,First,#,E:\\BIOL398_Patch_Reef_Residency\\GIS\\Geodatabases\\Scratch.gdb\\Subadult_Gray_Snapper,LIFE_STAGE,0,7999;PRES \"PRES\" true true false 4 Long 0 0,First,#,E:\\BIOL398_Patch_Reef_Residency\\GIS\\Geodatabases\\Scratch.gdb\\Subadult_Gray_Snapper,PRES,-1,-1;PRES2 \"PRES2\" true true false 8000 Text 0 0,First,#,E:\\BIOL398_Patch_Reef_Residency\\GIS\\Geodatabases\\Scratch.gdb\\Subadult_Gray_Snapper,PRES2,0,7999;N \"N\" true true false 8 Double 0 0,First,#,E:\\BIOL398_Patch_Reef_Residency\\GIS\\Geodatabases\\Scratch.gdb\\Subadult_Gray_Snapper,N,-1,-1;SOURCE \"SOURCE\" true true false 8000 Text 0 0,First,#,E:\\BIOL398_Patch_Reef_Residency\\GIS\\Geodatabases\\Scratch.gdb\\Subadult_Gray_Snapper,SOURCE,0,7999;LON_M \"LON_M\" true true false 8 Double 0 0,First,#,E:\\BIOL398_Patch_Reef_Residency\\GIS\\Geodatabases\\Scratch.gdb\\Subadult_Gray_Snapper,LON_M,-1,-1;LAT_M \"LAT_M\" true true false 8 Double 0 0,First,#,E:\\BIOL398_Patch_Reef_Residency\\GIS\\Geodatabases\\Scratch.gdb\\Subadult_Gray_Snapper,LAT_M,-1,-1;Habitat \"Habitat\" true true false 4 Long 0 0,First,#,E:\\BIOL398_Patch_Reef_Residency\\GIS\\Geodatabases\\Scratch.gdb\\Subadult_Gray_Snapper,Habitat,-1,-1;Mangrove_Dist \"Mangrove_Dist\" true true false 8 Double 0 0,First,#,E:\\BIOL398_Patch_Reef_Residency\\GIS\\Geodatabases\\Scratch.gdb\\Subadult_Gray_Snapper,Mangrove_Dist,-1,-1;Depth \"Depth\" true true false 8 Double 0 0,First,#,E:\\BIOL398_Patch_Reef_Residency\\GIS\\Geodatabases\\Scratch.gdb\\Subadult_Gray_Snapper,Depth,-1,-1;StDev_Depth \"StDev_Depth\" true true false 8 Double 0 0,First,#,E:\\BIOL398_Patch_Reef_Residency\\GIS\\Geodatabases\\Scratch.gdb\\Subadult_Gray_Snapper,StDev_Depth,-1,-1;Slope \"Slope\" true true false 8 Double 0 0,First,#,E:\\BIOL398_Patch_Reef_Residency\\GIS\\Geodatabases\\Scratch.gdb\\Subadult_Gray_Snapper,Slope,-1,-1;Curvature \"Curvature\" true true false 8 Double 0 0,First,#,E:\\BIOL398_Patch_Reef_Residency\\GIS\\Geodatabases\\Scratch.gdb\\Subadult_Gray_Snapper,Curvature,-1,-1;Plan_Curve \"Plan_Curve\" true true false 8 Double 0 0,First,#,E:\\BIOL398_Patch_Reef_Residency\\GIS\\Geodatabases\\Scratch.gdb\\Subadult_Gray_Snapper,Plan_Curve,-1,-1;BPI_Fine \"BPI_Fine\" true true false 4 Long 0 0,First,#,E:\\BIOL398_Patch_Reef_Residency\\GIS\\Geodatabases\\Scratch.gdb\\Subadult_Gray_Snapper,BPI_Fine,-1,-1;BPI_Broad \"BPI_Broad\" true true false 4 Long 0 0,First,#,E:\\BIOL398_Patch_Reef_Residency\\GIS\\Geodatabases\\Scratch.gdb\\Subadult_Gray_Snapper,BPI_Broad,-1,-1;Rugosity \"Rugosity\" true true false 8 Double 0 0,First,#,E:\\BIOL398_Patch_Reef_Residency\\GIS\\Geodatabases\\Scratch.gdb\\Subadult_Gray_Snapper,Rugosity,-1,-1;Mean_Sum_Temp \"Mean_Sum_Temp\" true true false 8 Double 0 0,First,#,E:\\BIOL398_Patch_Reef_Residency\\GIS\\Geodatabases\\Scratch.gdb\\Subadult_Gray_Snapper,Mean_Sum_Temp,-1,-1;Mean_Sum_DO \"Mean_Sum_DO\" true true false 8 Double 0 0,First,#,E:\\BIOL398_Patch_Reef_Residency\\GIS\\Geodatabases\\Scratch.gdb\\Subadult_Gray_Snapper,Mean_Sum_DO,-1,-1;Mean_Sum_Sal \"Mean_Sum_Sal\" true true false 8 Double 0 0,First,#,E:\\BIOL398_Patch_Reef_Residency\\GIS\\Geodatabases\\Scratch.gdb\\Subadult_Gray_Snapper,Mean_Sum_Sal,-1,-1;Mean_Win_Temp \"Mean_Win_Temp\" true true false 8 Double 0 0,First,#,E:\\BIOL398_Patch_Reef_Residency\\GIS\\Geodatabases\\Scratch.gdb\\Subadult_Gray_Snapper,Mean_Win_Temp,-1,-1;Mean_Win_DO \"Mean_Win_DO\" true true false 8 Double 0 0,First,#,E:\\BIOL398_Patch_Reef_Residency\\GIS\\Geodatabases\\Scratch.gdb\\Subadult_Gray_Snapper,Mean_Win_DO,-1,-1;Mean_Win_Sal \"Mean_Win_Sal\" true true false 8 Double 0 0,First,#,E:\\BIOL398_Patch_Reef_Residency\\GIS\\Geodatabases\\Scratch.gdb\\Subadult_Gray_Snapper,Mean_Win_Sal,-1,-1;OBJECTID \"OBJECTID\" true true false 4 Long 0 0,First,#,E:\\BIOL398_Patch_Reef_Residency\\GIS\\Geodatabases\\Patch_Reef_Residency.gdb\\Patch_Reefs,OBJECTID,-1,-1;Source_1 \"Source_1\" true true false 30 Text 0 0,First,#,E:\\BIOL398_Patch_Reef_Residency\\GIS\\Geodatabases\\Patch_Reef_Residency.gdb\\Patch_Reefs,Source,0,29;Zone_ \"Zone_\" true true false 50 Text 0 0,First,#,E:\\BIOL398_Patch_Reef_Residency\\GIS\\Geodatabases\\Patch_Reef_Residency.gdb\\Patch_Reefs,Zone_,0,49;GeoForm \"GeoForm\" true true false 50 Text 0 0,First,#,E:\\BIOL398_Patch_Reef_Residency\\GIS\\Geodatabases\\Patch_Reef_Residency.gdb\\Patch_Reefs,GeoForm,0,49;GeoformDet \"GeoformDet\" true true false 254 Text 0 0,First,#,E:\\BIOL398_Patch_Reef_Residency\\GIS\\Geodatabases\\Patch_Reef_Residency.gdb\\Patch_Reefs,GeoformDet,0,253;GeoformD_1 \"GeoformD_1\" true true false 254 Text 0 0,First,#,E:\\BIOL398_Patch_Reef_Residency\\GIS\\Geodatabases\\Patch_Reef_Residency.gdb\\Patch_Reefs,GeoformD_1,0,253;BioCover \"BioCover\" true true false 50 Text 0 0,First,#,E:\\BIOL398_Patch_Reef_Residency\\GIS\\Geodatabases\\Patch_Reef_Residency.gdb\\Patch_Reefs,BioCover,0,49;BioCoverDe \"BioCoverDe\" true true false 50 Text 0 0,First,#,E:\\BIOL398_Patch_Reef_Residency\\GIS\\Geodatabases\\Patch_Reef_Residency.gdb\\Patch_Reefs,BioCoverDe,0,49;PercentBio \"PercentBio\" true true false 50 Text 0 0,First,#,E:\\BIOL398_Patch_Reef_Residency\\GIS\\Geodatabases\\Patch_Reef_Residency.gdb\\Patch_Reefs,PercentBio,0,49;PercentCor \"PercentCor\" true true false 14 Text 0 0,First,#,E:\\BIOL398_Patch_Reef_Residency\\GIS\\Geodatabases\\Patch_Reef_Residency.gdb\\Patch_Reefs,PercentCor,0,13;CMECS_Geo \"CMECS_Geo\" true true false 20 Text 0 0,First,#,E:\\BIOL398_Patch_Reef_Residency\\GIS\\Geodatabases\\Patch_Reef_Residency.gdb\\Patch_Reefs,CMECS_Geo,0,19;CMECS_Bio \"CMECS_Bio\" true true false 20 Text 0 0,First,#,E:\\BIOL398_Patch_Reef_Residency\\GIS\\Geodatabases\\Patch_Reef_Residency.gdb\\Patch_Reefs,CMECS_Bio,0,19;ClassLv0 \"ClassLv0\" true true false 75 Text 0 0,First,#,E:\\BIOL398_Patch_Reef_Residency\\GIS\\Geodatabases\\Patch_Reef_Residency.gdb\\Patch_Reefs,ClassLv0,0,74;ClassLv1 \"ClassLv1\" true true false 100 Text 0 0,First,#,E:\\BIOL398_Patch_Reef_Residency\\GIS\\Geodatabases\\Patch_Reef_Residency.gdb\\Patch_Reefs,ClassLv1,0,99;ClassLv2 \"ClassLv2\" true true false 150 Text 0 0,First,#,E:\\BIOL398_Patch_Reef_Residency\\GIS\\Geodatabases\\Patch_Reef_Residency.gdb\\Patch_Reefs,ClassLv2,0,149;ClassLv3 \"ClassLv3\" true true false 150 Text 0 0,First,#,E:\\BIOL398_Patch_Reef_Residency\\GIS\\Geodatabases\\Patch_Reef_Residency.gdb\\Patch_Reefs,ClassLv3,0,149;ClassLv4 \"ClassLv4\" true true false 254 Text 0 0,First,#,E:\\BIOL398_Patch_Reef_Residency\\GIS\\Geodatabases\\Patch_Reef_Residency.gdb\\Patch_Reefs,ClassLv4,0,253;Shape_Leng \"Shape_Leng\" true true false 8 Double 0 0,First,#,E:\\BIOL398_Patch_Reef_Residency\\GIS\\Geodatabases\\Patch_Reef_Residency.gdb\\Patch_Reefs,Shape_Leng,-1,-1;Shape_Length \"Shape_Length\" false true true 8 Double 0 0,First,#,E:\\BIOL398_Patch_Reef_Residency\\GIS\\Geodatabases\\Patch_Reef_Residency.gdb\\Patch_Reefs,Shape_Length,-1,-1;Shape_Area \"Shape_Area\" false true true 8 Double 0 0,First,#,E:\\BIOL398_Patch_Reef_Residency\\GIS\\Geodatabases\\Patch_Reef_Residency.gdb\\Patch_Reefs,Shape_Area,-1,-1;POLY_AREA \"POLY_AREA\" true true false 8 Double 0 0,First,#,E:\\BIOL398_Patch_Reef_Residency\\GIS\\Geodatabases\\Patch_Reef_Residency.gdb\\Patch_Reefs,POLY_AREA,-1,-1;PERIMETER \"PERIMETER\" true true false 8 Double 0 0,First,#,E:\\BIOL398_Patch_Reef_Residency\\GIS\\Geodatabases\\Patch_Reef_Residency.gdb\\Patch_Reefs,PERIMETER,-1,-1;PA_Ratio \"PA_Ratio\" true true false 8 Double 0 0,First,#,E:\\BIOL398_Patch_Reef_Residency\\GIS\\Geodatabases\\Patch_Reef_Residency.gdb\\Patch_Reefs,PA_Ratio,-1,-1;NEAR_FID \"NEAR_FID\" true true false 4 Long 0 0,First,#,E:\\BIOL398_Patch_Reef_Residency\\GIS\\Geodatabases\\Patch_Reef_Residency.gdb\\Patch_Reefs,NEAR_FID,-1,-1;NEAR_DIST \"NEAR_DIST\" true true false 8 Double 0 0,First,#,E:\\BIOL398_Patch_Reef_Residency\\GIS\\Geodatabases\\Patch_Reef_Residency.gdb\\Patch_Reefs,NEAR_DIST,-1,-1;Patch_ID \"Patch_ID\" true true false 8 Double 0 0,First,#,E:\\BIOL398_Patch_Reef_Residency\\GIS\\Geodatabases\\Patch_Reef_Residency.gdb\\Patch_Reefs,Patch_ID,-1,-1", match_option="CLOSEST", search_radius="15 Meters", distance_field_name="Survey_to_Reef_Dist")

    # Process: Buffer (2) (Buffer) (analysis)
    Subadult_Gray_Snapper_Patch_Reefs_Buffer = "E:\\BIOL398_Patch_Reef_Residency\\GIS\\Geodatabases\\Scratch.gdb\\Subadult_Gray_Snapper_Patch_Reefs_Buffer"
    if Modified_Input_Features:
        arcpy.analysis.Buffer(in_features=Subadult_Gray_Snapper_Patch_Reefs, out_feature_class=Subadult_Gray_Snapper_Patch_Reefs_Buffer, buffer_distance_or_field="500 Meters")

    # Process: Tabulate Intersection (2) (Tabulate Intersection) (analysis)
    Subadult_Gray_Snapper_Seascape_Type_Long = "E:\\BIOL398_Patch_Reef_Residency\\GIS\\Geodatabases\\Scratch.gdb\\Subadult_Gray_Snapper_Seascape_Type_Long"
    if Modified_Input_Features:
        arcpy.analysis.TabulateIntersection(in_zone_features=Subadult_Gray_Snapper_Patch_Reefs_Buffer, zone_fields=["OBJECTID"], in_class_features=URM_Clipped, out_table=Subadult_Gray_Snapper_Seascape_Type_Long, class_fields=["ClassLv0"], sum_fields=["Shape_Area"], out_units="SQUARE_METERS")

    # Process: Pivot Table (2) (Pivot Table) (management)
    Subadult_Gray_Snapper_Seascape_Type = "E:\\BIOL398_Patch_Reef_Residency\\GIS\\Geodatabases\\Patch_Reef_Residency.gdb\\Subadult_Gray_Snapper_Seascape_Type"
    if Modified_Input_Features:
        arcpy.management.PivotTable(in_table=Subadult_Gray_Snapper_Seascape_Type_Long, fields=["OBJECTID_1"], pivot_field="ClassLv0", value_field="Shape_Area", out_table=Subadult_Gray_Snapper_Seascape_Type)

    # Process: Join Field (2) (Join Field) (management)
    if Modified_Input_Features:
        Subadult_Gray_Snapper_Patch_Reefs_2_ = arcpy.management.JoinField(in_data=Subadult_Gray_Snapper_Patch_Reefs, in_field="OBJECTID", join_table=Subadult_Gray_Snapper_Seascape_Type, join_field="OBJECTID_1", fields=["Coral_Reef_and_Hardbottom", "Not_Classified", "Seagrass", "Unconsolidated_Sediment"])[0]

    # Process: Extract Values to Points (2) (Extract Values to Points) (sa)
    Subadult_Gray_Snapper_Patch_Reefs_3_ = "E:\\BIOL398_Patch_Reef_Residency\\GIS\\Geodatabases\\Patch_Reef_Residency.gdb\\Subadult_Gray_Snapper_Patch_Reefs"
    if Modified_Input_Features:
        arcpy.sa.ExtractValuesToPoints(Subadult_Gray_Snapper_Patch_Reefs_2_, Reef_Dist, Subadult_Gray_Snapper_Patch_Reefs_3_, "NONE", "VALUE_ONLY")

    # Process: Alter Field (2) (Alter Field) (management)
    if Modified_Input_Features:
        Subadult_Gray_Snapper_Patch_Reefs_4_ = arcpy.management.AlterField(in_table=Subadult_Gray_Snapper_Patch_Reefs_3_, field="RASTERVALU", new_field_name="Reef_Dist")[0]

    # Process: Table To Excel (2) (Table To Excel) (conversion)
    Subadult_Gray_Snapper_Patch_Reef_Data_xls = "E:\\BIOL398_Patch_Reef_Residency\\Tabular_Data\\Subadult_Gray_Snapper_Patch_Reef_Data.xls"
    if Modified_Input_Features:
        arcpy.conversion.TableToExcel(Input_Table=Subadult_Gray_Snapper_Patch_Reefs_4_, Output_Excel_File=Subadult_Gray_Snapper_Patch_Reef_Data_xls)

if __name__ == '__main__':
    # Global Environment settings
    with arcpy.EnvManager(scratchWorkspace="D:\\Data\\Florida\\BIOL398_Patch_Reef_Residency\\GIS\\Geodatabases\\Patch_Reef_Residency.gdb", workspace="D:\\Data\\Florida\\BIOL398_Patch_Reef_Residency\\GIS\\Geodatabases\\Patch_Reef_Residency.gdb"):
        Geoprocessing()
