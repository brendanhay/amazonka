{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.SageMakerGeoSpatial.Lens
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMakerGeoSpatial.Lens
  ( -- * Operations

    -- ** DeleteEarthObservationJob
    deleteEarthObservationJob_arn,
    deleteEarthObservationJobResponse_httpStatus,

    -- ** DeleteVectorEnrichmentJob
    deleteVectorEnrichmentJob_arn,
    deleteVectorEnrichmentJobResponse_httpStatus,

    -- ** ExportEarthObservationJob
    exportEarthObservationJob_exportSourceImages,
    exportEarthObservationJob_arn,
    exportEarthObservationJob_executionRoleArn,
    exportEarthObservationJob_outputConfig,
    exportEarthObservationJobResponse_exportSourceImages,
    exportEarthObservationJobResponse_httpStatus,
    exportEarthObservationJobResponse_arn,
    exportEarthObservationJobResponse_creationTime,
    exportEarthObservationJobResponse_executionRoleArn,
    exportEarthObservationJobResponse_exportStatus,
    exportEarthObservationJobResponse_outputConfig,

    -- ** ExportVectorEnrichmentJob
    exportVectorEnrichmentJob_arn,
    exportVectorEnrichmentJob_executionRoleArn,
    exportVectorEnrichmentJob_outputConfig,
    exportVectorEnrichmentJobResponse_httpStatus,
    exportVectorEnrichmentJobResponse_arn,
    exportVectorEnrichmentJobResponse_creationTime,
    exportVectorEnrichmentJobResponse_executionRoleArn,
    exportVectorEnrichmentJobResponse_exportStatus,
    exportVectorEnrichmentJobResponse_outputConfig,

    -- ** GetEarthObservationJob
    getEarthObservationJob_arn,
    getEarthObservationJobResponse_errorDetails,
    getEarthObservationJobResponse_executionRoleArn,
    getEarthObservationJobResponse_exportErrorDetails,
    getEarthObservationJobResponse_exportStatus,
    getEarthObservationJobResponse_kmsKeyId,
    getEarthObservationJobResponse_outputBands,
    getEarthObservationJobResponse_tags,
    getEarthObservationJobResponse_httpStatus,
    getEarthObservationJobResponse_arn,
    getEarthObservationJobResponse_creationTime,
    getEarthObservationJobResponse_durationInSeconds,
    getEarthObservationJobResponse_inputConfig,
    getEarthObservationJobResponse_jobConfig,
    getEarthObservationJobResponse_name,
    getEarthObservationJobResponse_status,

    -- ** GetRasterDataCollection
    getRasterDataCollection_arn,
    getRasterDataCollectionResponse_tags,
    getRasterDataCollectionResponse_httpStatus,
    getRasterDataCollectionResponse_arn,
    getRasterDataCollectionResponse_description,
    getRasterDataCollectionResponse_descriptionPageUrl,
    getRasterDataCollectionResponse_imageSourceBands,
    getRasterDataCollectionResponse_name,
    getRasterDataCollectionResponse_supportedFilters,
    getRasterDataCollectionResponse_type,

    -- ** GetTile
    getTile_imageMask,
    getTile_outputDataType,
    getTile_outputFormat,
    getTile_propertyFilters,
    getTile_timeRangeFilter,
    getTile_arn,
    getTile_imageAssets,
    getTile_target,
    getTile_x,
    getTile_y,
    getTile_z,
    getTileResponse_httpStatus,
    getTileResponse_binaryFile,

    -- ** GetVectorEnrichmentJob
    getVectorEnrichmentJob_arn,
    getVectorEnrichmentJobResponse_errorDetails,
    getVectorEnrichmentJobResponse_exportErrorDetails,
    getVectorEnrichmentJobResponse_exportStatus,
    getVectorEnrichmentJobResponse_kmsKeyId,
    getVectorEnrichmentJobResponse_tags,
    getVectorEnrichmentJobResponse_httpStatus,
    getVectorEnrichmentJobResponse_arn,
    getVectorEnrichmentJobResponse_creationTime,
    getVectorEnrichmentJobResponse_durationInSeconds,
    getVectorEnrichmentJobResponse_executionRoleArn,
    getVectorEnrichmentJobResponse_inputConfig,
    getVectorEnrichmentJobResponse_jobConfig,
    getVectorEnrichmentJobResponse_name,
    getVectorEnrichmentJobResponse_status,
    getVectorEnrichmentJobResponse_type,

    -- ** ListEarthObservationJobs
    listEarthObservationJobs_maxResults,
    listEarthObservationJobs_nextToken,
    listEarthObservationJobs_sortBy,
    listEarthObservationJobs_sortOrder,
    listEarthObservationJobs_statusEquals,
    listEarthObservationJobsResponse_nextToken,
    listEarthObservationJobsResponse_httpStatus,
    listEarthObservationJobsResponse_earthObservationJobSummaries,

    -- ** ListRasterDataCollections
    listRasterDataCollections_maxResults,
    listRasterDataCollections_nextToken,
    listRasterDataCollectionsResponse_nextToken,
    listRasterDataCollectionsResponse_httpStatus,
    listRasterDataCollectionsResponse_rasterDataCollectionSummaries,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** ListVectorEnrichmentJobs
    listVectorEnrichmentJobs_maxResults,
    listVectorEnrichmentJobs_nextToken,
    listVectorEnrichmentJobs_sortBy,
    listVectorEnrichmentJobs_sortOrder,
    listVectorEnrichmentJobs_statusEquals,
    listVectorEnrichmentJobsResponse_nextToken,
    listVectorEnrichmentJobsResponse_httpStatus,
    listVectorEnrichmentJobsResponse_vectorEnrichmentJobSummaries,

    -- ** SearchRasterDataCollection
    searchRasterDataCollection_nextToken,
    searchRasterDataCollection_arn,
    searchRasterDataCollection_rasterDataCollectionQuery,
    searchRasterDataCollectionResponse_items,
    searchRasterDataCollectionResponse_nextToken,
    searchRasterDataCollectionResponse_httpStatus,
    searchRasterDataCollectionResponse_approximateResultCount,

    -- ** StartEarthObservationJob
    startEarthObservationJob_clientToken,
    startEarthObservationJob_executionRoleArn,
    startEarthObservationJob_kmsKeyId,
    startEarthObservationJob_tags,
    startEarthObservationJob_inputConfig,
    startEarthObservationJob_jobConfig,
    startEarthObservationJob_name,
    startEarthObservationJobResponse_executionRoleArn,
    startEarthObservationJobResponse_inputConfig,
    startEarthObservationJobResponse_kmsKeyId,
    startEarthObservationJobResponse_tags,
    startEarthObservationJobResponse_httpStatus,
    startEarthObservationJobResponse_arn,
    startEarthObservationJobResponse_creationTime,
    startEarthObservationJobResponse_durationInSeconds,
    startEarthObservationJobResponse_jobConfig,
    startEarthObservationJobResponse_name,
    startEarthObservationJobResponse_status,

    -- ** StartVectorEnrichmentJob
    startVectorEnrichmentJob_clientToken,
    startVectorEnrichmentJob_kmsKeyId,
    startVectorEnrichmentJob_tags,
    startVectorEnrichmentJob_executionRoleArn,
    startVectorEnrichmentJob_inputConfig,
    startVectorEnrichmentJob_jobConfig,
    startVectorEnrichmentJob_name,
    startVectorEnrichmentJobResponse_kmsKeyId,
    startVectorEnrichmentJobResponse_tags,
    startVectorEnrichmentJobResponse_httpStatus,
    startVectorEnrichmentJobResponse_arn,
    startVectorEnrichmentJobResponse_creationTime,
    startVectorEnrichmentJobResponse_durationInSeconds,
    startVectorEnrichmentJobResponse_executionRoleArn,
    startVectorEnrichmentJobResponse_inputConfig,
    startVectorEnrichmentJobResponse_jobConfig,
    startVectorEnrichmentJobResponse_name,
    startVectorEnrichmentJobResponse_status,
    startVectorEnrichmentJobResponse_type,

    -- ** StopEarthObservationJob
    stopEarthObservationJob_arn,
    stopEarthObservationJobResponse_httpStatus,

    -- ** StopVectorEnrichmentJob
    stopVectorEnrichmentJob_arn,
    stopVectorEnrichmentJobResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- * Types

    -- ** AreaOfInterest
    areaOfInterest_areaOfInterestGeometry,

    -- ** AreaOfInterestGeometry
    areaOfInterestGeometry_multiPolygonGeometry,
    areaOfInterestGeometry_polygonGeometry,

    -- ** AssetValue
    assetValue_href,

    -- ** BandMathConfigInput
    bandMathConfigInput_customIndices,
    bandMathConfigInput_predefinedIndices,

    -- ** CloudMaskingConfigInput

    -- ** CloudRemovalConfigInput
    cloudRemovalConfigInput_algorithmName,
    cloudRemovalConfigInput_interpolationValue,
    cloudRemovalConfigInput_targetBands,

    -- ** CustomIndicesInput
    customIndicesInput_operations,

    -- ** EarthObservationJobErrorDetails
    earthObservationJobErrorDetails_message,
    earthObservationJobErrorDetails_type,

    -- ** EoCloudCoverInput
    eoCloudCoverInput_lowerBound,
    eoCloudCoverInput_upperBound,

    -- ** EojDataSourceConfigInput
    eojDataSourceConfigInput_s3Data,

    -- ** ExportErrorDetails
    exportErrorDetails_exportResults,
    exportErrorDetails_exportSourceImages,

    -- ** ExportErrorDetailsOutput
    exportErrorDetailsOutput_message,
    exportErrorDetailsOutput_type,

    -- ** ExportS3DataInput
    exportS3DataInput_kmsKeyId,
    exportS3DataInput_s3Uri,

    -- ** ExportVectorEnrichmentJobOutputConfig
    exportVectorEnrichmentJobOutputConfig_s3Data,

    -- ** Filter
    filter_maximum,
    filter_minimum,
    filter_name,
    filter_type,

    -- ** GeoMosaicConfigInput
    geoMosaicConfigInput_algorithmName,
    geoMosaicConfigInput_targetBands,

    -- ** Geometry
    geometry_coordinates,
    geometry_type,

    -- ** InputConfigInput
    inputConfigInput_dataSourceConfig,
    inputConfigInput_previousEarthObservationJobArn,
    inputConfigInput_rasterDataCollectionQuery,

    -- ** InputConfigOutput
    inputConfigOutput_dataSourceConfig,
    inputConfigOutput_previousEarthObservationJobArn,
    inputConfigOutput_rasterDataCollectionQuery,

    -- ** ItemSource
    itemSource_assets,
    itemSource_properties,
    itemSource_dateTime,
    itemSource_geometry,
    itemSource_id,

    -- ** JobConfigInput
    jobConfigInput_bandMathConfig,
    jobConfigInput_cloudMaskingConfig,
    jobConfigInput_cloudRemovalConfig,
    jobConfigInput_geoMosaicConfig,
    jobConfigInput_landCoverSegmentationConfig,
    jobConfigInput_resamplingConfig,
    jobConfigInput_stackConfig,
    jobConfigInput_temporalStatisticsConfig,
    jobConfigInput_zonalStatisticsConfig,

    -- ** LandCoverSegmentationConfigInput

    -- ** LandsatCloudCoverLandInput
    landsatCloudCoverLandInput_lowerBound,
    landsatCloudCoverLandInput_upperBound,

    -- ** ListEarthObservationJobOutputConfig
    listEarthObservationJobOutputConfig_tags,
    listEarthObservationJobOutputConfig_arn,
    listEarthObservationJobOutputConfig_creationTime,
    listEarthObservationJobOutputConfig_durationInSeconds,
    listEarthObservationJobOutputConfig_name,
    listEarthObservationJobOutputConfig_operationType,
    listEarthObservationJobOutputConfig_status,

    -- ** ListVectorEnrichmentJobOutputConfig
    listVectorEnrichmentJobOutputConfig_tags,
    listVectorEnrichmentJobOutputConfig_arn,
    listVectorEnrichmentJobOutputConfig_creationTime,
    listVectorEnrichmentJobOutputConfig_durationInSeconds,
    listVectorEnrichmentJobOutputConfig_name,
    listVectorEnrichmentJobOutputConfig_status,
    listVectorEnrichmentJobOutputConfig_type,

    -- ** MapMatchingConfig
    mapMatchingConfig_idAttributeName,
    mapMatchingConfig_timestampAttributeName,
    mapMatchingConfig_xAttributeName,
    mapMatchingConfig_yAttributeName,

    -- ** MultiPolygonGeometryInput
    multiPolygonGeometryInput_coordinates,

    -- ** Operation
    operation_outputType,
    operation_equation,
    operation_name,

    -- ** OutputBand
    outputBand_bandName,
    outputBand_outputDataType,

    -- ** OutputConfigInput
    outputConfigInput_s3Data,

    -- ** OutputResolutionResamplingInput
    outputResolutionResamplingInput_userDefined,

    -- ** OutputResolutionStackInput
    outputResolutionStackInput_predefined,
    outputResolutionStackInput_userDefined,

    -- ** PlatformInput
    platformInput_comparisonOperator,
    platformInput_value,

    -- ** PolygonGeometryInput
    polygonGeometryInput_coordinates,

    -- ** Properties
    properties_eoCloudCover,
    properties_landsatCloudCoverLand,
    properties_platform,
    properties_viewOffNadir,
    properties_viewSunAzimuth,
    properties_viewSunElevation,

    -- ** Property
    property_eoCloudCover,
    property_landsatCloudCoverLand,
    property_platform,
    property_viewOffNadir,
    property_viewSunAzimuth,
    property_viewSunElevation,

    -- ** PropertyFilter
    propertyFilter_property,

    -- ** PropertyFilters
    propertyFilters_logicalOperator,
    propertyFilters_properties,

    -- ** RasterDataCollectionMetadata
    rasterDataCollectionMetadata_descriptionPageUrl,
    rasterDataCollectionMetadata_tags,
    rasterDataCollectionMetadata_arn,
    rasterDataCollectionMetadata_description,
    rasterDataCollectionMetadata_name,
    rasterDataCollectionMetadata_supportedFilters,
    rasterDataCollectionMetadata_type,

    -- ** RasterDataCollectionQueryInput
    rasterDataCollectionQueryInput_areaOfInterest,
    rasterDataCollectionQueryInput_propertyFilters,
    rasterDataCollectionQueryInput_rasterDataCollectionArn,
    rasterDataCollectionQueryInput_timeRangeFilter,

    -- ** RasterDataCollectionQueryOutput
    rasterDataCollectionQueryOutput_areaOfInterest,
    rasterDataCollectionQueryOutput_propertyFilters,
    rasterDataCollectionQueryOutput_rasterDataCollectionArn,
    rasterDataCollectionQueryOutput_rasterDataCollectionName,
    rasterDataCollectionQueryOutput_timeRangeFilter,

    -- ** RasterDataCollectionQueryWithBandFilterInput
    rasterDataCollectionQueryWithBandFilterInput_areaOfInterest,
    rasterDataCollectionQueryWithBandFilterInput_bandFilter,
    rasterDataCollectionQueryWithBandFilterInput_propertyFilters,
    rasterDataCollectionQueryWithBandFilterInput_timeRangeFilter,

    -- ** ResamplingConfigInput
    resamplingConfigInput_algorithmName,
    resamplingConfigInput_targetBands,
    resamplingConfigInput_outputResolution,

    -- ** ReverseGeocodingConfig
    reverseGeocodingConfig_xAttributeName,
    reverseGeocodingConfig_yAttributeName,

    -- ** S3DataInput
    s3DataInput_kmsKeyId,
    s3DataInput_metadataProvider,
    s3DataInput_s3Uri,

    -- ** StackConfigInput
    stackConfigInput_outputResolution,
    stackConfigInput_targetBands,

    -- ** TemporalStatisticsConfigInput
    temporalStatisticsConfigInput_groupBy,
    temporalStatisticsConfigInput_targetBands,
    temporalStatisticsConfigInput_statistics,

    -- ** TimeRangeFilterInput
    timeRangeFilterInput_endTime,
    timeRangeFilterInput_startTime,

    -- ** UserDefined
    userDefined_unit,
    userDefined_value,

    -- ** VectorEnrichmentJobConfig
    vectorEnrichmentJobConfig_mapMatchingConfig,
    vectorEnrichmentJobConfig_reverseGeocodingConfig,

    -- ** VectorEnrichmentJobDataSourceConfigInput
    vectorEnrichmentJobDataSourceConfigInput_s3Data,

    -- ** VectorEnrichmentJobErrorDetails
    vectorEnrichmentJobErrorDetails_errorMessage,
    vectorEnrichmentJobErrorDetails_errorType,

    -- ** VectorEnrichmentJobExportErrorDetails
    vectorEnrichmentJobExportErrorDetails_message,
    vectorEnrichmentJobExportErrorDetails_type,

    -- ** VectorEnrichmentJobInputConfig
    vectorEnrichmentJobInputConfig_dataSourceConfig,
    vectorEnrichmentJobInputConfig_documentType,

    -- ** VectorEnrichmentJobS3Data
    vectorEnrichmentJobS3Data_kmsKeyId,
    vectorEnrichmentJobS3Data_s3Uri,

    -- ** ViewOffNadirInput
    viewOffNadirInput_lowerBound,
    viewOffNadirInput_upperBound,

    -- ** ViewSunAzimuthInput
    viewSunAzimuthInput_lowerBound,
    viewSunAzimuthInput_upperBound,

    -- ** ViewSunElevationInput
    viewSunElevationInput_lowerBound,
    viewSunElevationInput_upperBound,

    -- ** ZonalStatisticsConfigInput
    zonalStatisticsConfigInput_targetBands,
    zonalStatisticsConfigInput_statistics,
    zonalStatisticsConfigInput_zoneS3Path,
  )
where

import Amazonka.SageMakerGeoSpatial.DeleteEarthObservationJob
import Amazonka.SageMakerGeoSpatial.DeleteVectorEnrichmentJob
import Amazonka.SageMakerGeoSpatial.ExportEarthObservationJob
import Amazonka.SageMakerGeoSpatial.ExportVectorEnrichmentJob
import Amazonka.SageMakerGeoSpatial.GetEarthObservationJob
import Amazonka.SageMakerGeoSpatial.GetRasterDataCollection
import Amazonka.SageMakerGeoSpatial.GetTile
import Amazonka.SageMakerGeoSpatial.GetVectorEnrichmentJob
import Amazonka.SageMakerGeoSpatial.ListEarthObservationJobs
import Amazonka.SageMakerGeoSpatial.ListRasterDataCollections
import Amazonka.SageMakerGeoSpatial.ListTagsForResource
import Amazonka.SageMakerGeoSpatial.ListVectorEnrichmentJobs
import Amazonka.SageMakerGeoSpatial.SearchRasterDataCollection
import Amazonka.SageMakerGeoSpatial.StartEarthObservationJob
import Amazonka.SageMakerGeoSpatial.StartVectorEnrichmentJob
import Amazonka.SageMakerGeoSpatial.StopEarthObservationJob
import Amazonka.SageMakerGeoSpatial.StopVectorEnrichmentJob
import Amazonka.SageMakerGeoSpatial.TagResource
import Amazonka.SageMakerGeoSpatial.Types.AreaOfInterest
import Amazonka.SageMakerGeoSpatial.Types.AreaOfInterestGeometry
import Amazonka.SageMakerGeoSpatial.Types.AssetValue
import Amazonka.SageMakerGeoSpatial.Types.BandMathConfigInput
import Amazonka.SageMakerGeoSpatial.Types.CloudMaskingConfigInput
import Amazonka.SageMakerGeoSpatial.Types.CloudRemovalConfigInput
import Amazonka.SageMakerGeoSpatial.Types.CustomIndicesInput
import Amazonka.SageMakerGeoSpatial.Types.EarthObservationJobErrorDetails
import Amazonka.SageMakerGeoSpatial.Types.EoCloudCoverInput
import Amazonka.SageMakerGeoSpatial.Types.EojDataSourceConfigInput
import Amazonka.SageMakerGeoSpatial.Types.ExportErrorDetails
import Amazonka.SageMakerGeoSpatial.Types.ExportErrorDetailsOutput
import Amazonka.SageMakerGeoSpatial.Types.ExportS3DataInput
import Amazonka.SageMakerGeoSpatial.Types.ExportVectorEnrichmentJobOutputConfig
import Amazonka.SageMakerGeoSpatial.Types.Filter
import Amazonka.SageMakerGeoSpatial.Types.GeoMosaicConfigInput
import Amazonka.SageMakerGeoSpatial.Types.Geometry
import Amazonka.SageMakerGeoSpatial.Types.InputConfigInput
import Amazonka.SageMakerGeoSpatial.Types.InputConfigOutput
import Amazonka.SageMakerGeoSpatial.Types.ItemSource
import Amazonka.SageMakerGeoSpatial.Types.JobConfigInput
import Amazonka.SageMakerGeoSpatial.Types.LandCoverSegmentationConfigInput
import Amazonka.SageMakerGeoSpatial.Types.LandsatCloudCoverLandInput
import Amazonka.SageMakerGeoSpatial.Types.ListEarthObservationJobOutputConfig
import Amazonka.SageMakerGeoSpatial.Types.ListVectorEnrichmentJobOutputConfig
import Amazonka.SageMakerGeoSpatial.Types.MapMatchingConfig
import Amazonka.SageMakerGeoSpatial.Types.MultiPolygonGeometryInput
import Amazonka.SageMakerGeoSpatial.Types.Operation
import Amazonka.SageMakerGeoSpatial.Types.OutputBand
import Amazonka.SageMakerGeoSpatial.Types.OutputConfigInput
import Amazonka.SageMakerGeoSpatial.Types.OutputResolutionResamplingInput
import Amazonka.SageMakerGeoSpatial.Types.OutputResolutionStackInput
import Amazonka.SageMakerGeoSpatial.Types.PlatformInput
import Amazonka.SageMakerGeoSpatial.Types.PolygonGeometryInput
import Amazonka.SageMakerGeoSpatial.Types.Properties
import Amazonka.SageMakerGeoSpatial.Types.Property
import Amazonka.SageMakerGeoSpatial.Types.PropertyFilter
import Amazonka.SageMakerGeoSpatial.Types.PropertyFilters
import Amazonka.SageMakerGeoSpatial.Types.RasterDataCollectionMetadata
import Amazonka.SageMakerGeoSpatial.Types.RasterDataCollectionQueryInput
import Amazonka.SageMakerGeoSpatial.Types.RasterDataCollectionQueryOutput
import Amazonka.SageMakerGeoSpatial.Types.RasterDataCollectionQueryWithBandFilterInput
import Amazonka.SageMakerGeoSpatial.Types.ResamplingConfigInput
import Amazonka.SageMakerGeoSpatial.Types.ReverseGeocodingConfig
import Amazonka.SageMakerGeoSpatial.Types.S3DataInput
import Amazonka.SageMakerGeoSpatial.Types.StackConfigInput
import Amazonka.SageMakerGeoSpatial.Types.TemporalStatisticsConfigInput
import Amazonka.SageMakerGeoSpatial.Types.TimeRangeFilterInput
import Amazonka.SageMakerGeoSpatial.Types.UserDefined
import Amazonka.SageMakerGeoSpatial.Types.VectorEnrichmentJobConfig
import Amazonka.SageMakerGeoSpatial.Types.VectorEnrichmentJobDataSourceConfigInput
import Amazonka.SageMakerGeoSpatial.Types.VectorEnrichmentJobErrorDetails
import Amazonka.SageMakerGeoSpatial.Types.VectorEnrichmentJobExportErrorDetails
import Amazonka.SageMakerGeoSpatial.Types.VectorEnrichmentJobInputConfig
import Amazonka.SageMakerGeoSpatial.Types.VectorEnrichmentJobS3Data
import Amazonka.SageMakerGeoSpatial.Types.ViewOffNadirInput
import Amazonka.SageMakerGeoSpatial.Types.ViewSunAzimuthInput
import Amazonka.SageMakerGeoSpatial.Types.ViewSunElevationInput
import Amazonka.SageMakerGeoSpatial.Types.ZonalStatisticsConfigInput
import Amazonka.SageMakerGeoSpatial.UntagResource
