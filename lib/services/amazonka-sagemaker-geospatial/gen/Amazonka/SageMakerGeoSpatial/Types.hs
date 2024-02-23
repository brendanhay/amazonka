{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.SageMakerGeoSpatial.Types
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMakerGeoSpatial.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _AccessDeniedException,
    _ConflictException,
    _InternalServerException,
    _ResourceNotFoundException,
    _ServiceQuotaExceededException,
    _ThrottlingException,
    _ValidationException,

    -- * AlgorithmNameCloudRemoval
    AlgorithmNameCloudRemoval (..),

    -- * AlgorithmNameGeoMosaic
    AlgorithmNameGeoMosaic (..),

    -- * AlgorithmNameResampling
    AlgorithmNameResampling (..),

    -- * ComparisonOperator
    ComparisonOperator (..),

    -- * DataCollectionType
    DataCollectionType (..),

    -- * EarthObservationJobErrorType
    EarthObservationJobErrorType (..),

    -- * EarthObservationJobExportStatus
    EarthObservationJobExportStatus (..),

    -- * EarthObservationJobStatus
    EarthObservationJobStatus (..),

    -- * ExportErrorType
    ExportErrorType (..),

    -- * GroupBy
    GroupBy (..),

    -- * LogicalOperator
    LogicalOperator (..),

    -- * MetadataProvider
    MetadataProvider (..),

    -- * OutputType
    OutputType (..),

    -- * PredefinedResolution
    PredefinedResolution (..),

    -- * SortOrder
    SortOrder (..),

    -- * TargetOptions
    TargetOptions (..),

    -- * TemporalStatistics
    TemporalStatistics (..),

    -- * Unit
    Unit (..),

    -- * VectorEnrichmentJobDocumentType
    VectorEnrichmentJobDocumentType (..),

    -- * VectorEnrichmentJobErrorType
    VectorEnrichmentJobErrorType (..),

    -- * VectorEnrichmentJobExportErrorType
    VectorEnrichmentJobExportErrorType (..),

    -- * VectorEnrichmentJobExportStatus
    VectorEnrichmentJobExportStatus (..),

    -- * VectorEnrichmentJobStatus
    VectorEnrichmentJobStatus (..),

    -- * VectorEnrichmentJobType
    VectorEnrichmentJobType (..),

    -- * ZonalStatistics
    ZonalStatistics (..),

    -- * AreaOfInterest
    AreaOfInterest (..),
    newAreaOfInterest,
    areaOfInterest_areaOfInterestGeometry,

    -- * AreaOfInterestGeometry
    AreaOfInterestGeometry (..),
    newAreaOfInterestGeometry,
    areaOfInterestGeometry_multiPolygonGeometry,
    areaOfInterestGeometry_polygonGeometry,

    -- * AssetValue
    AssetValue (..),
    newAssetValue,
    assetValue_href,

    -- * BandMathConfigInput
    BandMathConfigInput (..),
    newBandMathConfigInput,
    bandMathConfigInput_customIndices,
    bandMathConfigInput_predefinedIndices,

    -- * CloudMaskingConfigInput
    CloudMaskingConfigInput (..),
    newCloudMaskingConfigInput,

    -- * CloudRemovalConfigInput
    CloudRemovalConfigInput (..),
    newCloudRemovalConfigInput,
    cloudRemovalConfigInput_algorithmName,
    cloudRemovalConfigInput_interpolationValue,
    cloudRemovalConfigInput_targetBands,

    -- * CustomIndicesInput
    CustomIndicesInput (..),
    newCustomIndicesInput,
    customIndicesInput_operations,

    -- * EarthObservationJobErrorDetails
    EarthObservationJobErrorDetails (..),
    newEarthObservationJobErrorDetails,
    earthObservationJobErrorDetails_message,
    earthObservationJobErrorDetails_type,

    -- * EoCloudCoverInput
    EoCloudCoverInput (..),
    newEoCloudCoverInput,
    eoCloudCoverInput_lowerBound,
    eoCloudCoverInput_upperBound,

    -- * EojDataSourceConfigInput
    EojDataSourceConfigInput (..),
    newEojDataSourceConfigInput,
    eojDataSourceConfigInput_s3Data,

    -- * ExportErrorDetails
    ExportErrorDetails (..),
    newExportErrorDetails,
    exportErrorDetails_exportResults,
    exportErrorDetails_exportSourceImages,

    -- * ExportErrorDetailsOutput
    ExportErrorDetailsOutput (..),
    newExportErrorDetailsOutput,
    exportErrorDetailsOutput_message,
    exportErrorDetailsOutput_type,

    -- * ExportS3DataInput
    ExportS3DataInput (..),
    newExportS3DataInput,
    exportS3DataInput_kmsKeyId,
    exportS3DataInput_s3Uri,

    -- * ExportVectorEnrichmentJobOutputConfig
    ExportVectorEnrichmentJobOutputConfig (..),
    newExportVectorEnrichmentJobOutputConfig,
    exportVectorEnrichmentJobOutputConfig_s3Data,

    -- * Filter
    Filter (..),
    newFilter,
    filter_maximum,
    filter_minimum,
    filter_name,
    filter_type,

    -- * GeoMosaicConfigInput
    GeoMosaicConfigInput (..),
    newGeoMosaicConfigInput,
    geoMosaicConfigInput_algorithmName,
    geoMosaicConfigInput_targetBands,

    -- * Geometry
    Geometry (..),
    newGeometry,
    geometry_coordinates,
    geometry_type,

    -- * InputConfigInput
    InputConfigInput (..),
    newInputConfigInput,
    inputConfigInput_dataSourceConfig,
    inputConfigInput_previousEarthObservationJobArn,
    inputConfigInput_rasterDataCollectionQuery,

    -- * InputConfigOutput
    InputConfigOutput (..),
    newInputConfigOutput,
    inputConfigOutput_dataSourceConfig,
    inputConfigOutput_previousEarthObservationJobArn,
    inputConfigOutput_rasterDataCollectionQuery,

    -- * ItemSource
    ItemSource (..),
    newItemSource,
    itemSource_assets,
    itemSource_properties,
    itemSource_dateTime,
    itemSource_geometry,
    itemSource_id,

    -- * JobConfigInput
    JobConfigInput (..),
    newJobConfigInput,
    jobConfigInput_bandMathConfig,
    jobConfigInput_cloudMaskingConfig,
    jobConfigInput_cloudRemovalConfig,
    jobConfigInput_geoMosaicConfig,
    jobConfigInput_landCoverSegmentationConfig,
    jobConfigInput_resamplingConfig,
    jobConfigInput_stackConfig,
    jobConfigInput_temporalStatisticsConfig,
    jobConfigInput_zonalStatisticsConfig,

    -- * LandCoverSegmentationConfigInput
    LandCoverSegmentationConfigInput (..),
    newLandCoverSegmentationConfigInput,

    -- * LandsatCloudCoverLandInput
    LandsatCloudCoverLandInput (..),
    newLandsatCloudCoverLandInput,
    landsatCloudCoverLandInput_lowerBound,
    landsatCloudCoverLandInput_upperBound,

    -- * ListEarthObservationJobOutputConfig
    ListEarthObservationJobOutputConfig (..),
    newListEarthObservationJobOutputConfig,
    listEarthObservationJobOutputConfig_tags,
    listEarthObservationJobOutputConfig_arn,
    listEarthObservationJobOutputConfig_creationTime,
    listEarthObservationJobOutputConfig_durationInSeconds,
    listEarthObservationJobOutputConfig_name,
    listEarthObservationJobOutputConfig_operationType,
    listEarthObservationJobOutputConfig_status,

    -- * ListVectorEnrichmentJobOutputConfig
    ListVectorEnrichmentJobOutputConfig (..),
    newListVectorEnrichmentJobOutputConfig,
    listVectorEnrichmentJobOutputConfig_tags,
    listVectorEnrichmentJobOutputConfig_arn,
    listVectorEnrichmentJobOutputConfig_creationTime,
    listVectorEnrichmentJobOutputConfig_durationInSeconds,
    listVectorEnrichmentJobOutputConfig_name,
    listVectorEnrichmentJobOutputConfig_status,
    listVectorEnrichmentJobOutputConfig_type,

    -- * MapMatchingConfig
    MapMatchingConfig (..),
    newMapMatchingConfig,
    mapMatchingConfig_idAttributeName,
    mapMatchingConfig_timestampAttributeName,
    mapMatchingConfig_xAttributeName,
    mapMatchingConfig_yAttributeName,

    -- * MultiPolygonGeometryInput
    MultiPolygonGeometryInput (..),
    newMultiPolygonGeometryInput,
    multiPolygonGeometryInput_coordinates,

    -- * Operation
    Operation (..),
    newOperation,
    operation_outputType,
    operation_equation,
    operation_name,

    -- * OutputBand
    OutputBand (..),
    newOutputBand,
    outputBand_bandName,
    outputBand_outputDataType,

    -- * OutputConfigInput
    OutputConfigInput (..),
    newOutputConfigInput,
    outputConfigInput_s3Data,

    -- * OutputResolutionResamplingInput
    OutputResolutionResamplingInput (..),
    newOutputResolutionResamplingInput,
    outputResolutionResamplingInput_userDefined,

    -- * OutputResolutionStackInput
    OutputResolutionStackInput (..),
    newOutputResolutionStackInput,
    outputResolutionStackInput_predefined,
    outputResolutionStackInput_userDefined,

    -- * PlatformInput
    PlatformInput (..),
    newPlatformInput,
    platformInput_comparisonOperator,
    platformInput_value,

    -- * PolygonGeometryInput
    PolygonGeometryInput (..),
    newPolygonGeometryInput,
    polygonGeometryInput_coordinates,

    -- * Properties
    Properties (..),
    newProperties,
    properties_eoCloudCover,
    properties_landsatCloudCoverLand,
    properties_platform,
    properties_viewOffNadir,
    properties_viewSunAzimuth,
    properties_viewSunElevation,

    -- * Property
    Property (..),
    newProperty,
    property_eoCloudCover,
    property_landsatCloudCoverLand,
    property_platform,
    property_viewOffNadir,
    property_viewSunAzimuth,
    property_viewSunElevation,

    -- * PropertyFilter
    PropertyFilter (..),
    newPropertyFilter,
    propertyFilter_property,

    -- * PropertyFilters
    PropertyFilters (..),
    newPropertyFilters,
    propertyFilters_logicalOperator,
    propertyFilters_properties,

    -- * RasterDataCollectionMetadata
    RasterDataCollectionMetadata (..),
    newRasterDataCollectionMetadata,
    rasterDataCollectionMetadata_descriptionPageUrl,
    rasterDataCollectionMetadata_tags,
    rasterDataCollectionMetadata_arn,
    rasterDataCollectionMetadata_description,
    rasterDataCollectionMetadata_name,
    rasterDataCollectionMetadata_supportedFilters,
    rasterDataCollectionMetadata_type,

    -- * RasterDataCollectionQueryInput
    RasterDataCollectionQueryInput (..),
    newRasterDataCollectionQueryInput,
    rasterDataCollectionQueryInput_areaOfInterest,
    rasterDataCollectionQueryInput_propertyFilters,
    rasterDataCollectionQueryInput_rasterDataCollectionArn,
    rasterDataCollectionQueryInput_timeRangeFilter,

    -- * RasterDataCollectionQueryOutput
    RasterDataCollectionQueryOutput (..),
    newRasterDataCollectionQueryOutput,
    rasterDataCollectionQueryOutput_areaOfInterest,
    rasterDataCollectionQueryOutput_propertyFilters,
    rasterDataCollectionQueryOutput_rasterDataCollectionArn,
    rasterDataCollectionQueryOutput_rasterDataCollectionName,
    rasterDataCollectionQueryOutput_timeRangeFilter,

    -- * RasterDataCollectionQueryWithBandFilterInput
    RasterDataCollectionQueryWithBandFilterInput (..),
    newRasterDataCollectionQueryWithBandFilterInput,
    rasterDataCollectionQueryWithBandFilterInput_areaOfInterest,
    rasterDataCollectionQueryWithBandFilterInput_bandFilter,
    rasterDataCollectionQueryWithBandFilterInput_propertyFilters,
    rasterDataCollectionQueryWithBandFilterInput_timeRangeFilter,

    -- * ResamplingConfigInput
    ResamplingConfigInput (..),
    newResamplingConfigInput,
    resamplingConfigInput_algorithmName,
    resamplingConfigInput_targetBands,
    resamplingConfigInput_outputResolution,

    -- * ReverseGeocodingConfig
    ReverseGeocodingConfig (..),
    newReverseGeocodingConfig,
    reverseGeocodingConfig_xAttributeName,
    reverseGeocodingConfig_yAttributeName,

    -- * S3DataInput
    S3DataInput (..),
    newS3DataInput,
    s3DataInput_kmsKeyId,
    s3DataInput_metadataProvider,
    s3DataInput_s3Uri,

    -- * StackConfigInput
    StackConfigInput (..),
    newStackConfigInput,
    stackConfigInput_outputResolution,
    stackConfigInput_targetBands,

    -- * TemporalStatisticsConfigInput
    TemporalStatisticsConfigInput (..),
    newTemporalStatisticsConfigInput,
    temporalStatisticsConfigInput_groupBy,
    temporalStatisticsConfigInput_targetBands,
    temporalStatisticsConfigInput_statistics,

    -- * TimeRangeFilterInput
    TimeRangeFilterInput (..),
    newTimeRangeFilterInput,
    timeRangeFilterInput_endTime,
    timeRangeFilterInput_startTime,

    -- * UserDefined
    UserDefined (..),
    newUserDefined,
    userDefined_unit,
    userDefined_value,

    -- * VectorEnrichmentJobConfig
    VectorEnrichmentJobConfig (..),
    newVectorEnrichmentJobConfig,
    vectorEnrichmentJobConfig_mapMatchingConfig,
    vectorEnrichmentJobConfig_reverseGeocodingConfig,

    -- * VectorEnrichmentJobDataSourceConfigInput
    VectorEnrichmentJobDataSourceConfigInput (..),
    newVectorEnrichmentJobDataSourceConfigInput,
    vectorEnrichmentJobDataSourceConfigInput_s3Data,

    -- * VectorEnrichmentJobErrorDetails
    VectorEnrichmentJobErrorDetails (..),
    newVectorEnrichmentJobErrorDetails,
    vectorEnrichmentJobErrorDetails_errorMessage,
    vectorEnrichmentJobErrorDetails_errorType,

    -- * VectorEnrichmentJobExportErrorDetails
    VectorEnrichmentJobExportErrorDetails (..),
    newVectorEnrichmentJobExportErrorDetails,
    vectorEnrichmentJobExportErrorDetails_message,
    vectorEnrichmentJobExportErrorDetails_type,

    -- * VectorEnrichmentJobInputConfig
    VectorEnrichmentJobInputConfig (..),
    newVectorEnrichmentJobInputConfig,
    vectorEnrichmentJobInputConfig_dataSourceConfig,
    vectorEnrichmentJobInputConfig_documentType,

    -- * VectorEnrichmentJobS3Data
    VectorEnrichmentJobS3Data (..),
    newVectorEnrichmentJobS3Data,
    vectorEnrichmentJobS3Data_kmsKeyId,
    vectorEnrichmentJobS3Data_s3Uri,

    -- * ViewOffNadirInput
    ViewOffNadirInput (..),
    newViewOffNadirInput,
    viewOffNadirInput_lowerBound,
    viewOffNadirInput_upperBound,

    -- * ViewSunAzimuthInput
    ViewSunAzimuthInput (..),
    newViewSunAzimuthInput,
    viewSunAzimuthInput_lowerBound,
    viewSunAzimuthInput_upperBound,

    -- * ViewSunElevationInput
    ViewSunElevationInput (..),
    newViewSunElevationInput,
    viewSunElevationInput_lowerBound,
    viewSunElevationInput_upperBound,

    -- * ZonalStatisticsConfigInput
    ZonalStatisticsConfigInput (..),
    newZonalStatisticsConfigInput,
    zonalStatisticsConfigInput_targetBands,
    zonalStatisticsConfigInput_statistics,
    zonalStatisticsConfigInput_zoneS3Path,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMakerGeoSpatial.Types.AlgorithmNameCloudRemoval
import Amazonka.SageMakerGeoSpatial.Types.AlgorithmNameGeoMosaic
import Amazonka.SageMakerGeoSpatial.Types.AlgorithmNameResampling
import Amazonka.SageMakerGeoSpatial.Types.AreaOfInterest
import Amazonka.SageMakerGeoSpatial.Types.AreaOfInterestGeometry
import Amazonka.SageMakerGeoSpatial.Types.AssetValue
import Amazonka.SageMakerGeoSpatial.Types.BandMathConfigInput
import Amazonka.SageMakerGeoSpatial.Types.CloudMaskingConfigInput
import Amazonka.SageMakerGeoSpatial.Types.CloudRemovalConfigInput
import Amazonka.SageMakerGeoSpatial.Types.ComparisonOperator
import Amazonka.SageMakerGeoSpatial.Types.CustomIndicesInput
import Amazonka.SageMakerGeoSpatial.Types.DataCollectionType
import Amazonka.SageMakerGeoSpatial.Types.EarthObservationJobErrorDetails
import Amazonka.SageMakerGeoSpatial.Types.EarthObservationJobErrorType
import Amazonka.SageMakerGeoSpatial.Types.EarthObservationJobExportStatus
import Amazonka.SageMakerGeoSpatial.Types.EarthObservationJobStatus
import Amazonka.SageMakerGeoSpatial.Types.EoCloudCoverInput
import Amazonka.SageMakerGeoSpatial.Types.EojDataSourceConfigInput
import Amazonka.SageMakerGeoSpatial.Types.ExportErrorDetails
import Amazonka.SageMakerGeoSpatial.Types.ExportErrorDetailsOutput
import Amazonka.SageMakerGeoSpatial.Types.ExportErrorType
import Amazonka.SageMakerGeoSpatial.Types.ExportS3DataInput
import Amazonka.SageMakerGeoSpatial.Types.ExportVectorEnrichmentJobOutputConfig
import Amazonka.SageMakerGeoSpatial.Types.Filter
import Amazonka.SageMakerGeoSpatial.Types.GeoMosaicConfigInput
import Amazonka.SageMakerGeoSpatial.Types.Geometry
import Amazonka.SageMakerGeoSpatial.Types.GroupBy
import Amazonka.SageMakerGeoSpatial.Types.InputConfigInput
import Amazonka.SageMakerGeoSpatial.Types.InputConfigOutput
import Amazonka.SageMakerGeoSpatial.Types.ItemSource
import Amazonka.SageMakerGeoSpatial.Types.JobConfigInput
import Amazonka.SageMakerGeoSpatial.Types.LandCoverSegmentationConfigInput
import Amazonka.SageMakerGeoSpatial.Types.LandsatCloudCoverLandInput
import Amazonka.SageMakerGeoSpatial.Types.ListEarthObservationJobOutputConfig
import Amazonka.SageMakerGeoSpatial.Types.ListVectorEnrichmentJobOutputConfig
import Amazonka.SageMakerGeoSpatial.Types.LogicalOperator
import Amazonka.SageMakerGeoSpatial.Types.MapMatchingConfig
import Amazonka.SageMakerGeoSpatial.Types.MetadataProvider
import Amazonka.SageMakerGeoSpatial.Types.MultiPolygonGeometryInput
import Amazonka.SageMakerGeoSpatial.Types.Operation
import Amazonka.SageMakerGeoSpatial.Types.OutputBand
import Amazonka.SageMakerGeoSpatial.Types.OutputConfigInput
import Amazonka.SageMakerGeoSpatial.Types.OutputResolutionResamplingInput
import Amazonka.SageMakerGeoSpatial.Types.OutputResolutionStackInput
import Amazonka.SageMakerGeoSpatial.Types.OutputType
import Amazonka.SageMakerGeoSpatial.Types.PlatformInput
import Amazonka.SageMakerGeoSpatial.Types.PolygonGeometryInput
import Amazonka.SageMakerGeoSpatial.Types.PredefinedResolution
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
import Amazonka.SageMakerGeoSpatial.Types.SortOrder
import Amazonka.SageMakerGeoSpatial.Types.StackConfigInput
import Amazonka.SageMakerGeoSpatial.Types.TargetOptions
import Amazonka.SageMakerGeoSpatial.Types.TemporalStatistics
import Amazonka.SageMakerGeoSpatial.Types.TemporalStatisticsConfigInput
import Amazonka.SageMakerGeoSpatial.Types.TimeRangeFilterInput
import Amazonka.SageMakerGeoSpatial.Types.Unit
import Amazonka.SageMakerGeoSpatial.Types.UserDefined
import Amazonka.SageMakerGeoSpatial.Types.VectorEnrichmentJobConfig
import Amazonka.SageMakerGeoSpatial.Types.VectorEnrichmentJobDataSourceConfigInput
import Amazonka.SageMakerGeoSpatial.Types.VectorEnrichmentJobDocumentType
import Amazonka.SageMakerGeoSpatial.Types.VectorEnrichmentJobErrorDetails
import Amazonka.SageMakerGeoSpatial.Types.VectorEnrichmentJobErrorType
import Amazonka.SageMakerGeoSpatial.Types.VectorEnrichmentJobExportErrorDetails
import Amazonka.SageMakerGeoSpatial.Types.VectorEnrichmentJobExportErrorType
import Amazonka.SageMakerGeoSpatial.Types.VectorEnrichmentJobExportStatus
import Amazonka.SageMakerGeoSpatial.Types.VectorEnrichmentJobInputConfig
import Amazonka.SageMakerGeoSpatial.Types.VectorEnrichmentJobS3Data
import Amazonka.SageMakerGeoSpatial.Types.VectorEnrichmentJobStatus
import Amazonka.SageMakerGeoSpatial.Types.VectorEnrichmentJobType
import Amazonka.SageMakerGeoSpatial.Types.ViewOffNadirInput
import Amazonka.SageMakerGeoSpatial.Types.ViewSunAzimuthInput
import Amazonka.SageMakerGeoSpatial.Types.ViewSunElevationInput
import Amazonka.SageMakerGeoSpatial.Types.ZonalStatistics
import Amazonka.SageMakerGeoSpatial.Types.ZonalStatisticsConfigInput
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2020-05-27@ of the Amazon SageMaker geospatial capabilities SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev = "SageMakerGeoSpatial",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "sagemaker-geospatial",
      Core.signingName = "sagemaker-geospatial",
      Core.version = "2020-05-27",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error =
        Core.parseJSONError "SageMakerGeoSpatial",
      Core.retry = retry
    }
  where
    retry =
      Core.Exponential
        { Core.base = 5.0e-2,
          Core.growth = 2,
          Core.attempts = 5,
          Core.check = check
        }
    check e
      | Lens.has (Core.hasStatus 502) e =
          Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 504) e =
          Prelude.Just "gateway_timeout"
      | Lens.has (Core.hasStatus 500) e =
          Prelude.Just "general_server_error"
      | Lens.has (Core.hasStatus 509) e =
          Prelude.Just "limit_exceeded"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 503) e =
          Prelude.Just "service_unavailable"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throttled_exception"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throttling"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throttling_exception"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 429) e =
          Prelude.Just "too_many_requests"
      | Prelude.otherwise = Prelude.Nothing

-- | You do not have sufficient access to perform this action.
_AccessDeniedException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_AccessDeniedException =
  Core._MatchServiceError
    defaultService
    "AccessDeniedException"
    Prelude.. Core.hasStatus 403

_ConflictException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ConflictException =
  Core._MatchServiceError
    defaultService
    "ConflictException"
    Prelude.. Core.hasStatus 409

-- | The request processing has failed because of an unknown error,
-- exception, or failure.
_InternalServerException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InternalServerException =
  Core._MatchServiceError
    defaultService
    "InternalServerException"
    Prelude.. Core.hasStatus 500

_ResourceNotFoundException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"
    Prelude.. Core.hasStatus 404

-- | You have exceeded the service quota.
_ServiceQuotaExceededException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ServiceQuotaExceededException =
  Core._MatchServiceError
    defaultService
    "ServiceQuotaExceededException"
    Prelude.. Core.hasStatus 402

-- | The request was denied due to request throttling.
_ThrottlingException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ThrottlingException =
  Core._MatchServiceError
    defaultService
    "ThrottlingException"
    Prelude.. Core.hasStatus 429

-- | The input fails to satisfy the constraints specified by an Amazon Web
-- Services service.
_ValidationException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ValidationException =
  Core._MatchServiceError
    defaultService
    "ValidationException"
    Prelude.. Core.hasStatus 400
