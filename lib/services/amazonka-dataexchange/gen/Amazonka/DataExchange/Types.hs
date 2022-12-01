{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.DataExchange.Types
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DataExchange.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _AccessDeniedException,
    _InternalServerException,
    _ResourceNotFoundException,
    _ConflictException,
    _ThrottlingException,
    _ServiceLimitExceededException,
    _ValidationException,

    -- * AssetType
    AssetType (..),

    -- * Code
    Code (..),

    -- * JobErrorLimitName
    JobErrorLimitName (..),

    -- * JobErrorResourceTypes
    JobErrorResourceTypes (..),

    -- * Origin
    Origin (..),

    -- * ProtocolType
    ProtocolType (..),

    -- * ServerSideEncryptionTypes
    ServerSideEncryptionTypes (..),

    -- * State
    State (..),

    -- * Type
    Type (..),

    -- * Action
    Action (..),
    newAction,
    action_exportRevisionToS3,

    -- * ApiGatewayApiAsset
    ApiGatewayApiAsset (..),
    newApiGatewayApiAsset,
    apiGatewayApiAsset_apiEndpoint,
    apiGatewayApiAsset_apiId,
    apiGatewayApiAsset_apiSpecificationDownloadUrlExpiresAt,
    apiGatewayApiAsset_apiKey,
    apiGatewayApiAsset_apiSpecificationDownloadUrl,
    apiGatewayApiAsset_protocolType,
    apiGatewayApiAsset_stage,
    apiGatewayApiAsset_apiName,
    apiGatewayApiAsset_apiDescription,

    -- * AssetDestinationEntry
    AssetDestinationEntry (..),
    newAssetDestinationEntry,
    assetDestinationEntry_key,
    assetDestinationEntry_assetId,
    assetDestinationEntry_bucket,

    -- * AssetDetails
    AssetDetails (..),
    newAssetDetails,
    assetDetails_s3SnapshotAsset,
    assetDetails_apiGatewayApiAsset,
    assetDetails_redshiftDataShareAsset,

    -- * AssetEntry
    AssetEntry (..),
    newAssetEntry,
    assetEntry_sourceId,
    assetEntry_arn,
    assetEntry_assetDetails,
    assetEntry_assetType,
    assetEntry_createdAt,
    assetEntry_dataSetId,
    assetEntry_id,
    assetEntry_name,
    assetEntry_revisionId,
    assetEntry_updatedAt,

    -- * AssetSourceEntry
    AssetSourceEntry (..),
    newAssetSourceEntry,
    assetSourceEntry_bucket,
    assetSourceEntry_key,

    -- * AutoExportRevisionDestinationEntry
    AutoExportRevisionDestinationEntry (..),
    newAutoExportRevisionDestinationEntry,
    autoExportRevisionDestinationEntry_keyPattern,
    autoExportRevisionDestinationEntry_bucket,

    -- * AutoExportRevisionToS3RequestDetails
    AutoExportRevisionToS3RequestDetails (..),
    newAutoExportRevisionToS3RequestDetails,
    autoExportRevisionToS3RequestDetails_encryption,
    autoExportRevisionToS3RequestDetails_revisionDestination,

    -- * DataSetEntry
    DataSetEntry (..),
    newDataSetEntry,
    dataSetEntry_sourceId,
    dataSetEntry_originDetails,
    dataSetEntry_arn,
    dataSetEntry_assetType,
    dataSetEntry_createdAt,
    dataSetEntry_description,
    dataSetEntry_id,
    dataSetEntry_name,
    dataSetEntry_origin,
    dataSetEntry_updatedAt,

    -- * Details
    Details (..),
    newDetails,
    details_importAssetsFromS3JobErrorDetails,
    details_importAssetFromSignedUrlJobErrorDetails,

    -- * Event
    Event (..),
    newEvent,
    event_revisionPublished,

    -- * EventActionEntry
    EventActionEntry (..),
    newEventActionEntry,
    eventActionEntry_action,
    eventActionEntry_arn,
    eventActionEntry_createdAt,
    eventActionEntry_event,
    eventActionEntry_id,
    eventActionEntry_updatedAt,

    -- * ExportAssetToSignedUrlRequestDetails
    ExportAssetToSignedUrlRequestDetails (..),
    newExportAssetToSignedUrlRequestDetails,
    exportAssetToSignedUrlRequestDetails_assetId,
    exportAssetToSignedUrlRequestDetails_dataSetId,
    exportAssetToSignedUrlRequestDetails_revisionId,

    -- * ExportAssetToSignedUrlResponseDetails
    ExportAssetToSignedUrlResponseDetails (..),
    newExportAssetToSignedUrlResponseDetails,
    exportAssetToSignedUrlResponseDetails_signedUrlExpiresAt,
    exportAssetToSignedUrlResponseDetails_signedUrl,
    exportAssetToSignedUrlResponseDetails_assetId,
    exportAssetToSignedUrlResponseDetails_dataSetId,
    exportAssetToSignedUrlResponseDetails_revisionId,

    -- * ExportAssetsToS3RequestDetails
    ExportAssetsToS3RequestDetails (..),
    newExportAssetsToS3RequestDetails,
    exportAssetsToS3RequestDetails_encryption,
    exportAssetsToS3RequestDetails_assetDestinations,
    exportAssetsToS3RequestDetails_dataSetId,
    exportAssetsToS3RequestDetails_revisionId,

    -- * ExportAssetsToS3ResponseDetails
    ExportAssetsToS3ResponseDetails (..),
    newExportAssetsToS3ResponseDetails,
    exportAssetsToS3ResponseDetails_encryption,
    exportAssetsToS3ResponseDetails_assetDestinations,
    exportAssetsToS3ResponseDetails_dataSetId,
    exportAssetsToS3ResponseDetails_revisionId,

    -- * ExportRevisionsToS3RequestDetails
    ExportRevisionsToS3RequestDetails (..),
    newExportRevisionsToS3RequestDetails,
    exportRevisionsToS3RequestDetails_encryption,
    exportRevisionsToS3RequestDetails_dataSetId,
    exportRevisionsToS3RequestDetails_revisionDestinations,

    -- * ExportRevisionsToS3ResponseDetails
    ExportRevisionsToS3ResponseDetails (..),
    newExportRevisionsToS3ResponseDetails,
    exportRevisionsToS3ResponseDetails_eventActionArn,
    exportRevisionsToS3ResponseDetails_encryption,
    exportRevisionsToS3ResponseDetails_dataSetId,
    exportRevisionsToS3ResponseDetails_revisionDestinations,

    -- * ExportServerSideEncryption
    ExportServerSideEncryption (..),
    newExportServerSideEncryption,
    exportServerSideEncryption_kmsKeyArn,
    exportServerSideEncryption_type,

    -- * ImportAssetFromApiGatewayApiRequestDetails
    ImportAssetFromApiGatewayApiRequestDetails (..),
    newImportAssetFromApiGatewayApiRequestDetails,
    importAssetFromApiGatewayApiRequestDetails_apiKey,
    importAssetFromApiGatewayApiRequestDetails_apiDescription,
    importAssetFromApiGatewayApiRequestDetails_apiId,
    importAssetFromApiGatewayApiRequestDetails_apiName,
    importAssetFromApiGatewayApiRequestDetails_apiSpecificationMd5Hash,
    importAssetFromApiGatewayApiRequestDetails_dataSetId,
    importAssetFromApiGatewayApiRequestDetails_protocolType,
    importAssetFromApiGatewayApiRequestDetails_revisionId,
    importAssetFromApiGatewayApiRequestDetails_stage,

    -- * ImportAssetFromApiGatewayApiResponseDetails
    ImportAssetFromApiGatewayApiResponseDetails (..),
    newImportAssetFromApiGatewayApiResponseDetails,
    importAssetFromApiGatewayApiResponseDetails_apiKey,
    importAssetFromApiGatewayApiResponseDetails_apiDescription,
    importAssetFromApiGatewayApiResponseDetails_apiId,
    importAssetFromApiGatewayApiResponseDetails_apiName,
    importAssetFromApiGatewayApiResponseDetails_apiSpecificationMd5Hash,
    importAssetFromApiGatewayApiResponseDetails_apiSpecificationUploadUrl,
    importAssetFromApiGatewayApiResponseDetails_apiSpecificationUploadUrlExpiresAt,
    importAssetFromApiGatewayApiResponseDetails_dataSetId,
    importAssetFromApiGatewayApiResponseDetails_protocolType,
    importAssetFromApiGatewayApiResponseDetails_revisionId,
    importAssetFromApiGatewayApiResponseDetails_stage,

    -- * ImportAssetFromSignedUrlJobErrorDetails
    ImportAssetFromSignedUrlJobErrorDetails (..),
    newImportAssetFromSignedUrlJobErrorDetails,
    importAssetFromSignedUrlJobErrorDetails_assetName,

    -- * ImportAssetFromSignedUrlRequestDetails
    ImportAssetFromSignedUrlRequestDetails (..),
    newImportAssetFromSignedUrlRequestDetails,
    importAssetFromSignedUrlRequestDetails_assetName,
    importAssetFromSignedUrlRequestDetails_dataSetId,
    importAssetFromSignedUrlRequestDetails_md5Hash,
    importAssetFromSignedUrlRequestDetails_revisionId,

    -- * ImportAssetFromSignedUrlResponseDetails
    ImportAssetFromSignedUrlResponseDetails (..),
    newImportAssetFromSignedUrlResponseDetails,
    importAssetFromSignedUrlResponseDetails_signedUrlExpiresAt,
    importAssetFromSignedUrlResponseDetails_signedUrl,
    importAssetFromSignedUrlResponseDetails_md5Hash,
    importAssetFromSignedUrlResponseDetails_assetName,
    importAssetFromSignedUrlResponseDetails_dataSetId,
    importAssetFromSignedUrlResponseDetails_revisionId,

    -- * ImportAssetsFromRedshiftDataSharesRequestDetails
    ImportAssetsFromRedshiftDataSharesRequestDetails (..),
    newImportAssetsFromRedshiftDataSharesRequestDetails,
    importAssetsFromRedshiftDataSharesRequestDetails_assetSources,
    importAssetsFromRedshiftDataSharesRequestDetails_dataSetId,
    importAssetsFromRedshiftDataSharesRequestDetails_revisionId,

    -- * ImportAssetsFromRedshiftDataSharesResponseDetails
    ImportAssetsFromRedshiftDataSharesResponseDetails (..),
    newImportAssetsFromRedshiftDataSharesResponseDetails,
    importAssetsFromRedshiftDataSharesResponseDetails_assetSources,
    importAssetsFromRedshiftDataSharesResponseDetails_dataSetId,
    importAssetsFromRedshiftDataSharesResponseDetails_revisionId,

    -- * ImportAssetsFromS3RequestDetails
    ImportAssetsFromS3RequestDetails (..),
    newImportAssetsFromS3RequestDetails,
    importAssetsFromS3RequestDetails_assetSources,
    importAssetsFromS3RequestDetails_dataSetId,
    importAssetsFromS3RequestDetails_revisionId,

    -- * ImportAssetsFromS3ResponseDetails
    ImportAssetsFromS3ResponseDetails (..),
    newImportAssetsFromS3ResponseDetails,
    importAssetsFromS3ResponseDetails_assetSources,
    importAssetsFromS3ResponseDetails_dataSetId,
    importAssetsFromS3ResponseDetails_revisionId,

    -- * JobEntry
    JobEntry (..),
    newJobEntry,
    jobEntry_errors,
    jobEntry_arn,
    jobEntry_createdAt,
    jobEntry_details,
    jobEntry_id,
    jobEntry_state,
    jobEntry_type,
    jobEntry_updatedAt,

    -- * JobError
    JobError (..),
    newJobError,
    jobError_resourceId,
    jobError_resourceType,
    jobError_limitValue,
    jobError_limitName,
    jobError_details,
    jobError_code,
    jobError_message,

    -- * OriginDetails
    OriginDetails (..),
    newOriginDetails,
    originDetails_productId,

    -- * RedshiftDataShareAsset
    RedshiftDataShareAsset (..),
    newRedshiftDataShareAsset,
    redshiftDataShareAsset_arn,

    -- * RedshiftDataShareAssetSourceEntry
    RedshiftDataShareAssetSourceEntry (..),
    newRedshiftDataShareAssetSourceEntry,
    redshiftDataShareAssetSourceEntry_dataShareArn,

    -- * RequestDetails
    RequestDetails (..),
    newRequestDetails,
    requestDetails_importAssetFromSignedUrl,
    requestDetails_importAssetsFromRedshiftDataShares,
    requestDetails_exportAssetToSignedUrl,
    requestDetails_exportRevisionsToS3,
    requestDetails_exportAssetsToS3,
    requestDetails_importAssetsFromS3,
    requestDetails_importAssetFromApiGatewayApi,

    -- * ResponseDetails
    ResponseDetails (..),
    newResponseDetails,
    responseDetails_importAssetFromSignedUrl,
    responseDetails_importAssetsFromRedshiftDataShares,
    responseDetails_exportAssetToSignedUrl,
    responseDetails_exportRevisionsToS3,
    responseDetails_exportAssetsToS3,
    responseDetails_importAssetsFromS3,
    responseDetails_importAssetFromApiGatewayApi,

    -- * RevisionDestinationEntry
    RevisionDestinationEntry (..),
    newRevisionDestinationEntry,
    revisionDestinationEntry_keyPattern,
    revisionDestinationEntry_bucket,
    revisionDestinationEntry_revisionId,

    -- * RevisionEntry
    RevisionEntry (..),
    newRevisionEntry,
    revisionEntry_sourceId,
    revisionEntry_revocationComment,
    revisionEntry_comment,
    revisionEntry_finalized,
    revisionEntry_revokedAt,
    revisionEntry_revoked,
    revisionEntry_arn,
    revisionEntry_createdAt,
    revisionEntry_dataSetId,
    revisionEntry_id,
    revisionEntry_updatedAt,

    -- * RevisionPublished
    RevisionPublished (..),
    newRevisionPublished,
    revisionPublished_dataSetId,

    -- * S3SnapshotAsset
    S3SnapshotAsset (..),
    newS3SnapshotAsset,
    s3SnapshotAsset_size,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.DataExchange.Types.Action
import Amazonka.DataExchange.Types.ApiGatewayApiAsset
import Amazonka.DataExchange.Types.AssetDestinationEntry
import Amazonka.DataExchange.Types.AssetDetails
import Amazonka.DataExchange.Types.AssetEntry
import Amazonka.DataExchange.Types.AssetSourceEntry
import Amazonka.DataExchange.Types.AssetType
import Amazonka.DataExchange.Types.AutoExportRevisionDestinationEntry
import Amazonka.DataExchange.Types.AutoExportRevisionToS3RequestDetails
import Amazonka.DataExchange.Types.Code
import Amazonka.DataExchange.Types.DataSetEntry
import Amazonka.DataExchange.Types.Details
import Amazonka.DataExchange.Types.Event
import Amazonka.DataExchange.Types.EventActionEntry
import Amazonka.DataExchange.Types.ExportAssetToSignedUrlRequestDetails
import Amazonka.DataExchange.Types.ExportAssetToSignedUrlResponseDetails
import Amazonka.DataExchange.Types.ExportAssetsToS3RequestDetails
import Amazonka.DataExchange.Types.ExportAssetsToS3ResponseDetails
import Amazonka.DataExchange.Types.ExportRevisionsToS3RequestDetails
import Amazonka.DataExchange.Types.ExportRevisionsToS3ResponseDetails
import Amazonka.DataExchange.Types.ExportServerSideEncryption
import Amazonka.DataExchange.Types.ImportAssetFromApiGatewayApiRequestDetails
import Amazonka.DataExchange.Types.ImportAssetFromApiGatewayApiResponseDetails
import Amazonka.DataExchange.Types.ImportAssetFromSignedUrlJobErrorDetails
import Amazonka.DataExchange.Types.ImportAssetFromSignedUrlRequestDetails
import Amazonka.DataExchange.Types.ImportAssetFromSignedUrlResponseDetails
import Amazonka.DataExchange.Types.ImportAssetsFromRedshiftDataSharesRequestDetails
import Amazonka.DataExchange.Types.ImportAssetsFromRedshiftDataSharesResponseDetails
import Amazonka.DataExchange.Types.ImportAssetsFromS3RequestDetails
import Amazonka.DataExchange.Types.ImportAssetsFromS3ResponseDetails
import Amazonka.DataExchange.Types.JobEntry
import Amazonka.DataExchange.Types.JobError
import Amazonka.DataExchange.Types.JobErrorLimitName
import Amazonka.DataExchange.Types.JobErrorResourceTypes
import Amazonka.DataExchange.Types.Origin
import Amazonka.DataExchange.Types.OriginDetails
import Amazonka.DataExchange.Types.ProtocolType
import Amazonka.DataExchange.Types.RedshiftDataShareAsset
import Amazonka.DataExchange.Types.RedshiftDataShareAssetSourceEntry
import Amazonka.DataExchange.Types.RequestDetails
import Amazonka.DataExchange.Types.ResponseDetails
import Amazonka.DataExchange.Types.RevisionDestinationEntry
import Amazonka.DataExchange.Types.RevisionEntry
import Amazonka.DataExchange.Types.RevisionPublished
import Amazonka.DataExchange.Types.S3SnapshotAsset
import Amazonka.DataExchange.Types.ServerSideEncryptionTypes
import Amazonka.DataExchange.Types.State
import Amazonka.DataExchange.Types.Type
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2017-07-25@ of the Amazon Data Exchange SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev = "DataExchange",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "dataexchange",
      Core.signingName = "dataexchange",
      Core.version = "2017-07-25",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error = Core.parseJSONError "DataExchange",
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
      | Lens.has (Core.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Lens.has (Core.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Core.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has (Core.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Prelude.otherwise = Prelude.Nothing

-- | Access to the resource is denied.
_AccessDeniedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_AccessDeniedException =
  Core._MatchServiceError
    defaultService
    "AccessDeniedException"
    Prelude.. Core.hasStatus 403

-- | An exception occurred with the service.
_InternalServerException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InternalServerException =
  Core._MatchServiceError
    defaultService
    "InternalServerException"
    Prelude.. Core.hasStatus 500

-- | The resource couldn\'t be found.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"
    Prelude.. Core.hasStatus 404

-- | The request couldn\'t be completed because it conflicted with the
-- current state of the resource.
_ConflictException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ConflictException =
  Core._MatchServiceError
    defaultService
    "ConflictException"
    Prelude.. Core.hasStatus 409

-- | The limit on the number of requests per second was exceeded.
_ThrottlingException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ThrottlingException =
  Core._MatchServiceError
    defaultService
    "ThrottlingException"
    Prelude.. Core.hasStatus 429

-- | The request has exceeded the quotas imposed by the service.
_ServiceLimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ServiceLimitExceededException =
  Core._MatchServiceError
    defaultService
    "ServiceLimitExceededException"
    Prelude.. Core.hasStatus 402

-- | The request was invalid.
_ValidationException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ValidationException =
  Core._MatchServiceError
    defaultService
    "ValidationException"
    Prelude.. Core.hasStatus 400
