{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DataExchange.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DataExchange.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _ValidationException,
    _AccessDeniedException,
    _ConflictException,
    _ServiceLimitExceededException,
    _ThrottlingException,
    _InternalServerException,
    _ResourceNotFoundException,

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

    -- * AssetDestinationEntry
    AssetDestinationEntry (..),
    newAssetDestinationEntry,
    assetDestinationEntry_key,
    assetDestinationEntry_bucket,
    assetDestinationEntry_assetId,

    -- * AssetDetails
    AssetDetails (..),
    newAssetDetails,
    assetDetails_s3SnapshotAsset,
    assetDetails_redshiftDataShareAsset,

    -- * AssetEntry
    AssetEntry (..),
    newAssetEntry,
    assetEntry_sourceId,
    assetEntry_assetType,
    assetEntry_createdAt,
    assetEntry_dataSetId,
    assetEntry_id,
    assetEntry_arn,
    assetEntry_assetDetails,
    assetEntry_updatedAt,
    assetEntry_revisionId,
    assetEntry_name,

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
    dataSetEntry_origin,
    dataSetEntry_assetType,
    dataSetEntry_description,
    dataSetEntry_createdAt,
    dataSetEntry_id,
    dataSetEntry_arn,
    dataSetEntry_updatedAt,
    dataSetEntry_name,

    -- * Details
    Details (..),
    newDetails,
    details_importAssetFromSignedUrlJobErrorDetails,
    details_importAssetsFromS3JobErrorDetails,

    -- * Event
    Event (..),
    newEvent,
    event_revisionPublished,

    -- * EventActionEntry
    EventActionEntry (..),
    newEventActionEntry,
    eventActionEntry_action,
    eventActionEntry_createdAt,
    eventActionEntry_event,
    eventActionEntry_id,
    eventActionEntry_arn,
    eventActionEntry_updatedAt,

    -- * ExportAssetToSignedUrlRequestDetails
    ExportAssetToSignedUrlRequestDetails (..),
    newExportAssetToSignedUrlRequestDetails,
    exportAssetToSignedUrlRequestDetails_dataSetId,
    exportAssetToSignedUrlRequestDetails_assetId,
    exportAssetToSignedUrlRequestDetails_revisionId,

    -- * ExportAssetToSignedUrlResponseDetails
    ExportAssetToSignedUrlResponseDetails (..),
    newExportAssetToSignedUrlResponseDetails,
    exportAssetToSignedUrlResponseDetails_signedUrl,
    exportAssetToSignedUrlResponseDetails_signedUrlExpiresAt,
    exportAssetToSignedUrlResponseDetails_dataSetId,
    exportAssetToSignedUrlResponseDetails_assetId,
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
    exportRevisionsToS3RequestDetails_revisionDestinations,
    exportRevisionsToS3RequestDetails_dataSetId,

    -- * ExportRevisionsToS3ResponseDetails
    ExportRevisionsToS3ResponseDetails (..),
    newExportRevisionsToS3ResponseDetails,
    exportRevisionsToS3ResponseDetails_encryption,
    exportRevisionsToS3ResponseDetails_eventActionArn,
    exportRevisionsToS3ResponseDetails_revisionDestinations,
    exportRevisionsToS3ResponseDetails_dataSetId,

    -- * ExportServerSideEncryption
    ExportServerSideEncryption (..),
    newExportServerSideEncryption,
    exportServerSideEncryption_kmsKeyArn,
    exportServerSideEncryption_type,

    -- * ImportAssetFromSignedUrlJobErrorDetails
    ImportAssetFromSignedUrlJobErrorDetails (..),
    newImportAssetFromSignedUrlJobErrorDetails,
    importAssetFromSignedUrlJobErrorDetails_assetName,

    -- * ImportAssetFromSignedUrlRequestDetails
    ImportAssetFromSignedUrlRequestDetails (..),
    newImportAssetFromSignedUrlRequestDetails,
    importAssetFromSignedUrlRequestDetails_dataSetId,
    importAssetFromSignedUrlRequestDetails_md5Hash,
    importAssetFromSignedUrlRequestDetails_assetName,
    importAssetFromSignedUrlRequestDetails_revisionId,

    -- * ImportAssetFromSignedUrlResponseDetails
    ImportAssetFromSignedUrlResponseDetails (..),
    newImportAssetFromSignedUrlResponseDetails,
    importAssetFromSignedUrlResponseDetails_signedUrl,
    importAssetFromSignedUrlResponseDetails_signedUrlExpiresAt,
    importAssetFromSignedUrlResponseDetails_md5Hash,
    importAssetFromSignedUrlResponseDetails_dataSetId,
    importAssetFromSignedUrlResponseDetails_assetName,
    importAssetFromSignedUrlResponseDetails_revisionId,

    -- * ImportAssetsFromRedshiftDataSharesRequestDetails
    ImportAssetsFromRedshiftDataSharesRequestDetails (..),
    newImportAssetsFromRedshiftDataSharesRequestDetails,
    importAssetsFromRedshiftDataSharesRequestDetails_dataSetId,
    importAssetsFromRedshiftDataSharesRequestDetails_assetSources,
    importAssetsFromRedshiftDataSharesRequestDetails_revisionId,

    -- * ImportAssetsFromRedshiftDataSharesResponseDetails
    ImportAssetsFromRedshiftDataSharesResponseDetails (..),
    newImportAssetsFromRedshiftDataSharesResponseDetails,
    importAssetsFromRedshiftDataSharesResponseDetails_dataSetId,
    importAssetsFromRedshiftDataSharesResponseDetails_assetSources,
    importAssetsFromRedshiftDataSharesResponseDetails_revisionId,

    -- * ImportAssetsFromS3RequestDetails
    ImportAssetsFromS3RequestDetails (..),
    newImportAssetsFromS3RequestDetails,
    importAssetsFromS3RequestDetails_dataSetId,
    importAssetsFromS3RequestDetails_assetSources,
    importAssetsFromS3RequestDetails_revisionId,

    -- * ImportAssetsFromS3ResponseDetails
    ImportAssetsFromS3ResponseDetails (..),
    newImportAssetsFromS3ResponseDetails,
    importAssetsFromS3ResponseDetails_dataSetId,
    importAssetsFromS3ResponseDetails_assetSources,
    importAssetsFromS3ResponseDetails_revisionId,

    -- * JobEntry
    JobEntry (..),
    newJobEntry,
    jobEntry_errors,
    jobEntry_type,
    jobEntry_details,
    jobEntry_state,
    jobEntry_createdAt,
    jobEntry_id,
    jobEntry_arn,
    jobEntry_updatedAt,

    -- * JobError
    JobError (..),
    newJobError,
    jobError_resourceId,
    jobError_limitName,
    jobError_resourceType,
    jobError_details,
    jobError_limitValue,
    jobError_message,
    jobError_code,

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
    requestDetails_exportAssetsToS3,
    requestDetails_exportRevisionsToS3,
    requestDetails_importAssetFromSignedUrl,
    requestDetails_importAssetsFromRedshiftDataShares,
    requestDetails_importAssetsFromS3,
    requestDetails_exportAssetToSignedUrl,

    -- * ResponseDetails
    ResponseDetails (..),
    newResponseDetails,
    responseDetails_exportAssetsToS3,
    responseDetails_exportRevisionsToS3,
    responseDetails_importAssetFromSignedUrl,
    responseDetails_importAssetsFromRedshiftDataShares,
    responseDetails_importAssetsFromS3,
    responseDetails_exportAssetToSignedUrl,

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
    revisionEntry_finalized,
    revisionEntry_comment,
    revisionEntry_createdAt,
    revisionEntry_dataSetId,
    revisionEntry_id,
    revisionEntry_arn,
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

import qualified Network.AWS.Core as Core
import Network.AWS.DataExchange.Types.Action
import Network.AWS.DataExchange.Types.AssetDestinationEntry
import Network.AWS.DataExchange.Types.AssetDetails
import Network.AWS.DataExchange.Types.AssetEntry
import Network.AWS.DataExchange.Types.AssetSourceEntry
import Network.AWS.DataExchange.Types.AssetType
import Network.AWS.DataExchange.Types.AutoExportRevisionDestinationEntry
import Network.AWS.DataExchange.Types.AutoExportRevisionToS3RequestDetails
import Network.AWS.DataExchange.Types.Code
import Network.AWS.DataExchange.Types.DataSetEntry
import Network.AWS.DataExchange.Types.Details
import Network.AWS.DataExchange.Types.Event
import Network.AWS.DataExchange.Types.EventActionEntry
import Network.AWS.DataExchange.Types.ExportAssetToSignedUrlRequestDetails
import Network.AWS.DataExchange.Types.ExportAssetToSignedUrlResponseDetails
import Network.AWS.DataExchange.Types.ExportAssetsToS3RequestDetails
import Network.AWS.DataExchange.Types.ExportAssetsToS3ResponseDetails
import Network.AWS.DataExchange.Types.ExportRevisionsToS3RequestDetails
import Network.AWS.DataExchange.Types.ExportRevisionsToS3ResponseDetails
import Network.AWS.DataExchange.Types.ExportServerSideEncryption
import Network.AWS.DataExchange.Types.ImportAssetFromSignedUrlJobErrorDetails
import Network.AWS.DataExchange.Types.ImportAssetFromSignedUrlRequestDetails
import Network.AWS.DataExchange.Types.ImportAssetFromSignedUrlResponseDetails
import Network.AWS.DataExchange.Types.ImportAssetsFromRedshiftDataSharesRequestDetails
import Network.AWS.DataExchange.Types.ImportAssetsFromRedshiftDataSharesResponseDetails
import Network.AWS.DataExchange.Types.ImportAssetsFromS3RequestDetails
import Network.AWS.DataExchange.Types.ImportAssetsFromS3ResponseDetails
import Network.AWS.DataExchange.Types.JobEntry
import Network.AWS.DataExchange.Types.JobError
import Network.AWS.DataExchange.Types.JobErrorLimitName
import Network.AWS.DataExchange.Types.JobErrorResourceTypes
import Network.AWS.DataExchange.Types.Origin
import Network.AWS.DataExchange.Types.OriginDetails
import Network.AWS.DataExchange.Types.RedshiftDataShareAsset
import Network.AWS.DataExchange.Types.RedshiftDataShareAssetSourceEntry
import Network.AWS.DataExchange.Types.RequestDetails
import Network.AWS.DataExchange.Types.ResponseDetails
import Network.AWS.DataExchange.Types.RevisionDestinationEntry
import Network.AWS.DataExchange.Types.RevisionEntry
import Network.AWS.DataExchange.Types.RevisionPublished
import Network.AWS.DataExchange.Types.S3SnapshotAsset
import Network.AWS.DataExchange.Types.ServerSideEncryptionTypes
import Network.AWS.DataExchange.Types.State
import Network.AWS.DataExchange.Types.Type
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2017-07-25@ of the Amazon Data Exchange SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core._serviceAbbrev = "DataExchange",
      Core._serviceSigner = Sign.v4,
      Core._serviceEndpointPrefix = "dataexchange",
      Core._serviceSigningName = "dataexchange",
      Core._serviceVersion = "2017-07-25",
      Core._serviceEndpoint =
        Core.defaultEndpoint defaultService,
      Core._serviceTimeout = Prelude.Just 70,
      Core._serviceCheck = Core.statusSuccess,
      Core._serviceError =
        Core.parseJSONError "DataExchange",
      Core._serviceRetry = retry
    }
  where
    retry =
      Core.Exponential
        { Core._retryBase = 5.0e-2,
          Core._retryGrowth = 2,
          Core._retryAttempts = 5,
          Core._retryCheck = check
        }
    check e
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has (Core.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Core.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has (Core.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Prelude.otherwise = Prelude.Nothing

-- | The request was invalid.
_ValidationException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ValidationException =
  Core._MatchServiceError
    defaultService
    "ValidationException"
    Prelude.. Core.hasStatus 400

-- | Access to the resource is denied.
_AccessDeniedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_AccessDeniedException =
  Core._MatchServiceError
    defaultService
    "AccessDeniedException"
    Prelude.. Core.hasStatus 403

-- | The request couldn\'t be completed because it conflicted with the
-- current state of the resource.
_ConflictException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ConflictException =
  Core._MatchServiceError
    defaultService
    "ConflictException"
    Prelude.. Core.hasStatus 409

-- | The request has exceeded the quotas imposed by the service.
_ServiceLimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ServiceLimitExceededException =
  Core._MatchServiceError
    defaultService
    "ServiceLimitExceededException"
    Prelude.. Core.hasStatus 402

-- | The limit on the number of requests per second was exceeded.
_ThrottlingException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ThrottlingException =
  Core._MatchServiceError
    defaultService
    "ThrottlingException"
    Prelude.. Core.hasStatus 429

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
