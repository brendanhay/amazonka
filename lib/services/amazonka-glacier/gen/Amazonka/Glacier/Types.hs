{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Glacier.Types
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glacier.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _ServiceUnavailableException,
    _ResourceNotFoundException,
    _LimitExceededException,
    _InsufficientCapacityException,
    _MissingParameterValueException,
    _RequestTimeoutException,
    _InvalidParameterValueException,
    _PolicyEnforcedException,

    -- * ActionCode
    ActionCode (..),

    -- * CannedACL
    CannedACL (..),

    -- * EncryptionType
    EncryptionType (..),

    -- * ExpressionType
    ExpressionType (..),

    -- * FileHeaderInfo
    FileHeaderInfo (..),

    -- * Permission
    Permission (..),

    -- * QuoteFields
    QuoteFields (..),

    -- * StatusCode
    StatusCode (..),

    -- * StorageClass
    StorageClass (..),

    -- * Type
    Type (..),

    -- * ArchiveCreationOutput
    ArchiveCreationOutput (..),
    newArchiveCreationOutput,
    archiveCreationOutput_archiveId,
    archiveCreationOutput_checksum,
    archiveCreationOutput_location,

    -- * CSVInput
    CSVInput (..),
    newCSVInput,
    cSVInput_quoteCharacter,
    cSVInput_comments,
    cSVInput_quoteEscapeCharacter,
    cSVInput_fieldDelimiter,
    cSVInput_recordDelimiter,
    cSVInput_fileHeaderInfo,

    -- * CSVOutput
    CSVOutput (..),
    newCSVOutput,
    cSVOutput_quoteCharacter,
    cSVOutput_quoteFields,
    cSVOutput_quoteEscapeCharacter,
    cSVOutput_fieldDelimiter,
    cSVOutput_recordDelimiter,

    -- * DataRetrievalPolicy
    DataRetrievalPolicy (..),
    newDataRetrievalPolicy,
    dataRetrievalPolicy_rules,

    -- * DataRetrievalRule
    DataRetrievalRule (..),
    newDataRetrievalRule,
    dataRetrievalRule_bytesPerHour,
    dataRetrievalRule_strategy,

    -- * DescribeVaultOutput
    DescribeVaultOutput (..),
    newDescribeVaultOutput,
    describeVaultOutput_lastInventoryDate,
    describeVaultOutput_creationDate,
    describeVaultOutput_vaultName,
    describeVaultOutput_numberOfArchives,
    describeVaultOutput_sizeInBytes,
    describeVaultOutput_vaultARN,

    -- * Encryption
    Encryption (..),
    newEncryption,
    encryption_kmsContext,
    encryption_encryptionType,
    encryption_kmsKeyId,

    -- * GlacierJobDescription
    GlacierJobDescription (..),
    newGlacierJobDescription,
    glacierJobDescription_archiveSHA256TreeHash,
    glacierJobDescription_archiveSizeInBytes,
    glacierJobDescription_sNSTopic,
    glacierJobDescription_completionDate,
    glacierJobDescription_outputLocation,
    glacierJobDescription_retrievalByteRange,
    glacierJobDescription_creationDate,
    glacierJobDescription_jobId,
    glacierJobDescription_inventoryRetrievalParameters,
    glacierJobDescription_selectParameters,
    glacierJobDescription_archiveId,
    glacierJobDescription_inventorySizeInBytes,
    glacierJobDescription_jobOutputPath,
    glacierJobDescription_tier,
    glacierJobDescription_sHA256TreeHash,
    glacierJobDescription_completed,
    glacierJobDescription_vaultARN,
    glacierJobDescription_action,
    glacierJobDescription_statusCode,
    glacierJobDescription_statusMessage,
    glacierJobDescription_jobDescription,

    -- * Grant
    Grant (..),
    newGrant,
    grant_permission,
    grant_grantee,

    -- * Grantee
    Grantee (..),
    newGrantee,
    grantee_displayName,
    grantee_uri,
    grantee_id,
    grantee_emailAddress,
    grantee_type,

    -- * InputSerialization
    InputSerialization (..),
    newInputSerialization,
    inputSerialization_csv,

    -- * InventoryRetrievalJobDescription
    InventoryRetrievalJobDescription (..),
    newInventoryRetrievalJobDescription,
    inventoryRetrievalJobDescription_marker,
    inventoryRetrievalJobDescription_format,
    inventoryRetrievalJobDescription_endDate,
    inventoryRetrievalJobDescription_limit,
    inventoryRetrievalJobDescription_startDate,

    -- * InventoryRetrievalJobInput
    InventoryRetrievalJobInput (..),
    newInventoryRetrievalJobInput,
    inventoryRetrievalJobInput_marker,
    inventoryRetrievalJobInput_endDate,
    inventoryRetrievalJobInput_limit,
    inventoryRetrievalJobInput_startDate,

    -- * JobParameters
    JobParameters (..),
    newJobParameters,
    jobParameters_type,
    jobParameters_sNSTopic,
    jobParameters_format,
    jobParameters_outputLocation,
    jobParameters_retrievalByteRange,
    jobParameters_inventoryRetrievalParameters,
    jobParameters_selectParameters,
    jobParameters_description,
    jobParameters_archiveId,
    jobParameters_tier,

    -- * OutputLocation
    OutputLocation (..),
    newOutputLocation,
    outputLocation_s3,

    -- * OutputSerialization
    OutputSerialization (..),
    newOutputSerialization,
    outputSerialization_csv,

    -- * PartListElement
    PartListElement (..),
    newPartListElement,
    partListElement_rangeInBytes,
    partListElement_sHA256TreeHash,

    -- * ProvisionedCapacityDescription
    ProvisionedCapacityDescription (..),
    newProvisionedCapacityDescription,
    provisionedCapacityDescription_startDate,
    provisionedCapacityDescription_capacityId,
    provisionedCapacityDescription_expirationDate,

    -- * S3Location
    S3Location (..),
    newS3Location,
    s3Location_accessControlList,
    s3Location_bucketName,
    s3Location_tagging,
    s3Location_encryption,
    s3Location_prefix,
    s3Location_storageClass,
    s3Location_cannedACL,
    s3Location_userMetadata,

    -- * SelectParameters
    SelectParameters (..),
    newSelectParameters,
    selectParameters_inputSerialization,
    selectParameters_expression,
    selectParameters_outputSerialization,
    selectParameters_expressionType,

    -- * UploadListElement
    UploadListElement (..),
    newUploadListElement,
    uploadListElement_creationDate,
    uploadListElement_multipartUploadId,
    uploadListElement_archiveDescription,
    uploadListElement_vaultARN,
    uploadListElement_partSizeInBytes,

    -- * VaultAccessPolicy
    VaultAccessPolicy (..),
    newVaultAccessPolicy,
    vaultAccessPolicy_policy,

    -- * VaultLockPolicy
    VaultLockPolicy (..),
    newVaultLockPolicy,
    vaultLockPolicy_policy,

    -- * VaultNotificationConfig
    VaultNotificationConfig (..),
    newVaultNotificationConfig,
    vaultNotificationConfig_sNSTopic,
    vaultNotificationConfig_events,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Glacier.Types.ActionCode
import Amazonka.Glacier.Types.ArchiveCreationOutput
import Amazonka.Glacier.Types.CSVInput
import Amazonka.Glacier.Types.CSVOutput
import Amazonka.Glacier.Types.CannedACL
import Amazonka.Glacier.Types.DataRetrievalPolicy
import Amazonka.Glacier.Types.DataRetrievalRule
import Amazonka.Glacier.Types.DescribeVaultOutput
import Amazonka.Glacier.Types.Encryption
import Amazonka.Glacier.Types.EncryptionType
import Amazonka.Glacier.Types.ExpressionType
import Amazonka.Glacier.Types.FileHeaderInfo
import Amazonka.Glacier.Types.GlacierJobDescription
import Amazonka.Glacier.Types.Grant
import Amazonka.Glacier.Types.Grantee
import Amazonka.Glacier.Types.InputSerialization
import Amazonka.Glacier.Types.InventoryRetrievalJobDescription
import Amazonka.Glacier.Types.InventoryRetrievalJobInput
import Amazonka.Glacier.Types.JobParameters
import Amazonka.Glacier.Types.OutputLocation
import Amazonka.Glacier.Types.OutputSerialization
import Amazonka.Glacier.Types.PartListElement
import Amazonka.Glacier.Types.Permission
import Amazonka.Glacier.Types.ProvisionedCapacityDescription
import Amazonka.Glacier.Types.QuoteFields
import Amazonka.Glacier.Types.S3Location
import Amazonka.Glacier.Types.SelectParameters
import Amazonka.Glacier.Types.StatusCode
import Amazonka.Glacier.Types.StorageClass
import Amazonka.Glacier.Types.Type
import Amazonka.Glacier.Types.UploadListElement
import Amazonka.Glacier.Types.VaultAccessPolicy
import Amazonka.Glacier.Types.VaultLockPolicy
import Amazonka.Glacier.Types.VaultNotificationConfig
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2012-06-01@ of the Amazon Glacier SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev = "Glacier",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "glacier",
      Core.signingName = "glacier",
      Core.version = "2012-06-01",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error = Core.parseJSONError "Glacier",
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
      | Lens.has
          ( Core.hasCode "RequestTimeoutException"
              Prelude.. Core.hasStatus 408
          )
          e =
        Prelude.Just "timeouts"
      | Prelude.otherwise = Prelude.Nothing

-- | Returned if the service cannot complete the request.
_ServiceUnavailableException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ServiceUnavailableException =
  Core._MatchServiceError
    defaultService
    "ServiceUnavailableException"
    Prelude.. Core.hasStatus 500

-- | Returned if the specified resource (such as a vault, upload ID, or job
-- ID) doesn\'t exist.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"
    Prelude.. Core.hasStatus 404

-- | Returned if the request results in a vault or account limit being
-- exceeded.
_LimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_LimitExceededException =
  Core._MatchServiceError
    defaultService
    "LimitExceededException"
    Prelude.. Core.hasStatus 400

-- | Returned if there is insufficient capacity to process this expedited
-- request. This error only applies to expedited retrievals and not to
-- standard or bulk retrievals.
_InsufficientCapacityException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InsufficientCapacityException =
  Core._MatchServiceError
    defaultService
    "InsufficientCapacityException"
    Prelude.. Core.hasStatus 400

-- | Returned if a required header or parameter is missing from the request.
_MissingParameterValueException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_MissingParameterValueException =
  Core._MatchServiceError
    defaultService
    "MissingParameterValueException"
    Prelude.. Core.hasStatus 400

-- | Returned if, when uploading an archive, Amazon S3 Glacier times out
-- while receiving the upload.
_RequestTimeoutException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_RequestTimeoutException =
  Core._MatchServiceError
    defaultService
    "RequestTimeoutException"
    Prelude.. Core.hasStatus 408

-- | Returned if a parameter of the request is incorrectly specified.
_InvalidParameterValueException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidParameterValueException =
  Core._MatchServiceError
    defaultService
    "InvalidParameterValueException"
    Prelude.. Core.hasStatus 400

-- | Returned if a retrieval job would exceed the current data policy\'s
-- retrieval rate limit. For more information about data retrieval
-- policies,
_PolicyEnforcedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_PolicyEnforcedException =
  Core._MatchServiceError
    defaultService
    "PolicyEnforcedException"
    Prelude.. Core.hasStatus 400
