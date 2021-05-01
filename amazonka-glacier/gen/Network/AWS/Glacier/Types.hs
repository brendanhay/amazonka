{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glacier.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glacier.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _ServiceUnavailableException,
    _PolicyEnforcedException,
    _InvalidParameterValueException,
    _LimitExceededException,
    _MissingParameterValueException,
    _InsufficientCapacityException,
    _ResourceNotFoundException,
    _RequestTimeoutException,

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
    archiveCreationOutput_location,
    archiveCreationOutput_checksum,

    -- * CSVInput
    CSVInput (..),
    newCSVInput,
    cSVInput_recordDelimiter,
    cSVInput_quoteCharacter,
    cSVInput_fileHeaderInfo,
    cSVInput_fieldDelimiter,
    cSVInput_comments,
    cSVInput_quoteEscapeCharacter,

    -- * CSVOutput
    CSVOutput (..),
    newCSVOutput,
    cSVOutput_recordDelimiter,
    cSVOutput_quoteCharacter,
    cSVOutput_fieldDelimiter,
    cSVOutput_quoteFields,
    cSVOutput_quoteEscapeCharacter,

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
    describeVaultOutput_vaultARN,
    describeVaultOutput_sizeInBytes,
    describeVaultOutput_numberOfArchives,

    -- * Encryption
    Encryption (..),
    newEncryption,
    encryption_encryptionType,
    encryption_kmsKeyId,
    encryption_kmsContext,

    -- * GlacierJobDescription
    GlacierJobDescription (..),
    newGlacierJobDescription,
    glacierJobDescription_sHA256TreeHash,
    glacierJobDescription_statusMessage,
    glacierJobDescription_jobDescription,
    glacierJobDescription_retrievalByteRange,
    glacierJobDescription_creationDate,
    glacierJobDescription_jobOutputPath,
    glacierJobDescription_selectParameters,
    glacierJobDescription_vaultARN,
    glacierJobDescription_archiveId,
    glacierJobDescription_sNSTopic,
    glacierJobDescription_inventorySizeInBytes,
    glacierJobDescription_statusCode,
    glacierJobDescription_archiveSizeInBytes,
    glacierJobDescription_action,
    glacierJobDescription_inventoryRetrievalParameters,
    glacierJobDescription_completionDate,
    glacierJobDescription_archiveSHA256TreeHash,
    glacierJobDescription_completed,
    glacierJobDescription_jobId,
    glacierJobDescription_outputLocation,
    glacierJobDescription_tier,

    -- * Grant
    Grant (..),
    newGrant,
    grant_grantee,
    grant_permission,

    -- * Grantee
    Grantee (..),
    newGrantee,
    grantee_uri,
    grantee_id,
    grantee_displayName,
    grantee_emailAddress,
    grantee_type,

    -- * InputSerialization
    InputSerialization (..),
    newInputSerialization,
    inputSerialization_csv,

    -- * InventoryRetrievalJobDescription
    InventoryRetrievalJobDescription (..),
    newInventoryRetrievalJobDescription,
    inventoryRetrievalJobDescription_startDate,
    inventoryRetrievalJobDescription_format,
    inventoryRetrievalJobDescription_endDate,
    inventoryRetrievalJobDescription_limit,
    inventoryRetrievalJobDescription_marker,

    -- * InventoryRetrievalJobInput
    InventoryRetrievalJobInput (..),
    newInventoryRetrievalJobInput,
    inventoryRetrievalJobInput_startDate,
    inventoryRetrievalJobInput_endDate,
    inventoryRetrievalJobInput_limit,
    inventoryRetrievalJobInput_marker,

    -- * JobParameters
    JobParameters (..),
    newJobParameters,
    jobParameters_retrievalByteRange,
    jobParameters_format,
    jobParameters_selectParameters,
    jobParameters_archiveId,
    jobParameters_sNSTopic,
    jobParameters_description,
    jobParameters_inventoryRetrievalParameters,
    jobParameters_type,
    jobParameters_outputLocation,
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
    partListElement_sHA256TreeHash,
    partListElement_rangeInBytes,

    -- * ProvisionedCapacityDescription
    ProvisionedCapacityDescription (..),
    newProvisionedCapacityDescription,
    provisionedCapacityDescription_startDate,
    provisionedCapacityDescription_capacityId,
    provisionedCapacityDescription_expirationDate,

    -- * S3Location
    S3Location (..),
    newS3Location,
    s3Location_bucketName,
    s3Location_prefix,
    s3Location_cannedACL,
    s3Location_encryption,
    s3Location_storageClass,
    s3Location_userMetadata,
    s3Location_accessControlList,
    s3Location_tagging,

    -- * SelectParameters
    SelectParameters (..),
    newSelectParameters,
    selectParameters_expressionType,
    selectParameters_outputSerialization,
    selectParameters_inputSerialization,
    selectParameters_expression,

    -- * UploadListElement
    UploadListElement (..),
    newUploadListElement,
    uploadListElement_partSizeInBytes,
    uploadListElement_creationDate,
    uploadListElement_vaultARN,
    uploadListElement_archiveDescription,
    uploadListElement_multipartUploadId,

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
    vaultNotificationConfig_events,
    vaultNotificationConfig_sNSTopic,
  )
where

import Network.AWS.Glacier.Types.ActionCode
import Network.AWS.Glacier.Types.ArchiveCreationOutput
import Network.AWS.Glacier.Types.CSVInput
import Network.AWS.Glacier.Types.CSVOutput
import Network.AWS.Glacier.Types.CannedACL
import Network.AWS.Glacier.Types.DataRetrievalPolicy
import Network.AWS.Glacier.Types.DataRetrievalRule
import Network.AWS.Glacier.Types.DescribeVaultOutput
import Network.AWS.Glacier.Types.Encryption
import Network.AWS.Glacier.Types.EncryptionType
import Network.AWS.Glacier.Types.ExpressionType
import Network.AWS.Glacier.Types.FileHeaderInfo
import Network.AWS.Glacier.Types.GlacierJobDescription
import Network.AWS.Glacier.Types.Grant
import Network.AWS.Glacier.Types.Grantee
import Network.AWS.Glacier.Types.InputSerialization
import Network.AWS.Glacier.Types.InventoryRetrievalJobDescription
import Network.AWS.Glacier.Types.InventoryRetrievalJobInput
import Network.AWS.Glacier.Types.JobParameters
import Network.AWS.Glacier.Types.OutputLocation
import Network.AWS.Glacier.Types.OutputSerialization
import Network.AWS.Glacier.Types.PartListElement
import Network.AWS.Glacier.Types.Permission
import Network.AWS.Glacier.Types.ProvisionedCapacityDescription
import Network.AWS.Glacier.Types.QuoteFields
import Network.AWS.Glacier.Types.S3Location
import Network.AWS.Glacier.Types.SelectParameters
import Network.AWS.Glacier.Types.StatusCode
import Network.AWS.Glacier.Types.StorageClass
import Network.AWS.Glacier.Types.Type
import Network.AWS.Glacier.Types.UploadListElement
import Network.AWS.Glacier.Types.VaultAccessPolicy
import Network.AWS.Glacier.Types.VaultLockPolicy
import Network.AWS.Glacier.Types.VaultNotificationConfig
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2012-06-01@ of the Amazon Glacier SDK configuration.
defaultService :: Prelude.Service
defaultService =
  Prelude.Service
    { Prelude._svcAbbrev = "Glacier",
      Prelude._svcSigner = Sign.v4,
      Prelude._svcPrefix = "glacier",
      Prelude._svcVersion = "2012-06-01",
      Prelude._svcEndpoint =
        Prelude.defaultEndpoint defaultService,
      Prelude._svcTimeout = Prelude.Just 70,
      Prelude._svcCheck = Prelude.statusSuccess,
      Prelude._svcError = Prelude.parseJSONError "Glacier",
      Prelude._svcRetry = retry
    }
  where
    retry =
      Prelude.Exponential
        { Prelude._retryBase = 5.0e-2,
          Prelude._retryGrowth = 2,
          Prelude._retryAttempts = 5,
          Prelude._retryCheck = check
        }
    check e
      | Lens.has (Prelude.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Prelude.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Lens.has (Prelude.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Prelude.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Prelude.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Lens.has
          ( Prelude.hasCode "RequestThrottledException"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has
          ( Prelude.hasCode "ThrottledException"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has (Prelude.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Lens.has (Prelude.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has
          ( Prelude.hasCode "RequestTimeoutException"
              Prelude.. Prelude.hasStatus 408
          )
          e =
        Prelude.Just "timeouts"
      | Lens.has
          ( Prelude.hasCode "ThrottlingException"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has
          ( Prelude.hasCode "Throttling"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Prelude.otherwise = Prelude.Nothing

-- | Returned if the service cannot complete the request.
_ServiceUnavailableException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_ServiceUnavailableException =
  Prelude._MatchServiceError
    defaultService
    "ServiceUnavailableException"
    Prelude.. Prelude.hasStatus 500

-- | Returned if a retrieval job would exceed the current data policy\'s
-- retrieval rate limit. For more information about data retrieval
-- policies,
_PolicyEnforcedException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_PolicyEnforcedException =
  Prelude._MatchServiceError
    defaultService
    "PolicyEnforcedException"
    Prelude.. Prelude.hasStatus 400

-- | Returned if a parameter of the request is incorrectly specified.
_InvalidParameterValueException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidParameterValueException =
  Prelude._MatchServiceError
    defaultService
    "InvalidParameterValueException"
    Prelude.. Prelude.hasStatus 400

-- | Returned if the request results in a vault or account limit being
-- exceeded.
_LimitExceededException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_LimitExceededException =
  Prelude._MatchServiceError
    defaultService
    "LimitExceededException"
    Prelude.. Prelude.hasStatus 400

-- | Returned if a required header or parameter is missing from the request.
_MissingParameterValueException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_MissingParameterValueException =
  Prelude._MatchServiceError
    defaultService
    "MissingParameterValueException"
    Prelude.. Prelude.hasStatus 400

-- | Returned if there is insufficient capacity to process this expedited
-- request. This error only applies to expedited retrievals and not to
-- standard or bulk retrievals.
_InsufficientCapacityException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InsufficientCapacityException =
  Prelude._MatchServiceError
    defaultService
    "InsufficientCapacityException"
    Prelude.. Prelude.hasStatus 400

-- | Returned if the specified resource (such as a vault, upload ID, or job
-- ID) doesn\'t exist.
_ResourceNotFoundException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_ResourceNotFoundException =
  Prelude._MatchServiceError
    defaultService
    "ResourceNotFoundException"
    Prelude.. Prelude.hasStatus 404

-- | Returned if, when uploading an archive, Amazon S3 Glacier times out
-- while receiving the upload.
_RequestTimeoutException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_RequestTimeoutException =
  Prelude._MatchServiceError
    defaultService
    "RequestTimeoutException"
    Prelude.. Prelude.hasStatus 408
