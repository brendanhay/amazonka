{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glacier.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glacier.Types
  ( -- * Service Configuration
    glacier,

    -- * Errors

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
    ArchiveCreationOutput,
    archiveCreationOutput,
    acoArchiveId,
    acoChecksum,
    acoLocation,

    -- * CSVInput
    CSVInput,
    csvInput,
    ciQuoteCharacter,
    ciRecordDelimiter,
    ciFileHeaderInfo,
    ciQuoteEscapeCharacter,
    ciComments,
    ciFieldDelimiter,

    -- * CSVOutput
    CSVOutput,
    csvOutput,
    coQuoteCharacter,
    coQuoteFields,
    coRecordDelimiter,
    coQuoteEscapeCharacter,
    coFieldDelimiter,

    -- * DataRetrievalPolicy
    DataRetrievalPolicy,
    dataRetrievalPolicy,
    drpRules,

    -- * DataRetrievalRule
    DataRetrievalRule,
    dataRetrievalRule,
    drrStrategy,
    drrBytesPerHour,

    -- * DescribeVaultOutput
    DescribeVaultOutput,
    describeVaultOutput,
    dvoVaultName,
    dvoSizeInBytes,
    dvoLastInventoryDate,
    dvoVaultARN,
    dvoCreationDate,
    dvoNumberOfArchives,

    -- * Encryption
    Encryption,
    encryption,
    eEncryptionType,
    eKMSKeyId,
    eKMSContext,

    -- * GlacierJobDescription
    GlacierJobDescription,
    glacierJobDescription,
    gjdSHA256TreeHash,
    gjdArchiveId,
    gjdSelectParameters,
    gjdJobId,
    gjdJobOutputPath,
    gjdRetrievalByteRange,
    gjdInventoryRetrievalParameters,
    gjdAction,
    gjdJobDescription,
    gjdSNSTopic,
    gjdStatusMessage,
    gjdVaultARN,
    gjdOutputLocation,
    gjdTier,
    gjdArchiveSHA256TreeHash,
    gjdCreationDate,
    gjdCompleted,
    gjdCompletionDate,
    gjdInventorySizeInBytes,
    gjdArchiveSizeInBytes,
    gjdStatusCode,

    -- * Grant
    Grant,
    grant,
    gPermission,
    gGrantee,

    -- * Grantee
    Grantee,
    grantee,
    gURI,
    gEmailAddress,
    gDisplayName,
    gId,
    gType,

    -- * InputSerialization
    InputSerialization,
    inputSerialization,
    isCsv,

    -- * InventoryRetrievalJobDescription
    InventoryRetrievalJobDescription,
    inventoryRetrievalJobDescription,
    irjdFormat,
    irjdEndDate,
    irjdStartDate,
    irjdMarker,
    irjdLimit,

    -- * InventoryRetrievalJobInput
    InventoryRetrievalJobInput,
    inventoryRetrievalJobInput,
    irjiEndDate,
    irjiStartDate,
    irjiMarker,
    irjiLimit,

    -- * JobParameters
    JobParameters,
    jobParameters,
    jpArchiveId,
    jpSelectParameters,
    jpFormat,
    jpRetrievalByteRange,
    jpInventoryRetrievalParameters,
    jpSNSTopic,
    jpOutputLocation,
    jpTier,
    jpType,
    jpDescription,

    -- * OutputLocation
    OutputLocation,
    outputLocation,
    olS3,

    -- * OutputSerialization
    OutputSerialization,
    outputSerialization,
    osCsv,

    -- * PartListElement
    PartListElement,
    partListElement,
    pleSHA256TreeHash,
    pleRangeInBytes,

    -- * ProvisionedCapacityDescription
    ProvisionedCapacityDescription,
    provisionedCapacityDescription,
    pcdCapacityId,
    pcdStartDate,
    pcdExpirationDate,

    -- * S3Location
    S3Location,
    s3Location,
    slCannedACL,
    slPrefix,
    slBucketName,
    slAccessControlList,
    slUserMetadata,
    slEncryption,
    slStorageClass,
    slTagging,

    -- * SelectParameters
    SelectParameters,
    selectParameters,
    spExpressionType,
    spOutputSerialization,
    spExpression,
    spInputSerialization,

    -- * UploadListElement
    UploadListElement,
    uploadListElement,
    uleMultipartUploadId,
    ulePartSizeInBytes,
    uleArchiveDescription,
    uleVaultARN,
    uleCreationDate,

    -- * VaultAccessPolicy
    VaultAccessPolicy,
    vaultAccessPolicy,
    vapPolicy,

    -- * VaultLockPolicy
    VaultLockPolicy,
    vaultLockPolicy,
    vlpPolicy,

    -- * VaultNotificationConfig
    VaultNotificationConfig,
    vaultNotificationConfig,
    vncSNSTopic,
    vncEvents,
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
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Sign.V4

-- | API version @2012-06-01@ of the Amazon Glacier SDK configuration.
glacier :: Service
glacier =
  Service
    { _svcAbbrev = "Glacier",
      _svcSigner = v4,
      _svcPrefix = "glacier",
      _svcVersion = "2012-06-01",
      _svcEndpoint = defaultEndpoint glacier,
      _svcTimeout = Just 70,
      _svcCheck = statusSuccess,
      _svcError = parseJSONError "Glacier",
      _svcRetry = retry
    }
  where
    retry =
      Exponential
        { _retryBase = 5.0e-2,
          _retryGrowth = 2,
          _retryAttempts = 5,
          _retryCheck = check
        }
    check e
      | has (hasCode "ThrottledException" . hasStatus 400) e =
        Just "throttled_exception"
      | has (hasStatus 429) e = Just "too_many_requests"
      | has (hasCode "ThrottlingException" . hasStatus 400) e =
        Just "throttling_exception"
      | has (hasCode "Throttling" . hasStatus 400) e = Just "throttling"
      | has
          (hasCode "ProvisionedThroughputExceededException" . hasStatus 400)
          e =
        Just "throughput_exceeded"
      | has (hasStatus 504) e = Just "gateway_timeout"
      | has (hasCode "RequestThrottledException" . hasStatus 400) e =
        Just "request_throttled_exception"
      | has (hasStatus 502) e = Just "bad_gateway"
      | has (hasStatus 503) e = Just "service_unavailable"
      | has (hasCode "RequestTimeoutException" . hasStatus 408) e =
        Just "timeouts"
      | has (hasStatus 500) e = Just "general_server_error"
      | has (hasStatus 509) e = Just "limit_exceeded"
      | otherwise = Nothing
