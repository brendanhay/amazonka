-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glacier.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glacier.Types
  ( -- * Service configuration
    glacierService,

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
    ArchiveCreationOutput (..),
    mkArchiveCreationOutput,
    acoArchiveId,
    acoChecksum,
    acoLocation,

    -- * CSVInput
    CSVInput (..),
    mkCSVInput,
    ciQuoteCharacter,
    ciRecordDelimiter,
    ciFileHeaderInfo,
    ciQuoteEscapeCharacter,
    ciComments,
    ciFieldDelimiter,

    -- * CSVOutput
    CSVOutput (..),
    mkCSVOutput,
    coQuoteCharacter,
    coQuoteFields,
    coRecordDelimiter,
    coQuoteEscapeCharacter,
    coFieldDelimiter,

    -- * DataRetrievalPolicy
    DataRetrievalPolicy (..),
    mkDataRetrievalPolicy,
    drpRules,

    -- * DataRetrievalRule
    DataRetrievalRule (..),
    mkDataRetrievalRule,
    drrStrategy,
    drrBytesPerHour,

    -- * DescribeVaultOutput
    DescribeVaultOutput (..),
    mkDescribeVaultOutput,
    dvoVaultName,
    dvoSizeInBytes,
    dvoLastInventoryDate,
    dvoVaultARN,
    dvoCreationDate,
    dvoNumberOfArchives,

    -- * Encryption
    Encryption (..),
    mkEncryption,
    eEncryptionType,
    eKMSKeyId,
    eKMSContext,

    -- * GlacierJobDescription
    GlacierJobDescription (..),
    mkGlacierJobDescription,
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
    Grant (..),
    mkGrant,
    gPermission,
    gGrantee,

    -- * Grantee
    Grantee (..),
    mkGrantee,
    gURI,
    gEmailAddress,
    gDisplayName,
    gId,
    gType,

    -- * InputSerialization
    InputSerialization (..),
    mkInputSerialization,
    isCsv,

    -- * InventoryRetrievalJobDescription
    InventoryRetrievalJobDescription (..),
    mkInventoryRetrievalJobDescription,
    irjdFormat,
    irjdEndDate,
    irjdStartDate,
    irjdMarker,
    irjdLimit,

    -- * InventoryRetrievalJobInput
    InventoryRetrievalJobInput (..),
    mkInventoryRetrievalJobInput,
    irjiEndDate,
    irjiStartDate,
    irjiMarker,
    irjiLimit,

    -- * JobParameters
    JobParameters (..),
    mkJobParameters,
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
    OutputLocation (..),
    mkOutputLocation,
    olS3,

    -- * OutputSerialization
    OutputSerialization (..),
    mkOutputSerialization,
    osCsv,

    -- * PartListElement
    PartListElement (..),
    mkPartListElement,
    pleSHA256TreeHash,
    pleRangeInBytes,

    -- * ProvisionedCapacityDescription
    ProvisionedCapacityDescription (..),
    mkProvisionedCapacityDescription,
    pcdCapacityId,
    pcdStartDate,
    pcdExpirationDate,

    -- * S3Location
    S3Location (..),
    mkS3Location,
    slCannedACL,
    slPrefix,
    slBucketName,
    slAccessControlList,
    slUserMetadata,
    slEncryption,
    slStorageClass,
    slTagging,

    -- * SelectParameters
    SelectParameters (..),
    mkSelectParameters,
    spExpressionType,
    spOutputSerialization,
    spExpression,
    spInputSerialization,

    -- * UploadListElement
    UploadListElement (..),
    mkUploadListElement,
    uleMultipartUploadId,
    ulePartSizeInBytes,
    uleArchiveDescription,
    uleVaultARN,
    uleCreationDate,

    -- * VaultAccessPolicy
    VaultAccessPolicy (..),
    mkVaultAccessPolicy,
    vapPolicy,

    -- * VaultLockPolicy
    VaultLockPolicy (..),
    mkVaultLockPolicy,
    vlpPolicy,

    -- * VaultNotificationConfig
    VaultNotificationConfig (..),
    mkVaultNotificationConfig,
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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2012-06-01@ of the Amazon Glacier SDK configuration.
glacierService :: Lude.Service
glacierService =
  Lude.Service
    { Lude._svcAbbrev = "Glacier",
      Lude._svcSigner = Sign.v4,
      Lude._svcPrefix = "glacier",
      Lude._svcVersion = "2012-06-01",
      Lude._svcEndpoint = Lude.defaultEndpoint glacierService,
      Lude._svcTimeout = Lude.Just 70,
      Lude._svcCheck = Lude.statusSuccess,
      Lude._svcError = Lude.parseJSONError "Glacier",
      Lude._svcRetry = retry
    }
  where
    retry =
      Lude.Exponential
        { Lude._retryBase = 5.0e-2,
          Lude._retryGrowth = 2,
          Lude._retryAttempts = 5,
          Lude._retryCheck = check
        }
    check e
      | Lens.has
          (Lude.hasCode "ThrottledException" Lude.. Lude.hasStatus 400)
          e =
        Lude.Just "throttled_exception"
      | Lens.has (Lude.hasStatus 429) e = Lude.Just "too_many_requests"
      | Lens.has
          (Lude.hasCode "ThrottlingException" Lude.. Lude.hasStatus 400)
          e =
        Lude.Just "throttling_exception"
      | Lens.has (Lude.hasCode "Throttling" Lude.. Lude.hasStatus 400) e =
        Lude.Just "throttling"
      | Lens.has
          ( Lude.hasCode "ProvisionedThroughputExceededException"
              Lude.. Lude.hasStatus 400
          )
          e =
        Lude.Just "throughput_exceeded"
      | Lens.has (Lude.hasStatus 504) e = Lude.Just "gateway_timeout"
      | Lens.has
          ( Lude.hasCode "RequestThrottledException"
              Lude.. Lude.hasStatus 400
          )
          e =
        Lude.Just "request_throttled_exception"
      | Lens.has (Lude.hasStatus 502) e = Lude.Just "bad_gateway"
      | Lens.has (Lude.hasStatus 503) e = Lude.Just "service_unavailable"
      | Lens.has (Lude.hasStatus 500) e =
        Lude.Just "general_server_error"
      | Lens.has (Lude.hasStatus 509) e = Lude.Just "limit_exceeded"
      | Lens.has
          (Lude.hasCode "RequestTimeoutException" Lude.. Lude.hasStatus 408)
          e =
        Lude.Just "timeouts"
      | Lude.otherwise = Lude.Nothing
