-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glacier.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Glacier.Types
    (
    -- * Service configuration
      mkServiceConfig

    -- * Errors
    , _PolicyEnforcedException
    , _InvalidParameterValueException
    , _RequestTimeoutException
    , _ServiceUnavailableException
    , _InsufficientCapacityException
    , _ResourceNotFoundException
    , _LimitExceededException
    , _MissingParameterValueException

    -- * ArchiveCreationOutput
    , ArchiveCreationOutput (..)
    , mkArchiveCreationOutput
    , acoArchiveId
    , acoChecksum
    , acoLocation

    -- * EncryptionType
    , EncryptionType (..)

    -- * UploadListElement
    , UploadListElement (..)
    , mkUploadListElement
    , uleArchiveDescription
    , uleCreationDate
    , uleMultipartUploadId
    , ulePartSizeInBytes
    , uleVaultARN

    -- * ExpressionType
    , ExpressionType (..)

    -- * SelectParameters
    , SelectParameters (..)
    , mkSelectParameters
    , spExpression
    , spExpressionType
    , spInputSerialization
    , spOutputSerialization

    -- * InventoryRetrievalJobDescription
    , InventoryRetrievalJobDescription (..)
    , mkInventoryRetrievalJobDescription
    , irjdEndDate
    , irjdFormat
    , irjdLimit
    , irjdMarker
    , irjdStartDate

    -- * CannedACL
    , CannedACL (..)

    -- * JobParameters
    , JobParameters (..)
    , mkJobParameters
    , jpArchiveId
    , jpDescription
    , jpFormat
    , jpInventoryRetrievalParameters
    , jpOutputLocation
    , jpRetrievalByteRange
    , jpSNSTopic
    , jpSelectParameters
    , jpTier
    , jpType

    -- * DescribeVaultOutput
    , DescribeVaultOutput (..)
    , mkDescribeVaultOutput
    , dvoCreationDate
    , dvoLastInventoryDate
    , dvoNumberOfArchives
    , dvoSizeInBytes
    , dvoVaultARN
    , dvoVaultName

    -- * CSVInput
    , CSVInput (..)
    , mkCSVInput
    , csviComments
    , csviFieldDelimiter
    , csviFileHeaderInfo
    , csviQuoteCharacter
    , csviQuoteEscapeCharacter
    , csviRecordDelimiter

    -- * DataRetrievalRule
    , DataRetrievalRule (..)
    , mkDataRetrievalRule
    , drrBytesPerHour
    , drrStrategy

    -- * QuoteFields
    , QuoteFields (..)

    -- * ActionCode
    , ActionCode (..)

    -- * Grant
    , Grant (..)
    , mkGrant
    , gGrantee
    , gPermission

    -- * TagValue
    , TagValue (..)

    -- * OutputSerialization
    , OutputSerialization (..)
    , mkOutputSerialization
    , osCsv

    -- * CSVOutput
    , CSVOutput (..)
    , mkCSVOutput
    , csvoFieldDelimiter
    , csvoQuoteCharacter
    , csvoQuoteEscapeCharacter
    , csvoQuoteFields
    , csvoRecordDelimiter

    -- * Encryption
    , Encryption (..)
    , mkEncryption
    , eEncryptionType
    , eKMSContext
    , eKMSKeyId

    -- * FileHeaderInfo
    , FileHeaderInfo (..)

    -- * StorageClass
    , StorageClass (..)

    -- * OutputLocation
    , OutputLocation (..)
    , mkOutputLocation
    , olS3

    -- * ProvisionedCapacityDescription
    , ProvisionedCapacityDescription (..)
    , mkProvisionedCapacityDescription
    , pcdCapacityId
    , pcdExpirationDate
    , pcdStartDate

    -- * VaultNotificationConfig
    , VaultNotificationConfig (..)
    , mkVaultNotificationConfig
    , vncEvents
    , vncSNSTopic

    -- * InventoryRetrievalJobInput
    , InventoryRetrievalJobInput (..)
    , mkInventoryRetrievalJobInput
    , irjiEndDate
    , irjiLimit
    , irjiMarker
    , irjiStartDate

    -- * TagKey
    , TagKey (..)

    -- * S3Location
    , S3Location (..)
    , mkS3Location
    , slAccessControlList
    , slBucketName
    , slCannedACL
    , slEncryption
    , slPrefix
    , slStorageClass
    , slTagging
    , slUserMetadata

    -- * Type
    , Type (..)

    -- * InputSerialization
    , InputSerialization (..)
    , mkInputSerialization
    , isCsv

    -- * PartListElement
    , PartListElement (..)
    , mkPartListElement
    , pleRangeInBytes
    , pleSHA256TreeHash

    -- * Permission
    , Permission (..)

    -- * Grantee
    , Grantee (..)
    , mkGrantee
    , gType
    , gDisplayName
    , gEmailAddress
    , gID
    , gURI

    -- * DataRetrievalPolicy
    , DataRetrievalPolicy (..)
    , mkDataRetrievalPolicy
    , drpRules

    -- * GlacierJobDescription
    , GlacierJobDescription (..)
    , mkGlacierJobDescription
    , gjdAction
    , gjdArchiveId
    , gjdArchiveSHA256TreeHash
    , gjdArchiveSizeInBytes
    , gjdCompleted
    , gjdCompletionDate
    , gjdCreationDate
    , gjdInventoryRetrievalParameters
    , gjdInventorySizeInBytes
    , gjdJobDescription
    , gjdJobId
    , gjdJobOutputPath
    , gjdOutputLocation
    , gjdRetrievalByteRange
    , gjdSHA256TreeHash
    , gjdSNSTopic
    , gjdSelectParameters
    , gjdStatusCode
    , gjdStatusMessage
    , gjdTier
    , gjdVaultARN

    -- * VaultAccessPolicy
    , VaultAccessPolicy (..)
    , mkVaultAccessPolicy
    , vapPolicy

    -- * VaultLockPolicy
    , VaultLockPolicy (..)
    , mkVaultLockPolicy
    , vlpPolicy

    -- * StatusCode
    , StatusCode (..)

    -- * EndDate
    , EndDate (..)

    -- * StartDate
    , StartDate (..)
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Sign.V4 as Sign
import Network.AWS.Glacier.Types.ArchiveCreationOutput
  
import Network.AWS.Glacier.Types.EncryptionType
  
import Network.AWS.Glacier.Types.UploadListElement
  
import Network.AWS.Glacier.Types.ExpressionType
  
import Network.AWS.Glacier.Types.SelectParameters
  
import Network.AWS.Glacier.Types.InventoryRetrievalJobDescription
  
import Network.AWS.Glacier.Types.CannedACL
  
  
import Network.AWS.Glacier.Types.JobParameters
  
import Network.AWS.Glacier.Types.DescribeVaultOutput
  
import Network.AWS.Glacier.Types.CSVInput
  
import Network.AWS.Glacier.Types.DataRetrievalRule
  
import Network.AWS.Glacier.Types.QuoteFields
  
import Network.AWS.Glacier.Types.ActionCode
  
import Network.AWS.Glacier.Types.Grant
  
import Network.AWS.Glacier.Types.TagValue
  
import Network.AWS.Glacier.Types.OutputSerialization
  
import Network.AWS.Glacier.Types.CSVOutput
  
  
import Network.AWS.Glacier.Types.Encryption
  
import Network.AWS.Glacier.Types.FileHeaderInfo
  
import Network.AWS.Glacier.Types.StorageClass
  
import Network.AWS.Glacier.Types.OutputLocation
  
import Network.AWS.Glacier.Types.ProvisionedCapacityDescription
  
import Network.AWS.Glacier.Types.VaultNotificationConfig
  
  
import Network.AWS.Glacier.Types.InventoryRetrievalJobInput
  
import Network.AWS.Glacier.Types.TagKey
  
import Network.AWS.Glacier.Types.S3Location
  
  
import Network.AWS.Glacier.Types.Type
  
import Network.AWS.Glacier.Types.InputSerialization
  
import Network.AWS.Glacier.Types.PartListElement
  
import Network.AWS.Glacier.Types.Permission
  
import Network.AWS.Glacier.Types.Grantee
  
import Network.AWS.Glacier.Types.DataRetrievalPolicy
  
  
import Network.AWS.Glacier.Types.GlacierJobDescription
  
import Network.AWS.Glacier.Types.VaultAccessPolicy
  
  
import Network.AWS.Glacier.Types.VaultLockPolicy
  
import Network.AWS.Glacier.Types.StatusCode
  
  
  
import Network.AWS.Glacier.Types.EndDate
  
import Network.AWS.Glacier.Types.StartDate
  

-- | API version @2012-06-01@ of the Amazon Glacier SDK configuration.
mkServiceConfig :: Core.Service
mkServiceConfig
  = Core.Service{Core._svcAbbrev = "Glacier",
                 Core._svcSigner = Sign.v4, Core._svcPrefix = "glacier",
                 Core._svcVersion = "2012-06-01", Core._svcTimeout = Core.Just 70,
                 Core._svcCheck = Core.statusSuccess, Core._svcRetry = retry,
                 Core._svcError = Core.parseJSONError "Glacier",
                 Core._svcEndpoint = Core.defaultEndpoint mkServiceConfig}
  where retry
          = Core.Exponential{Core._retryBase = 5.0e-2, Core._retryGrowth = 2,
                             Core._retryAttempts = 5, Core._retryCheck = check}
        check e
          | Lens.has
              (Core.hasCode "ThrottledException" Core.. Core.hasStatus 400)
              e
            = Core.Just "throttled_exception"
          | Lens.has (Core.hasStatus 429) e = Core.Just "too_many_requests"
          | Lens.has
              (Core.hasCode "ThrottlingException" Core.. Core.hasStatus 400)
              e
            = Core.Just "throttling_exception"
          | Lens.has (Core.hasCode "Throttling" Core.. Core.hasStatus 400) e
            = Core.Just "throttling"
          | Lens.has
              (Core.hasCode "ProvisionedThroughputExceededException" Core..
                 Core.hasStatus 400)
              e
            = Core.Just "throughput_exceeded"
          | Lens.has (Core.hasStatus 504) e = Core.Just "gateway_timeout"
          | Lens.has
              (Core.hasCode "RequestThrottledException" Core..
                 Core.hasStatus 400)
              e
            = Core.Just "request_throttled_exception"
          | Lens.has (Core.hasStatus 502) e = Core.Just "bad_gateway"
          | Lens.has (Core.hasStatus 503) e = Core.Just "service_unavailable"
          | Lens.has (Core.hasStatus 500) e =
            Core.Just "general_server_error"
          | Lens.has (Core.hasStatus 509) e = Core.Just "limit_exceeded"
          | Lens.has
              (Core.hasCode "RequestTimeoutException" Core.. Core.hasStatus 408)
              e
            = Core.Just "timeouts"
          | Core.otherwise = Core.Nothing

-- | Returned if a retrieval job would exceed the current data policy's retrieval rate limit. For more information about data retrieval policies,
_PolicyEnforcedException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_PolicyEnforcedException
  = Core._MatchServiceError mkServiceConfig "PolicyEnforcedException"
      Core.. Core.hasStatues 400
{-# INLINEABLE _PolicyEnforcedException #-}
{-# DEPRECATED _PolicyEnforcedException "Use generic-lens or generic-optics instead"  #-}

-- | Returned if a parameter of the request is incorrectly specified.
_InvalidParameterValueException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidParameterValueException
  = Core._MatchServiceError mkServiceConfig
      "InvalidParameterValueException"
      Core.. Core.hasStatues 400
{-# INLINEABLE _InvalidParameterValueException #-}
{-# DEPRECATED _InvalidParameterValueException "Use generic-lens or generic-optics instead"  #-}

-- | Returned if, when uploading an archive, Amazon S3 Glacier times out while receiving the upload.
_RequestTimeoutException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_RequestTimeoutException
  = Core._MatchServiceError mkServiceConfig "RequestTimeoutException"
      Core.. Core.hasStatues 408
{-# INLINEABLE _RequestTimeoutException #-}
{-# DEPRECATED _RequestTimeoutException "Use generic-lens or generic-optics instead"  #-}

-- | Returned if the service cannot complete the request.
_ServiceUnavailableException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ServiceUnavailableException
  = Core._MatchServiceError mkServiceConfig
      "ServiceUnavailableException"
      Core.. Core.hasStatues 500
{-# INLINEABLE _ServiceUnavailableException #-}
{-# DEPRECATED _ServiceUnavailableException "Use generic-lens or generic-optics instead"  #-}

-- | Returned if there is insufficient capacity to process this expedited request. This error only applies to expedited retrievals and not to standard or bulk retrievals.
_InsufficientCapacityException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InsufficientCapacityException
  = Core._MatchServiceError mkServiceConfig
      "InsufficientCapacityException"
      Core.. Core.hasStatues 400
{-# INLINEABLE _InsufficientCapacityException #-}
{-# DEPRECATED _InsufficientCapacityException "Use generic-lens or generic-optics instead"  #-}

-- | Returned if the specified resource (such as a vault, upload ID, or job ID) doesn't exist.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException
  = Core._MatchServiceError mkServiceConfig
      "ResourceNotFoundException"
      Core.. Core.hasStatues 404
{-# INLINEABLE _ResourceNotFoundException #-}
{-# DEPRECATED _ResourceNotFoundException "Use generic-lens or generic-optics instead"  #-}

-- | Returned if the request results in a vault or account limit being exceeded.
_LimitExceededException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_LimitExceededException
  = Core._MatchServiceError mkServiceConfig "LimitExceededException"
      Core.. Core.hasStatues 400
{-# INLINEABLE _LimitExceededException #-}
{-# DEPRECATED _LimitExceededException "Use generic-lens or generic-optics instead"  #-}

-- | Returned if a required header or parameter is missing from the request.
_MissingParameterValueException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_MissingParameterValueException
  = Core._MatchServiceError mkServiceConfig
      "MissingParameterValueException"
      Core.. Core.hasStatues 400
{-# INLINEABLE _MissingParameterValueException #-}
{-# DEPRECATED _MissingParameterValueException "Use generic-lens or generic-optics instead"  #-}
