{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glacier.Types
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Glacier.Types
    (
    -- * Service Configuration
      glacier

    -- * Errors
    , _PolicyEnforcedException
    , _InvalidParameterValueException
    , _RequestTimeoutException
    , _ServiceUnavailableException
    , _InsufficientCapacityException
    , _ResourceNotFoundException
    , _LimitExceededException
    , _MissingParameterValueException

    -- * ActionCode
    , ActionCode (..)

    -- * CannedACL
    , CannedACL (..)

    -- * EncryptionType
    , EncryptionType (..)

    -- * ExpressionType
    , ExpressionType (..)

    -- * FileHeaderInfo
    , FileHeaderInfo (..)

    -- * Permission
    , Permission (..)

    -- * QuoteFields
    , QuoteFields (..)

    -- * StatusCode
    , StatusCode (..)

    -- * StorageClass
    , StorageClass (..)

    -- * Type
    , Type (..)

    -- * ArchiveCreationOutput
    , ArchiveCreationOutput
    , archiveCreationOutput
    , acoArchiveId
    , acoChecksum
    , acoLocation

    -- * CSVInput
    , CSVInput
    , csvInput
    , ciQuoteCharacter
    , ciRecordDelimiter
    , ciFileHeaderInfo
    , ciQuoteEscapeCharacter
    , ciComments
    , ciFieldDelimiter

    -- * CSVOutput
    , CSVOutput
    , csvOutput
    , coQuoteCharacter
    , coQuoteFields
    , coRecordDelimiter
    , coQuoteEscapeCharacter
    , coFieldDelimiter

    -- * DataRetrievalPolicy
    , DataRetrievalPolicy
    , dataRetrievalPolicy
    , drpRules

    -- * DataRetrievalRule
    , DataRetrievalRule
    , dataRetrievalRule
    , drrStrategy
    , drrBytesPerHour

    -- * DescribeVaultOutput
    , DescribeVaultOutput
    , describeVaultOutput
    , dvoVaultName
    , dvoSizeInBytes
    , dvoLastInventoryDate
    , dvoVaultARN
    , dvoCreationDate
    , dvoNumberOfArchives

    -- * Encryption
    , Encryption
    , encryption
    , eEncryptionType
    , eKMSKeyId
    , eKMSContext

    -- * GlacierJobDescription
    , GlacierJobDescription
    , glacierJobDescription
    , gjdSHA256TreeHash
    , gjdArchiveId
    , gjdSelectParameters
    , gjdJobId
    , gjdJobOutputPath
    , gjdRetrievalByteRange
    , gjdInventoryRetrievalParameters
    , gjdAction
    , gjdJobDescription
    , gjdSNSTopic
    , gjdStatusMessage
    , gjdVaultARN
    , gjdOutputLocation
    , gjdTier
    , gjdArchiveSHA256TreeHash
    , gjdCreationDate
    , gjdCompleted
    , gjdCompletionDate
    , gjdInventorySizeInBytes
    , gjdArchiveSizeInBytes
    , gjdStatusCode

    -- * Grant
    , Grant
    , grant
    , gPermission
    , gGrantee

    -- * Grantee
    , Grantee
    , grantee
    , gURI
    , gEmailAddress
    , gDisplayName
    , gId
    , gType

    -- * InputSerialization
    , InputSerialization
    , inputSerialization
    , isCsv

    -- * InventoryRetrievalJobDescription
    , InventoryRetrievalJobDescription
    , inventoryRetrievalJobDescription
    , irjdFormat
    , irjdEndDate
    , irjdStartDate
    , irjdMarker
    , irjdLimit

    -- * InventoryRetrievalJobInput
    , InventoryRetrievalJobInput
    , inventoryRetrievalJobInput
    , irjiEndDate
    , irjiStartDate
    , irjiMarker
    , irjiLimit

    -- * JobParameters
    , JobParameters
    , jobParameters
    , jpArchiveId
    , jpSelectParameters
    , jpFormat
    , jpRetrievalByteRange
    , jpInventoryRetrievalParameters
    , jpSNSTopic
    , jpOutputLocation
    , jpTier
    , jpType
    , jpDescription

    -- * OutputLocation
    , OutputLocation
    , outputLocation
    , olS3

    -- * OutputSerialization
    , OutputSerialization
    , outputSerialization
    , osCsv

    -- * PartListElement
    , PartListElement
    , partListElement
    , pleSHA256TreeHash
    , pleRangeInBytes

    -- * ProvisionedCapacityDescription
    , ProvisionedCapacityDescription
    , provisionedCapacityDescription
    , pcdCapacityId
    , pcdStartDate
    , pcdExpirationDate

    -- * S3Location
    , S3Location
    , s3Location
    , slCannedACL
    , slPrefix
    , slBucketName
    , slAccessControlList
    , slUserMetadata
    , slEncryption
    , slStorageClass
    , slTagging

    -- * SelectParameters
    , SelectParameters
    , selectParameters
    , spExpressionType
    , spOutputSerialization
    , spExpression
    , spInputSerialization

    -- * UploadListElement
    , UploadListElement
    , uploadListElement
    , uleMultipartUploadId
    , ulePartSizeInBytes
    , uleArchiveDescription
    , uleVaultARN
    , uleCreationDate

    -- * VaultAccessPolicy
    , VaultAccessPolicy
    , vaultAccessPolicy
    , vapPolicy

    -- * VaultLockPolicy
    , VaultLockPolicy
    , vaultLockPolicy
    , vlpPolicy

    -- * VaultNotificationConfig
    , VaultNotificationConfig
    , vaultNotificationConfig
    , vncSNSTopic
    , vncEvents
    ) where

import Network.AWS.Glacier.Types.Product
import Network.AWS.Glacier.Types.Sum
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Sign.V4

-- | API version @2012-06-01@ of the Amazon Glacier SDK configuration.
glacier :: Service
glacier =
  Service
    { _svcAbbrev = "Glacier"
    , _svcSigner = v4
    , _svcPrefix = "glacier"
    , _svcVersion = "2012-06-01"
    , _svcEndpoint = defaultEndpoint glacier
    , _svcTimeout = Just 70
    , _svcCheck = statusSuccess
    , _svcError = parseJSONError "Glacier"
    , _svcRetry = retry
    }
  where
    retry =
      Exponential
        { _retryBase = 5.0e-2
        , _retryGrowth = 2
        , _retryAttempts = 5
        , _retryCheck = check
        }
    check e
      | has (hasCode "ThrottledException" . hasStatus 400) e =
        Just "throttled_exception"
      | has (hasStatus 429) e = Just "too_many_requests"
      | has (hasCode "ThrottlingException" . hasStatus 400) e =
        Just "throttling_exception"
      | has (hasCode "Throttling" . hasStatus 400) e = Just "throttling"
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


-- | Returned if a retrieval job would exceed the current data policy's retrieval rate limit. For more information about data retrieval policies,
--
--
_PolicyEnforcedException :: AsError a => Getting (First ServiceError) a ServiceError
_PolicyEnforcedException =
  _MatchServiceError glacier "PolicyEnforcedException" . hasStatus 400


-- | Returned if a parameter of the request is incorrectly specified.
--
--
_InvalidParameterValueException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidParameterValueException =
  _MatchServiceError glacier "InvalidParameterValueException" . hasStatus 400


-- | Returned if, when uploading an archive, Amazon Glacier times out while receiving the upload.
--
--
_RequestTimeoutException :: AsError a => Getting (First ServiceError) a ServiceError
_RequestTimeoutException =
  _MatchServiceError glacier "RequestTimeoutException" . hasStatus 408


-- | Returned if the service cannot complete the request.
--
--
_ServiceUnavailableException :: AsError a => Getting (First ServiceError) a ServiceError
_ServiceUnavailableException =
  _MatchServiceError glacier "ServiceUnavailableException" . hasStatus 500


-- | Returned if there is insufficient capacity to process this expedited request. This error only applies to expedited retrievals and not to standard or bulk retrievals.
--
--
_InsufficientCapacityException :: AsError a => Getting (First ServiceError) a ServiceError
_InsufficientCapacityException =
  _MatchServiceError glacier "InsufficientCapacityException" . hasStatus 400


-- | Returned if the specified resource (such as a vault, upload ID, or job ID) doesn't exist.
--
--
_ResourceNotFoundException :: AsError a => Getting (First ServiceError) a ServiceError
_ResourceNotFoundException =
  _MatchServiceError glacier "ResourceNotFoundException" . hasStatus 404


-- | Returned if the request results in a vault or account limit being exceeded.
--
--
_LimitExceededException :: AsError a => Getting (First ServiceError) a ServiceError
_LimitExceededException =
  _MatchServiceError glacier "LimitExceededException" . hasStatus 400


-- | Returned if a required header or parameter is missing from the request.
--
--
_MissingParameterValueException :: AsError a => Getting (First ServiceError) a ServiceError
_MissingParameterValueException =
  _MatchServiceError glacier "MissingParameterValueException" . hasStatus 400

