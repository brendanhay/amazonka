{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glacier.Types
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Glacier.Types
    (
    -- * Service Decription
      Glacier

    -- * Error Matchers
    , _PolicyEnforcedException
    , _InvalidParameterValueException
    , _RequestTimeoutException
    , _ServiceUnavailableException
    , _ResourceNotFoundException
    , _LimitExceededException
    , _MissingParameterValueException

    -- * ActionCode
    , ActionCode (..)

    -- * StatusCode
    , StatusCode (..)

    -- * ArchiveCreationOutput
    , ArchiveCreationOutput
    , archiveCreationOutput
    , acoArchiveId
    , acoChecksum
    , acoLocation

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

    -- * GlacierJobDescription
    , GlacierJobDescription
    , glacierJobDescription
    , gjdArchiveId
    , gjdSHA256TreeHash
    , gjdJobId
    , gjdRetrievalByteRange
    , gjdInventoryRetrievalParameters
    , gjdAction
    , gjdJobDescription
    , gjdSNSTopic
    , gjdVaultARN
    , gjdStatusMessage
    , gjdArchiveSHA256TreeHash
    , gjdCreationDate
    , gjdCompleted
    , gjdCompletionDate
    , gjdArchiveSizeInBytes
    , gjdStatusCode
    , gjdInventorySizeInBytes

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
    , jpRetrievalByteRange
    , jpFormat
    , jpInventoryRetrievalParameters
    , jpSNSTopic
    , jpType
    , jpDescription

    -- * PartListElement
    , PartListElement
    , partListElement
    , pleSHA256TreeHash
    , pleRangeInBytes

    -- * UploadListElement
    , UploadListElement
    , uploadListElement
    , uleMultipartUploadId
    , uleArchiveDescription
    , ulePartSizeInBytes
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

import           Network.AWS.Glacier.Types.Product
import           Network.AWS.Glacier.Types.Sum
import           Network.AWS.Prelude
import           Network.AWS.Sign.V4

-- | Version @2012-06-01@ of the Amazon Glacier SDK.
data Glacier

instance AWSService Glacier where
    type Sg Glacier = V4
    service = const svc
      where
        svc =
            Service
            { _svcAbbrev = "Glacier"
            , _svcPrefix = "glacier"
            , _svcVersion = "2012-06-01"
            , _svcEndpoint = defaultEndpoint svc
            , _svcTimeout = Just 70
            , _svcStatus = statusSuccess
            , _svcError = parseJSONError
            , _svcRetry = retry
            }
        retry =
            Exponential
            { _retryBase = 5.0e-2
            , _retryGrowth = 2
            , _retryAttempts = 5
            , _retryCheck = check
            }
        check e
          | has (hasCode "ThrottlingException" . hasStatus 400) e =
              Just "throttling_exception"
          | has (hasCode "Throttling" . hasStatus 400) e = Just "throttling"
          | has (hasStatus 503) e = Just "service_unavailable"
          | has (hasStatus 500) e = Just "general_server_error"
          | has (hasStatus 509) e = Just "limit_exceeded"
          | otherwise = Nothing

-- | Returned if a retrieval job would exceed the current data policy\'s
-- retrieval rate limit. For more information about data retrieval
-- policies,
_PolicyEnforcedException :: AsError a => Getting (First ServiceError) a ServiceError
_PolicyEnforcedException =
    _ServiceError . hasStatus 400 . hasCode "PolicyEnforcedException"

-- | Returned if a parameter of the request is incorrectly specified.
_InvalidParameterValueException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidParameterValueException =
    _ServiceError . hasStatus 400 . hasCode "InvalidParameterValueException"

-- | Returned if, when uploading an archive, Amazon Glacier times out while
-- receiving the upload.
_RequestTimeoutException :: AsError a => Getting (First ServiceError) a ServiceError
_RequestTimeoutException =
    _ServiceError . hasStatus 408 . hasCode "RequestTimeoutException"

-- | Returned if the service cannot complete the request.
_ServiceUnavailableException :: AsError a => Getting (First ServiceError) a ServiceError
_ServiceUnavailableException =
    _ServiceError . hasStatus 500 . hasCode "ServiceUnavailableException"

-- | Returned if the specified resource, such as a vault, upload ID, or job
-- ID, does not exist.
_ResourceNotFoundException :: AsError a => Getting (First ServiceError) a ServiceError
_ResourceNotFoundException =
    _ServiceError . hasStatus 404 . hasCode "ResourceNotFoundException"

-- | Returned if the request results in a vault or account limit being
-- exceeded.
_LimitExceededException :: AsError a => Getting (First ServiceError) a ServiceError
_LimitExceededException =
    _ServiceError . hasStatus 400 . hasCode "LimitExceededException"

-- | Returned if a required header or parameter is missing from the request.
_MissingParameterValueException :: AsError a => Getting (First ServiceError) a ServiceError
_MissingParameterValueException =
    _ServiceError . hasStatus 400 . hasCode "MissingParameterValueException"
