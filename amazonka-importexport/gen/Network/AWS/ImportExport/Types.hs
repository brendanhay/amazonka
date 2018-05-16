{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ImportExport.Types
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ImportExport.Types
    (
    -- * Service Configuration
      importExport

    -- * Errors
    , _InvalidJobIdException
    , _InvalidParameterException
    , _ExpiredJobIdException
    , _InvalidFileSystemException
    , _InvalidAccessKeyIdException
    , _UnableToUpdateJobIdException
    , _UnableToCancelJobIdException
    , _MultipleRegionsException
    , _InvalidVersionException
    , _MalformedManifestException
    , _MissingParameterException
    , _CanceledJobIdException
    , _BucketPermissionException
    , _NoSuchBucketException
    , _InvalidAddressException
    , _MissingCustomsException
    , _InvalidManifestFieldException
    , _InvalidCustomsException
    , _MissingManifestFieldException
    , _CreateJobQuotaExceededException

    -- * JobType
    , JobType (..)

    -- * Artifact
    , Artifact
    , artifact
    , aURL
    , aDescription

    -- * Job
    , Job
    , job
    , jobJobType
    , jobJobId
    , jobIsCanceled
    , jobCreationDate
    ) where

import Network.AWS.ImportExport.Types.Product
import Network.AWS.ImportExport.Types.Sum
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Sign.V2

-- | API version @2010-06-01@ of the Amazon Import/Export SDK configuration.
importExport :: Service
importExport =
  Service
    { _svcAbbrev = "ImportExport"
    , _svcSigner = v2
    , _svcPrefix = "importexport"
    , _svcVersion = "2010-06-01"
    , _svcEndpoint = defaultEndpoint importExport
    , _svcTimeout = Just 70
    , _svcCheck = statusSuccess
    , _svcError = parseXMLError "ImportExport"
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
      | has (hasStatus 500) e = Just "general_server_error"
      | has (hasStatus 509) e = Just "limit_exceeded"
      | otherwise = Nothing


-- | The JOBID was missing, not found, or not associated with the AWS account.
_InvalidJobIdException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidJobIdException = _MatchServiceError importExport "InvalidJobIdException"


-- | One or more parameters had an invalid value.
_InvalidParameterException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidParameterException =
  _MatchServiceError importExport "InvalidParameterException"


-- | Indicates that the specified job has expired out of the system.
_ExpiredJobIdException :: AsError a => Getting (First ServiceError) a ServiceError
_ExpiredJobIdException = _MatchServiceError importExport "ExpiredJobIdException"


-- | File system specified in export manifest is invalid.
_InvalidFileSystemException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidFileSystemException =
  _MatchServiceError importExport "InvalidFileSystemException"


-- | The AWS Access Key ID specified in the request did not match the manifest's accessKeyId value. The manifest and the request authentication must use the same AWS Access Key ID.
_InvalidAccessKeyIdException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidAccessKeyIdException =
  _MatchServiceError importExport "InvalidAccessKeyIdException"


-- | AWS Import/Export cannot update the job
_UnableToUpdateJobIdException :: AsError a => Getting (First ServiceError) a ServiceError
_UnableToUpdateJobIdException =
  _MatchServiceError importExport "UnableToUpdateJobIdException"


-- | AWS Import/Export cannot cancel the job
_UnableToCancelJobIdException :: AsError a => Getting (First ServiceError) a ServiceError
_UnableToCancelJobIdException =
  _MatchServiceError importExport "UnableToCancelJobIdException"


-- | Your manifest file contained buckets from multiple regions. A job is restricted to buckets from one region. Please correct and resubmit.
_MultipleRegionsException :: AsError a => Getting (First ServiceError) a ServiceError
_MultipleRegionsException =
  _MatchServiceError importExport "MultipleRegionsException"


-- | The client tool version is invalid.
_InvalidVersionException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidVersionException =
  _MatchServiceError importExport "InvalidVersionException"


-- | Your manifest is not well-formed.
_MalformedManifestException :: AsError a => Getting (First ServiceError) a ServiceError
_MalformedManifestException =
  _MatchServiceError importExport "MalformedManifestException"


-- | One or more required parameters was missing from the request.
_MissingParameterException :: AsError a => Getting (First ServiceError) a ServiceError
_MissingParameterException =
  _MatchServiceError importExport "MissingParameterException"


-- | The specified job ID has been canceled and is no longer valid.
_CanceledJobIdException :: AsError a => Getting (First ServiceError) a ServiceError
_CanceledJobIdException =
  _MatchServiceError importExport "CanceledJobIdException"


-- | The account specified does not have the appropriate bucket permissions.
_BucketPermissionException :: AsError a => Getting (First ServiceError) a ServiceError
_BucketPermissionException =
  _MatchServiceError importExport "BucketPermissionException"


-- | The specified bucket does not exist. Create the specified bucket or change the manifest's bucket, exportBucket, or logBucket field to a bucket that the account, as specified by the manifest's Access Key ID, has write permissions to.
_NoSuchBucketException :: AsError a => Getting (First ServiceError) a ServiceError
_NoSuchBucketException = _MatchServiceError importExport "NoSuchBucketException"


-- | The address specified in the manifest is invalid.
_InvalidAddressException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidAddressException =
  _MatchServiceError importExport "InvalidAddressException"


-- | One or more required customs parameters was missing from the manifest.
_MissingCustomsException :: AsError a => Getting (First ServiceError) a ServiceError
_MissingCustomsException =
  _MatchServiceError importExport "MissingCustomsException"


-- | One or more manifest fields was invalid. Please correct and resubmit.
_InvalidManifestFieldException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidManifestFieldException =
  _MatchServiceError importExport "InvalidManifestFieldException"


-- | One or more customs parameters was invalid. Please correct and resubmit.
_InvalidCustomsException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidCustomsException =
  _MatchServiceError importExport "InvalidCustomsException"


-- | One or more required fields were missing from the manifest file. Please correct and resubmit.
_MissingManifestFieldException :: AsError a => Getting (First ServiceError) a ServiceError
_MissingManifestFieldException =
  _MatchServiceError importExport "MissingManifestFieldException"


-- | Each account can create only a certain number of jobs per day. If you need to create more than this, please contact awsimportexport@amazon.com to explain your particular use case.
_CreateJobQuotaExceededException :: AsError a => Getting (First ServiceError) a ServiceError
_CreateJobQuotaExceededException =
  _MatchServiceError importExport "CreateJobQuotaExceededException"

