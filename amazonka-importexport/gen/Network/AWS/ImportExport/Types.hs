{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ImportExport.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ImportExport.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _InvalidVersionException,
    _UnableToCancelJobIdException,
    _CreateJobQuotaExceededException,
    _InvalidManifestFieldException,
    _MissingCustomsException,
    _InvalidAddressException,
    _MissingParameterException,
    _BucketPermissionException,
    _MalformedManifestException,
    _InvalidParameterException,
    _MultipleRegionsException,
    _InvalidJobIdException,
    _UnableToUpdateJobIdException,
    _MissingManifestFieldException,
    _InvalidCustomsException,
    _InvalidAccessKeyIdException,
    _InvalidFileSystemException,
    _NoSuchBucketException,
    _CanceledJobIdException,
    _ExpiredJobIdException,

    -- * JobType
    JobType (..),

    -- * Artifact
    Artifact (..),
    newArtifact,
    artifact_description,
    artifact_url,

    -- * Job
    Job (..),
    newJob,
    job_jobType,
    job_jobId,
    job_isCanceled,
    job_creationDate,
  )
where

import Network.AWS.ImportExport.Types.Artifact
import Network.AWS.ImportExport.Types.Job
import Network.AWS.ImportExport.Types.JobType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Sign.V2 as Sign

-- | API version @2010-06-01@ of the Amazon Import/Export SDK configuration.
defaultService :: Prelude.Service
defaultService =
  Prelude.Service
    { Prelude._svcAbbrev =
        "ImportExport",
      Prelude._svcSigner = Sign.v2,
      Prelude._svcPrefix = "importexport",
      Prelude._svcVersion = "2010-06-01",
      Prelude._svcEndpoint =
        Prelude.defaultEndpoint defaultService,
      Prelude._svcTimeout = Prelude.Just 70,
      Prelude._svcCheck = Prelude.statusSuccess,
      Prelude._svcError =
        Prelude.parseXMLError "ImportExport",
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

-- | The client tool version is invalid.
_InvalidVersionException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidVersionException =
  Prelude._MatchServiceError
    defaultService
    "InvalidVersionException"

-- | AWS Import\/Export cannot cancel the job
_UnableToCancelJobIdException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_UnableToCancelJobIdException =
  Prelude._MatchServiceError
    defaultService
    "UnableToCancelJobIdException"

-- | Each account can create only a certain number of jobs per day. If you
-- need to create more than this, please contact
-- awsimportexport\@amazon.com to explain your particular use case.
_CreateJobQuotaExceededException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_CreateJobQuotaExceededException =
  Prelude._MatchServiceError
    defaultService
    "CreateJobQuotaExceededException"

-- | One or more manifest fields was invalid. Please correct and resubmit.
_InvalidManifestFieldException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidManifestFieldException =
  Prelude._MatchServiceError
    defaultService
    "InvalidManifestFieldException"

-- | One or more required customs parameters was missing from the manifest.
_MissingCustomsException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_MissingCustomsException =
  Prelude._MatchServiceError
    defaultService
    "MissingCustomsException"

-- | The address specified in the manifest is invalid.
_InvalidAddressException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidAddressException =
  Prelude._MatchServiceError
    defaultService
    "InvalidAddressException"

-- | One or more required parameters was missing from the request.
_MissingParameterException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_MissingParameterException =
  Prelude._MatchServiceError
    defaultService
    "MissingParameterException"

-- | The account specified does not have the appropriate bucket permissions.
_BucketPermissionException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_BucketPermissionException =
  Prelude._MatchServiceError
    defaultService
    "BucketPermissionException"

-- | Your manifest is not well-formed.
_MalformedManifestException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_MalformedManifestException =
  Prelude._MatchServiceError
    defaultService
    "MalformedManifestException"

-- | One or more parameters had an invalid value.
_InvalidParameterException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidParameterException =
  Prelude._MatchServiceError
    defaultService
    "InvalidParameterException"

-- | Your manifest file contained buckets from multiple regions. A job is
-- restricted to buckets from one region. Please correct and resubmit.
_MultipleRegionsException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_MultipleRegionsException =
  Prelude._MatchServiceError
    defaultService
    "MultipleRegionsException"

-- | The JOBID was missing, not found, or not associated with the AWS
-- account.
_InvalidJobIdException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidJobIdException =
  Prelude._MatchServiceError
    defaultService
    "InvalidJobIdException"

-- | AWS Import\/Export cannot update the job
_UnableToUpdateJobIdException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_UnableToUpdateJobIdException =
  Prelude._MatchServiceError
    defaultService
    "UnableToUpdateJobIdException"

-- | One or more required fields were missing from the manifest file. Please
-- correct and resubmit.
_MissingManifestFieldException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_MissingManifestFieldException =
  Prelude._MatchServiceError
    defaultService
    "MissingManifestFieldException"

-- | One or more customs parameters was invalid. Please correct and resubmit.
_InvalidCustomsException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidCustomsException =
  Prelude._MatchServiceError
    defaultService
    "InvalidCustomsException"

-- | The AWS Access Key ID specified in the request did not match the
-- manifest\'s accessKeyId value. The manifest and the request
-- authentication must use the same AWS Access Key ID.
_InvalidAccessKeyIdException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidAccessKeyIdException =
  Prelude._MatchServiceError
    defaultService
    "InvalidAccessKeyIdException"

-- | File system specified in export manifest is invalid.
_InvalidFileSystemException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidFileSystemException =
  Prelude._MatchServiceError
    defaultService
    "InvalidFileSystemException"

-- | The specified bucket does not exist. Create the specified bucket or
-- change the manifest\'s bucket, exportBucket, or logBucket field to a
-- bucket that the account, as specified by the manifest\'s Access Key ID,
-- has write permissions to.
_NoSuchBucketException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_NoSuchBucketException =
  Prelude._MatchServiceError
    defaultService
    "NoSuchBucketException"

-- | The specified job ID has been canceled and is no longer valid.
_CanceledJobIdException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_CanceledJobIdException =
  Prelude._MatchServiceError
    defaultService
    "CanceledJobIdException"

-- | Indicates that the specified job has expired out of the system.
_ExpiredJobIdException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_ExpiredJobIdException =
  Prelude._MatchServiceError
    defaultService
    "ExpiredJobIdException"
