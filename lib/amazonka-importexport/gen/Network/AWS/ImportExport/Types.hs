-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ImportExport.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ImportExport.Types
    (
    -- * Service configuration
      mkServiceConfig

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

    -- * Carrier
    , Carrier (..)

    -- * TrackingNumber
    , TrackingNumber (..)

    -- * Signature
    , Signature (..)

    -- * JobType
    , JobType (..)

    -- * Street3
    , Street3 (..)

    -- * APIVersion
    , APIVersion (..)

    -- * JobId
    , JobId (..)

    -- * Country
    , Country (..)

    -- * StateOrProvince
    , StateOrProvince (..)

    -- * SignatureFileContents
    , SignatureFileContents (..)

    -- * PostalCode
    , PostalCode (..)

    -- * URL
    , URL (..)

    -- * Street2
    , Street2 (..)

    -- * Artifact
    , Artifact (..)
    , mkArtifact
    , aDescription
    , aURL

    -- * CurrentManifest
    , CurrentManifest (..)

    -- * WarningMessage
    , WarningMessage (..)

    -- * GenericString
    , GenericString (..)

    -- * Job
    , Job (..)
    , mkJob
    , jCreationDate
    , jIsCanceled
    , jJobId
    , jJobType

    -- * Manifest
    , Manifest (..)

    -- * LogBucket
    , LogBucket (..)

    -- * Marker
    , Marker (..)

    -- * ProgressCode
    , ProgressCode (..)

    -- * LocationCode
    , LocationCode (..)

    -- * LogKey
    , LogKey (..)

    -- * Description
    , Description (..)

    -- * LocationMessage
    , LocationMessage (..)

    -- * ProgressMessage
    , ProgressMessage (..)

    -- * ManifestAddendum
    , ManifestAddendum (..)

    -- * City
    , City (..)

    -- * Company
    , Company (..)

    -- * Name
    , Name (..)

    -- * PhoneNumber
    , PhoneNumber (..)

    -- * Street1
    , Street1 (..)
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Sign.V2 as Sign
  
import Network.AWS.ImportExport.Types.Carrier
  
import Network.AWS.ImportExport.Types.TrackingNumber
  
import Network.AWS.ImportExport.Types.Signature
  
import Network.AWS.ImportExport.Types.JobType
  
  
  
import Network.AWS.ImportExport.Types.Street3
  
import Network.AWS.ImportExport.Types.APIVersion
  
import Network.AWS.ImportExport.Types.JobId
  
  
import Network.AWS.ImportExport.Types.Country
  
import Network.AWS.ImportExport.Types.StateOrProvince
  
import Network.AWS.ImportExport.Types.SignatureFileContents
  
import Network.AWS.ImportExport.Types.PostalCode
  
  
import Network.AWS.ImportExport.Types.URL
  
import Network.AWS.ImportExport.Types.Street2
  
import Network.AWS.ImportExport.Types.Artifact
  
import Network.AWS.ImportExport.Types.CurrentManifest
  
  
import Network.AWS.ImportExport.Types.WarningMessage
  
  
import Network.AWS.ImportExport.Types.GenericString
  
  
  
  
import Network.AWS.ImportExport.Types.Job
  
import Network.AWS.ImportExport.Types.Manifest
  
  
import Network.AWS.ImportExport.Types.LogBucket
  
  
import Network.AWS.ImportExport.Types.Marker
  
  
  
import Network.AWS.ImportExport.Types.ProgressCode
  
import Network.AWS.ImportExport.Types.LocationCode
  
  
  
  
import Network.AWS.ImportExport.Types.LogKey
  
import Network.AWS.ImportExport.Types.Description
  
  
import Network.AWS.ImportExport.Types.LocationMessage
  
  
import Network.AWS.ImportExport.Types.ProgressMessage
  
  
import Network.AWS.ImportExport.Types.ManifestAddendum
  
import Network.AWS.ImportExport.Types.City
  
import Network.AWS.ImportExport.Types.Company
  
import Network.AWS.ImportExport.Types.Name
  
import Network.AWS.ImportExport.Types.PhoneNumber
  
import Network.AWS.ImportExport.Types.Street1
  

-- | API version @2010-06-01@ of the Amazon Import/Export SDK configuration.
mkServiceConfig :: Core.Service
mkServiceConfig
  = Core.Service{Core._svcAbbrev = "ImportExport",
                 Core._svcSigner = Sign.v2, Core._svcPrefix = "importexport",
                 Core._svcVersion = "2010-06-01", Core._svcTimeout = Core.Just 70,
                 Core._svcCheck = Core.statusSuccess, Core._svcRetry = retry,
                 Core._svcError = Core.parseXMLError "ImportExport",
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
          | Core.otherwise = Core.Nothing

-- | The JOBID was missing, not found, or not associated with the AWS account.
_InvalidJobIdException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidJobIdException
  = Core._MatchServiceError mkServiceConfig "InvalidJobIdException"
{-# INLINEABLE _InvalidJobIdException #-}
{-# DEPRECATED _InvalidJobIdException "Use generic-lens or generic-optics instead"  #-}

-- | One or more parameters had an invalid value.
_InvalidParameterException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidParameterException
  = Core._MatchServiceError mkServiceConfig
      "InvalidParameterException"
{-# INLINEABLE _InvalidParameterException #-}
{-# DEPRECATED _InvalidParameterException "Use generic-lens or generic-optics instead"  #-}

-- | Indicates that the specified job has expired out of the system.
_ExpiredJobIdException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ExpiredJobIdException
  = Core._MatchServiceError mkServiceConfig "ExpiredJobIdException"
{-# INLINEABLE _ExpiredJobIdException #-}
{-# DEPRECATED _ExpiredJobIdException "Use generic-lens or generic-optics instead"  #-}

-- | File system specified in export manifest is invalid.
_InvalidFileSystemException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidFileSystemException
  = Core._MatchServiceError mkServiceConfig
      "InvalidFileSystemException"
{-# INLINEABLE _InvalidFileSystemException #-}
{-# DEPRECATED _InvalidFileSystemException "Use generic-lens or generic-optics instead"  #-}

-- | The AWS Access Key ID specified in the request did not match the manifest's accessKeyId value. The manifest and the request authentication must use the same AWS Access Key ID.
_InvalidAccessKeyIdException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidAccessKeyIdException
  = Core._MatchServiceError mkServiceConfig
      "InvalidAccessKeyIdException"
{-# INLINEABLE _InvalidAccessKeyIdException #-}
{-# DEPRECATED _InvalidAccessKeyIdException "Use generic-lens or generic-optics instead"  #-}

-- | AWS Import/Export cannot update the job
_UnableToUpdateJobIdException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_UnableToUpdateJobIdException
  = Core._MatchServiceError mkServiceConfig
      "UnableToUpdateJobIdException"
{-# INLINEABLE _UnableToUpdateJobIdException #-}
{-# DEPRECATED _UnableToUpdateJobIdException "Use generic-lens or generic-optics instead"  #-}

-- | AWS Import/Export cannot cancel the job
_UnableToCancelJobIdException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_UnableToCancelJobIdException
  = Core._MatchServiceError mkServiceConfig
      "UnableToCancelJobIdException"
{-# INLINEABLE _UnableToCancelJobIdException #-}
{-# DEPRECATED _UnableToCancelJobIdException "Use generic-lens or generic-optics instead"  #-}

-- | Your manifest file contained buckets from multiple regions. A job is restricted to buckets from one region. Please correct and resubmit.
_MultipleRegionsException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_MultipleRegionsException
  = Core._MatchServiceError mkServiceConfig
      "MultipleRegionsException"
{-# INLINEABLE _MultipleRegionsException #-}
{-# DEPRECATED _MultipleRegionsException "Use generic-lens or generic-optics instead"  #-}

-- | The client tool version is invalid.
_InvalidVersionException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidVersionException
  = Core._MatchServiceError mkServiceConfig "InvalidVersionException"
{-# INLINEABLE _InvalidVersionException #-}
{-# DEPRECATED _InvalidVersionException "Use generic-lens or generic-optics instead"  #-}

-- | Your manifest is not well-formed.
_MalformedManifestException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_MalformedManifestException
  = Core._MatchServiceError mkServiceConfig
      "MalformedManifestException"
{-# INLINEABLE _MalformedManifestException #-}
{-# DEPRECATED _MalformedManifestException "Use generic-lens or generic-optics instead"  #-}

-- | One or more required parameters was missing from the request.
_MissingParameterException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_MissingParameterException
  = Core._MatchServiceError mkServiceConfig
      "MissingParameterException"
{-# INLINEABLE _MissingParameterException #-}
{-# DEPRECATED _MissingParameterException "Use generic-lens or generic-optics instead"  #-}

-- | The specified job ID has been canceled and is no longer valid.
_CanceledJobIdException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_CanceledJobIdException
  = Core._MatchServiceError mkServiceConfig "CanceledJobIdException"
{-# INLINEABLE _CanceledJobIdException #-}
{-# DEPRECATED _CanceledJobIdException "Use generic-lens or generic-optics instead"  #-}

-- | The account specified does not have the appropriate bucket permissions.
_BucketPermissionException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_BucketPermissionException
  = Core._MatchServiceError mkServiceConfig
      "BucketPermissionException"
{-# INLINEABLE _BucketPermissionException #-}
{-# DEPRECATED _BucketPermissionException "Use generic-lens or generic-optics instead"  #-}

-- | The specified bucket does not exist. Create the specified bucket or change the manifest's bucket, exportBucket, or logBucket field to a bucket that the account, as specified by the manifest's Access Key ID, has write permissions to.
_NoSuchBucketException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_NoSuchBucketException
  = Core._MatchServiceError mkServiceConfig "NoSuchBucketException"
{-# INLINEABLE _NoSuchBucketException #-}
{-# DEPRECATED _NoSuchBucketException "Use generic-lens or generic-optics instead"  #-}

-- | The address specified in the manifest is invalid.
_InvalidAddressException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidAddressException
  = Core._MatchServiceError mkServiceConfig "InvalidAddressException"
{-# INLINEABLE _InvalidAddressException #-}
{-# DEPRECATED _InvalidAddressException "Use generic-lens or generic-optics instead"  #-}

-- | One or more required customs parameters was missing from the manifest.
_MissingCustomsException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_MissingCustomsException
  = Core._MatchServiceError mkServiceConfig "MissingCustomsException"
{-# INLINEABLE _MissingCustomsException #-}
{-# DEPRECATED _MissingCustomsException "Use generic-lens or generic-optics instead"  #-}

-- | One or more manifest fields was invalid. Please correct and resubmit.
_InvalidManifestFieldException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidManifestFieldException
  = Core._MatchServiceError mkServiceConfig
      "InvalidManifestFieldException"
{-# INLINEABLE _InvalidManifestFieldException #-}
{-# DEPRECATED _InvalidManifestFieldException "Use generic-lens or generic-optics instead"  #-}

-- | One or more customs parameters was invalid. Please correct and resubmit.
_InvalidCustomsException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidCustomsException
  = Core._MatchServiceError mkServiceConfig "InvalidCustomsException"
{-# INLINEABLE _InvalidCustomsException #-}
{-# DEPRECATED _InvalidCustomsException "Use generic-lens or generic-optics instead"  #-}

-- | One or more required fields were missing from the manifest file. Please correct and resubmit.
_MissingManifestFieldException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_MissingManifestFieldException
  = Core._MatchServiceError mkServiceConfig
      "MissingManifestFieldException"
{-# INLINEABLE _MissingManifestFieldException #-}
{-# DEPRECATED _MissingManifestFieldException "Use generic-lens or generic-optics instead"  #-}

-- | Each account can create only a certain number of jobs per day. If you need to create more than this, please contact awsimportexport@amazon.com to explain your particular use case.
_CreateJobQuotaExceededException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_CreateJobQuotaExceededException
  = Core._MatchServiceError mkServiceConfig
      "CreateJobQuotaExceededException"
{-# INLINEABLE _CreateJobQuotaExceededException #-}
{-# DEPRECATED _CreateJobQuotaExceededException "Use generic-lens or generic-optics instead"  #-}
