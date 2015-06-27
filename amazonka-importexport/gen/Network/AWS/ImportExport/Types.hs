{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.ImportExport.Types
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

module Network.AWS.ImportExport.Types
    (
    -- * Service
      ImportExport

    -- * Errors
    , _InvalidJobIdException
    , _InvalidParameterException
    , _ExpiredJobIdException
    , _InvalidFileSystemException
    , _InvalidAccessKeyIdException
    , _UnableToUpdateJobIdException
    , _UnableToCancelJobIdException
    , _InvalidVersionException
    , _MultipleRegionsException
    , _MalformedManifestException
    , _CanceledJobIdException
    , _BucketPermissionException
    , _MissingParameterException
    , _NoSuchBucketException
    , _InvalidAddressException
    , _InvalidManifestFieldException
    , _MissingCustomsException
    , _InvalidCustomsException
    , _MissingManifestFieldException
    , _CreateJobQuotaExceededException

    -- * JobType
    , JobType (..)

    -- * Artifact
    , Artifact
    , artifact
    , artURL
    , artDescription

    -- * Job
    , Job
    , job
    , jobJobType
    , jobJobId
    , jobIsCanceled
    , jobCreationDate
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Sign.V2

-- | Version @2010-06-01@ of the Amazon Import/Export SDK.
data ImportExport

instance AWSService ImportExport where
    type Sg ImportExport = V2
    service = const svc
      where
        svc =
            Service
            { _svcAbbrev = "ImportExport"
            , _svcPrefix = "importexport"
            , _svcVersion = "2010-06-01"
            , _svcEndpoint = defaultEndpoint svc
            , _svcTimeout = 80000000
            , _svcStatus = statusSuccess
            , _svcError = parseXMLError
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

-- | The JOBID was missing, not found, or not associated with the AWS
-- account.
_InvalidJobIdException :: AWSError a => Getting (First ServiceError) a ServiceError
_InvalidJobIdException = _ServiceError . hasCode "InvalidJobIdException"

-- | One or more parameters had an invalid value.
_InvalidParameterException :: AWSError a => Getting (First ServiceError) a ServiceError
_InvalidParameterException =
    _ServiceError . hasCode "InvalidParameterException"

-- | Indicates that the specified job has expired out of the system.
_ExpiredJobIdException :: AWSError a => Getting (First ServiceError) a ServiceError
_ExpiredJobIdException = _ServiceError . hasCode "ExpiredJobIdException"

-- | File system specified in export manifest is invalid.
_InvalidFileSystemException :: AWSError a => Getting (First ServiceError) a ServiceError
_InvalidFileSystemException =
    _ServiceError . hasCode "InvalidFileSystemException"

-- | The AWS Access Key ID specified in the request did not match the
-- manifest\'s accessKeyId value. The manifest and the request
-- authentication must use the same AWS Access Key ID.
_InvalidAccessKeyIdException :: AWSError a => Getting (First ServiceError) a ServiceError
_InvalidAccessKeyIdException =
    _ServiceError . hasCode "InvalidAccessKeyIdException"

-- | AWS Import\/Export cannot update the job
_UnableToUpdateJobIdException :: AWSError a => Getting (First ServiceError) a ServiceError
_UnableToUpdateJobIdException =
    _ServiceError . hasCode "UnableToUpdateJobIdException"

-- | AWS Import\/Export cannot cancel the job
_UnableToCancelJobIdException :: AWSError a => Getting (First ServiceError) a ServiceError
_UnableToCancelJobIdException =
    _ServiceError . hasCode "UnableToCancelJobIdException"

-- | The client tool version is invalid.
_InvalidVersionException :: AWSError a => Getting (First ServiceError) a ServiceError
_InvalidVersionException = _ServiceError . hasCode "InvalidVersionException"

-- | Your manifest file contained buckets from multiple regions. A job is
-- restricted to buckets from one region. Please correct and resubmit.
_MultipleRegionsException :: AWSError a => Getting (First ServiceError) a ServiceError
_MultipleRegionsException = _ServiceError . hasCode "MultipleRegionsException"

-- | Your manifest is not well-formed.
_MalformedManifestException :: AWSError a => Getting (First ServiceError) a ServiceError
_MalformedManifestException =
    _ServiceError . hasCode "MalformedManifestException"

-- | The specified job ID has been canceled and is no longer valid.
_CanceledJobIdException :: AWSError a => Getting (First ServiceError) a ServiceError
_CanceledJobIdException = _ServiceError . hasCode "CanceledJobIdException"

-- | The account specified does not have the appropriate bucket permissions.
_BucketPermissionException :: AWSError a => Getting (First ServiceError) a ServiceError
_BucketPermissionException =
    _ServiceError . hasCode "BucketPermissionException"

-- | One or more required parameters was missing from the request.
_MissingParameterException :: AWSError a => Getting (First ServiceError) a ServiceError
_MissingParameterException =
    _ServiceError . hasCode "MissingParameterException"

-- | The specified bucket does not exist. Create the specified bucket or
-- change the manifest\'s bucket, exportBucket, or logBucket field to a
-- bucket that the account, as specified by the manifest\'s Access Key ID,
-- has write permissions to.
_NoSuchBucketException :: AWSError a => Getting (First ServiceError) a ServiceError
_NoSuchBucketException = _ServiceError . hasCode "NoSuchBucketException"

-- | The address specified in the manifest is invalid.
_InvalidAddressException :: AWSError a => Getting (First ServiceError) a ServiceError
_InvalidAddressException = _ServiceError . hasCode "InvalidAddressException"

-- | One or more manifest fields was invalid. Please correct and resubmit.
_InvalidManifestFieldException :: AWSError a => Getting (First ServiceError) a ServiceError
_InvalidManifestFieldException =
    _ServiceError . hasCode "InvalidManifestFieldException"

-- | One or more required customs parameters was missing from the manifest.
_MissingCustomsException :: AWSError a => Getting (First ServiceError) a ServiceError
_MissingCustomsException = _ServiceError . hasCode "MissingCustomsException"

-- | One or more customs parameters was invalid. Please correct and resubmit.
_InvalidCustomsException :: AWSError a => Getting (First ServiceError) a ServiceError
_InvalidCustomsException = _ServiceError . hasCode "InvalidCustomsException"

-- | One or more required fields were missing from the manifest file. Please
-- correct and resubmit.
_MissingManifestFieldException :: AWSError a => Getting (First ServiceError) a ServiceError
_MissingManifestFieldException =
    _ServiceError . hasCode "MissingManifestFieldException"

-- | Each account can create only a certain number of jobs per day. If you
-- need to create more than this, please contact
-- awsimportexport\@amazon.com to explain your particular use case.
_CreateJobQuotaExceededException :: AWSError a => Getting (First ServiceError) a ServiceError
_CreateJobQuotaExceededException =
    _ServiceError . hasCode "CreateJobQuotaExceededException"

-- | Specifies whether the job to initiate is an import or export job.
data JobType
    = Export
    | Import
    deriving (Eq,Ord,Read,Show,Enum,Generic)

instance FromText JobType where
    parser = takeLowerText >>= \case
        "Export" -> pure Export
        "Import" -> pure Import
        e -> fail ("Failure parsing JobType from " ++ show e)

instance ToText JobType where
    toText = \case
        Export -> "Export"
        Import -> "Import"

instance Hashable JobType
instance ToQuery JobType
instance ToHeader JobType

instance FromXML JobType where
    parseXML = parseXMLText "JobType"

-- | A discrete item that contains the description and URL of an artifact
-- (such as a PDF).
--
-- /See:/ 'artifact' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'artURL'
--
-- * 'artDescription'
data Artifact = Artifact'
    { _artURL         :: !(Maybe Text)
    , _artDescription :: !(Maybe Text)
    } deriving (Eq,Read,Show)

-- | 'Artifact' smart constructor.
artifact :: Artifact
artifact =
    Artifact'
    { _artURL = Nothing
    , _artDescription = Nothing
    }

-- | FIXME: Undocumented member.
artURL :: Lens' Artifact (Maybe Text)
artURL = lens _artURL (\ s a -> s{_artURL = a});

-- | FIXME: Undocumented member.
artDescription :: Lens' Artifact (Maybe Text)
artDescription = lens _artDescription (\ s a -> s{_artDescription = a});

instance FromXML Artifact where
        parseXML x
          = Artifact' <$>
              (x .@? "URL") <*> (x .@? "Description")

-- | Representation of a job returned by the ListJobs operation.
--
-- /See:/ 'job' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'jobJobType'
--
-- * 'jobJobId'
--
-- * 'jobIsCanceled'
--
-- * 'jobCreationDate'
data Job = Job'
    { _jobJobType      :: !JobType
    , _jobJobId        :: !Text
    , _jobIsCanceled   :: !Bool
    , _jobCreationDate :: !ISO8601
    } deriving (Eq,Read,Show)

-- | 'Job' smart constructor.
job :: JobType -> Text -> Bool -> UTCTime -> Job
job pJobType pJobId pIsCanceled pCreationDate =
    Job'
    { _jobJobType = pJobType
    , _jobJobId = pJobId
    , _jobIsCanceled = pIsCanceled
    , _jobCreationDate = _Time # pCreationDate
    }

-- | FIXME: Undocumented member.
jobJobType :: Lens' Job JobType
jobJobType = lens _jobJobType (\ s a -> s{_jobJobType = a});

-- | FIXME: Undocumented member.
jobJobId :: Lens' Job Text
jobJobId = lens _jobJobId (\ s a -> s{_jobJobId = a});

-- | FIXME: Undocumented member.
jobIsCanceled :: Lens' Job Bool
jobIsCanceled = lens _jobIsCanceled (\ s a -> s{_jobIsCanceled = a});

-- | FIXME: Undocumented member.
jobCreationDate :: Lens' Job UTCTime
jobCreationDate = lens _jobCreationDate (\ s a -> s{_jobCreationDate = a}) . _Time;

instance FromXML Job where
        parseXML x
          = Job' <$>
              (x .@ "JobType") <*> (x .@ "JobId") <*>
                (x .@ "IsCanceled")
                <*> (x .@ "CreationDate")
