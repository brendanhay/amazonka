{-# LANGUAGE DeriveDataTypeable          #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE StandaloneDeriving          #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ImportExport.Types
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | AWS Import/Export accelerates moving large amounts of data into and out of
-- AWS using portable storage devices for transport. AWS transfers your data
-- directly onto and off of storage devices using Amazon’s high-speed internal
-- network and bypassing the Internet. For significant data sets, AWS
-- Import/Export is often faster than Internet transfer and more cost
-- effective than upgrading your connectivity.
module Network.AWS.ImportExport.Types
    (
    -- * Service
      ImportExport
    -- ** Errors
    , ImportExportError (..)
    , _BucketPermissionException
    , _CanceledJobIdException
    , _ExpiredJobIdException
    , _ImportExportClient
    , _ImportExportSerializer
    , _ImportExportService
    , _InvalidAccessKeyIdException
    , _InvalidAddressException
    , _InvalidCustomsException
    , _InvalidFileSystemException
    , _InvalidJobIdException
    , _InvalidManifestFieldException
    , _InvalidParameterException
    , _MalformedManifestException
    , _MissingCustomsException
    , _MissingManifestFieldException
    , _MissingParameterException
    , _MultipleRegionsException
    , _NoSuchBucketException
    , _UnableToCancelJobIdException
    -- ** XML
    , xmlOptions

    -- * JobType
    , JobType (..)

    -- * Job
    , Job
    , job
    , jJobId
    , jCreationDate
    , jIsCanceled
    , jJobType
    ) where

import Network.AWS.Prelude
import Network.AWS.Signing.V2

-- | Supported version (@2010-06-01@) of the
-- @AWS Import/Export@ service.
data ImportExport deriving (Typeable)

instance AWSService ImportExport where
    type Sg ImportExport = V2
    type Er ImportExport = ImportExportError

    service = Service
        { _svcEndpoint = Regional
        , _svcPrefix   = "importexport"
        , _svcVersion  = "2010-06-01"
        , _svcTarget   = Nothing
        }

-- | A sum type representing possible errors returned by the 'ImportExport' service.
--
-- These typically include 'HTTPException's thrown by the underlying HTTP
-- mechanisms, serialisation errors, and typed errors as specified by the
-- service description where applicable.
data ImportExportError
      -- | The account specified does not have the appropriate bucket
      -- permissions.
    = BucketPermissionException
        { _bpeMessage :: Maybe Text
        }
      -- | The specified job ID has been canceled and is no longer valid.
    | CanceledJobIdException
        { _cjieMessage :: Maybe Text
        }
      -- | Indicates that the specified job has expired out of the system.
    | ExpiredJobIdException
        { _ejieMessage :: Maybe Text
        }
    | ImportExportClient HttpException
    | ImportExportSerializer String
    | ImportExportService String
      -- | The AWS Access Key ID specified in the request did not match the
      -- manifest's accessKeyId value. The manifest and the request
      -- authentication must use the same AWS Access Key ID.
    | InvalidAccessKeyIdException
        { _iakieMessage :: Maybe Text
        }
      -- | The address specified in the manifest is invalid.
    | InvalidAddressException
        { _iaeMessage :: Maybe Text
        }
      -- | One or more customs parameters was invalid. Please correct and
      -- resubmit.
    | InvalidCustomsException
        { _iceMessage :: Maybe Text
        }
      -- | File system specified in export manifest is invalid.
    | InvalidFileSystemException
        { _ifseMessage :: Maybe Text
        }
      -- | The JOBID was missing, not found, or not associated with the AWS
      -- account.
    | InvalidJobIdException
        { _ijieMessage :: Maybe Text
        }
      -- | One or more manifest fields was invalid. Please correct and
      -- resubmit.
    | InvalidManifestFieldException
        { _imfeMessage :: Maybe Text
        }
      -- | One or more parameters had an invalid value.
    | InvalidParameterException
        { _ipeMessage :: Maybe Text
        }
      -- | Your manifest is not well-formed.
    | MalformedManifestException
        { _mmeMessage :: Maybe Text
        }
      -- | One or more required customs parameters was missing from the
      -- manifest.
    | MissingCustomsException
        { _mceMessage :: Maybe Text
        }
      -- | One or more required fields were missing from the manifest file.
      -- Please correct and resubmit.
    | MissingManifestFieldException
        { _mmfeMessage :: Maybe Text
        }
      -- | One or more required parameters was missing from the request.
    | MissingParameterException
        { _mpeMessage :: Maybe Text
        }
      -- | Your manifest file contained buckets from multiple regions. A job
      -- is restricted to buckets from one region. Please correct and
      -- resubmit.
    | MultipleRegionsException
        { _mreMessage :: Maybe Text
        }
      -- | The specified bucket does not exist. Create the specified bucket
      -- or change the manifest's bucket, exportBucket, or logBucket field
      -- to a bucket that the account, as specified by the manifest's
      -- Access Key ID, has write permissions to.
    | NoSuchBucketException
        { _nsbeMessage :: Maybe Text
        }
      -- | AWS Import/Export cannot cancel the job.
    | UnableToCancelJobIdException
        { _utcjieMessage :: Maybe Text
        }
      deriving (Show, Typeable, Generic)

instance AWSError ImportExportError where
    awsError = const "ImportExportError"

instance AWSServiceError ImportExportError where
    serviceError    = ImportExportService
    clientError     = ImportExportClient
    serializerError = ImportExportSerializer

instance Exception ImportExportError

-- | The account specified does not have the appropriate bucket permissions.
--
-- See: 'BucketPermissionException'
_BucketPermissionException :: Prism' ImportExportError (Maybe Text)
_BucketPermissionException = prism
    BucketPermissionException
    (\case
        BucketPermissionException p1 -> Right p1
        x -> Left x)

-- | The specified job ID has been canceled and is no longer valid.
--
-- See: 'CanceledJobIdException'
_CanceledJobIdException :: Prism' ImportExportError (Maybe Text)
_CanceledJobIdException = prism
    CanceledJobIdException
    (\case
        CanceledJobIdException p1 -> Right p1
        x -> Left x)

-- | Indicates that the specified job has expired out of the system.
--
-- See: 'ExpiredJobIdException'
_ExpiredJobIdException :: Prism' ImportExportError (Maybe Text)
_ExpiredJobIdException = prism
    ExpiredJobIdException
    (\case
        ExpiredJobIdException p1 -> Right p1
        x -> Left x)

-- | See: 'ImportExportClient'
_ImportExportClient :: Prism' ImportExportError HttpException
_ImportExportClient = prism
    ImportExportClient
    (\case
        ImportExportClient p1 -> Right p1
        x -> Left x)

-- | See: 'ImportExportSerializer'
_ImportExportSerializer :: Prism' ImportExportError String
_ImportExportSerializer = prism
    ImportExportSerializer
    (\case
        ImportExportSerializer p1 -> Right p1
        x -> Left x)

-- | See: 'ImportExportService'
_ImportExportService :: Prism' ImportExportError String
_ImportExportService = prism
    ImportExportService
    (\case
        ImportExportService p1 -> Right p1
        x -> Left x)

-- | The AWS Access Key ID specified in the request did not match the manifest's
-- accessKeyId value. The manifest and the request authentication must use the
-- same AWS Access Key ID.
--
-- See: 'InvalidAccessKeyIdException'
_InvalidAccessKeyIdException :: Prism' ImportExportError (Maybe Text)
_InvalidAccessKeyIdException = prism
    InvalidAccessKeyIdException
    (\case
        InvalidAccessKeyIdException p1 -> Right p1
        x -> Left x)

-- | The address specified in the manifest is invalid.
--
-- See: 'InvalidAddressException'
_InvalidAddressException :: Prism' ImportExportError (Maybe Text)
_InvalidAddressException = prism
    InvalidAddressException
    (\case
        InvalidAddressException p1 -> Right p1
        x -> Left x)

-- | One or more customs parameters was invalid. Please correct and resubmit.
--
-- See: 'InvalidCustomsException'
_InvalidCustomsException :: Prism' ImportExportError (Maybe Text)
_InvalidCustomsException = prism
    InvalidCustomsException
    (\case
        InvalidCustomsException p1 -> Right p1
        x -> Left x)

-- | File system specified in export manifest is invalid.
--
-- See: 'InvalidFileSystemException'
_InvalidFileSystemException :: Prism' ImportExportError (Maybe Text)
_InvalidFileSystemException = prism
    InvalidFileSystemException
    (\case
        InvalidFileSystemException p1 -> Right p1
        x -> Left x)

-- | The JOBID was missing, not found, or not associated with the AWS account.
--
-- See: 'InvalidJobIdException'
_InvalidJobIdException :: Prism' ImportExportError (Maybe Text)
_InvalidJobIdException = prism
    InvalidJobIdException
    (\case
        InvalidJobIdException p1 -> Right p1
        x -> Left x)

-- | One or more manifest fields was invalid. Please correct and resubmit.
--
-- See: 'InvalidManifestFieldException'
_InvalidManifestFieldException :: Prism' ImportExportError (Maybe Text)
_InvalidManifestFieldException = prism
    InvalidManifestFieldException
    (\case
        InvalidManifestFieldException p1 -> Right p1
        x -> Left x)

-- | One or more parameters had an invalid value.
--
-- See: 'InvalidParameterException'
_InvalidParameterException :: Prism' ImportExportError (Maybe Text)
_InvalidParameterException = prism
    InvalidParameterException
    (\case
        InvalidParameterException p1 -> Right p1
        x -> Left x)

-- | Your manifest is not well-formed.
--
-- See: 'MalformedManifestException'
_MalformedManifestException :: Prism' ImportExportError (Maybe Text)
_MalformedManifestException = prism
    MalformedManifestException
    (\case
        MalformedManifestException p1 -> Right p1
        x -> Left x)

-- | One or more required customs parameters was missing from the manifest.
--
-- See: 'MissingCustomsException'
_MissingCustomsException :: Prism' ImportExportError (Maybe Text)
_MissingCustomsException = prism
    MissingCustomsException
    (\case
        MissingCustomsException p1 -> Right p1
        x -> Left x)

-- | One or more required fields were missing from the manifest file. Please
-- correct and resubmit.
--
-- See: 'MissingManifestFieldException'
_MissingManifestFieldException :: Prism' ImportExportError (Maybe Text)
_MissingManifestFieldException = prism
    MissingManifestFieldException
    (\case
        MissingManifestFieldException p1 -> Right p1
        x -> Left x)

-- | One or more required parameters was missing from the request.
--
-- See: 'MissingParameterException'
_MissingParameterException :: Prism' ImportExportError (Maybe Text)
_MissingParameterException = prism
    MissingParameterException
    (\case
        MissingParameterException p1 -> Right p1
        x -> Left x)

-- | Your manifest file contained buckets from multiple regions. A job is
-- restricted to buckets from one region. Please correct and resubmit.
--
-- See: 'MultipleRegionsException'
_MultipleRegionsException :: Prism' ImportExportError (Maybe Text)
_MultipleRegionsException = prism
    MultipleRegionsException
    (\case
        MultipleRegionsException p1 -> Right p1
        x -> Left x)

-- | The specified bucket does not exist. Create the specified bucket or change
-- the manifest's bucket, exportBucket, or logBucket field to a bucket that
-- the account, as specified by the manifest's Access Key ID, has write
-- permissions to.
--
-- See: 'NoSuchBucketException'
_NoSuchBucketException :: Prism' ImportExportError (Maybe Text)
_NoSuchBucketException = prism
    NoSuchBucketException
    (\case
        NoSuchBucketException p1 -> Right p1
        x -> Left x)

-- | AWS Import/Export cannot cancel the job.
--
-- See: 'UnableToCancelJobIdException'
_UnableToCancelJobIdException :: Prism' ImportExportError (Maybe Text)
_UnableToCancelJobIdException = prism
    UnableToCancelJobIdException
    (\case
        UnableToCancelJobIdException p1 -> Right p1
        x -> Left x)

xmlOptions :: Tagged a XMLOptions
xmlOptions = Tagged def

data JobType
    = JobTypeExport -- ^ Export
    | JobTypeImport -- ^ Import
      deriving (Eq, Ord, Show, Generic)

instance Hashable JobType

instance FromText JobType where
    parser = match "Export" JobTypeExport
         <|> match "Import" JobTypeImport

instance ToText JobType where
    toText JobTypeExport = "Export"
    toText JobTypeImport = "Import"

instance ToByteString JobType

instance FromXML JobType where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "JobType"

instance ToQuery JobType where
    toQuery = genericQuery def

-- | Representation of a job returned by the ListJobs operation.
data Job = Job
    { _jJobId :: Text
    , _jCreationDate :: ISO8601
    , _jIsCanceled :: Bool
    , _jJobType :: JobType
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'Job' data type.
--
-- 'Job' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @JobId ::@ @Text@
--
-- * @CreationDate ::@ @ISO8601@
--
-- * @IsCanceled ::@ @Bool@
--
-- * @JobType ::@ @JobType@
--
job :: Text -- ^ 'jJobId'
    -> ISO8601 -- ^ 'jCreationDate'
    -> Bool -- ^ 'jIsCanceled'
    -> JobType -- ^ 'jJobType'
    -> Job
job p1 p2 p3 p4 = Job
    { _jJobId = p1
    , _jCreationDate = p2
    , _jIsCanceled = p3
    , _jJobType = p4
    }

-- | A unique identifier which refers to a particular job.
jJobId :: Lens' Job Text
jJobId = lens _jJobId (\s a -> s { _jJobId = a })

-- | Timestamp of the CreateJob request in ISO8601 date format. For example
-- "2010-03-28T20:27:35Z".
jCreationDate :: Lens' Job ISO8601
jCreationDate = lens _jCreationDate (\s a -> s { _jCreationDate = a })

-- | Indicates whether the job was canceled.
jIsCanceled :: Lens' Job Bool
jIsCanceled = lens _jIsCanceled (\s a -> s { _jIsCanceled = a })

-- | Specifies whether the job to initiate is an import or export job.
jJobType :: Lens' Job JobType
jJobType = lens _jJobType (\s a -> s { _jJobType = a })

instance FromXML Job where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Job"
