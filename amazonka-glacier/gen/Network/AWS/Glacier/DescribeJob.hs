{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Glacier.DescribeJob
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | This operation returns information about a job you previously initiated,
-- including the job initiation date, the user who initiated the job, the job
-- status code/message and the Amazon SNS topic to notify after Amazon Glacier
-- completes the job. For more information about initiating a job, see 'InitiateJob'.
--
-- This operation enables you to check the status of your job. However, it is
-- strongly recommended that you set up an Amazon SNS topic and specify it in
-- your initiate job request so that Amazon Glacier can notify the topic after
-- it completes the job.
--
-- A job ID will not expire for at least 24 hours after Amazon Glacier
-- completes the job.
--
-- An AWS account has full permission to perform all operations (actions).
-- However, AWS Identity and Access Management (IAM) users don't have any
-- permissions by default. You must grant them explicit permission to perform
-- specific actions. For more information, see <http://docs.aws.amazon.com/amazonglacier/latest/dev/using-iam-with-amazon-glacier.html Access Control Using AWS Identityand Access Management (IAM)>.
--
-- For information about the underlying REST API, go to <http://docs.aws.amazon.com/amazonglacier/latest/dev/api-describe-job-get.html Working with Archivesin Amazon Glacier> in the /Amazon Glacier Developer Guide/.
--
-- <http://docs.aws.amazon.com/amazonglacier/latest/dev/api-DescribeJob.html>
module Network.AWS.Glacier.DescribeJob
    (
    -- * Request
      DescribeJob
    -- ** Request constructor
    , describeJob
    -- ** Request lenses
    , djAccountId
    , djJobId
    , djVaultName

    -- * Response
    , DescribeJobResponse
    -- ** Response constructor
    , describeJobResponse
    -- ** Response lenses
    , djrAction
    , djrArchiveId
    , djrArchiveSHA256TreeHash
    , djrArchiveSizeInBytes
    , djrCompleted
    , djrCompletionDate
    , djrCreationDate
    , djrInventoryRetrievalParameters
    , djrInventorySizeInBytes
    , djrJobDescription
    , djrJobId
    , djrRetrievalByteRange
    , djrSHA256TreeHash
    , djrSNSTopic
    , djrStatusCode
    , djrStatusMessage
    , djrVaultARN
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.RestJSON
import Network.AWS.Glacier.Types
import qualified GHC.Exts

data DescribeJob = DescribeJob
    { _djAccountId :: Text
    , _djJobId     :: Text
    , _djVaultName :: Text
    } deriving (Eq, Ord, Read, Show)

-- | 'DescribeJob' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'djAccountId' @::@ 'Text'
--
-- * 'djJobId' @::@ 'Text'
--
-- * 'djVaultName' @::@ 'Text'
--
describeJob :: Text -- ^ 'djAccountId'
            -> Text -- ^ 'djVaultName'
            -> Text -- ^ 'djJobId'
            -> DescribeJob
describeJob p1 p2 p3 = DescribeJob
    { _djAccountId = p1
    , _djVaultName = p2
    , _djJobId     = p3
    }

-- | The 'AccountId' is the AWS Account ID. You can specify either the AWS Account
-- ID or optionally a '-', in which case Amazon Glacier uses the AWS Account ID
-- associated with the credentials used to sign the request. If you specify your
-- Account ID, do not include hyphens in it.
djAccountId :: Lens' DescribeJob Text
djAccountId = lens _djAccountId (\s a -> s { _djAccountId = a })

-- | The ID of the job to describe.
djJobId :: Lens' DescribeJob Text
djJobId = lens _djJobId (\s a -> s { _djJobId = a })

-- | The name of the vault.
djVaultName :: Lens' DescribeJob Text
djVaultName = lens _djVaultName (\s a -> s { _djVaultName = a })

data DescribeJobResponse = DescribeJobResponse
    { _djrAction                       :: Maybe ActionCode
    , _djrArchiveId                    :: Maybe Text
    , _djrArchiveSHA256TreeHash        :: Maybe Text
    , _djrArchiveSizeInBytes           :: Maybe Integer
    , _djrCompleted                    :: Maybe Bool
    , _djrCompletionDate               :: Maybe Text
    , _djrCreationDate                 :: Maybe Text
    , _djrInventoryRetrievalParameters :: Maybe InventoryRetrievalJobDescription
    , _djrInventorySizeInBytes         :: Maybe Integer
    , _djrJobDescription               :: Maybe Text
    , _djrJobId                        :: Maybe Text
    , _djrRetrievalByteRange           :: Maybe Text
    , _djrSHA256TreeHash               :: Maybe Text
    , _djrSNSTopic                     :: Maybe Text
    , _djrStatusCode                   :: Maybe StatusCode
    , _djrStatusMessage                :: Maybe Text
    , _djrVaultARN                     :: Maybe Text
    } deriving (Eq, Read, Show)

-- | 'DescribeJobResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'djrAction' @::@ 'Maybe' 'ActionCode'
--
-- * 'djrArchiveId' @::@ 'Maybe' 'Text'
--
-- * 'djrArchiveSHA256TreeHash' @::@ 'Maybe' 'Text'
--
-- * 'djrArchiveSizeInBytes' @::@ 'Maybe' 'Integer'
--
-- * 'djrCompleted' @::@ 'Maybe' 'Bool'
--
-- * 'djrCompletionDate' @::@ 'Maybe' 'Text'
--
-- * 'djrCreationDate' @::@ 'Maybe' 'Text'
--
-- * 'djrInventoryRetrievalParameters' @::@ 'Maybe' 'InventoryRetrievalJobDescription'
--
-- * 'djrInventorySizeInBytes' @::@ 'Maybe' 'Integer'
--
-- * 'djrJobDescription' @::@ 'Maybe' 'Text'
--
-- * 'djrJobId' @::@ 'Maybe' 'Text'
--
-- * 'djrRetrievalByteRange' @::@ 'Maybe' 'Text'
--
-- * 'djrSHA256TreeHash' @::@ 'Maybe' 'Text'
--
-- * 'djrSNSTopic' @::@ 'Maybe' 'Text'
--
-- * 'djrStatusCode' @::@ 'Maybe' 'StatusCode'
--
-- * 'djrStatusMessage' @::@ 'Maybe' 'Text'
--
-- * 'djrVaultARN' @::@ 'Maybe' 'Text'
--
describeJobResponse :: DescribeJobResponse
describeJobResponse = DescribeJobResponse
    { _djrJobId                        = Nothing
    , _djrJobDescription               = Nothing
    , _djrAction                       = Nothing
    , _djrArchiveId                    = Nothing
    , _djrVaultARN                     = Nothing
    , _djrCreationDate                 = Nothing
    , _djrCompleted                    = Nothing
    , _djrStatusCode                   = Nothing
    , _djrStatusMessage                = Nothing
    , _djrArchiveSizeInBytes           = Nothing
    , _djrInventorySizeInBytes         = Nothing
    , _djrSNSTopic                     = Nothing
    , _djrCompletionDate               = Nothing
    , _djrSHA256TreeHash               = Nothing
    , _djrArchiveSHA256TreeHash        = Nothing
    , _djrRetrievalByteRange           = Nothing
    , _djrInventoryRetrievalParameters = Nothing
    }

-- | The job type. It is either ArchiveRetrieval or InventoryRetrieval.
djrAction :: Lens' DescribeJobResponse (Maybe ActionCode)
djrAction = lens _djrAction (\s a -> s { _djrAction = a })

-- | For an ArchiveRetrieval job, this is the archive ID requested for download.
-- Otherwise, this field is null.
djrArchiveId :: Lens' DescribeJobResponse (Maybe Text)
djrArchiveId = lens _djrArchiveId (\s a -> s { _djrArchiveId = a })

-- | The SHA256 tree hash of the entire archive for an archive retrieval. For
-- inventory retrieval jobs, this field is null.
djrArchiveSHA256TreeHash :: Lens' DescribeJobResponse (Maybe Text)
djrArchiveSHA256TreeHash =
    lens _djrArchiveSHA256TreeHash
        (\s a -> s { _djrArchiveSHA256TreeHash = a })

-- | For an ArchiveRetrieval job, this is the size in bytes of the archive being
-- requested for download. For the InventoryRetrieval job, the value is null.
djrArchiveSizeInBytes :: Lens' DescribeJobResponse (Maybe Integer)
djrArchiveSizeInBytes =
    lens _djrArchiveSizeInBytes (\s a -> s { _djrArchiveSizeInBytes = a })

-- | The job status. When a job is completed, you get the job's output.
djrCompleted :: Lens' DescribeJobResponse (Maybe Bool)
djrCompleted = lens _djrCompleted (\s a -> s { _djrCompleted = a })

-- | The UTC time that the archive retrieval request completed. While the job is
-- in progress, the value will be null.
djrCompletionDate :: Lens' DescribeJobResponse (Maybe Text)
djrCompletionDate =
    lens _djrCompletionDate (\s a -> s { _djrCompletionDate = a })

-- | The UTC date when the job was created. A string representation of ISO 8601
-- date format, for example, "2012-03-20T17:03:43.221Z".
djrCreationDate :: Lens' DescribeJobResponse (Maybe Text)
djrCreationDate = lens _djrCreationDate (\s a -> s { _djrCreationDate = a })

-- | Parameters used for range inventory retrieval.
djrInventoryRetrievalParameters :: Lens' DescribeJobResponse (Maybe InventoryRetrievalJobDescription)
djrInventoryRetrievalParameters =
    lens _djrInventoryRetrievalParameters
        (\s a -> s { _djrInventoryRetrievalParameters = a })

-- | For an InventoryRetrieval job, this is the size in bytes of the inventory
-- requested for download. For the ArchiveRetrieval job, the value is null.
djrInventorySizeInBytes :: Lens' DescribeJobResponse (Maybe Integer)
djrInventorySizeInBytes =
    lens _djrInventorySizeInBytes (\s a -> s { _djrInventorySizeInBytes = a })

-- | The job description you provided when you initiated the job.
djrJobDescription :: Lens' DescribeJobResponse (Maybe Text)
djrJobDescription =
    lens _djrJobDescription (\s a -> s { _djrJobDescription = a })

-- | An opaque string that identifies an Amazon Glacier job.
djrJobId :: Lens' DescribeJobResponse (Maybe Text)
djrJobId = lens _djrJobId (\s a -> s { _djrJobId = a })

-- | The retrieved byte range for archive retrieval jobs in the form "/StartByteValue/-/EndByteValue/" If no range was specified in the archive retrieval, then the
-- whole archive is retrieved and /StartByteValue/ equals 0 and /EndByteValue/
-- equals the size of the archive minus 1. For inventory retrieval jobs this
-- field is null.
djrRetrievalByteRange :: Lens' DescribeJobResponse (Maybe Text)
djrRetrievalByteRange =
    lens _djrRetrievalByteRange (\s a -> s { _djrRetrievalByteRange = a })

-- | For an ArchiveRetrieval job, it is the checksum of the archive. Otherwise,
-- the value is null.
--
-- The SHA256 tree hash value for the requested range of an archive. If the
-- Initiate a Job request for an archive specified a tree-hash aligned range,
-- then this field returns a value.
--
-- For the specific case when the whole archive is retrieved, this value is
-- the same as the ArchiveSHA256TreeHash value.
--
-- This field is null in the following situations:  Archive retrieval jobs
-- that specify a range that is not tree-hash aligned.
--
-- Archival jobs that specify a range that is equal to the whole archive and
-- the job status is InProgress.
--
-- Inventory jobs.
--
--
djrSHA256TreeHash :: Lens' DescribeJobResponse (Maybe Text)
djrSHA256TreeHash =
    lens _djrSHA256TreeHash (\s a -> s { _djrSHA256TreeHash = a })

-- | An Amazon Simple Notification Service (Amazon SNS) topic that receives
-- notification.
djrSNSTopic :: Lens' DescribeJobResponse (Maybe Text)
djrSNSTopic = lens _djrSNSTopic (\s a -> s { _djrSNSTopic = a })

-- | The status code can be InProgress, Succeeded, or Failed, and indicates the
-- status of the job.
djrStatusCode :: Lens' DescribeJobResponse (Maybe StatusCode)
djrStatusCode = lens _djrStatusCode (\s a -> s { _djrStatusCode = a })

-- | A friendly message that describes the job status.
djrStatusMessage :: Lens' DescribeJobResponse (Maybe Text)
djrStatusMessage = lens _djrStatusMessage (\s a -> s { _djrStatusMessage = a })

-- | The Amazon Resource Name (ARN) of the vault from which the archive retrieval
-- was requested.
djrVaultARN :: Lens' DescribeJobResponse (Maybe Text)
djrVaultARN = lens _djrVaultARN (\s a -> s { _djrVaultARN = a })

instance ToPath DescribeJob where
    toPath DescribeJob{..} = mconcat
        [ "/"
        , toText _djAccountId
        , "/vaults/"
        , toText _djVaultName
        , "/jobs/"
        , toText _djJobId
        ]

instance ToQuery DescribeJob where
    toQuery = const mempty

instance ToHeaders DescribeJob

instance ToJSON DescribeJob where
    toJSON = const (toJSON Empty)

instance AWSRequest DescribeJob where
    type Sv DescribeJob = Glacier
    type Rs DescribeJob = DescribeJobResponse

    request  = get
    response = jsonResponse

instance FromJSON DescribeJobResponse where
    parseJSON = withObject "DescribeJobResponse" $ \o -> DescribeJobResponse
        <$> o .:? "Action"
        <*> o .:? "ArchiveId"
        <*> o .:? "ArchiveSHA256TreeHash"
        <*> o .:? "ArchiveSizeInBytes"
        <*> o .:? "Completed"
        <*> o .:? "CompletionDate"
        <*> o .:? "CreationDate"
        <*> o .:? "InventoryRetrievalParameters"
        <*> o .:? "InventorySizeInBytes"
        <*> o .:? "JobDescription"
        <*> o .:? "JobId"
        <*> o .:? "RetrievalByteRange"
        <*> o .:? "SHA256TreeHash"
        <*> o .:? "SNSTopic"
        <*> o .:? "StatusCode"
        <*> o .:? "StatusMessage"
        <*> o .:? "VaultARN"
