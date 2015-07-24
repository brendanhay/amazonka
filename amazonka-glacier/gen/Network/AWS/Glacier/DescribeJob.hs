{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glacier.DescribeJob
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- This operation returns information about a job you previously initiated,
-- including the job initiation date, the user who initiated the job, the
-- job status code\/message and the Amazon SNS topic to notify after Amazon
-- Glacier completes the job. For more information about initiating a job,
-- see InitiateJob.
--
-- This operation enables you to check the status of your job. However, it
-- is strongly recommended that you set up an Amazon SNS topic and specify
-- it in your initiate job request so that Amazon Glacier can notify the
-- topic after it completes the job.
--
-- A job ID will not expire for at least 24 hours after Amazon Glacier
-- completes the job.
--
-- An AWS account has full permission to perform all operations (actions).
-- However, AWS Identity and Access Management (IAM) users don\'t have any
-- permissions by default. You must grant them explicit permission to
-- perform specific actions. For more information, see
-- <http://docs.aws.amazon.com/amazonglacier/latest/dev/using-iam-with-amazon-glacier.html Access Control Using AWS Identity and Access Management (IAM)>.
--
-- For information about the underlying REST API, go to
-- <http://docs.aws.amazon.com/amazonglacier/latest/dev/api-describe-job-get.html Working with Archives in Amazon Glacier>
-- in the /Amazon Glacier Developer Guide/.
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
    , djVaultName
    , djJobId

    -- * Response
    , GlacierJobDescription
    -- ** Response constructor
    , glacierJobDescription
    -- ** Response lenses
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
    ) where

import           Network.AWS.Glacier.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Provides options for retrieving a job description.
--
-- /See:/ 'describeJob' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'djAccountId'
--
-- * 'djVaultName'
--
-- * 'djJobId'
data DescribeJob = DescribeJob'
    { _djAccountId :: !Text
    , _djVaultName :: !Text
    , _djJobId     :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeJob' smart constructor.
describeJob :: Text -> Text -> Text -> DescribeJob
describeJob pAccountId_ pVaultName_ pJobId_ =
    DescribeJob'
    { _djAccountId = pAccountId_
    , _djVaultName = pVaultName_
    , _djJobId = pJobId_
    }

-- | The @AccountId@ value is the AWS account ID of the account that owns the
-- vault. You can either specify an AWS account ID or optionally a single
-- apos@-@apos (hyphen), in which case Amazon Glacier uses the AWS account
-- ID associated with the credentials used to sign the request. If you use
-- an account ID, do not include any hyphens (apos-apos) in the ID.
djAccountId :: Lens' DescribeJob Text
djAccountId = lens _djAccountId (\ s a -> s{_djAccountId = a});

-- | The name of the vault.
djVaultName :: Lens' DescribeJob Text
djVaultName = lens _djVaultName (\ s a -> s{_djVaultName = a});

-- | The ID of the job to describe.
djJobId :: Lens' DescribeJob Text
djJobId = lens _djJobId (\ s a -> s{_djJobId = a});

instance AWSRequest DescribeJob where
        type Sv DescribeJob = Glacier
        type Rs DescribeJob = GlacierJobDescription
        request = get
        response = receiveJSON (\ s h x -> eitherParseJSON x)

instance ToHeaders DescribeJob where
        toHeaders = const mempty

instance ToPath DescribeJob where
        toPath DescribeJob'{..}
          = mconcat
              ["/", toText _djAccountId, "/vaults/",
               toText _djVaultName, "/jobs/", toText _djJobId]

instance ToQuery DescribeJob where
        toQuery = const mempty
