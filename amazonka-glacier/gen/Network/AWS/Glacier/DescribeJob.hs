{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glacier.DescribeJob
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation returns information about a job you previously initiated, including the job initiation date, the user who initiated the job, the job status code/message and the Amazon SNS topic to notify after Amazon Glacier completes the job. For more information about initiating a job, see 'InitiateJob' .
--
--
-- A job ID will not expire for at least 24 hours after Amazon Glacier completes the job.
--
-- An AWS account has full permission to perform all operations (actions). However, AWS Identity and Access Management (IAM) users don't have any permissions by default. You must grant them explicit permission to perform specific actions. For more information, see <http://docs.aws.amazon.com/amazonglacier/latest/dev/using-iam-with-amazon-glacier.html Access Control Using AWS Identity and Access Management (IAM)> .
--
-- For more information about using this operation, see the documentation for the underlying REST API <http://docs.aws.amazon.com/amazonglacier/latest/dev/api-describe-job-get.html Describe Job> in the /Amazon Glacier Developer Guide/ .
--
module Network.AWS.Glacier.DescribeJob
    (
    -- * Creating a Request
      describeJob
    , DescribeJob
    -- * Request Lenses
    , djAccountId
    , djVaultName
    , djJobId

    -- * Destructuring the Response
    , glacierJobDescription
    , GlacierJobDescription
    -- * Response Lenses
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
    ) where

import Network.AWS.Glacier.Types
import Network.AWS.Glacier.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Provides options for retrieving a job description.
--
--
--
-- /See:/ 'describeJob' smart constructor.
data DescribeJob = DescribeJob'
  { _djAccountId :: !Text
  , _djVaultName :: !Text
  , _djJobId     :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeJob' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'djAccountId' - The @AccountId@ value is the AWS account ID of the account that owns the vault. You can either specify an AWS account ID or optionally a single '@-@ ' (hyphen), in which case Amazon Glacier uses the AWS account ID associated with the credentials used to sign the request. If you use an account ID, do not include any hyphens ('-') in the ID.
--
-- * 'djVaultName' - The name of the vault.
--
-- * 'djJobId' - The ID of the job to describe.
describeJob
    :: Text -- ^ 'djAccountId'
    -> Text -- ^ 'djVaultName'
    -> Text -- ^ 'djJobId'
    -> DescribeJob
describeJob pAccountId_ pVaultName_ pJobId_ =
  DescribeJob'
    {_djAccountId = pAccountId_, _djVaultName = pVaultName_, _djJobId = pJobId_}


-- | The @AccountId@ value is the AWS account ID of the account that owns the vault. You can either specify an AWS account ID or optionally a single '@-@ ' (hyphen), in which case Amazon Glacier uses the AWS account ID associated with the credentials used to sign the request. If you use an account ID, do not include any hyphens ('-') in the ID.
djAccountId :: Lens' DescribeJob Text
djAccountId = lens _djAccountId (\ s a -> s{_djAccountId = a})

-- | The name of the vault.
djVaultName :: Lens' DescribeJob Text
djVaultName = lens _djVaultName (\ s a -> s{_djVaultName = a})

-- | The ID of the job to describe.
djJobId :: Lens' DescribeJob Text
djJobId = lens _djJobId (\ s a -> s{_djJobId = a})

instance AWSRequest DescribeJob where
        type Rs DescribeJob = GlacierJobDescription
        request = get glacier
        response = receiveJSON (\ s h x -> eitherParseJSON x)

instance Hashable DescribeJob where

instance NFData DescribeJob where

instance ToHeaders DescribeJob where
        toHeaders = const mempty

instance ToPath DescribeJob where
        toPath DescribeJob'{..}
          = mconcat
              ["/", toBS _djAccountId, "/vaults/",
               toBS _djVaultName, "/jobs/", toBS _djJobId]

instance ToQuery DescribeJob where
        toQuery = const mempty
