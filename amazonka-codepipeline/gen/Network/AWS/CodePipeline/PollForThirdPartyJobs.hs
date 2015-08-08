{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.PollForThirdPartyJobs
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Determines whether there are any third party jobs for a job worker to
-- act on. Only used for partner actions.
--
-- When this API is called, AWS CodePipeline returns temporary credentials
-- for the Amazon S3 bucket used to store artifacts for the pipeline, if
-- the action requires access to that Amazon S3 bucket for input or output
-- artifacts.
--
-- /See:/ <http://docs.aws.amazon.com/codepipeline/latest/APIReference/API_PollForThirdPartyJobs.html AWS API Reference> for PollForThirdPartyJobs.
module Network.AWS.CodePipeline.PollForThirdPartyJobs
    (
    -- * Creating a Request
      PollForThirdPartyJobs
    , pollForThirdPartyJobs
    -- * Request Lenses
    , pftpjMaxBatchSize
    , pftpjActionTypeId

    -- * Destructuring the Response
    , PollForThirdPartyJobsResponse
    , pollForThirdPartyJobsResponse
    -- * Response Lenses
    , pftpjrsJobs
    , pftpjrsStatus
    ) where

import           Network.AWS.CodePipeline.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Represents the input of a poll for third party jobs action.
--
-- /See:/ 'pollForThirdPartyJobs' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'pftpjMaxBatchSize'
--
-- * 'pftpjActionTypeId'
data PollForThirdPartyJobs = PollForThirdPartyJobs'
    { _pftpjMaxBatchSize :: !(Maybe Nat)
    , _pftpjActionTypeId :: !ActionTypeId
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'PollForThirdPartyJobs' smart constructor.
pollForThirdPartyJobs :: ActionTypeId -> PollForThirdPartyJobs
pollForThirdPartyJobs pActionTypeId_ =
    PollForThirdPartyJobs'
    { _pftpjMaxBatchSize = Nothing
    , _pftpjActionTypeId = pActionTypeId_
    }

-- | The maximum number of jobs to return in a poll for jobs call.
pftpjMaxBatchSize :: Lens' PollForThirdPartyJobs (Maybe Natural)
pftpjMaxBatchSize = lens _pftpjMaxBatchSize (\ s a -> s{_pftpjMaxBatchSize = a}) . mapping _Nat;

-- | Undocumented member.
pftpjActionTypeId :: Lens' PollForThirdPartyJobs ActionTypeId
pftpjActionTypeId = lens _pftpjActionTypeId (\ s a -> s{_pftpjActionTypeId = a});

instance AWSRequest PollForThirdPartyJobs where
        type Sv PollForThirdPartyJobs = CodePipeline
        type Rs PollForThirdPartyJobs =
             PollForThirdPartyJobsResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 PollForThirdPartyJobsResponse' <$>
                   (x .?> "jobs" .!@ mempty) <*> (pure (fromEnum s)))

instance ToHeaders PollForThirdPartyJobs where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("CodePipeline_20150709.PollForThirdPartyJobs" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON PollForThirdPartyJobs where
        toJSON PollForThirdPartyJobs'{..}
          = object
              ["maxBatchSize" .= _pftpjMaxBatchSize,
               "actionTypeId" .= _pftpjActionTypeId]

instance ToPath PollForThirdPartyJobs where
        toPath = const "/"

instance ToQuery PollForThirdPartyJobs where
        toQuery = const mempty

-- | Represents the output of a poll for third party jobs action.
--
-- /See:/ 'pollForThirdPartyJobsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'pftpjrsJobs'
--
-- * 'pftpjrsStatus'
data PollForThirdPartyJobsResponse = PollForThirdPartyJobsResponse'
    { _pftpjrsJobs   :: !(Maybe [ThirdPartyJob])
    , _pftpjrsStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'PollForThirdPartyJobsResponse' smart constructor.
pollForThirdPartyJobsResponse :: Int -> PollForThirdPartyJobsResponse
pollForThirdPartyJobsResponse pStatus_ =
    PollForThirdPartyJobsResponse'
    { _pftpjrsJobs = Nothing
    , _pftpjrsStatus = pStatus_
    }

-- | Information about the jobs to take action on.
pftpjrsJobs :: Lens' PollForThirdPartyJobsResponse [ThirdPartyJob]
pftpjrsJobs = lens _pftpjrsJobs (\ s a -> s{_pftpjrsJobs = a}) . _Default . _Coerce;

-- | Undocumented member.
pftpjrsStatus :: Lens' PollForThirdPartyJobsResponse Int
pftpjrsStatus = lens _pftpjrsStatus (\ s a -> s{_pftpjrsStatus = a});
