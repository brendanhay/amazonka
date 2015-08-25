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
-- Module      : Network.AWS.CodePipeline.PollForThirdPartyJobs
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
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
      pollForThirdPartyJobs
    , PollForThirdPartyJobs
    -- * Request Lenses
    , pftpjMaxBatchSize
    , pftpjActionTypeId

    -- * Destructuring the Response
    , pollForThirdPartyJobsResponse
    , PollForThirdPartyJobsResponse
    -- * Response Lenses
    , pftpjrsJobs
    , pftpjrsStatus
    ) where

import           Network.AWS.CodePipeline.Types
import           Network.AWS.CodePipeline.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Represents the input of a poll for third party jobs action.
--
-- /See:/ 'pollForThirdPartyJobs' smart constructor.
data PollForThirdPartyJobs = PollForThirdPartyJobs'
    { _pftpjMaxBatchSize :: !(Maybe Nat)
    , _pftpjActionTypeId :: !ActionTypeId
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'PollForThirdPartyJobs' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pftpjMaxBatchSize'
--
-- * 'pftpjActionTypeId'
pollForThirdPartyJobs
    :: ActionTypeId -- ^ 'pftpjActionTypeId'
    -> PollForThirdPartyJobs
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
        type Rs PollForThirdPartyJobs =
             PollForThirdPartyJobsResponse
        request = postJSON codePipeline
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
              (catMaybes
                 [("maxBatchSize" .=) <$> _pftpjMaxBatchSize,
                  Just ("actionTypeId" .= _pftpjActionTypeId)])

instance ToPath PollForThirdPartyJobs where
        toPath = const "/"

instance ToQuery PollForThirdPartyJobs where
        toQuery = const mempty

-- | Represents the output of a poll for third party jobs action.
--
-- /See:/ 'pollForThirdPartyJobsResponse' smart constructor.
data PollForThirdPartyJobsResponse = PollForThirdPartyJobsResponse'
    { _pftpjrsJobs   :: !(Maybe [ThirdPartyJob])
    , _pftpjrsStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'PollForThirdPartyJobsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pftpjrsJobs'
--
-- * 'pftpjrsStatus'
pollForThirdPartyJobsResponse
    :: Int -- ^ 'pftpjrsStatus'
    -> PollForThirdPartyJobsResponse
pollForThirdPartyJobsResponse pStatus_ =
    PollForThirdPartyJobsResponse'
    { _pftpjrsJobs = Nothing
    , _pftpjrsStatus = pStatus_
    }

-- | Information about the jobs to take action on.
pftpjrsJobs :: Lens' PollForThirdPartyJobsResponse [ThirdPartyJob]
pftpjrsJobs = lens _pftpjrsJobs (\ s a -> s{_pftpjrsJobs = a}) . _Default . _Coerce;

-- | The response status code.
pftpjrsStatus :: Lens' PollForThirdPartyJobsResponse Int
pftpjrsStatus = lens _pftpjrsStatus (\ s a -> s{_pftpjrsStatus = a});
