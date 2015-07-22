{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.GetJobDetails
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a job. Only used for custom actions.
--
-- When this API is called, AWS CodePipeline returns temporary credentials
-- for the Amazon S3 bucket used to store artifacts for the pipeline, if
-- the action requires access to that Amazon S3 bucket for input or output
-- artifacts. Additionally, this API returns any secret values defined for
-- the action.
--
-- <http://docs.aws.amazon.com/codepipeline/latest/APIReference/API_GetJobDetails.html>
module Network.AWS.CodePipeline.GetJobDetails
    (
    -- * Request
      GetJobDetails
    -- ** Request constructor
    , getJobDetails
    -- ** Request lenses
    , gjdrqJobId

    -- * Response
    , GetJobDetailsResponse
    -- ** Response constructor
    , getJobDetailsResponse
    -- ** Response lenses
    , gjdrsJobDetails
    , gjdrsStatus
    ) where

import           Network.AWS.CodePipeline.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Represents the input of a get job details action.
--
-- /See:/ 'getJobDetails' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gjdrqJobId'
newtype GetJobDetails = GetJobDetails'
    { _gjdrqJobId :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'GetJobDetails' smart constructor.
getJobDetails :: Text -> GetJobDetails
getJobDetails pJobId =
    GetJobDetails'
    { _gjdrqJobId = pJobId
    }

-- | The unique system-generated ID for the job.
gjdrqJobId :: Lens' GetJobDetails Text
gjdrqJobId = lens _gjdrqJobId (\ s a -> s{_gjdrqJobId = a});

instance AWSRequest GetJobDetails where
        type Sv GetJobDetails = CodePipeline
        type Rs GetJobDetails = GetJobDetailsResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 GetJobDetailsResponse' <$>
                   (x .?> "jobDetails") <*> (pure (fromEnum s)))

instance ToHeaders GetJobDetails where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("CodePipeline_20150709.GetJobDetails" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetJobDetails where
        toJSON GetJobDetails'{..}
          = object ["jobId" .= _gjdrqJobId]

instance ToPath GetJobDetails where
        toPath = const "/"

instance ToQuery GetJobDetails where
        toQuery = const mempty

-- | Represents the output of a get job details action.
--
-- /See:/ 'getJobDetailsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gjdrsJobDetails'
--
-- * 'gjdrsStatus'
data GetJobDetailsResponse = GetJobDetailsResponse'
    { _gjdrsJobDetails :: !(Maybe JobDetails)
    , _gjdrsStatus     :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'GetJobDetailsResponse' smart constructor.
getJobDetailsResponse :: Int -> GetJobDetailsResponse
getJobDetailsResponse pStatus =
    GetJobDetailsResponse'
    { _gjdrsJobDetails = Nothing
    , _gjdrsStatus = pStatus
    }

-- | The details of the job.
--
-- If AWSSessionCredentials is used, a long-running job can call
-- GetJobDetails again to obtain new credentials.
gjdrsJobDetails :: Lens' GetJobDetailsResponse (Maybe JobDetails)
gjdrsJobDetails = lens _gjdrsJobDetails (\ s a -> s{_gjdrsJobDetails = a});

-- | FIXME: Undocumented member.
gjdrsStatus :: Lens' GetJobDetailsResponse Int
gjdrsStatus = lens _gjdrsStatus (\ s a -> s{_gjdrsStatus = a});
