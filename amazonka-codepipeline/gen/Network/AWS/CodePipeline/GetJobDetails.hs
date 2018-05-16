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
-- Module      : Network.AWS.CodePipeline.GetJobDetails
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a job. Only used for custom actions.
--
--
-- /Important:/ When this API is called, AWS CodePipeline returns temporary credentials for the Amazon S3 bucket used to store artifacts for the pipeline, if the action requires access to that Amazon S3 bucket for input or output artifacts. Additionally, this API returns any secret values defined for the action.
--
module Network.AWS.CodePipeline.GetJobDetails
    (
    -- * Creating a Request
      getJobDetails
    , GetJobDetails
    -- * Request Lenses
    , gjdJobId

    -- * Destructuring the Response
    , getJobDetailsResponse
    , GetJobDetailsResponse
    -- * Response Lenses
    , gjdrsJobDetails
    , gjdrsResponseStatus
    ) where

import Network.AWS.CodePipeline.Types
import Network.AWS.CodePipeline.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents the input of a GetJobDetails action.
--
--
--
-- /See:/ 'getJobDetails' smart constructor.
newtype GetJobDetails = GetJobDetails'
  { _gjdJobId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetJobDetails' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gjdJobId' - The unique system-generated ID for the job.
getJobDetails
    :: Text -- ^ 'gjdJobId'
    -> GetJobDetails
getJobDetails pJobId_ = GetJobDetails' {_gjdJobId = pJobId_}


-- | The unique system-generated ID for the job.
gjdJobId :: Lens' GetJobDetails Text
gjdJobId = lens _gjdJobId (\ s a -> s{_gjdJobId = a})

instance AWSRequest GetJobDetails where
        type Rs GetJobDetails = GetJobDetailsResponse
        request = postJSON codePipeline
        response
          = receiveJSON
              (\ s h x ->
                 GetJobDetailsResponse' <$>
                   (x .?> "jobDetails") <*> (pure (fromEnum s)))

instance Hashable GetJobDetails where

instance NFData GetJobDetails where

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
          = object (catMaybes [Just ("jobId" .= _gjdJobId)])

instance ToPath GetJobDetails where
        toPath = const "/"

instance ToQuery GetJobDetails where
        toQuery = const mempty

-- | Represents the output of a GetJobDetails action.
--
--
--
-- /See:/ 'getJobDetailsResponse' smart constructor.
data GetJobDetailsResponse = GetJobDetailsResponse'
  { _gjdrsJobDetails     :: !(Maybe JobDetails)
  , _gjdrsResponseStatus :: !Int
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetJobDetailsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gjdrsJobDetails' - The details of the job.
--
-- * 'gjdrsResponseStatus' - -- | The response status code.
getJobDetailsResponse
    :: Int -- ^ 'gjdrsResponseStatus'
    -> GetJobDetailsResponse
getJobDetailsResponse pResponseStatus_ =
  GetJobDetailsResponse'
    {_gjdrsJobDetails = Nothing, _gjdrsResponseStatus = pResponseStatus_}


-- | The details of the job.
gjdrsJobDetails :: Lens' GetJobDetailsResponse (Maybe JobDetails)
gjdrsJobDetails = lens _gjdrsJobDetails (\ s a -> s{_gjdrsJobDetails = a})

-- | -- | The response status code.
gjdrsResponseStatus :: Lens' GetJobDetailsResponse Int
gjdrsResponseStatus = lens _gjdrsResponseStatus (\ s a -> s{_gjdrsResponseStatus = a})

instance NFData GetJobDetailsResponse where
