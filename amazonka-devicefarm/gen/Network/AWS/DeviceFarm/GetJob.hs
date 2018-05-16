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
-- Module      : Network.AWS.DeviceFarm.GetJob
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about a job.
--
--
module Network.AWS.DeviceFarm.GetJob
    (
    -- * Creating a Request
      getJob
    , GetJob
    -- * Request Lenses
    , gjArn

    -- * Destructuring the Response
    , getJobResponse
    , GetJobResponse
    -- * Response Lenses
    , gjrsJob
    , gjrsResponseStatus
    ) where

import Network.AWS.DeviceFarm.Types
import Network.AWS.DeviceFarm.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents a request to the get job operation.
--
--
--
-- /See:/ 'getJob' smart constructor.
newtype GetJob = GetJob'
  { _gjArn :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetJob' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gjArn' - The job's ARN.
getJob
    :: Text -- ^ 'gjArn'
    -> GetJob
getJob pArn_ = GetJob' {_gjArn = pArn_}


-- | The job's ARN.
gjArn :: Lens' GetJob Text
gjArn = lens _gjArn (\ s a -> s{_gjArn = a})

instance AWSRequest GetJob where
        type Rs GetJob = GetJobResponse
        request = postJSON deviceFarm
        response
          = receiveJSON
              (\ s h x ->
                 GetJobResponse' <$>
                   (x .?> "job") <*> (pure (fromEnum s)))

instance Hashable GetJob where

instance NFData GetJob where

instance ToHeaders GetJob where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("DeviceFarm_20150623.GetJob" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetJob where
        toJSON GetJob'{..}
          = object (catMaybes [Just ("arn" .= _gjArn)])

instance ToPath GetJob where
        toPath = const "/"

instance ToQuery GetJob where
        toQuery = const mempty

-- | Represents the result of a get job request.
--
--
--
-- /See:/ 'getJobResponse' smart constructor.
data GetJobResponse = GetJobResponse'
  { _gjrsJob            :: !(Maybe Job)
  , _gjrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetJobResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gjrsJob' - An object containing information about the requested job.
--
-- * 'gjrsResponseStatus' - -- | The response status code.
getJobResponse
    :: Int -- ^ 'gjrsResponseStatus'
    -> GetJobResponse
getJobResponse pResponseStatus_ =
  GetJobResponse' {_gjrsJob = Nothing, _gjrsResponseStatus = pResponseStatus_}


-- | An object containing information about the requested job.
gjrsJob :: Lens' GetJobResponse (Maybe Job)
gjrsJob = lens _gjrsJob (\ s a -> s{_gjrsJob = a})

-- | -- | The response status code.
gjrsResponseStatus :: Lens' GetJobResponse Int
gjrsResponseStatus = lens _gjrsResponseStatus (\ s a -> s{_gjrsResponseStatus = a})

instance NFData GetJobResponse where
