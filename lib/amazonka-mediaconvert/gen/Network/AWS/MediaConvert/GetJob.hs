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
-- Module      : Network.AWS.MediaConvert.GetJob
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieve the JSON for a specific completed transcoding job.
module Network.AWS.MediaConvert.GetJob
    (
    -- * Creating a Request
      getJob
    , GetJob
    -- * Request Lenses
    , gjId

    -- * Destructuring the Response
    , getJobResponse
    , GetJobResponse
    -- * Response Lenses
    , gjrsJob
    , gjrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.MediaConvert.Types
import Network.AWS.MediaConvert.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getJob' smart constructor.
newtype GetJob = GetJob'
  { _gjId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetJob' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gjId' - the job ID of the job.
getJob
    :: Text -- ^ 'gjId'
    -> GetJob
getJob pId_ = GetJob' {_gjId = pId_}


-- | the job ID of the job.
gjId :: Lens' GetJob Text
gjId = lens _gjId (\ s a -> s{_gjId = a})

instance AWSRequest GetJob where
        type Rs GetJob = GetJobResponse
        request = get mediaConvert
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
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath GetJob where
        toPath GetJob'{..}
          = mconcat ["/2017-08-29/jobs/", toBS _gjId]

instance ToQuery GetJob where
        toQuery = const mempty

-- | /See:/ 'getJobResponse' smart constructor.
data GetJobResponse = GetJobResponse'
  { _gjrsJob            :: !(Maybe Job)
  , _gjrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetJobResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gjrsJob' - Undocumented member.
--
-- * 'gjrsResponseStatus' - -- | The response status code.
getJobResponse
    :: Int -- ^ 'gjrsResponseStatus'
    -> GetJobResponse
getJobResponse pResponseStatus_ =
  GetJobResponse' {_gjrsJob = Nothing, _gjrsResponseStatus = pResponseStatus_}


-- | Undocumented member.
gjrsJob :: Lens' GetJobResponse (Maybe Job)
gjrsJob = lens _gjrsJob (\ s a -> s{_gjrsJob = a})

-- | -- | The response status code.
gjrsResponseStatus :: Lens' GetJobResponse Int
gjrsResponseStatus = lens _gjrsResponseStatus (\ s a -> s{_gjrsResponseStatus = a})

instance NFData GetJobResponse where
