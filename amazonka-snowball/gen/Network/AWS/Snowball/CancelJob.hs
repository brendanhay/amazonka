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
-- Module      : Network.AWS.Snowball.CancelJob
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Cancels the specified job. You can only cancel a job before its @JobState@ value changes to @PreparingAppliance@ . Requesting the @ListJobs@ or @DescribeJob@ action will return a job's @JobState@ as part of the response element data returned.
--
--
module Network.AWS.Snowball.CancelJob
    (
    -- * Creating a Request
      cancelJob
    , CancelJob
    -- * Request Lenses
    , cjJobId

    -- * Destructuring the Response
    , cancelJobResponse
    , CancelJobResponse
    -- * Response Lenses
    , crsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Snowball.Types
import Network.AWS.Snowball.Types.Product

-- | /See:/ 'cancelJob' smart constructor.
newtype CancelJob = CancelJob'
  { _cjJobId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CancelJob' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cjJobId' - The 39-character job ID for the job that you want to cancel, for example @JID123e4567-e89b-12d3-a456-426655440000@ .
cancelJob
    :: Text -- ^ 'cjJobId'
    -> CancelJob
cancelJob pJobId_ = CancelJob' {_cjJobId = pJobId_}


-- | The 39-character job ID for the job that you want to cancel, for example @JID123e4567-e89b-12d3-a456-426655440000@ .
cjJobId :: Lens' CancelJob Text
cjJobId = lens _cjJobId (\ s a -> s{_cjJobId = a})

instance AWSRequest CancelJob where
        type Rs CancelJob = CancelJobResponse
        request = postJSON snowball
        response
          = receiveEmpty
              (\ s h x ->
                 CancelJobResponse' <$> (pure (fromEnum s)))

instance Hashable CancelJob where

instance NFData CancelJob where

instance ToHeaders CancelJob where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSIESnowballJobManagementService.CancelJob" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CancelJob where
        toJSON CancelJob'{..}
          = object (catMaybes [Just ("JobId" .= _cjJobId)])

instance ToPath CancelJob where
        toPath = const "/"

instance ToQuery CancelJob where
        toQuery = const mempty

-- | /See:/ 'cancelJobResponse' smart constructor.
newtype CancelJobResponse = CancelJobResponse'
  { _crsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CancelJobResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'crsResponseStatus' - -- | The response status code.
cancelJobResponse
    :: Int -- ^ 'crsResponseStatus'
    -> CancelJobResponse
cancelJobResponse pResponseStatus_ =
  CancelJobResponse' {_crsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
crsResponseStatus :: Lens' CancelJobResponse Int
crsResponseStatus = lens _crsResponseStatus (\ s a -> s{_crsResponseStatus = a})

instance NFData CancelJobResponse where
