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
-- Module      : Network.AWS.DeviceFarm.StopJob
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Initiates a stop request for the current job. AWS Device Farm will immediately stop the job on the device where tests have not started executing, and you will not be billed for this device. On the device where tests have started executing, Setup Suite and Teardown Suite tests will run to completion before stopping execution on the device. You will be billed for Setup, Teardown, and any tests that were in progress or already completed.
--
--
module Network.AWS.DeviceFarm.StopJob
    (
    -- * Creating a Request
      stopJob
    , StopJob
    -- * Request Lenses
    , sjArn

    -- * Destructuring the Response
    , stopJobResponse
    , StopJobResponse
    -- * Response Lenses
    , sjrsJob
    , sjrsResponseStatus
    ) where

import Network.AWS.DeviceFarm.Types
import Network.AWS.DeviceFarm.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'stopJob' smart constructor.
newtype StopJob = StopJob'
  { _sjArn :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StopJob' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sjArn' - Represents the Amazon Resource Name (ARN) of the Device Farm job you wish to stop.
stopJob
    :: Text -- ^ 'sjArn'
    -> StopJob
stopJob pArn_ = StopJob' {_sjArn = pArn_}


-- | Represents the Amazon Resource Name (ARN) of the Device Farm job you wish to stop.
sjArn :: Lens' StopJob Text
sjArn = lens _sjArn (\ s a -> s{_sjArn = a})

instance AWSRequest StopJob where
        type Rs StopJob = StopJobResponse
        request = postJSON deviceFarm
        response
          = receiveJSON
              (\ s h x ->
                 StopJobResponse' <$>
                   (x .?> "job") <*> (pure (fromEnum s)))

instance Hashable StopJob where

instance NFData StopJob where

instance ToHeaders StopJob where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("DeviceFarm_20150623.StopJob" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON StopJob where
        toJSON StopJob'{..}
          = object (catMaybes [Just ("arn" .= _sjArn)])

instance ToPath StopJob where
        toPath = const "/"

instance ToQuery StopJob where
        toQuery = const mempty

-- | /See:/ 'stopJobResponse' smart constructor.
data StopJobResponse = StopJobResponse'
  { _sjrsJob            :: !(Maybe Job)
  , _sjrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StopJobResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sjrsJob' - The job that was stopped.
--
-- * 'sjrsResponseStatus' - -- | The response status code.
stopJobResponse
    :: Int -- ^ 'sjrsResponseStatus'
    -> StopJobResponse
stopJobResponse pResponseStatus_ =
  StopJobResponse' {_sjrsJob = Nothing, _sjrsResponseStatus = pResponseStatus_}


-- | The job that was stopped.
sjrsJob :: Lens' StopJobResponse (Maybe Job)
sjrsJob = lens _sjrsJob (\ s a -> s{_sjrsJob = a})

-- | -- | The response status code.
sjrsResponseStatus :: Lens' StopJobResponse Int
sjrsResponseStatus = lens _sjrsResponseStatus (\ s a -> s{_sjrsResponseStatus = a})

instance NFData StopJobResponse where
