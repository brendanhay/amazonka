{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.StopJob
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Initiates a stop request for the current job. AWS Device Farm immediately stops the job on the device where tests have not started. You are not billed for this device. On the device where tests have started, setup suite and teardown suite tests run to completion on the device. You are billed for setup, teardown, and any tests that were in progress or already completed.
module Network.AWS.DeviceFarm.StopJob
  ( -- * Creating a Request
    stopJob,
    StopJob,

    -- * Request Lenses
    sjArn,

    -- * Destructuring the Response
    stopJobResponse,
    StopJobResponse,

    -- * Response Lenses
    sjrsJob,
    sjrsResponseStatus,
  )
where

import Network.AWS.DeviceFarm.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'stopJob' smart constructor.
newtype StopJob = StopJob' {_sjArn :: Text}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'StopJob' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sjArn' - Represents the Amazon Resource Name (ARN) of the Device Farm job to stop.
stopJob ::
  -- | 'sjArn'
  Text ->
  StopJob
stopJob pArn_ = StopJob' {_sjArn = pArn_}

-- | Represents the Amazon Resource Name (ARN) of the Device Farm job to stop.
sjArn :: Lens' StopJob Text
sjArn = lens _sjArn (\s a -> s {_sjArn = a})

instance AWSRequest StopJob where
  type Rs StopJob = StopJobResponse
  request = postJSON deviceFarm
  response =
    receiveJSON
      ( \s h x ->
          StopJobResponse' <$> (x .?> "job") <*> (pure (fromEnum s))
      )

instance Hashable StopJob

instance NFData StopJob

instance ToHeaders StopJob where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target" =# ("DeviceFarm_20150623.StopJob" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON StopJob where
  toJSON StopJob' {..} = object (catMaybes [Just ("arn" .= _sjArn)])

instance ToPath StopJob where
  toPath = const "/"

instance ToQuery StopJob where
  toQuery = const mempty

-- | /See:/ 'stopJobResponse' smart constructor.
data StopJobResponse = StopJobResponse'
  { _sjrsJob :: !(Maybe Job),
    _sjrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'StopJobResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sjrsJob' - The job that was stopped.
--
-- * 'sjrsResponseStatus' - -- | The response status code.
stopJobResponse ::
  -- | 'sjrsResponseStatus'
  Int ->
  StopJobResponse
stopJobResponse pResponseStatus_ =
  StopJobResponse'
    { _sjrsJob = Nothing,
      _sjrsResponseStatus = pResponseStatus_
    }

-- | The job that was stopped.
sjrsJob :: Lens' StopJobResponse (Maybe Job)
sjrsJob = lens _sjrsJob (\s a -> s {_sjrsJob = a})

-- | -- | The response status code.
sjrsResponseStatus :: Lens' StopJobResponse Int
sjrsResponseStatus = lens _sjrsResponseStatus (\s a -> s {_sjrsResponseStatus = a})

instance NFData StopJobResponse
