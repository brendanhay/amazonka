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
-- Module      : Network.AWS.DeviceFarm.StopRun
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Initiates a stop request for the current test run. AWS Device Farm will immediately stop the run on devices where tests have not started executing, and you will not be billed for these devices. On devices where tests have started executing, Setup Suite and Teardown Suite tests will run to completion before stopping execution on those devices. You will be billed for Setup, Teardown, and any tests that were in progress or already completed.
--
--
module Network.AWS.DeviceFarm.StopRun
    (
    -- * Creating a Request
      stopRun
    , StopRun
    -- * Request Lenses
    , srArn

    -- * Destructuring the Response
    , stopRunResponse
    , StopRunResponse
    -- * Response Lenses
    , srsRun
    , srsResponseStatus
    ) where

import Network.AWS.DeviceFarm.Types
import Network.AWS.DeviceFarm.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents the request to stop a specific run.
--
--
--
-- /See:/ 'stopRun' smart constructor.
newtype StopRun = StopRun'
  { _srArn :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StopRun' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'srArn' - Represents the Amazon Resource Name (ARN) of the Device Farm run you wish to stop.
stopRun
    :: Text -- ^ 'srArn'
    -> StopRun
stopRun pArn_ = StopRun' {_srArn = pArn_}


-- | Represents the Amazon Resource Name (ARN) of the Device Farm run you wish to stop.
srArn :: Lens' StopRun Text
srArn = lens _srArn (\ s a -> s{_srArn = a})

instance AWSRequest StopRun where
        type Rs StopRun = StopRunResponse
        request = postJSON deviceFarm
        response
          = receiveJSON
              (\ s h x ->
                 StopRunResponse' <$>
                   (x .?> "run") <*> (pure (fromEnum s)))

instance Hashable StopRun where

instance NFData StopRun where

instance ToHeaders StopRun where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("DeviceFarm_20150623.StopRun" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON StopRun where
        toJSON StopRun'{..}
          = object (catMaybes [Just ("arn" .= _srArn)])

instance ToPath StopRun where
        toPath = const "/"

instance ToQuery StopRun where
        toQuery = const mempty

-- | Represents the results of your stop run attempt.
--
--
--
-- /See:/ 'stopRunResponse' smart constructor.
data StopRunResponse = StopRunResponse'
  { _srsRun            :: !(Maybe Run)
  , _srsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StopRunResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'srsRun' - The run that was stopped.
--
-- * 'srsResponseStatus' - -- | The response status code.
stopRunResponse
    :: Int -- ^ 'srsResponseStatus'
    -> StopRunResponse
stopRunResponse pResponseStatus_ =
  StopRunResponse' {_srsRun = Nothing, _srsResponseStatus = pResponseStatus_}


-- | The run that was stopped.
srsRun :: Lens' StopRunResponse (Maybe Run)
srsRun = lens _srsRun (\ s a -> s{_srsRun = a})

-- | -- | The response status code.
srsResponseStatus :: Lens' StopRunResponse Int
srsResponseStatus = lens _srsResponseStatus (\ s a -> s{_srsResponseStatus = a})

instance NFData StopRunResponse where
