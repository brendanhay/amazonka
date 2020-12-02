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
-- Module      : Network.AWS.DeviceFarm.GetRun
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about a run.
--
--
module Network.AWS.DeviceFarm.GetRun
    (
    -- * Creating a Request
      getRun
    , GetRun
    -- * Request Lenses
    , grArn

    -- * Destructuring the Response
    , getRunResponse
    , GetRunResponse
    -- * Response Lenses
    , grrsRun
    , grrsResponseStatus
    ) where

import Network.AWS.DeviceFarm.Types
import Network.AWS.DeviceFarm.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents a request to the get run operation.
--
--
--
-- /See:/ 'getRun' smart constructor.
newtype GetRun = GetRun'
  { _grArn :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetRun' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'grArn' - The run's ARN.
getRun
    :: Text -- ^ 'grArn'
    -> GetRun
getRun pArn_ = GetRun' {_grArn = pArn_}


-- | The run's ARN.
grArn :: Lens' GetRun Text
grArn = lens _grArn (\ s a -> s{_grArn = a})

instance AWSRequest GetRun where
        type Rs GetRun = GetRunResponse
        request = postJSON deviceFarm
        response
          = receiveJSON
              (\ s h x ->
                 GetRunResponse' <$>
                   (x .?> "run") <*> (pure (fromEnum s)))

instance Hashable GetRun where

instance NFData GetRun where

instance ToHeaders GetRun where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("DeviceFarm_20150623.GetRun" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetRun where
        toJSON GetRun'{..}
          = object (catMaybes [Just ("arn" .= _grArn)])

instance ToPath GetRun where
        toPath = const "/"

instance ToQuery GetRun where
        toQuery = const mempty

-- | Represents the result of a get run request.
--
--
--
-- /See:/ 'getRunResponse' smart constructor.
data GetRunResponse = GetRunResponse'
  { _grrsRun            :: !(Maybe Run)
  , _grrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetRunResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'grrsRun' - The run you wish to get results from.
--
-- * 'grrsResponseStatus' - -- | The response status code.
getRunResponse
    :: Int -- ^ 'grrsResponseStatus'
    -> GetRunResponse
getRunResponse pResponseStatus_ =
  GetRunResponse' {_grrsRun = Nothing, _grrsResponseStatus = pResponseStatus_}


-- | The run you wish to get results from.
grrsRun :: Lens' GetRunResponse (Maybe Run)
grrsRun = lens _grrsRun (\ s a -> s{_grrsRun = a})

-- | -- | The response status code.
grrsResponseStatus :: Lens' GetRunResponse Int
grrsResponseStatus = lens _grrsResponseStatus (\ s a -> s{_grrsResponseStatus = a})

instance NFData GetRunResponse where
