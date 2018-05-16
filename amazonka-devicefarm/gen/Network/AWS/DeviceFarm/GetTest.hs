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
-- Module      : Network.AWS.DeviceFarm.GetTest
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about a test.
--
--
module Network.AWS.DeviceFarm.GetTest
    (
    -- * Creating a Request
      getTest
    , GetTest
    -- * Request Lenses
    , gtArn

    -- * Destructuring the Response
    , getTestResponse
    , GetTestResponse
    -- * Response Lenses
    , gtrsTest
    , gtrsResponseStatus
    ) where

import Network.AWS.DeviceFarm.Types
import Network.AWS.DeviceFarm.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents a request to the get test operation.
--
--
--
-- /See:/ 'getTest' smart constructor.
newtype GetTest = GetTest'
  { _gtArn :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetTest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gtArn' - The test's ARN.
getTest
    :: Text -- ^ 'gtArn'
    -> GetTest
getTest pArn_ = GetTest' {_gtArn = pArn_}


-- | The test's ARN.
gtArn :: Lens' GetTest Text
gtArn = lens _gtArn (\ s a -> s{_gtArn = a})

instance AWSRequest GetTest where
        type Rs GetTest = GetTestResponse
        request = postJSON deviceFarm
        response
          = receiveJSON
              (\ s h x ->
                 GetTestResponse' <$>
                   (x .?> "test") <*> (pure (fromEnum s)))

instance Hashable GetTest where

instance NFData GetTest where

instance ToHeaders GetTest where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("DeviceFarm_20150623.GetTest" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetTest where
        toJSON GetTest'{..}
          = object (catMaybes [Just ("arn" .= _gtArn)])

instance ToPath GetTest where
        toPath = const "/"

instance ToQuery GetTest where
        toQuery = const mempty

-- | Represents the result of a get test request.
--
--
--
-- /See:/ 'getTestResponse' smart constructor.
data GetTestResponse = GetTestResponse'
  { _gtrsTest           :: !(Maybe Test)
  , _gtrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetTestResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gtrsTest' - A test condition that is evaluated.
--
-- * 'gtrsResponseStatus' - -- | The response status code.
getTestResponse
    :: Int -- ^ 'gtrsResponseStatus'
    -> GetTestResponse
getTestResponse pResponseStatus_ =
  GetTestResponse' {_gtrsTest = Nothing, _gtrsResponseStatus = pResponseStatus_}


-- | A test condition that is evaluated.
gtrsTest :: Lens' GetTestResponse (Maybe Test)
gtrsTest = lens _gtrsTest (\ s a -> s{_gtrsTest = a})

-- | -- | The response status code.
gtrsResponseStatus :: Lens' GetTestResponse Int
gtrsResponseStatus = lens _gtrsResponseStatus (\ s a -> s{_gtrsResponseStatus = a})

instance NFData GetTestResponse where
