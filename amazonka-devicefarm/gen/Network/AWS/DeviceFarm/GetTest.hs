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
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about a test.
--
-- /See:/ <http://docs.aws.amazon.com/devicefarm/latest/APIReference/API_GetTest.html AWS API Reference> for GetTest.
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
    , gtrsStatus
    ) where

import           Network.AWS.DeviceFarm.Types
import           Network.AWS.DeviceFarm.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Represents a request to the get test operation.
--
-- /See:/ 'getTest' smart constructor.
newtype GetTest = GetTest'
    { _gtArn :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'GetTest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gtArn'
getTest
    :: Text -- ^ 'gtArn'
    -> GetTest
getTest pArn_ =
    GetTest'
    { _gtArn = pArn_
    }

-- | The test\'s ARN.
gtArn :: Lens' GetTest Text
gtArn = lens _gtArn (\ s a -> s{_gtArn = a});

instance AWSRequest GetTest where
        type Rs GetTest = GetTestResponse
        request = postJSON deviceFarm
        response
          = receiveJSON
              (\ s h x ->
                 GetTestResponse' <$>
                   (x .?> "test") <*> (pure (fromEnum s)))

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
-- /See:/ 'getTestResponse' smart constructor.
data GetTestResponse = GetTestResponse'
    { _gtrsTest   :: !(Maybe Test)
    , _gtrsStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'GetTestResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gtrsTest'
--
-- * 'gtrsStatus'
getTestResponse
    :: Int -- ^ 'gtrsStatus'
    -> GetTestResponse
getTestResponse pStatus_ =
    GetTestResponse'
    { _gtrsTest = Nothing
    , _gtrsStatus = pStatus_
    }

-- | Undocumented member.
gtrsTest :: Lens' GetTestResponse (Maybe Test)
gtrsTest = lens _gtrsTest (\ s a -> s{_gtrsTest = a});

-- | The response status code.
gtrsStatus :: Lens' GetTestResponse Int
gtrsStatus = lens _gtrsStatus (\ s a -> s{_gtrsStatus = a});
