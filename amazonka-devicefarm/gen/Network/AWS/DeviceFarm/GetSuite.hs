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
-- Module      : Network.AWS.DeviceFarm.GetSuite
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about a suite.
--
--
module Network.AWS.DeviceFarm.GetSuite
    (
    -- * Creating a Request
      getSuite
    , GetSuite
    -- * Request Lenses
    , gsArn

    -- * Destructuring the Response
    , getSuiteResponse
    , GetSuiteResponse
    -- * Response Lenses
    , gsrsSuite
    , gsrsResponseStatus
    ) where

import Network.AWS.DeviceFarm.Types
import Network.AWS.DeviceFarm.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents a request to the get suite operation.
--
--
--
-- /See:/ 'getSuite' smart constructor.
newtype GetSuite = GetSuite'
  { _gsArn :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetSuite' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gsArn' - The suite's ARN.
getSuite
    :: Text -- ^ 'gsArn'
    -> GetSuite
getSuite pArn_ = GetSuite' {_gsArn = pArn_}


-- | The suite's ARN.
gsArn :: Lens' GetSuite Text
gsArn = lens _gsArn (\ s a -> s{_gsArn = a})

instance AWSRequest GetSuite where
        type Rs GetSuite = GetSuiteResponse
        request = postJSON deviceFarm
        response
          = receiveJSON
              (\ s h x ->
                 GetSuiteResponse' <$>
                   (x .?> "suite") <*> (pure (fromEnum s)))

instance Hashable GetSuite where

instance NFData GetSuite where

instance ToHeaders GetSuite where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("DeviceFarm_20150623.GetSuite" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetSuite where
        toJSON GetSuite'{..}
          = object (catMaybes [Just ("arn" .= _gsArn)])

instance ToPath GetSuite where
        toPath = const "/"

instance ToQuery GetSuite where
        toQuery = const mempty

-- | Represents the result of a get suite request.
--
--
--
-- /See:/ 'getSuiteResponse' smart constructor.
data GetSuiteResponse = GetSuiteResponse'
  { _gsrsSuite          :: !(Maybe Suite)
  , _gsrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetSuiteResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gsrsSuite' - A collection of one or more tests.
--
-- * 'gsrsResponseStatus' - -- | The response status code.
getSuiteResponse
    :: Int -- ^ 'gsrsResponseStatus'
    -> GetSuiteResponse
getSuiteResponse pResponseStatus_ =
  GetSuiteResponse'
    {_gsrsSuite = Nothing, _gsrsResponseStatus = pResponseStatus_}


-- | A collection of one or more tests.
gsrsSuite :: Lens' GetSuiteResponse (Maybe Suite)
gsrsSuite = lens _gsrsSuite (\ s a -> s{_gsrsSuite = a})

-- | -- | The response status code.
gsrsResponseStatus :: Lens' GetSuiteResponse Int
gsrsResponseStatus = lens _gsrsResponseStatus (\ s a -> s{_gsrsResponseStatus = a})

instance NFData GetSuiteResponse where
