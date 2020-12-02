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
-- Module      : Network.AWS.DeviceFarm.ListTests
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about tests in a given test suite.
--
--
--
-- This operation returns paginated results.
module Network.AWS.DeviceFarm.ListTests
    (
    -- * Creating a Request
      listTests
    , ListTests
    -- * Request Lenses
    , ltNextToken
    , ltArn

    -- * Destructuring the Response
    , listTestsResponse
    , ListTestsResponse
    -- * Response Lenses
    , ltrsTests
    , ltrsNextToken
    , ltrsResponseStatus
    ) where

import Network.AWS.DeviceFarm.Types
import Network.AWS.DeviceFarm.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents a request to the list tests operation.
--
--
--
-- /See:/ 'listTests' smart constructor.
data ListTests = ListTests'
  { _ltNextToken :: !(Maybe Text)
  , _ltArn       :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListTests' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ltNextToken' - An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
--
-- * 'ltArn' - The test suite's Amazon Resource Name (ARN).
listTests
    :: Text -- ^ 'ltArn'
    -> ListTests
listTests pArn_ = ListTests' {_ltNextToken = Nothing, _ltArn = pArn_}


-- | An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
ltNextToken :: Lens' ListTests (Maybe Text)
ltNextToken = lens _ltNextToken (\ s a -> s{_ltNextToken = a})

-- | The test suite's Amazon Resource Name (ARN).
ltArn :: Lens' ListTests Text
ltArn = lens _ltArn (\ s a -> s{_ltArn = a})

instance AWSPager ListTests where
        page rq rs
          | stop (rs ^. ltrsNextToken) = Nothing
          | stop (rs ^. ltrsTests) = Nothing
          | otherwise =
            Just $ rq & ltNextToken .~ rs ^. ltrsNextToken

instance AWSRequest ListTests where
        type Rs ListTests = ListTestsResponse
        request = postJSON deviceFarm
        response
          = receiveJSON
              (\ s h x ->
                 ListTestsResponse' <$>
                   (x .?> "tests" .!@ mempty) <*> (x .?> "nextToken")
                     <*> (pure (fromEnum s)))

instance Hashable ListTests where

instance NFData ListTests where

instance ToHeaders ListTests where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("DeviceFarm_20150623.ListTests" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListTests where
        toJSON ListTests'{..}
          = object
              (catMaybes
                 [("nextToken" .=) <$> _ltNextToken,
                  Just ("arn" .= _ltArn)])

instance ToPath ListTests where
        toPath = const "/"

instance ToQuery ListTests where
        toQuery = const mempty

-- | Represents the result of a list tests request.
--
--
--
-- /See:/ 'listTestsResponse' smart constructor.
data ListTestsResponse = ListTestsResponse'
  { _ltrsTests          :: !(Maybe [Test])
  , _ltrsNextToken      :: !(Maybe Text)
  , _ltrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListTestsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ltrsTests' - Information about the tests.
--
-- * 'ltrsNextToken' - If the number of items that are returned is significantly large, this is an identifier that is also returned, which can be used in a subsequent call to this operation to return the next set of items in the list.
--
-- * 'ltrsResponseStatus' - -- | The response status code.
listTestsResponse
    :: Int -- ^ 'ltrsResponseStatus'
    -> ListTestsResponse
listTestsResponse pResponseStatus_ =
  ListTestsResponse'
    { _ltrsTests = Nothing
    , _ltrsNextToken = Nothing
    , _ltrsResponseStatus = pResponseStatus_
    }


-- | Information about the tests.
ltrsTests :: Lens' ListTestsResponse [Test]
ltrsTests = lens _ltrsTests (\ s a -> s{_ltrsTests = a}) . _Default . _Coerce

-- | If the number of items that are returned is significantly large, this is an identifier that is also returned, which can be used in a subsequent call to this operation to return the next set of items in the list.
ltrsNextToken :: Lens' ListTestsResponse (Maybe Text)
ltrsNextToken = lens _ltrsNextToken (\ s a -> s{_ltrsNextToken = a})

-- | -- | The response status code.
ltrsResponseStatus :: Lens' ListTestsResponse Int
ltrsResponseStatus = lens _ltrsResponseStatus (\ s a -> s{_ltrsResponseStatus = a})

instance NFData ListTestsResponse where
