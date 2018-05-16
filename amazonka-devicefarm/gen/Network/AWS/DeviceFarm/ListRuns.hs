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
-- Module      : Network.AWS.DeviceFarm.ListRuns
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about runs, given an AWS Device Farm project ARN.
--
--
--
-- This operation returns paginated results.
module Network.AWS.DeviceFarm.ListRuns
    (
    -- * Creating a Request
      listRuns
    , ListRuns
    -- * Request Lenses
    , lrNextToken
    , lrArn

    -- * Destructuring the Response
    , listRunsResponse
    , ListRunsResponse
    -- * Response Lenses
    , lrrsRuns
    , lrrsNextToken
    , lrrsResponseStatus
    ) where

import Network.AWS.DeviceFarm.Types
import Network.AWS.DeviceFarm.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents a request to the list runs operation.
--
--
--
-- /See:/ 'listRuns' smart constructor.
data ListRuns = ListRuns'
  { _lrNextToken :: !(Maybe Text)
  , _lrArn       :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListRuns' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lrNextToken' - An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
--
-- * 'lrArn' - The Amazon Resource Name (ARN) of the project for which you want to list runs.
listRuns
    :: Text -- ^ 'lrArn'
    -> ListRuns
listRuns pArn_ = ListRuns' {_lrNextToken = Nothing, _lrArn = pArn_}


-- | An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
lrNextToken :: Lens' ListRuns (Maybe Text)
lrNextToken = lens _lrNextToken (\ s a -> s{_lrNextToken = a})

-- | The Amazon Resource Name (ARN) of the project for which you want to list runs.
lrArn :: Lens' ListRuns Text
lrArn = lens _lrArn (\ s a -> s{_lrArn = a})

instance AWSPager ListRuns where
        page rq rs
          | stop (rs ^. lrrsNextToken) = Nothing
          | stop (rs ^. lrrsRuns) = Nothing
          | otherwise =
            Just $ rq & lrNextToken .~ rs ^. lrrsNextToken

instance AWSRequest ListRuns where
        type Rs ListRuns = ListRunsResponse
        request = postJSON deviceFarm
        response
          = receiveJSON
              (\ s h x ->
                 ListRunsResponse' <$>
                   (x .?> "runs" .!@ mempty) <*> (x .?> "nextToken") <*>
                     (pure (fromEnum s)))

instance Hashable ListRuns where

instance NFData ListRuns where

instance ToHeaders ListRuns where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("DeviceFarm_20150623.ListRuns" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListRuns where
        toJSON ListRuns'{..}
          = object
              (catMaybes
                 [("nextToken" .=) <$> _lrNextToken,
                  Just ("arn" .= _lrArn)])

instance ToPath ListRuns where
        toPath = const "/"

instance ToQuery ListRuns where
        toQuery = const mempty

-- | Represents the result of a list runs request.
--
--
--
-- /See:/ 'listRunsResponse' smart constructor.
data ListRunsResponse = ListRunsResponse'
  { _lrrsRuns           :: !(Maybe [Run])
  , _lrrsNextToken      :: !(Maybe Text)
  , _lrrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListRunsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lrrsRuns' - Information about the runs.
--
-- * 'lrrsNextToken' - If the number of items that are returned is significantly large, this is an identifier that is also returned, which can be used in a subsequent call to this operation to return the next set of items in the list.
--
-- * 'lrrsResponseStatus' - -- | The response status code.
listRunsResponse
    :: Int -- ^ 'lrrsResponseStatus'
    -> ListRunsResponse
listRunsResponse pResponseStatus_ =
  ListRunsResponse'
    { _lrrsRuns = Nothing
    , _lrrsNextToken = Nothing
    , _lrrsResponseStatus = pResponseStatus_
    }


-- | Information about the runs.
lrrsRuns :: Lens' ListRunsResponse [Run]
lrrsRuns = lens _lrrsRuns (\ s a -> s{_lrrsRuns = a}) . _Default . _Coerce

-- | If the number of items that are returned is significantly large, this is an identifier that is also returned, which can be used in a subsequent call to this operation to return the next set of items in the list.
lrrsNextToken :: Lens' ListRunsResponse (Maybe Text)
lrrsNextToken = lens _lrrsNextToken (\ s a -> s{_lrrsNextToken = a})

-- | -- | The response status code.
lrrsResponseStatus :: Lens' ListRunsResponse Int
lrrsResponseStatus = lens _lrrsResponseStatus (\ s a -> s{_lrrsResponseStatus = a})

instance NFData ListRunsResponse where
