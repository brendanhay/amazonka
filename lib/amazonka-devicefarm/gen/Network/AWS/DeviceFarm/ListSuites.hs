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
-- Module      : Network.AWS.DeviceFarm.ListSuites
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about test suites for a given job.
--
--
--
-- This operation returns paginated results.
module Network.AWS.DeviceFarm.ListSuites
    (
    -- * Creating a Request
      listSuites
    , ListSuites
    -- * Request Lenses
    , lNextToken
    , lArn

    -- * Destructuring the Response
    , listSuitesResponse
    , ListSuitesResponse
    -- * Response Lenses
    , lsrsNextToken
    , lsrsSuites
    , lsrsResponseStatus
    ) where

import Network.AWS.DeviceFarm.Types
import Network.AWS.DeviceFarm.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents a request to the list suites operation.
--
--
--
-- /See:/ 'listSuites' smart constructor.
data ListSuites = ListSuites'
  { _lNextToken :: !(Maybe Text)
  , _lArn       :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListSuites' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lNextToken' - An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
--
-- * 'lArn' - The job's Amazon Resource Name (ARN).
listSuites
    :: Text -- ^ 'lArn'
    -> ListSuites
listSuites pArn_ = ListSuites' {_lNextToken = Nothing, _lArn = pArn_}


-- | An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
lNextToken :: Lens' ListSuites (Maybe Text)
lNextToken = lens _lNextToken (\ s a -> s{_lNextToken = a})

-- | The job's Amazon Resource Name (ARN).
lArn :: Lens' ListSuites Text
lArn = lens _lArn (\ s a -> s{_lArn = a})

instance AWSPager ListSuites where
        page rq rs
          | stop (rs ^. lsrsNextToken) = Nothing
          | stop (rs ^. lsrsSuites) = Nothing
          | otherwise =
            Just $ rq & lNextToken .~ rs ^. lsrsNextToken

instance AWSRequest ListSuites where
        type Rs ListSuites = ListSuitesResponse
        request = postJSON deviceFarm
        response
          = receiveJSON
              (\ s h x ->
                 ListSuitesResponse' <$>
                   (x .?> "nextToken") <*> (x .?> "suites" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable ListSuites where

instance NFData ListSuites where

instance ToHeaders ListSuites where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("DeviceFarm_20150623.ListSuites" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListSuites where
        toJSON ListSuites'{..}
          = object
              (catMaybes
                 [("nextToken" .=) <$> _lNextToken,
                  Just ("arn" .= _lArn)])

instance ToPath ListSuites where
        toPath = const "/"

instance ToQuery ListSuites where
        toQuery = const mempty

-- | Represents the result of a list suites request.
--
--
--
-- /See:/ 'listSuitesResponse' smart constructor.
data ListSuitesResponse = ListSuitesResponse'
  { _lsrsNextToken      :: !(Maybe Text)
  , _lsrsSuites         :: !(Maybe [Suite])
  , _lsrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListSuitesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lsrsNextToken' - If the number of items that are returned is significantly large, this is an identifier that is also returned, which can be used in a subsequent call to this operation to return the next set of items in the list.
--
-- * 'lsrsSuites' - Information about the suites.
--
-- * 'lsrsResponseStatus' - -- | The response status code.
listSuitesResponse
    :: Int -- ^ 'lsrsResponseStatus'
    -> ListSuitesResponse
listSuitesResponse pResponseStatus_ =
  ListSuitesResponse'
    { _lsrsNextToken = Nothing
    , _lsrsSuites = Nothing
    , _lsrsResponseStatus = pResponseStatus_
    }


-- | If the number of items that are returned is significantly large, this is an identifier that is also returned, which can be used in a subsequent call to this operation to return the next set of items in the list.
lsrsNextToken :: Lens' ListSuitesResponse (Maybe Text)
lsrsNextToken = lens _lsrsNextToken (\ s a -> s{_lsrsNextToken = a})

-- | Information about the suites.
lsrsSuites :: Lens' ListSuitesResponse [Suite]
lsrsSuites = lens _lsrsSuites (\ s a -> s{_lsrsSuites = a}) . _Default . _Coerce

-- | -- | The response status code.
lsrsResponseStatus :: Lens' ListSuitesResponse Int
lsrsResponseStatus = lens _lsrsResponseStatus (\ s a -> s{_lsrsResponseStatus = a})

instance NFData ListSuitesResponse where
