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
-- Module      : Network.AWS.DeviceFarm.ListUniqueProblems
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about unique problems.
--
--
--
-- This operation returns paginated results.
module Network.AWS.DeviceFarm.ListUniqueProblems
    (
    -- * Creating a Request
      listUniqueProblems
    , ListUniqueProblems
    -- * Request Lenses
    , lupNextToken
    , lupArn

    -- * Destructuring the Response
    , listUniqueProblemsResponse
    , ListUniqueProblemsResponse
    -- * Response Lenses
    , luprsNextToken
    , luprsUniqueProblems
    , luprsResponseStatus
    ) where

import Network.AWS.DeviceFarm.Types
import Network.AWS.DeviceFarm.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents a request to the list unique problems operation.
--
--
--
-- /See:/ 'listUniqueProblems' smart constructor.
data ListUniqueProblems = ListUniqueProblems'
  { _lupNextToken :: !(Maybe Text)
  , _lupArn       :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListUniqueProblems' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lupNextToken' - An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
--
-- * 'lupArn' - The unique problems' ARNs.
listUniqueProblems
    :: Text -- ^ 'lupArn'
    -> ListUniqueProblems
listUniqueProblems pArn_ =
  ListUniqueProblems' {_lupNextToken = Nothing, _lupArn = pArn_}


-- | An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
lupNextToken :: Lens' ListUniqueProblems (Maybe Text)
lupNextToken = lens _lupNextToken (\ s a -> s{_lupNextToken = a})

-- | The unique problems' ARNs.
lupArn :: Lens' ListUniqueProblems Text
lupArn = lens _lupArn (\ s a -> s{_lupArn = a})

instance AWSPager ListUniqueProblems where
        page rq rs
          | stop (rs ^. luprsNextToken) = Nothing
          | stop (rs ^. luprsUniqueProblems) = Nothing
          | otherwise =
            Just $ rq & lupNextToken .~ rs ^. luprsNextToken

instance AWSRequest ListUniqueProblems where
        type Rs ListUniqueProblems =
             ListUniqueProblemsResponse
        request = postJSON deviceFarm
        response
          = receiveJSON
              (\ s h x ->
                 ListUniqueProblemsResponse' <$>
                   (x .?> "nextToken") <*>
                     (x .?> "uniqueProblems" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable ListUniqueProblems where

instance NFData ListUniqueProblems where

instance ToHeaders ListUniqueProblems where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("DeviceFarm_20150623.ListUniqueProblems" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListUniqueProblems where
        toJSON ListUniqueProblems'{..}
          = object
              (catMaybes
                 [("nextToken" .=) <$> _lupNextToken,
                  Just ("arn" .= _lupArn)])

instance ToPath ListUniqueProblems where
        toPath = const "/"

instance ToQuery ListUniqueProblems where
        toQuery = const mempty

-- | Represents the result of a list unique problems request.
--
--
--
-- /See:/ 'listUniqueProblemsResponse' smart constructor.
data ListUniqueProblemsResponse = ListUniqueProblemsResponse'
  { _luprsNextToken      :: !(Maybe Text)
  , _luprsUniqueProblems :: !(Maybe (Map ExecutionResult [UniqueProblem]))
  , _luprsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListUniqueProblemsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'luprsNextToken' - If the number of items that are returned is significantly large, this is an identifier that is also returned, which can be used in a subsequent call to this operation to return the next set of items in the list.
--
-- * 'luprsUniqueProblems' - Information about the unique problems. Allowed values include:     * PENDING: A pending condition.     * PASSED: A passing condition.     * WARNED: A warning condition.     * FAILED: A failed condition.     * SKIPPED: A skipped condition.     * ERRORED: An error condition.     * STOPPED: A stopped condition.
--
-- * 'luprsResponseStatus' - -- | The response status code.
listUniqueProblemsResponse
    :: Int -- ^ 'luprsResponseStatus'
    -> ListUniqueProblemsResponse
listUniqueProblemsResponse pResponseStatus_ =
  ListUniqueProblemsResponse'
    { _luprsNextToken = Nothing
    , _luprsUniqueProblems = Nothing
    , _luprsResponseStatus = pResponseStatus_
    }


-- | If the number of items that are returned is significantly large, this is an identifier that is also returned, which can be used in a subsequent call to this operation to return the next set of items in the list.
luprsNextToken :: Lens' ListUniqueProblemsResponse (Maybe Text)
luprsNextToken = lens _luprsNextToken (\ s a -> s{_luprsNextToken = a})

-- | Information about the unique problems. Allowed values include:     * PENDING: A pending condition.     * PASSED: A passing condition.     * WARNED: A warning condition.     * FAILED: A failed condition.     * SKIPPED: A skipped condition.     * ERRORED: An error condition.     * STOPPED: A stopped condition.
luprsUniqueProblems :: Lens' ListUniqueProblemsResponse (HashMap ExecutionResult [UniqueProblem])
luprsUniqueProblems = lens _luprsUniqueProblems (\ s a -> s{_luprsUniqueProblems = a}) . _Default . _Map

-- | -- | The response status code.
luprsResponseStatus :: Lens' ListUniqueProblemsResponse Int
luprsResponseStatus = lens _luprsResponseStatus (\ s a -> s{_luprsResponseStatus = a})

instance NFData ListUniqueProblemsResponse where
