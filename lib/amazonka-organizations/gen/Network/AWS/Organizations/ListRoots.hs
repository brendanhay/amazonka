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
-- Module      : Network.AWS.Organizations.ListRoots
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the roots that are defined in the current organization.
--
--
-- This operation can be called only from the organization's master account.
--
--
-- This operation returns paginated results.
module Network.AWS.Organizations.ListRoots
    (
    -- * Creating a Request
      listRoots
    , ListRoots
    -- * Request Lenses
    , lrNextToken
    , lrMaxResults

    -- * Destructuring the Response
    , listRootsResponse
    , ListRootsResponse
    -- * Response Lenses
    , lrrsRoots
    , lrrsNextToken
    , lrrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Organizations.Types
import Network.AWS.Organizations.Types.Product
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listRoots' smart constructor.
data ListRoots = ListRoots'
  { _lrNextToken  :: !(Maybe Text)
  , _lrMaxResults :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListRoots' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lrNextToken' - Use this parameter if you receive a @NextToken@ response in a previous request that indicates that there is more output available. Set it to the value of the previous call's @NextToken@ response to indicate where the output should continue from.
--
-- * 'lrMaxResults' - (Optional) Use this to limit the number of results you want included in the response. If you do not include this parameter, it defaults to a value that is specific to the operation. If additional items exist beyond the maximum you specify, the @NextToken@ response element is present and has a value (is not null). Include that value as the @NextToken@ request parameter in the next call to the operation to get the next part of the results. Note that Organizations might return fewer results than the maximum even when there are more results available. You should check @NextToken@ after every operation to ensure that you receive all of the results.
listRoots
    :: ListRoots
listRoots = ListRoots' {_lrNextToken = Nothing, _lrMaxResults = Nothing}


-- | Use this parameter if you receive a @NextToken@ response in a previous request that indicates that there is more output available. Set it to the value of the previous call's @NextToken@ response to indicate where the output should continue from.
lrNextToken :: Lens' ListRoots (Maybe Text)
lrNextToken = lens _lrNextToken (\ s a -> s{_lrNextToken = a})

-- | (Optional) Use this to limit the number of results you want included in the response. If you do not include this parameter, it defaults to a value that is specific to the operation. If additional items exist beyond the maximum you specify, the @NextToken@ response element is present and has a value (is not null). Include that value as the @NextToken@ request parameter in the next call to the operation to get the next part of the results. Note that Organizations might return fewer results than the maximum even when there are more results available. You should check @NextToken@ after every operation to ensure that you receive all of the results.
lrMaxResults :: Lens' ListRoots (Maybe Natural)
lrMaxResults = lens _lrMaxResults (\ s a -> s{_lrMaxResults = a}) . mapping _Nat

instance AWSPager ListRoots where
        page rq rs
          | stop (rs ^. lrrsNextToken) = Nothing
          | stop (rs ^. lrrsRoots) = Nothing
          | otherwise =
            Just $ rq & lrNextToken .~ rs ^. lrrsNextToken

instance AWSRequest ListRoots where
        type Rs ListRoots = ListRootsResponse
        request = postJSON organizations
        response
          = receiveJSON
              (\ s h x ->
                 ListRootsResponse' <$>
                   (x .?> "Roots" .!@ mempty) <*> (x .?> "NextToken")
                     <*> (pure (fromEnum s)))

instance Hashable ListRoots where

instance NFData ListRoots where

instance ToHeaders ListRoots where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSOrganizationsV20161128.ListRoots" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListRoots where
        toJSON ListRoots'{..}
          = object
              (catMaybes
                 [("NextToken" .=) <$> _lrNextToken,
                  ("MaxResults" .=) <$> _lrMaxResults])

instance ToPath ListRoots where
        toPath = const "/"

instance ToQuery ListRoots where
        toQuery = const mempty

-- | /See:/ 'listRootsResponse' smart constructor.
data ListRootsResponse = ListRootsResponse'
  { _lrrsRoots          :: !(Maybe [Root])
  , _lrrsNextToken      :: !(Maybe Text)
  , _lrrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListRootsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lrrsRoots' - A list of roots that are defined in an organization.
--
-- * 'lrrsNextToken' - If present, this value indicates that there is more output available than is included in the current response. Use this value in the @NextToken@ request parameter in a subsequent call to the operation to get the next part of the output. You should repeat this until the @NextToken@ response element comes back as @null@ .
--
-- * 'lrrsResponseStatus' - -- | The response status code.
listRootsResponse
    :: Int -- ^ 'lrrsResponseStatus'
    -> ListRootsResponse
listRootsResponse pResponseStatus_ =
  ListRootsResponse'
    { _lrrsRoots = Nothing
    , _lrrsNextToken = Nothing
    , _lrrsResponseStatus = pResponseStatus_
    }


-- | A list of roots that are defined in an organization.
lrrsRoots :: Lens' ListRootsResponse [Root]
lrrsRoots = lens _lrrsRoots (\ s a -> s{_lrrsRoots = a}) . _Default . _Coerce

-- | If present, this value indicates that there is more output available than is included in the current response. Use this value in the @NextToken@ request parameter in a subsequent call to the operation to get the next part of the output. You should repeat this until the @NextToken@ response element comes back as @null@ .
lrrsNextToken :: Lens' ListRootsResponse (Maybe Text)
lrrsNextToken = lens _lrrsNextToken (\ s a -> s{_lrrsNextToken = a})

-- | -- | The response status code.
lrrsResponseStatus :: Lens' ListRootsResponse Int
lrrsResponseStatus = lens _lrrsResponseStatus (\ s a -> s{_lrrsResponseStatus = a})

instance NFData ListRootsResponse where
