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
-- Module      : Network.AWS.CodeCommit.ListRepositories
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about one or more repositories.
--
--
--
-- This operation returns paginated results.
module Network.AWS.CodeCommit.ListRepositories
    (
    -- * Creating a Request
      listRepositories
    , ListRepositories
    -- * Request Lenses
    , lrNextToken
    , lrOrder
    , lrSortBy

    -- * Destructuring the Response
    , listRepositoriesResponse
    , ListRepositoriesResponse
    -- * Response Lenses
    , lrrsRepositories
    , lrrsNextToken
    , lrrsResponseStatus
    ) where

import Network.AWS.CodeCommit.Types
import Network.AWS.CodeCommit.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents the input of a list repositories operation.
--
--
--
-- /See:/ 'listRepositories' smart constructor.
data ListRepositories = ListRepositories'
  { _lrNextToken :: !(Maybe Text)
  , _lrOrder     :: !(Maybe OrderEnum)
  , _lrSortBy    :: !(Maybe SortByEnum)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListRepositories' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lrNextToken' - An enumeration token that allows the operation to batch the results of the operation. Batch sizes are 1,000 for list repository operations. When the client sends the token back to AWS CodeCommit, another page of 1,000 records is retrieved.
--
-- * 'lrOrder' - The order in which to sort the results of a list repositories operation.
--
-- * 'lrSortBy' - The criteria used to sort the results of a list repositories operation.
listRepositories
    :: ListRepositories
listRepositories =
  ListRepositories'
    {_lrNextToken = Nothing, _lrOrder = Nothing, _lrSortBy = Nothing}


-- | An enumeration token that allows the operation to batch the results of the operation. Batch sizes are 1,000 for list repository operations. When the client sends the token back to AWS CodeCommit, another page of 1,000 records is retrieved.
lrNextToken :: Lens' ListRepositories (Maybe Text)
lrNextToken = lens _lrNextToken (\ s a -> s{_lrNextToken = a})

-- | The order in which to sort the results of a list repositories operation.
lrOrder :: Lens' ListRepositories (Maybe OrderEnum)
lrOrder = lens _lrOrder (\ s a -> s{_lrOrder = a})

-- | The criteria used to sort the results of a list repositories operation.
lrSortBy :: Lens' ListRepositories (Maybe SortByEnum)
lrSortBy = lens _lrSortBy (\ s a -> s{_lrSortBy = a})

instance AWSPager ListRepositories where
        page rq rs
          | stop (rs ^. lrrsNextToken) = Nothing
          | stop (rs ^. lrrsRepositories) = Nothing
          | otherwise =
            Just $ rq & lrNextToken .~ rs ^. lrrsNextToken

instance AWSRequest ListRepositories where
        type Rs ListRepositories = ListRepositoriesResponse
        request = postJSON codeCommit
        response
          = receiveJSON
              (\ s h x ->
                 ListRepositoriesResponse' <$>
                   (x .?> "repositories" .!@ mempty) <*>
                     (x .?> "nextToken")
                     <*> (pure (fromEnum s)))

instance Hashable ListRepositories where

instance NFData ListRepositories where

instance ToHeaders ListRepositories where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("CodeCommit_20150413.ListRepositories" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListRepositories where
        toJSON ListRepositories'{..}
          = object
              (catMaybes
                 [("nextToken" .=) <$> _lrNextToken,
                  ("order" .=) <$> _lrOrder,
                  ("sortBy" .=) <$> _lrSortBy])

instance ToPath ListRepositories where
        toPath = const "/"

instance ToQuery ListRepositories where
        toQuery = const mempty

-- | Represents the output of a list repositories operation.
--
--
--
-- /See:/ 'listRepositoriesResponse' smart constructor.
data ListRepositoriesResponse = ListRepositoriesResponse'
  { _lrrsRepositories   :: !(Maybe [RepositoryNameIdPair])
  , _lrrsNextToken      :: !(Maybe Text)
  , _lrrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListRepositoriesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lrrsRepositories' - Lists the repositories called by the list repositories operation.
--
-- * 'lrrsNextToken' - An enumeration token that allows the operation to batch the results of the operation. Batch sizes are 1,000 for list repository operations. When the client sends the token back to AWS CodeCommit, another page of 1,000 records is retrieved.
--
-- * 'lrrsResponseStatus' - -- | The response status code.
listRepositoriesResponse
    :: Int -- ^ 'lrrsResponseStatus'
    -> ListRepositoriesResponse
listRepositoriesResponse pResponseStatus_ =
  ListRepositoriesResponse'
    { _lrrsRepositories = Nothing
    , _lrrsNextToken = Nothing
    , _lrrsResponseStatus = pResponseStatus_
    }


-- | Lists the repositories called by the list repositories operation.
lrrsRepositories :: Lens' ListRepositoriesResponse [RepositoryNameIdPair]
lrrsRepositories = lens _lrrsRepositories (\ s a -> s{_lrrsRepositories = a}) . _Default . _Coerce

-- | An enumeration token that allows the operation to batch the results of the operation. Batch sizes are 1,000 for list repository operations. When the client sends the token back to AWS CodeCommit, another page of 1,000 records is retrieved.
lrrsNextToken :: Lens' ListRepositoriesResponse (Maybe Text)
lrrsNextToken = lens _lrrsNextToken (\ s a -> s{_lrrsNextToken = a})

-- | -- | The response status code.
lrrsResponseStatus :: Lens' ListRepositoriesResponse Int
lrrsResponseStatus = lens _lrrsResponseStatus (\ s a -> s{_lrrsResponseStatus = a})

instance NFData ListRepositoriesResponse where
