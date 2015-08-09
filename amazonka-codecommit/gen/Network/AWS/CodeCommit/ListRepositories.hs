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
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about one or more repositories.
--
-- /See:/ <http://docs.aws.amazon.com/codecommit/latest/APIReference/API_ListRepositories.html AWS API Reference> for ListRepositories.
module Network.AWS.CodeCommit.ListRepositories
    (
    -- * Creating a Request
      ListRepositories
    , listRepositories
    -- * Request Lenses
    , lrNextToken
    , lrOrder
    , lrSortBy

    -- * Destructuring the Response
    , ListRepositoriesResponse
    , listRepositoriesResponse
    -- * Response Lenses
    , lrrsNextToken
    , lrrsRepositories
    , lrrsStatus
    ) where

import           Network.AWS.CodeCommit.Types
import           Network.AWS.CodeCommit.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Represents the input of a list repositories operation.
--
-- /See:/ 'listRepositories' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lrNextToken'
--
-- * 'lrOrder'
--
-- * 'lrSortBy'
data ListRepositories = ListRepositories'
    { _lrNextToken :: !(Maybe Text)
    , _lrOrder     :: !(Maybe OrderEnum)
    , _lrSortBy    :: !(Maybe SortByEnum)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ListRepositories' smart constructor.
listRepositories :: ListRepositories
listRepositories =
    ListRepositories'
    { _lrNextToken = Nothing
    , _lrOrder = Nothing
    , _lrSortBy = Nothing
    }

-- | An enumeration token that allows the operation to batch the results of
-- the operation. Batch sizes are 1,000 for list repository operations.
-- When the client sends the token back to AWS CodeCommit, another page of
-- 1,000 records is retrieved.
lrNextToken :: Lens' ListRepositories (Maybe Text)
lrNextToken = lens _lrNextToken (\ s a -> s{_lrNextToken = a});

-- | The order in which to sort the results of a list repositories operation.
lrOrder :: Lens' ListRepositories (Maybe OrderEnum)
lrOrder = lens _lrOrder (\ s a -> s{_lrOrder = a});

-- | The criteria used to sort the results of a list repositories operation.
lrSortBy :: Lens' ListRepositories (Maybe SortByEnum)
lrSortBy = lens _lrSortBy (\ s a -> s{_lrSortBy = a});

instance AWSRequest ListRepositories where
        type Sv ListRepositories = CodeCommit
        type Rs ListRepositories = ListRepositoriesResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 ListRepositoriesResponse' <$>
                   (x .?> "nextToken") <*>
                     (x .?> "repositories" .!@ mempty)
                     <*> (pure (fromEnum s)))

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
              ["nextToken" .= _lrNextToken, "order" .= _lrOrder,
               "sortBy" .= _lrSortBy]

instance ToPath ListRepositories where
        toPath = const "/"

instance ToQuery ListRepositories where
        toQuery = const mempty

-- | Represents the output of a list repositories operation.
--
-- /See:/ 'listRepositoriesResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lrrsNextToken'
--
-- * 'lrrsRepositories'
--
-- * 'lrrsStatus'
data ListRepositoriesResponse = ListRepositoriesResponse'
    { _lrrsNextToken    :: !(Maybe Text)
    , _lrrsRepositories :: !(Maybe [RepositoryNameIdPair])
    , _lrrsStatus       :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ListRepositoriesResponse' smart constructor.
listRepositoriesResponse :: Int -> ListRepositoriesResponse
listRepositoriesResponse pStatus_ =
    ListRepositoriesResponse'
    { _lrrsNextToken = Nothing
    , _lrrsRepositories = Nothing
    , _lrrsStatus = pStatus_
    }

-- | An enumeration token that allows the operation to batch the results of
-- the operation. Batch sizes are 1,000 for list repository operations.
-- When the client sends the token back to AWS CodeCommit, another page of
-- 1,000 records is retrieved.
lrrsNextToken :: Lens' ListRepositoriesResponse (Maybe Text)
lrrsNextToken = lens _lrrsNextToken (\ s a -> s{_lrrsNextToken = a});

-- | Lists the repositories called by the list repositories operation.
lrrsRepositories :: Lens' ListRepositoriesResponse [RepositoryNameIdPair]
lrrsRepositories = lens _lrrsRepositories (\ s a -> s{_lrrsRepositories = a}) . _Default . _Coerce;

-- | Undocumented member.
lrrsStatus :: Lens' ListRepositoriesResponse Int
lrrsStatus = lens _lrrsStatus (\ s a -> s{_lrrsStatus = a});
