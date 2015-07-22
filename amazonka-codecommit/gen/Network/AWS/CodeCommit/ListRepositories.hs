{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.ListRepositories
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Gets information about one or more repositories.
--
-- <http://docs.aws.amazon.com/codecommit/latest/APIReference/API_ListRepositories.html>
module Network.AWS.CodeCommit.ListRepositories
    (
    -- * Request
      ListRepositories
    -- ** Request constructor
    , listRepositories
    -- ** Request lenses
    , lrrqNextToken
    , lrrqOrder
    , lrrqSortBy

    -- * Response
    , ListRepositoriesResponse
    -- ** Response constructor
    , listRepositoriesResponse
    -- ** Response lenses
    , lrrsNextToken
    , lrrsRepositories
    , lrrsStatus
    ) where

import           Network.AWS.CodeCommit.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Represents the input of a list repositories operation.
--
-- /See:/ 'listRepositories' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lrrqNextToken'
--
-- * 'lrrqOrder'
--
-- * 'lrrqSortBy'
data ListRepositories = ListRepositories'
    { _lrrqNextToken :: !(Maybe Text)
    , _lrrqOrder     :: !(Maybe OrderEnum)
    , _lrrqSortBy    :: !(Maybe SortByEnum)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ListRepositories' smart constructor.
listRepositories :: ListRepositories
listRepositories =
    ListRepositories'
    { _lrrqNextToken = Nothing
    , _lrrqOrder = Nothing
    , _lrrqSortBy = Nothing
    }

-- | An enumeration token that allows the operation to batch the results of
-- the operation. Batch sizes are 1,000 for list repository operations.
-- When the client sends the token back to AWS CodeCommit, another page of
-- 1,000 records is retrieved.
lrrqNextToken :: Lens' ListRepositories (Maybe Text)
lrrqNextToken = lens _lrrqNextToken (\ s a -> s{_lrrqNextToken = a});

-- | The order in which to sort the results of a list repositories operation.
lrrqOrder :: Lens' ListRepositories (Maybe OrderEnum)
lrrqOrder = lens _lrrqOrder (\ s a -> s{_lrrqOrder = a});

-- | The criteria used to sort the results of a list repositories operation.
lrrqSortBy :: Lens' ListRepositories (Maybe SortByEnum)
lrrqSortBy = lens _lrrqSortBy (\ s a -> s{_lrrqSortBy = a});

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
              ["nextToken" .= _lrrqNextToken,
               "order" .= _lrrqOrder, "sortBy" .= _lrrqSortBy]

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
listRepositoriesResponse pStatus =
    ListRepositoriesResponse'
    { _lrrsNextToken = Nothing
    , _lrrsRepositories = Nothing
    , _lrrsStatus = pStatus
    }

-- | An enumeration token that allows the operation to batch the results of
-- the operation. Batch sizes are 1,000 for list repository operations.
-- When the client sends the token back to AWS CodeCommit, another page of
-- 1,000 records is retrieved.
lrrsNextToken :: Lens' ListRepositoriesResponse (Maybe Text)
lrrsNextToken = lens _lrrsNextToken (\ s a -> s{_lrrsNextToken = a});

-- | Lists the repositories called by the list repositories operation.
lrrsRepositories :: Lens' ListRepositoriesResponse [RepositoryNameIdPair]
lrrsRepositories = lens _lrrsRepositories (\ s a -> s{_lrrsRepositories = a}) . _Default;

-- | FIXME: Undocumented member.
lrrsStatus :: Lens' ListRepositoriesResponse Int
lrrsStatus = lens _lrrsStatus (\ s a -> s{_lrrsStatus = a});
