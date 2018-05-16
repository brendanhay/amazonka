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
-- Module      : Network.AWS.MigrationHub.ListMigrationTasks
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all, or filtered by resource name, migration tasks associated with the user account making this call. This API has the following traits:
--
--
--     * Can show a summary list of the most recent migration tasks.
--
--     * Can show a summary list of migration tasks associated with a given discovered resource.
--
--     * Lists migration tasks in a paginated interface.
--
--
--
module Network.AWS.MigrationHub.ListMigrationTasks
    (
    -- * Creating a Request
      listMigrationTasks
    , ListMigrationTasks
    -- * Request Lenses
    , lmtResourceName
    , lmtNextToken
    , lmtMaxResults

    -- * Destructuring the Response
    , listMigrationTasksResponse
    , ListMigrationTasksResponse
    -- * Response Lenses
    , lmtrsMigrationTaskSummaryList
    , lmtrsNextToken
    , lmtrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.MigrationHub.Types
import Network.AWS.MigrationHub.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listMigrationTasks' smart constructor.
data ListMigrationTasks = ListMigrationTasks'
  { _lmtResourceName :: !(Maybe Text)
  , _lmtNextToken    :: !(Maybe Text)
  , _lmtMaxResults   :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListMigrationTasks' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lmtResourceName' - Filter migration tasks by discovered resource name.
--
-- * 'lmtNextToken' - If a @NextToken@ was returned by a previous call, there are more results available. To retrieve the next page of results, make the call again using the returned token in @NextToken@ .
--
-- * 'lmtMaxResults' - Value to specify how many results are returned per page.
listMigrationTasks
    :: ListMigrationTasks
listMigrationTasks =
  ListMigrationTasks'
    { _lmtResourceName = Nothing
    , _lmtNextToken = Nothing
    , _lmtMaxResults = Nothing
    }


-- | Filter migration tasks by discovered resource name.
lmtResourceName :: Lens' ListMigrationTasks (Maybe Text)
lmtResourceName = lens _lmtResourceName (\ s a -> s{_lmtResourceName = a})

-- | If a @NextToken@ was returned by a previous call, there are more results available. To retrieve the next page of results, make the call again using the returned token in @NextToken@ .
lmtNextToken :: Lens' ListMigrationTasks (Maybe Text)
lmtNextToken = lens _lmtNextToken (\ s a -> s{_lmtNextToken = a})

-- | Value to specify how many results are returned per page.
lmtMaxResults :: Lens' ListMigrationTasks (Maybe Natural)
lmtMaxResults = lens _lmtMaxResults (\ s a -> s{_lmtMaxResults = a}) . mapping _Nat

instance AWSRequest ListMigrationTasks where
        type Rs ListMigrationTasks =
             ListMigrationTasksResponse
        request = postJSON migrationHub
        response
          = receiveJSON
              (\ s h x ->
                 ListMigrationTasksResponse' <$>
                   (x .?> "MigrationTaskSummaryList" .!@ mempty) <*>
                     (x .?> "NextToken")
                     <*> (pure (fromEnum s)))

instance Hashable ListMigrationTasks where

instance NFData ListMigrationTasks where

instance ToHeaders ListMigrationTasks where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSMigrationHub.ListMigrationTasks" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListMigrationTasks where
        toJSON ListMigrationTasks'{..}
          = object
              (catMaybes
                 [("ResourceName" .=) <$> _lmtResourceName,
                  ("NextToken" .=) <$> _lmtNextToken,
                  ("MaxResults" .=) <$> _lmtMaxResults])

instance ToPath ListMigrationTasks where
        toPath = const "/"

instance ToQuery ListMigrationTasks where
        toQuery = const mempty

-- | /See:/ 'listMigrationTasksResponse' smart constructor.
data ListMigrationTasksResponse = ListMigrationTasksResponse'
  { _lmtrsMigrationTaskSummaryList :: !(Maybe [MigrationTaskSummary])
  , _lmtrsNextToken                :: !(Maybe Text)
  , _lmtrsResponseStatus           :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListMigrationTasksResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lmtrsMigrationTaskSummaryList' - Lists the migration task's summary which includes: @MigrationTaskName@ , @ProgressPercent@ , @ProgressUpdateStream@ , @Status@ , and the @UpdateDateTime@ for each task.
--
-- * 'lmtrsNextToken' - If there are more migration tasks than the max result, return the next token to be passed to the next call as a bookmark of where to start from.
--
-- * 'lmtrsResponseStatus' - -- | The response status code.
listMigrationTasksResponse
    :: Int -- ^ 'lmtrsResponseStatus'
    -> ListMigrationTasksResponse
listMigrationTasksResponse pResponseStatus_ =
  ListMigrationTasksResponse'
    { _lmtrsMigrationTaskSummaryList = Nothing
    , _lmtrsNextToken = Nothing
    , _lmtrsResponseStatus = pResponseStatus_
    }


-- | Lists the migration task's summary which includes: @MigrationTaskName@ , @ProgressPercent@ , @ProgressUpdateStream@ , @Status@ , and the @UpdateDateTime@ for each task.
lmtrsMigrationTaskSummaryList :: Lens' ListMigrationTasksResponse [MigrationTaskSummary]
lmtrsMigrationTaskSummaryList = lens _lmtrsMigrationTaskSummaryList (\ s a -> s{_lmtrsMigrationTaskSummaryList = a}) . _Default . _Coerce

-- | If there are more migration tasks than the max result, return the next token to be passed to the next call as a bookmark of where to start from.
lmtrsNextToken :: Lens' ListMigrationTasksResponse (Maybe Text)
lmtrsNextToken = lens _lmtrsNextToken (\ s a -> s{_lmtrsNextToken = a})

-- | -- | The response status code.
lmtrsResponseStatus :: Lens' ListMigrationTasksResponse Int
lmtrsResponseStatus = lens _lmtrsResponseStatus (\ s a -> s{_lmtrsResponseStatus = a})

instance NFData ListMigrationTasksResponse where
