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
-- Module      : Network.AWS.SageMaker.ListCodeRepositories
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of the Git repositories in your account.
--
--
--
-- This operation returns paginated results.
module Network.AWS.SageMaker.ListCodeRepositories
    (
    -- * Creating a Request
      listCodeRepositories
    , ListCodeRepositories
    -- * Request Lenses
    , lcrNameContains
    , lcrLastModifiedTimeBefore
    , lcrCreationTimeAfter
    , lcrNextToken
    , lcrSortOrder
    , lcrLastModifiedTimeAfter
    , lcrCreationTimeBefore
    , lcrMaxResults
    , lcrSortBy

    -- * Destructuring the Response
    , listCodeRepositoriesResponse
    , ListCodeRepositoriesResponse
    -- * Response Lenses
    , lcrrsNextToken
    , lcrrsResponseStatus
    , lcrrsCodeRepositorySummaryList
    ) where

import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SageMaker.Types
import Network.AWS.SageMaker.Types.Product

-- | /See:/ 'listCodeRepositories' smart constructor.
data ListCodeRepositories = ListCodeRepositories'
  { _lcrNameContains           :: !(Maybe Text)
  , _lcrLastModifiedTimeBefore :: !(Maybe POSIX)
  , _lcrCreationTimeAfter      :: !(Maybe POSIX)
  , _lcrNextToken              :: !(Maybe Text)
  , _lcrSortOrder              :: !(Maybe CodeRepositorySortOrder)
  , _lcrLastModifiedTimeAfter  :: !(Maybe POSIX)
  , _lcrCreationTimeBefore     :: !(Maybe POSIX)
  , _lcrMaxResults             :: !(Maybe Nat)
  , _lcrSortBy                 :: !(Maybe CodeRepositorySortBy)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListCodeRepositories' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lcrNameContains' - A string in the Git repositories name. This filter returns only repositories whose name contains the specified string.
--
-- * 'lcrLastModifiedTimeBefore' - A filter that returns only Git repositories that were last modified before the specified time.
--
-- * 'lcrCreationTimeAfter' - A filter that returns only Git repositories that were created after the specified time.
--
-- * 'lcrNextToken' - If the result of a @ListCodeRepositoriesOutput@ request was truncated, the response includes a @NextToken@ . To get the next set of Git repositories, use the token in the next request.
--
-- * 'lcrSortOrder' - The sort order for results. The default is @Ascending@ .
--
-- * 'lcrLastModifiedTimeAfter' - A filter that returns only Git repositories that were last modified after the specified time.
--
-- * 'lcrCreationTimeBefore' - A filter that returns only Git repositories that were created before the specified time.
--
-- * 'lcrMaxResults' - The maximum number of Git repositories to return in the response.
--
-- * 'lcrSortBy' - The field to sort results by. The default is @Name@ .
listCodeRepositories
    :: ListCodeRepositories
listCodeRepositories =
  ListCodeRepositories'
    { _lcrNameContains = Nothing
    , _lcrLastModifiedTimeBefore = Nothing
    , _lcrCreationTimeAfter = Nothing
    , _lcrNextToken = Nothing
    , _lcrSortOrder = Nothing
    , _lcrLastModifiedTimeAfter = Nothing
    , _lcrCreationTimeBefore = Nothing
    , _lcrMaxResults = Nothing
    , _lcrSortBy = Nothing
    }


-- | A string in the Git repositories name. This filter returns only repositories whose name contains the specified string.
lcrNameContains :: Lens' ListCodeRepositories (Maybe Text)
lcrNameContains = lens _lcrNameContains (\ s a -> s{_lcrNameContains = a})

-- | A filter that returns only Git repositories that were last modified before the specified time.
lcrLastModifiedTimeBefore :: Lens' ListCodeRepositories (Maybe UTCTime)
lcrLastModifiedTimeBefore = lens _lcrLastModifiedTimeBefore (\ s a -> s{_lcrLastModifiedTimeBefore = a}) . mapping _Time

-- | A filter that returns only Git repositories that were created after the specified time.
lcrCreationTimeAfter :: Lens' ListCodeRepositories (Maybe UTCTime)
lcrCreationTimeAfter = lens _lcrCreationTimeAfter (\ s a -> s{_lcrCreationTimeAfter = a}) . mapping _Time

-- | If the result of a @ListCodeRepositoriesOutput@ request was truncated, the response includes a @NextToken@ . To get the next set of Git repositories, use the token in the next request.
lcrNextToken :: Lens' ListCodeRepositories (Maybe Text)
lcrNextToken = lens _lcrNextToken (\ s a -> s{_lcrNextToken = a})

-- | The sort order for results. The default is @Ascending@ .
lcrSortOrder :: Lens' ListCodeRepositories (Maybe CodeRepositorySortOrder)
lcrSortOrder = lens _lcrSortOrder (\ s a -> s{_lcrSortOrder = a})

-- | A filter that returns only Git repositories that were last modified after the specified time.
lcrLastModifiedTimeAfter :: Lens' ListCodeRepositories (Maybe UTCTime)
lcrLastModifiedTimeAfter = lens _lcrLastModifiedTimeAfter (\ s a -> s{_lcrLastModifiedTimeAfter = a}) . mapping _Time

-- | A filter that returns only Git repositories that were created before the specified time.
lcrCreationTimeBefore :: Lens' ListCodeRepositories (Maybe UTCTime)
lcrCreationTimeBefore = lens _lcrCreationTimeBefore (\ s a -> s{_lcrCreationTimeBefore = a}) . mapping _Time

-- | The maximum number of Git repositories to return in the response.
lcrMaxResults :: Lens' ListCodeRepositories (Maybe Natural)
lcrMaxResults = lens _lcrMaxResults (\ s a -> s{_lcrMaxResults = a}) . mapping _Nat

-- | The field to sort results by. The default is @Name@ .
lcrSortBy :: Lens' ListCodeRepositories (Maybe CodeRepositorySortBy)
lcrSortBy = lens _lcrSortBy (\ s a -> s{_lcrSortBy = a})

instance AWSPager ListCodeRepositories where
        page rq rs
          | stop (rs ^. lcrrsNextToken) = Nothing
          | stop (rs ^. lcrrsCodeRepositorySummaryList) =
            Nothing
          | otherwise =
            Just $ rq & lcrNextToken .~ rs ^. lcrrsNextToken

instance AWSRequest ListCodeRepositories where
        type Rs ListCodeRepositories =
             ListCodeRepositoriesResponse
        request = postJSON sageMaker
        response
          = receiveJSON
              (\ s h x ->
                 ListCodeRepositoriesResponse' <$>
                   (x .?> "NextToken") <*> (pure (fromEnum s)) <*>
                     (x .?> "CodeRepositorySummaryList" .!@ mempty))

instance Hashable ListCodeRepositories where

instance NFData ListCodeRepositories where

instance ToHeaders ListCodeRepositories where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("SageMaker.ListCodeRepositories" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListCodeRepositories where
        toJSON ListCodeRepositories'{..}
          = object
              (catMaybes
                 [("NameContains" .=) <$> _lcrNameContains,
                  ("LastModifiedTimeBefore" .=) <$>
                    _lcrLastModifiedTimeBefore,
                  ("CreationTimeAfter" .=) <$> _lcrCreationTimeAfter,
                  ("NextToken" .=) <$> _lcrNextToken,
                  ("SortOrder" .=) <$> _lcrSortOrder,
                  ("LastModifiedTimeAfter" .=) <$>
                    _lcrLastModifiedTimeAfter,
                  ("CreationTimeBefore" .=) <$> _lcrCreationTimeBefore,
                  ("MaxResults" .=) <$> _lcrMaxResults,
                  ("SortBy" .=) <$> _lcrSortBy])

instance ToPath ListCodeRepositories where
        toPath = const "/"

instance ToQuery ListCodeRepositories where
        toQuery = const mempty

-- | /See:/ 'listCodeRepositoriesResponse' smart constructor.
data ListCodeRepositoriesResponse = ListCodeRepositoriesResponse'
  { _lcrrsNextToken                 :: !(Maybe Text)
  , _lcrrsResponseStatus            :: !Int
  , _lcrrsCodeRepositorySummaryList :: ![CodeRepositorySummary]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListCodeRepositoriesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lcrrsNextToken' - If the result of a @ListCodeRepositoriesOutput@ request was truncated, the response includes a @NextToken@ . To get the next set of Git repositories, use the token in the next request.
--
-- * 'lcrrsResponseStatus' - -- | The response status code.
--
-- * 'lcrrsCodeRepositorySummaryList' - Gets a list of summaries of the Git repositories. Each summary specifies the following values for the repository:      * Name     * Amazon Resource Name (ARN)     * Creation time     * Last modified time     * Configuration information, including the URL location of the repository and the ARN of the AWS Secrets Manager secret that contains the credentials used to access the repository.
listCodeRepositoriesResponse
    :: Int -- ^ 'lcrrsResponseStatus'
    -> ListCodeRepositoriesResponse
listCodeRepositoriesResponse pResponseStatus_ =
  ListCodeRepositoriesResponse'
    { _lcrrsNextToken = Nothing
    , _lcrrsResponseStatus = pResponseStatus_
    , _lcrrsCodeRepositorySummaryList = mempty
    }


-- | If the result of a @ListCodeRepositoriesOutput@ request was truncated, the response includes a @NextToken@ . To get the next set of Git repositories, use the token in the next request.
lcrrsNextToken :: Lens' ListCodeRepositoriesResponse (Maybe Text)
lcrrsNextToken = lens _lcrrsNextToken (\ s a -> s{_lcrrsNextToken = a})

-- | -- | The response status code.
lcrrsResponseStatus :: Lens' ListCodeRepositoriesResponse Int
lcrrsResponseStatus = lens _lcrrsResponseStatus (\ s a -> s{_lcrrsResponseStatus = a})

-- | Gets a list of summaries of the Git repositories. Each summary specifies the following values for the repository:      * Name     * Amazon Resource Name (ARN)     * Creation time     * Last modified time     * Configuration information, including the URL location of the repository and the ARN of the AWS Secrets Manager secret that contains the credentials used to access the repository.
lcrrsCodeRepositorySummaryList :: Lens' ListCodeRepositoriesResponse [CodeRepositorySummary]
lcrrsCodeRepositorySummaryList = lens _lcrrsCodeRepositorySummaryList (\ s a -> s{_lcrrsCodeRepositorySummaryList = a}) . _Coerce

instance NFData ListCodeRepositoriesResponse where
