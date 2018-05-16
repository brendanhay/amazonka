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
-- Module      : Network.AWS.SageMaker.ListNotebookInstanceLifecycleConfigs
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists notebook instance lifestyle configurations created with the API.
--
--
module Network.AWS.SageMaker.ListNotebookInstanceLifecycleConfigs
    (
    -- * Creating a Request
      listNotebookInstanceLifecycleConfigs
    , ListNotebookInstanceLifecycleConfigs
    -- * Request Lenses
    , lnilcNameContains
    , lnilcLastModifiedTimeBefore
    , lnilcCreationTimeAfter
    , lnilcNextToken
    , lnilcSortOrder
    , lnilcLastModifiedTimeAfter
    , lnilcCreationTimeBefore
    , lnilcMaxResults
    , lnilcSortBy

    -- * Destructuring the Response
    , listNotebookInstanceLifecycleConfigsResponse
    , ListNotebookInstanceLifecycleConfigsResponse
    -- * Response Lenses
    , lnilcrsNextToken
    , lnilcrsNotebookInstanceLifecycleConfigs
    , lnilcrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SageMaker.Types
import Network.AWS.SageMaker.Types.Product

-- | /See:/ 'listNotebookInstanceLifecycleConfigs' smart constructor.
data ListNotebookInstanceLifecycleConfigs = ListNotebookInstanceLifecycleConfigs'
  { _lnilcNameContains :: !(Maybe Text)
  , _lnilcLastModifiedTimeBefore :: !(Maybe POSIX)
  , _lnilcCreationTimeAfter :: !(Maybe POSIX)
  , _lnilcNextToken :: !(Maybe Text)
  , _lnilcSortOrder :: !(Maybe NotebookInstanceLifecycleConfigSortOrder)
  , _lnilcLastModifiedTimeAfter :: !(Maybe POSIX)
  , _lnilcCreationTimeBefore :: !(Maybe POSIX)
  , _lnilcMaxResults :: !(Maybe Nat)
  , _lnilcSortBy :: !(Maybe NotebookInstanceLifecycleConfigSortKey)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListNotebookInstanceLifecycleConfigs' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lnilcNameContains' - A string in the lifecycle configuration name. This filter returns only lifecycle configurations whose name contains the specified string.
--
-- * 'lnilcLastModifiedTimeBefore' - A filter that returns only lifecycle configurations that were modified before the specified time (timestamp).
--
-- * 'lnilcCreationTimeAfter' - A filter that returns only lifecycle configurations that were created after the specified time (timestamp).
--
-- * 'lnilcNextToken' - If the result of a @ListNotebookInstanceLifecycleConfigs@ request was truncated, the response includes a @NextToken@ . To get the next set of lifecycle configurations, use the token in the next request.
--
-- * 'lnilcSortOrder' - The sort order for results.
--
-- * 'lnilcLastModifiedTimeAfter' - A filter that returns only lifecycle configurations that were modified after the specified time (timestamp).
--
-- * 'lnilcCreationTimeBefore' - A filter that returns only lifecycle configurations that were created before the specified time (timestamp).
--
-- * 'lnilcMaxResults' - The maximum number of lifecycle configurations to return in the response.
--
-- * 'lnilcSortBy' - Sorts the list of results. The default is @CreationTime@ .
listNotebookInstanceLifecycleConfigs
    :: ListNotebookInstanceLifecycleConfigs
listNotebookInstanceLifecycleConfigs =
  ListNotebookInstanceLifecycleConfigs'
    { _lnilcNameContains = Nothing
    , _lnilcLastModifiedTimeBefore = Nothing
    , _lnilcCreationTimeAfter = Nothing
    , _lnilcNextToken = Nothing
    , _lnilcSortOrder = Nothing
    , _lnilcLastModifiedTimeAfter = Nothing
    , _lnilcCreationTimeBefore = Nothing
    , _lnilcMaxResults = Nothing
    , _lnilcSortBy = Nothing
    }


-- | A string in the lifecycle configuration name. This filter returns only lifecycle configurations whose name contains the specified string.
lnilcNameContains :: Lens' ListNotebookInstanceLifecycleConfigs (Maybe Text)
lnilcNameContains = lens _lnilcNameContains (\ s a -> s{_lnilcNameContains = a})

-- | A filter that returns only lifecycle configurations that were modified before the specified time (timestamp).
lnilcLastModifiedTimeBefore :: Lens' ListNotebookInstanceLifecycleConfigs (Maybe UTCTime)
lnilcLastModifiedTimeBefore = lens _lnilcLastModifiedTimeBefore (\ s a -> s{_lnilcLastModifiedTimeBefore = a}) . mapping _Time

-- | A filter that returns only lifecycle configurations that were created after the specified time (timestamp).
lnilcCreationTimeAfter :: Lens' ListNotebookInstanceLifecycleConfigs (Maybe UTCTime)
lnilcCreationTimeAfter = lens _lnilcCreationTimeAfter (\ s a -> s{_lnilcCreationTimeAfter = a}) . mapping _Time

-- | If the result of a @ListNotebookInstanceLifecycleConfigs@ request was truncated, the response includes a @NextToken@ . To get the next set of lifecycle configurations, use the token in the next request.
lnilcNextToken :: Lens' ListNotebookInstanceLifecycleConfigs (Maybe Text)
lnilcNextToken = lens _lnilcNextToken (\ s a -> s{_lnilcNextToken = a})

-- | The sort order for results.
lnilcSortOrder :: Lens' ListNotebookInstanceLifecycleConfigs (Maybe NotebookInstanceLifecycleConfigSortOrder)
lnilcSortOrder = lens _lnilcSortOrder (\ s a -> s{_lnilcSortOrder = a})

-- | A filter that returns only lifecycle configurations that were modified after the specified time (timestamp).
lnilcLastModifiedTimeAfter :: Lens' ListNotebookInstanceLifecycleConfigs (Maybe UTCTime)
lnilcLastModifiedTimeAfter = lens _lnilcLastModifiedTimeAfter (\ s a -> s{_lnilcLastModifiedTimeAfter = a}) . mapping _Time

-- | A filter that returns only lifecycle configurations that were created before the specified time (timestamp).
lnilcCreationTimeBefore :: Lens' ListNotebookInstanceLifecycleConfigs (Maybe UTCTime)
lnilcCreationTimeBefore = lens _lnilcCreationTimeBefore (\ s a -> s{_lnilcCreationTimeBefore = a}) . mapping _Time

-- | The maximum number of lifecycle configurations to return in the response.
lnilcMaxResults :: Lens' ListNotebookInstanceLifecycleConfigs (Maybe Natural)
lnilcMaxResults = lens _lnilcMaxResults (\ s a -> s{_lnilcMaxResults = a}) . mapping _Nat

-- | Sorts the list of results. The default is @CreationTime@ .
lnilcSortBy :: Lens' ListNotebookInstanceLifecycleConfigs (Maybe NotebookInstanceLifecycleConfigSortKey)
lnilcSortBy = lens _lnilcSortBy (\ s a -> s{_lnilcSortBy = a})

instance AWSRequest
           ListNotebookInstanceLifecycleConfigs
         where
        type Rs ListNotebookInstanceLifecycleConfigs =
             ListNotebookInstanceLifecycleConfigsResponse
        request = postJSON sageMaker
        response
          = receiveJSON
              (\ s h x ->
                 ListNotebookInstanceLifecycleConfigsResponse' <$>
                   (x .?> "NextToken") <*>
                     (x .?> "NotebookInstanceLifecycleConfigs" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable
           ListNotebookInstanceLifecycleConfigs
         where

instance NFData ListNotebookInstanceLifecycleConfigs
         where

instance ToHeaders
           ListNotebookInstanceLifecycleConfigs
         where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("SageMaker.ListNotebookInstanceLifecycleConfigs" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListNotebookInstanceLifecycleConfigs
         where
        toJSON ListNotebookInstanceLifecycleConfigs'{..}
          = object
              (catMaybes
                 [("NameContains" .=) <$> _lnilcNameContains,
                  ("LastModifiedTimeBefore" .=) <$>
                    _lnilcLastModifiedTimeBefore,
                  ("CreationTimeAfter" .=) <$> _lnilcCreationTimeAfter,
                  ("NextToken" .=) <$> _lnilcNextToken,
                  ("SortOrder" .=) <$> _lnilcSortOrder,
                  ("LastModifiedTimeAfter" .=) <$>
                    _lnilcLastModifiedTimeAfter,
                  ("CreationTimeBefore" .=) <$>
                    _lnilcCreationTimeBefore,
                  ("MaxResults" .=) <$> _lnilcMaxResults,
                  ("SortBy" .=) <$> _lnilcSortBy])

instance ToPath ListNotebookInstanceLifecycleConfigs
         where
        toPath = const "/"

instance ToQuery ListNotebookInstanceLifecycleConfigs
         where
        toQuery = const mempty

-- | /See:/ 'listNotebookInstanceLifecycleConfigsResponse' smart constructor.
data ListNotebookInstanceLifecycleConfigsResponse = ListNotebookInstanceLifecycleConfigsResponse'
  { _lnilcrsNextToken :: !(Maybe Text)
  , _lnilcrsNotebookInstanceLifecycleConfigs :: !(Maybe [NotebookInstanceLifecycleConfigSummary])
  , _lnilcrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListNotebookInstanceLifecycleConfigsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lnilcrsNextToken' - If the response is truncated, Amazon SageMaker returns this token. To get the next set of lifecycle configurations, use it in the next request.
--
-- * 'lnilcrsNotebookInstanceLifecycleConfigs' - An array of @NotebookInstanceLifecycleConfiguration@ objects, each listing a lifecycle configuration.
--
-- * 'lnilcrsResponseStatus' - -- | The response status code.
listNotebookInstanceLifecycleConfigsResponse
    :: Int -- ^ 'lnilcrsResponseStatus'
    -> ListNotebookInstanceLifecycleConfigsResponse
listNotebookInstanceLifecycleConfigsResponse pResponseStatus_ =
  ListNotebookInstanceLifecycleConfigsResponse'
    { _lnilcrsNextToken = Nothing
    , _lnilcrsNotebookInstanceLifecycleConfigs = Nothing
    , _lnilcrsResponseStatus = pResponseStatus_
    }


-- | If the response is truncated, Amazon SageMaker returns this token. To get the next set of lifecycle configurations, use it in the next request.
lnilcrsNextToken :: Lens' ListNotebookInstanceLifecycleConfigsResponse (Maybe Text)
lnilcrsNextToken = lens _lnilcrsNextToken (\ s a -> s{_lnilcrsNextToken = a})

-- | An array of @NotebookInstanceLifecycleConfiguration@ objects, each listing a lifecycle configuration.
lnilcrsNotebookInstanceLifecycleConfigs :: Lens' ListNotebookInstanceLifecycleConfigsResponse [NotebookInstanceLifecycleConfigSummary]
lnilcrsNotebookInstanceLifecycleConfigs = lens _lnilcrsNotebookInstanceLifecycleConfigs (\ s a -> s{_lnilcrsNotebookInstanceLifecycleConfigs = a}) . _Default . _Coerce

-- | -- | The response status code.
lnilcrsResponseStatus :: Lens' ListNotebookInstanceLifecycleConfigsResponse Int
lnilcrsResponseStatus = lens _lnilcrsResponseStatus (\ s a -> s{_lnilcrsResponseStatus = a})

instance NFData
           ListNotebookInstanceLifecycleConfigsResponse
         where
