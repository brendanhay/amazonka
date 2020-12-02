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
-- Module      : Network.AWS.SageMaker.ListNotebookInstances
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of the Amazon SageMaker notebook instances in the requester's account in an AWS Region.
--
--
--
-- This operation returns paginated results.
module Network.AWS.SageMaker.ListNotebookInstances
    (
    -- * Creating a Request
      listNotebookInstances
    , ListNotebookInstances
    -- * Request Lenses
    , lniNameContains
    , lniLastModifiedTimeBefore
    , lniNotebookInstanceLifecycleConfigNameContains
    , lniCreationTimeAfter
    , lniNextToken
    , lniSortOrder
    , lniLastModifiedTimeAfter
    , lniCreationTimeBefore
    , lniStatusEquals
    , lniMaxResults
    , lniSortBy

    -- * Destructuring the Response
    , listNotebookInstancesResponse
    , ListNotebookInstancesResponse
    -- * Response Lenses
    , lnirsNotebookInstances
    , lnirsNextToken
    , lnirsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SageMaker.Types
import Network.AWS.SageMaker.Types.Product

-- | /See:/ 'listNotebookInstances' smart constructor.
data ListNotebookInstances = ListNotebookInstances'
  { _lniNameContains :: !(Maybe Text)
  , _lniLastModifiedTimeBefore :: !(Maybe POSIX)
  , _lniNotebookInstanceLifecycleConfigNameContains :: !(Maybe Text)
  , _lniCreationTimeAfter :: !(Maybe POSIX)
  , _lniNextToken :: !(Maybe Text)
  , _lniSortOrder :: !(Maybe NotebookInstanceSortOrder)
  , _lniLastModifiedTimeAfter :: !(Maybe POSIX)
  , _lniCreationTimeBefore :: !(Maybe POSIX)
  , _lniStatusEquals :: !(Maybe NotebookInstanceStatus)
  , _lniMaxResults :: !(Maybe Nat)
  , _lniSortBy :: !(Maybe NotebookInstanceSortKey)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListNotebookInstances' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lniNameContains' - A string in the notebook instances' name. This filter returns only notebook instances whose name contains the specified string.
--
-- * 'lniLastModifiedTimeBefore' - A filter that returns only notebook instances that were modified before the specified time (timestamp).
--
-- * 'lniNotebookInstanceLifecycleConfigNameContains' - A string in the name of a notebook instances lifecycle configuration associated with this notebook instance. This filter returns only notebook instances associated with a lifecycle configuration with a name that contains the specified string.
--
-- * 'lniCreationTimeAfter' - A filter that returns only notebook instances that were created after the specified time (timestamp).
--
-- * 'lniNextToken' - If the previous call to the @ListNotebookInstances@ is truncated, the response includes a @NextToken@ . You can use this token in your subsequent @ListNotebookInstances@ request to fetch the next set of notebook instances.
--
-- * 'lniSortOrder' - The sort order for results.
--
-- * 'lniLastModifiedTimeAfter' - A filter that returns only notebook instances that were modified after the specified time (timestamp).
--
-- * 'lniCreationTimeBefore' - A filter that returns only notebook instances that were created before the specified time (timestamp).
--
-- * 'lniStatusEquals' - A filter that returns only notebook instances with the specified status.
--
-- * 'lniMaxResults' - The maximum number of notebook instances to return.
--
-- * 'lniSortBy' - The field to sort results by. The default is @Name@ .
listNotebookInstances
    :: ListNotebookInstances
listNotebookInstances =
  ListNotebookInstances'
    { _lniNameContains = Nothing
    , _lniLastModifiedTimeBefore = Nothing
    , _lniNotebookInstanceLifecycleConfigNameContains = Nothing
    , _lniCreationTimeAfter = Nothing
    , _lniNextToken = Nothing
    , _lniSortOrder = Nothing
    , _lniLastModifiedTimeAfter = Nothing
    , _lniCreationTimeBefore = Nothing
    , _lniStatusEquals = Nothing
    , _lniMaxResults = Nothing
    , _lniSortBy = Nothing
    }


-- | A string in the notebook instances' name. This filter returns only notebook instances whose name contains the specified string.
lniNameContains :: Lens' ListNotebookInstances (Maybe Text)
lniNameContains = lens _lniNameContains (\ s a -> s{_lniNameContains = a})

-- | A filter that returns only notebook instances that were modified before the specified time (timestamp).
lniLastModifiedTimeBefore :: Lens' ListNotebookInstances (Maybe UTCTime)
lniLastModifiedTimeBefore = lens _lniLastModifiedTimeBefore (\ s a -> s{_lniLastModifiedTimeBefore = a}) . mapping _Time

-- | A string in the name of a notebook instances lifecycle configuration associated with this notebook instance. This filter returns only notebook instances associated with a lifecycle configuration with a name that contains the specified string.
lniNotebookInstanceLifecycleConfigNameContains :: Lens' ListNotebookInstances (Maybe Text)
lniNotebookInstanceLifecycleConfigNameContains = lens _lniNotebookInstanceLifecycleConfigNameContains (\ s a -> s{_lniNotebookInstanceLifecycleConfigNameContains = a})

-- | A filter that returns only notebook instances that were created after the specified time (timestamp).
lniCreationTimeAfter :: Lens' ListNotebookInstances (Maybe UTCTime)
lniCreationTimeAfter = lens _lniCreationTimeAfter (\ s a -> s{_lniCreationTimeAfter = a}) . mapping _Time

-- | If the previous call to the @ListNotebookInstances@ is truncated, the response includes a @NextToken@ . You can use this token in your subsequent @ListNotebookInstances@ request to fetch the next set of notebook instances.
lniNextToken :: Lens' ListNotebookInstances (Maybe Text)
lniNextToken = lens _lniNextToken (\ s a -> s{_lniNextToken = a})

-- | The sort order for results.
lniSortOrder :: Lens' ListNotebookInstances (Maybe NotebookInstanceSortOrder)
lniSortOrder = lens _lniSortOrder (\ s a -> s{_lniSortOrder = a})

-- | A filter that returns only notebook instances that were modified after the specified time (timestamp).
lniLastModifiedTimeAfter :: Lens' ListNotebookInstances (Maybe UTCTime)
lniLastModifiedTimeAfter = lens _lniLastModifiedTimeAfter (\ s a -> s{_lniLastModifiedTimeAfter = a}) . mapping _Time

-- | A filter that returns only notebook instances that were created before the specified time (timestamp).
lniCreationTimeBefore :: Lens' ListNotebookInstances (Maybe UTCTime)
lniCreationTimeBefore = lens _lniCreationTimeBefore (\ s a -> s{_lniCreationTimeBefore = a}) . mapping _Time

-- | A filter that returns only notebook instances with the specified status.
lniStatusEquals :: Lens' ListNotebookInstances (Maybe NotebookInstanceStatus)
lniStatusEquals = lens _lniStatusEquals (\ s a -> s{_lniStatusEquals = a})

-- | The maximum number of notebook instances to return.
lniMaxResults :: Lens' ListNotebookInstances (Maybe Natural)
lniMaxResults = lens _lniMaxResults (\ s a -> s{_lniMaxResults = a}) . mapping _Nat

-- | The field to sort results by. The default is @Name@ .
lniSortBy :: Lens' ListNotebookInstances (Maybe NotebookInstanceSortKey)
lniSortBy = lens _lniSortBy (\ s a -> s{_lniSortBy = a})

instance AWSPager ListNotebookInstances where
        page rq rs
          | stop (rs ^. lnirsNextToken) = Nothing
          | stop (rs ^. lnirsNotebookInstances) = Nothing
          | otherwise =
            Just $ rq & lniNextToken .~ rs ^. lnirsNextToken

instance AWSRequest ListNotebookInstances where
        type Rs ListNotebookInstances =
             ListNotebookInstancesResponse
        request = postJSON sageMaker
        response
          = receiveJSON
              (\ s h x ->
                 ListNotebookInstancesResponse' <$>
                   (x .?> "NotebookInstances" .!@ mempty) <*>
                     (x .?> "NextToken")
                     <*> (pure (fromEnum s)))

instance Hashable ListNotebookInstances where

instance NFData ListNotebookInstances where

instance ToHeaders ListNotebookInstances where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("SageMaker.ListNotebookInstances" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListNotebookInstances where
        toJSON ListNotebookInstances'{..}
          = object
              (catMaybes
                 [("NameContains" .=) <$> _lniNameContains,
                  ("LastModifiedTimeBefore" .=) <$>
                    _lniLastModifiedTimeBefore,
                  ("NotebookInstanceLifecycleConfigNameContains" .=)
                    <$> _lniNotebookInstanceLifecycleConfigNameContains,
                  ("CreationTimeAfter" .=) <$> _lniCreationTimeAfter,
                  ("NextToken" .=) <$> _lniNextToken,
                  ("SortOrder" .=) <$> _lniSortOrder,
                  ("LastModifiedTimeAfter" .=) <$>
                    _lniLastModifiedTimeAfter,
                  ("CreationTimeBefore" .=) <$> _lniCreationTimeBefore,
                  ("StatusEquals" .=) <$> _lniStatusEquals,
                  ("MaxResults" .=) <$> _lniMaxResults,
                  ("SortBy" .=) <$> _lniSortBy])

instance ToPath ListNotebookInstances where
        toPath = const "/"

instance ToQuery ListNotebookInstances where
        toQuery = const mempty

-- | /See:/ 'listNotebookInstancesResponse' smart constructor.
data ListNotebookInstancesResponse = ListNotebookInstancesResponse'
  { _lnirsNotebookInstances :: !(Maybe [NotebookInstanceSummary])
  , _lnirsNextToken         :: !(Maybe Text)
  , _lnirsResponseStatus    :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListNotebookInstancesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lnirsNotebookInstances' - An array of @NotebookInstanceSummary@ objects, one for each notebook instance.
--
-- * 'lnirsNextToken' - If the response to the previous @ListNotebookInstances@ request was truncated, Amazon SageMaker returns this token. To retrieve the next set of notebook instances, use the token in the next request.
--
-- * 'lnirsResponseStatus' - -- | The response status code.
listNotebookInstancesResponse
    :: Int -- ^ 'lnirsResponseStatus'
    -> ListNotebookInstancesResponse
listNotebookInstancesResponse pResponseStatus_ =
  ListNotebookInstancesResponse'
    { _lnirsNotebookInstances = Nothing
    , _lnirsNextToken = Nothing
    , _lnirsResponseStatus = pResponseStatus_
    }


-- | An array of @NotebookInstanceSummary@ objects, one for each notebook instance.
lnirsNotebookInstances :: Lens' ListNotebookInstancesResponse [NotebookInstanceSummary]
lnirsNotebookInstances = lens _lnirsNotebookInstances (\ s a -> s{_lnirsNotebookInstances = a}) . _Default . _Coerce

-- | If the response to the previous @ListNotebookInstances@ request was truncated, Amazon SageMaker returns this token. To retrieve the next set of notebook instances, use the token in the next request.
lnirsNextToken :: Lens' ListNotebookInstancesResponse (Maybe Text)
lnirsNextToken = lens _lnirsNextToken (\ s a -> s{_lnirsNextToken = a})

-- | -- | The response status code.
lnirsResponseStatus :: Lens' ListNotebookInstancesResponse Int
lnirsResponseStatus = lens _lnirsResponseStatus (\ s a -> s{_lnirsResponseStatus = a})

instance NFData ListNotebookInstancesResponse where
