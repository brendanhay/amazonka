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
-- Module      : Network.AWS.SageMaker.ListAlgorithms
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the machine learning algorithms that have been created.
--
--
--
-- This operation returns paginated results.
module Network.AWS.SageMaker.ListAlgorithms
    (
    -- * Creating a Request
      listAlgorithms
    , ListAlgorithms
    -- * Request Lenses
    , laNameContains
    , laCreationTimeAfter
    , laNextToken
    , laSortOrder
    , laCreationTimeBefore
    , laMaxResults
    , laSortBy

    -- * Destructuring the Response
    , listAlgorithmsResponse
    , ListAlgorithmsResponse
    -- * Response Lenses
    , larsNextToken
    , larsResponseStatus
    , larsAlgorithmSummaryList
    ) where

import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SageMaker.Types
import Network.AWS.SageMaker.Types.Product

-- | /See:/ 'listAlgorithms' smart constructor.
data ListAlgorithms = ListAlgorithms'
  { _laNameContains       :: !(Maybe Text)
  , _laCreationTimeAfter  :: !(Maybe POSIX)
  , _laNextToken          :: !(Maybe Text)
  , _laSortOrder          :: !(Maybe SortOrder)
  , _laCreationTimeBefore :: !(Maybe POSIX)
  , _laMaxResults         :: !(Maybe Nat)
  , _laSortBy             :: !(Maybe AlgorithmSortBy)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListAlgorithms' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'laNameContains' - A string in the algorithm name. This filter returns only algorithms whose name contains the specified string.
--
-- * 'laCreationTimeAfter' - A filter that returns only algorithms created after the specified time (timestamp).
--
-- * 'laNextToken' - If the response to a previous @ListAlgorithms@ request was truncated, the response includes a @NextToken@ . To retrieve the next set of algorithms, use the token in the next request.
--
-- * 'laSortOrder' - The sort order for the results. The default is @Ascending@ .
--
-- * 'laCreationTimeBefore' - A filter that returns only algorithms created before the specified time (timestamp).
--
-- * 'laMaxResults' - The maximum number of algorithms to return in the response.
--
-- * 'laSortBy' - The parameter by which to sort the results. The default is @CreationTime@ .
listAlgorithms
    :: ListAlgorithms
listAlgorithms =
  ListAlgorithms'
    { _laNameContains = Nothing
    , _laCreationTimeAfter = Nothing
    , _laNextToken = Nothing
    , _laSortOrder = Nothing
    , _laCreationTimeBefore = Nothing
    , _laMaxResults = Nothing
    , _laSortBy = Nothing
    }


-- | A string in the algorithm name. This filter returns only algorithms whose name contains the specified string.
laNameContains :: Lens' ListAlgorithms (Maybe Text)
laNameContains = lens _laNameContains (\ s a -> s{_laNameContains = a})

-- | A filter that returns only algorithms created after the specified time (timestamp).
laCreationTimeAfter :: Lens' ListAlgorithms (Maybe UTCTime)
laCreationTimeAfter = lens _laCreationTimeAfter (\ s a -> s{_laCreationTimeAfter = a}) . mapping _Time

-- | If the response to a previous @ListAlgorithms@ request was truncated, the response includes a @NextToken@ . To retrieve the next set of algorithms, use the token in the next request.
laNextToken :: Lens' ListAlgorithms (Maybe Text)
laNextToken = lens _laNextToken (\ s a -> s{_laNextToken = a})

-- | The sort order for the results. The default is @Ascending@ .
laSortOrder :: Lens' ListAlgorithms (Maybe SortOrder)
laSortOrder = lens _laSortOrder (\ s a -> s{_laSortOrder = a})

-- | A filter that returns only algorithms created before the specified time (timestamp).
laCreationTimeBefore :: Lens' ListAlgorithms (Maybe UTCTime)
laCreationTimeBefore = lens _laCreationTimeBefore (\ s a -> s{_laCreationTimeBefore = a}) . mapping _Time

-- | The maximum number of algorithms to return in the response.
laMaxResults :: Lens' ListAlgorithms (Maybe Natural)
laMaxResults = lens _laMaxResults (\ s a -> s{_laMaxResults = a}) . mapping _Nat

-- | The parameter by which to sort the results. The default is @CreationTime@ .
laSortBy :: Lens' ListAlgorithms (Maybe AlgorithmSortBy)
laSortBy = lens _laSortBy (\ s a -> s{_laSortBy = a})

instance AWSPager ListAlgorithms where
        page rq rs
          | stop (rs ^. larsNextToken) = Nothing
          | stop (rs ^. larsAlgorithmSummaryList) = Nothing
          | otherwise =
            Just $ rq & laNextToken .~ rs ^. larsNextToken

instance AWSRequest ListAlgorithms where
        type Rs ListAlgorithms = ListAlgorithmsResponse
        request = postJSON sageMaker
        response
          = receiveJSON
              (\ s h x ->
                 ListAlgorithmsResponse' <$>
                   (x .?> "NextToken") <*> (pure (fromEnum s)) <*>
                     (x .?> "AlgorithmSummaryList" .!@ mempty))

instance Hashable ListAlgorithms where

instance NFData ListAlgorithms where

instance ToHeaders ListAlgorithms where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("SageMaker.ListAlgorithms" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListAlgorithms where
        toJSON ListAlgorithms'{..}
          = object
              (catMaybes
                 [("NameContains" .=) <$> _laNameContains,
                  ("CreationTimeAfter" .=) <$> _laCreationTimeAfter,
                  ("NextToken" .=) <$> _laNextToken,
                  ("SortOrder" .=) <$> _laSortOrder,
                  ("CreationTimeBefore" .=) <$> _laCreationTimeBefore,
                  ("MaxResults" .=) <$> _laMaxResults,
                  ("SortBy" .=) <$> _laSortBy])

instance ToPath ListAlgorithms where
        toPath = const "/"

instance ToQuery ListAlgorithms where
        toQuery = const mempty

-- | /See:/ 'listAlgorithmsResponse' smart constructor.
data ListAlgorithmsResponse = ListAlgorithmsResponse'
  { _larsNextToken            :: !(Maybe Text)
  , _larsResponseStatus       :: !Int
  , _larsAlgorithmSummaryList :: ![AlgorithmSummary]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListAlgorithmsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'larsNextToken' - If the response is truncated, Amazon SageMaker returns this token. To retrieve the next set of algorithms, use it in the subsequent request.
--
-- * 'larsResponseStatus' - -- | The response status code.
--
-- * 'larsAlgorithmSummaryList' - >An array of @AlgorithmSummary@ objects, each of which lists an algorithm.
listAlgorithmsResponse
    :: Int -- ^ 'larsResponseStatus'
    -> ListAlgorithmsResponse
listAlgorithmsResponse pResponseStatus_ =
  ListAlgorithmsResponse'
    { _larsNextToken = Nothing
    , _larsResponseStatus = pResponseStatus_
    , _larsAlgorithmSummaryList = mempty
    }


-- | If the response is truncated, Amazon SageMaker returns this token. To retrieve the next set of algorithms, use it in the subsequent request.
larsNextToken :: Lens' ListAlgorithmsResponse (Maybe Text)
larsNextToken = lens _larsNextToken (\ s a -> s{_larsNextToken = a})

-- | -- | The response status code.
larsResponseStatus :: Lens' ListAlgorithmsResponse Int
larsResponseStatus = lens _larsResponseStatus (\ s a -> s{_larsResponseStatus = a})

-- | >An array of @AlgorithmSummary@ objects, each of which lists an algorithm.
larsAlgorithmSummaryList :: Lens' ListAlgorithmsResponse [AlgorithmSummary]
larsAlgorithmSummaryList = lens _larsAlgorithmSummaryList (\ s a -> s{_larsAlgorithmSummaryList = a}) . _Coerce

instance NFData ListAlgorithmsResponse where
