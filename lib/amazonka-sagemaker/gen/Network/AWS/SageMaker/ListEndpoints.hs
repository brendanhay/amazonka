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
-- Module      : Network.AWS.SageMaker.ListEndpoints
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists endpoints.
--
--
--
-- This operation returns paginated results.
module Network.AWS.SageMaker.ListEndpoints
    (
    -- * Creating a Request
      listEndpoints
    , ListEndpoints
    -- * Request Lenses
    , leNameContains
    , leLastModifiedTimeBefore
    , leCreationTimeAfter
    , leNextToken
    , leSortOrder
    , leLastModifiedTimeAfter
    , leCreationTimeBefore
    , leStatusEquals
    , leMaxResults
    , leSortBy

    -- * Destructuring the Response
    , listEndpointsResponse
    , ListEndpointsResponse
    -- * Response Lenses
    , lersNextToken
    , lersResponseStatus
    , lersEndpoints
    ) where

import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SageMaker.Types
import Network.AWS.SageMaker.Types.Product

-- | /See:/ 'listEndpoints' smart constructor.
data ListEndpoints = ListEndpoints'
  { _leNameContains           :: !(Maybe Text)
  , _leLastModifiedTimeBefore :: !(Maybe POSIX)
  , _leCreationTimeAfter      :: !(Maybe POSIX)
  , _leNextToken              :: !(Maybe Text)
  , _leSortOrder              :: !(Maybe OrderKey)
  , _leLastModifiedTimeAfter  :: !(Maybe POSIX)
  , _leCreationTimeBefore     :: !(Maybe POSIX)
  , _leStatusEquals           :: !(Maybe EndpointStatus)
  , _leMaxResults             :: !(Maybe Nat)
  , _leSortBy                 :: !(Maybe EndpointSortKey)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListEndpoints' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'leNameContains' - A string in endpoint names. This filter returns only endpoints whose name contains the specified string.
--
-- * 'leLastModifiedTimeBefore' - A filter that returns only endpoints that were modified before the specified timestamp.
--
-- * 'leCreationTimeAfter' - A filter that returns only endpoints that were created after the specified time (timestamp).
--
-- * 'leNextToken' - If the result of a @ListEndpoints@ request was truncated, the response includes a @NextToken@ . To retrieve the next set of endpoints, use the token in the next request.
--
-- * 'leSortOrder' - The sort order for results. The default is @Ascending@ .
--
-- * 'leLastModifiedTimeAfter' - A filter that returns only endpoints that were modified after the specified timestamp.
--
-- * 'leCreationTimeBefore' - A filter that returns only endpoints that were created before the specified time (timestamp).
--
-- * 'leStatusEquals' - A filter that returns only endpoints with the specified status.
--
-- * 'leMaxResults' - The maximum number of endpoints to return in the response.
--
-- * 'leSortBy' - Sorts the list of results. The default is @CreationTime@ .
listEndpoints
    :: ListEndpoints
listEndpoints =
  ListEndpoints'
    { _leNameContains = Nothing
    , _leLastModifiedTimeBefore = Nothing
    , _leCreationTimeAfter = Nothing
    , _leNextToken = Nothing
    , _leSortOrder = Nothing
    , _leLastModifiedTimeAfter = Nothing
    , _leCreationTimeBefore = Nothing
    , _leStatusEquals = Nothing
    , _leMaxResults = Nothing
    , _leSortBy = Nothing
    }


-- | A string in endpoint names. This filter returns only endpoints whose name contains the specified string.
leNameContains :: Lens' ListEndpoints (Maybe Text)
leNameContains = lens _leNameContains (\ s a -> s{_leNameContains = a})

-- | A filter that returns only endpoints that were modified before the specified timestamp.
leLastModifiedTimeBefore :: Lens' ListEndpoints (Maybe UTCTime)
leLastModifiedTimeBefore = lens _leLastModifiedTimeBefore (\ s a -> s{_leLastModifiedTimeBefore = a}) . mapping _Time

-- | A filter that returns only endpoints that were created after the specified time (timestamp).
leCreationTimeAfter :: Lens' ListEndpoints (Maybe UTCTime)
leCreationTimeAfter = lens _leCreationTimeAfter (\ s a -> s{_leCreationTimeAfter = a}) . mapping _Time

-- | If the result of a @ListEndpoints@ request was truncated, the response includes a @NextToken@ . To retrieve the next set of endpoints, use the token in the next request.
leNextToken :: Lens' ListEndpoints (Maybe Text)
leNextToken = lens _leNextToken (\ s a -> s{_leNextToken = a})

-- | The sort order for results. The default is @Ascending@ .
leSortOrder :: Lens' ListEndpoints (Maybe OrderKey)
leSortOrder = lens _leSortOrder (\ s a -> s{_leSortOrder = a})

-- | A filter that returns only endpoints that were modified after the specified timestamp.
leLastModifiedTimeAfter :: Lens' ListEndpoints (Maybe UTCTime)
leLastModifiedTimeAfter = lens _leLastModifiedTimeAfter (\ s a -> s{_leLastModifiedTimeAfter = a}) . mapping _Time

-- | A filter that returns only endpoints that were created before the specified time (timestamp).
leCreationTimeBefore :: Lens' ListEndpoints (Maybe UTCTime)
leCreationTimeBefore = lens _leCreationTimeBefore (\ s a -> s{_leCreationTimeBefore = a}) . mapping _Time

-- | A filter that returns only endpoints with the specified status.
leStatusEquals :: Lens' ListEndpoints (Maybe EndpointStatus)
leStatusEquals = lens _leStatusEquals (\ s a -> s{_leStatusEquals = a})

-- | The maximum number of endpoints to return in the response.
leMaxResults :: Lens' ListEndpoints (Maybe Natural)
leMaxResults = lens _leMaxResults (\ s a -> s{_leMaxResults = a}) . mapping _Nat

-- | Sorts the list of results. The default is @CreationTime@ .
leSortBy :: Lens' ListEndpoints (Maybe EndpointSortKey)
leSortBy = lens _leSortBy (\ s a -> s{_leSortBy = a})

instance AWSPager ListEndpoints where
        page rq rs
          | stop (rs ^. lersNextToken) = Nothing
          | stop (rs ^. lersEndpoints) = Nothing
          | otherwise =
            Just $ rq & leNextToken .~ rs ^. lersNextToken

instance AWSRequest ListEndpoints where
        type Rs ListEndpoints = ListEndpointsResponse
        request = postJSON sageMaker
        response
          = receiveJSON
              (\ s h x ->
                 ListEndpointsResponse' <$>
                   (x .?> "NextToken") <*> (pure (fromEnum s)) <*>
                     (x .?> "Endpoints" .!@ mempty))

instance Hashable ListEndpoints where

instance NFData ListEndpoints where

instance ToHeaders ListEndpoints where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("SageMaker.ListEndpoints" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListEndpoints where
        toJSON ListEndpoints'{..}
          = object
              (catMaybes
                 [("NameContains" .=) <$> _leNameContains,
                  ("LastModifiedTimeBefore" .=) <$>
                    _leLastModifiedTimeBefore,
                  ("CreationTimeAfter" .=) <$> _leCreationTimeAfter,
                  ("NextToken" .=) <$> _leNextToken,
                  ("SortOrder" .=) <$> _leSortOrder,
                  ("LastModifiedTimeAfter" .=) <$>
                    _leLastModifiedTimeAfter,
                  ("CreationTimeBefore" .=) <$> _leCreationTimeBefore,
                  ("StatusEquals" .=) <$> _leStatusEquals,
                  ("MaxResults" .=) <$> _leMaxResults,
                  ("SortBy" .=) <$> _leSortBy])

instance ToPath ListEndpoints where
        toPath = const "/"

instance ToQuery ListEndpoints where
        toQuery = const mempty

-- | /See:/ 'listEndpointsResponse' smart constructor.
data ListEndpointsResponse = ListEndpointsResponse'
  { _lersNextToken      :: !(Maybe Text)
  , _lersResponseStatus :: !Int
  , _lersEndpoints      :: ![EndpointSummary]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListEndpointsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lersNextToken' - If the response is truncated, Amazon SageMaker returns this token. To retrieve the next set of training jobs, use it in the subsequent request.
--
-- * 'lersResponseStatus' - -- | The response status code.
--
-- * 'lersEndpoints' - An array or endpoint objects.
listEndpointsResponse
    :: Int -- ^ 'lersResponseStatus'
    -> ListEndpointsResponse
listEndpointsResponse pResponseStatus_ =
  ListEndpointsResponse'
    { _lersNextToken = Nothing
    , _lersResponseStatus = pResponseStatus_
    , _lersEndpoints = mempty
    }


-- | If the response is truncated, Amazon SageMaker returns this token. To retrieve the next set of training jobs, use it in the subsequent request.
lersNextToken :: Lens' ListEndpointsResponse (Maybe Text)
lersNextToken = lens _lersNextToken (\ s a -> s{_lersNextToken = a})

-- | -- | The response status code.
lersResponseStatus :: Lens' ListEndpointsResponse Int
lersResponseStatus = lens _lersResponseStatus (\ s a -> s{_lersResponseStatus = a})

-- | An array or endpoint objects.
lersEndpoints :: Lens' ListEndpointsResponse [EndpointSummary]
lersEndpoints = lens _lersEndpoints (\ s a -> s{_lersEndpoints = a}) . _Coerce

instance NFData ListEndpointsResponse where
