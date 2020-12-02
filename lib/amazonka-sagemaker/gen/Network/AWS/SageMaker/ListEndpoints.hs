{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.ListEndpoints
-- Copyright   : (c) 2013-2020 Brendan Hay
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
  ( -- * Creating a Request
    listEndpoints,
    ListEndpoints,

    -- * Request Lenses
    lesNameContains,
    lesLastModifiedTimeBefore,
    lesCreationTimeAfter,
    lesNextToken,
    lesSortOrder,
    lesLastModifiedTimeAfter,
    lesCreationTimeBefore,
    lesStatusEquals,
    lesMaxResults,
    lesSortBy,

    -- * Destructuring the Response
    listEndpointsResponse,
    ListEndpointsResponse,

    -- * Response Lenses
    lesrsNextToken,
    lesrsResponseStatus,
    lesrsEndpoints,
  )
where

import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'listEndpoints' smart constructor.
data ListEndpoints = ListEndpoints'
  { _lesNameContains ::
      !(Maybe Text),
    _lesLastModifiedTimeBefore :: !(Maybe POSIX),
    _lesCreationTimeAfter :: !(Maybe POSIX),
    _lesNextToken :: !(Maybe Text),
    _lesSortOrder :: !(Maybe OrderKey),
    _lesLastModifiedTimeAfter :: !(Maybe POSIX),
    _lesCreationTimeBefore :: !(Maybe POSIX),
    _lesStatusEquals :: !(Maybe EndpointStatus),
    _lesMaxResults :: !(Maybe Nat),
    _lesSortBy :: !(Maybe EndpointSortKey)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListEndpoints' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lesNameContains' - A string in endpoint names. This filter returns only endpoints whose name contains the specified string.
--
-- * 'lesLastModifiedTimeBefore' - A filter that returns only endpoints that were modified before the specified timestamp.
--
-- * 'lesCreationTimeAfter' - A filter that returns only endpoints with a creation time greater than or equal to the specified time (timestamp).
--
-- * 'lesNextToken' - If the result of a @ListEndpoints@ request was truncated, the response includes a @NextToken@ . To retrieve the next set of endpoints, use the token in the next request.
--
-- * 'lesSortOrder' - The sort order for results. The default is @Descending@ .
--
-- * 'lesLastModifiedTimeAfter' - A filter that returns only endpoints that were modified after the specified timestamp.
--
-- * 'lesCreationTimeBefore' - A filter that returns only endpoints that were created before the specified time (timestamp).
--
-- * 'lesStatusEquals' - A filter that returns only endpoints with the specified status.
--
-- * 'lesMaxResults' - The maximum number of endpoints to return in the response.
--
-- * 'lesSortBy' - Sorts the list of results. The default is @CreationTime@ .
listEndpoints ::
  ListEndpoints
listEndpoints =
  ListEndpoints'
    { _lesNameContains = Nothing,
      _lesLastModifiedTimeBefore = Nothing,
      _lesCreationTimeAfter = Nothing,
      _lesNextToken = Nothing,
      _lesSortOrder = Nothing,
      _lesLastModifiedTimeAfter = Nothing,
      _lesCreationTimeBefore = Nothing,
      _lesStatusEquals = Nothing,
      _lesMaxResults = Nothing,
      _lesSortBy = Nothing
    }

-- | A string in endpoint names. This filter returns only endpoints whose name contains the specified string.
lesNameContains :: Lens' ListEndpoints (Maybe Text)
lesNameContains = lens _lesNameContains (\s a -> s {_lesNameContains = a})

-- | A filter that returns only endpoints that were modified before the specified timestamp.
lesLastModifiedTimeBefore :: Lens' ListEndpoints (Maybe UTCTime)
lesLastModifiedTimeBefore = lens _lesLastModifiedTimeBefore (\s a -> s {_lesLastModifiedTimeBefore = a}) . mapping _Time

-- | A filter that returns only endpoints with a creation time greater than or equal to the specified time (timestamp).
lesCreationTimeAfter :: Lens' ListEndpoints (Maybe UTCTime)
lesCreationTimeAfter = lens _lesCreationTimeAfter (\s a -> s {_lesCreationTimeAfter = a}) . mapping _Time

-- | If the result of a @ListEndpoints@ request was truncated, the response includes a @NextToken@ . To retrieve the next set of endpoints, use the token in the next request.
lesNextToken :: Lens' ListEndpoints (Maybe Text)
lesNextToken = lens _lesNextToken (\s a -> s {_lesNextToken = a})

-- | The sort order for results. The default is @Descending@ .
lesSortOrder :: Lens' ListEndpoints (Maybe OrderKey)
lesSortOrder = lens _lesSortOrder (\s a -> s {_lesSortOrder = a})

-- | A filter that returns only endpoints that were modified after the specified timestamp.
lesLastModifiedTimeAfter :: Lens' ListEndpoints (Maybe UTCTime)
lesLastModifiedTimeAfter = lens _lesLastModifiedTimeAfter (\s a -> s {_lesLastModifiedTimeAfter = a}) . mapping _Time

-- | A filter that returns only endpoints that were created before the specified time (timestamp).
lesCreationTimeBefore :: Lens' ListEndpoints (Maybe UTCTime)
lesCreationTimeBefore = lens _lesCreationTimeBefore (\s a -> s {_lesCreationTimeBefore = a}) . mapping _Time

-- | A filter that returns only endpoints with the specified status.
lesStatusEquals :: Lens' ListEndpoints (Maybe EndpointStatus)
lesStatusEquals = lens _lesStatusEquals (\s a -> s {_lesStatusEquals = a})

-- | The maximum number of endpoints to return in the response.
lesMaxResults :: Lens' ListEndpoints (Maybe Natural)
lesMaxResults = lens _lesMaxResults (\s a -> s {_lesMaxResults = a}) . mapping _Nat

-- | Sorts the list of results. The default is @CreationTime@ .
lesSortBy :: Lens' ListEndpoints (Maybe EndpointSortKey)
lesSortBy = lens _lesSortBy (\s a -> s {_lesSortBy = a})

instance AWSPager ListEndpoints where
  page rq rs
    | stop (rs ^. lesrsNextToken) = Nothing
    | stop (rs ^. lesrsEndpoints) = Nothing
    | otherwise = Just $ rq & lesNextToken .~ rs ^. lesrsNextToken

instance AWSRequest ListEndpoints where
  type Rs ListEndpoints = ListEndpointsResponse
  request = postJSON sageMaker
  response =
    receiveJSON
      ( \s h x ->
          ListEndpointsResponse'
            <$> (x .?> "NextToken")
            <*> (pure (fromEnum s))
            <*> (x .?> "Endpoints" .!@ mempty)
      )

instance Hashable ListEndpoints

instance NFData ListEndpoints

instance ToHeaders ListEndpoints where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target" =# ("SageMaker.ListEndpoints" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON ListEndpoints where
  toJSON ListEndpoints' {..} =
    object
      ( catMaybes
          [ ("NameContains" .=) <$> _lesNameContains,
            ("LastModifiedTimeBefore" .=) <$> _lesLastModifiedTimeBefore,
            ("CreationTimeAfter" .=) <$> _lesCreationTimeAfter,
            ("NextToken" .=) <$> _lesNextToken,
            ("SortOrder" .=) <$> _lesSortOrder,
            ("LastModifiedTimeAfter" .=) <$> _lesLastModifiedTimeAfter,
            ("CreationTimeBefore" .=) <$> _lesCreationTimeBefore,
            ("StatusEquals" .=) <$> _lesStatusEquals,
            ("MaxResults" .=) <$> _lesMaxResults,
            ("SortBy" .=) <$> _lesSortBy
          ]
      )

instance ToPath ListEndpoints where
  toPath = const "/"

instance ToQuery ListEndpoints where
  toQuery = const mempty

-- | /See:/ 'listEndpointsResponse' smart constructor.
data ListEndpointsResponse = ListEndpointsResponse'
  { _lesrsNextToken ::
      !(Maybe Text),
    _lesrsResponseStatus :: !Int,
    _lesrsEndpoints :: ![EndpointSummary]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListEndpointsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lesrsNextToken' - If the response is truncated, Amazon SageMaker returns this token. To retrieve the next set of training jobs, use it in the subsequent request.
--
-- * 'lesrsResponseStatus' - -- | The response status code.
--
-- * 'lesrsEndpoints' - An array or endpoint objects.
listEndpointsResponse ::
  -- | 'lesrsResponseStatus'
  Int ->
  ListEndpointsResponse
listEndpointsResponse pResponseStatus_ =
  ListEndpointsResponse'
    { _lesrsNextToken = Nothing,
      _lesrsResponseStatus = pResponseStatus_,
      _lesrsEndpoints = mempty
    }

-- | If the response is truncated, Amazon SageMaker returns this token. To retrieve the next set of training jobs, use it in the subsequent request.
lesrsNextToken :: Lens' ListEndpointsResponse (Maybe Text)
lesrsNextToken = lens _lesrsNextToken (\s a -> s {_lesrsNextToken = a})

-- | -- | The response status code.
lesrsResponseStatus :: Lens' ListEndpointsResponse Int
lesrsResponseStatus = lens _lesrsResponseStatus (\s a -> s {_lesrsResponseStatus = a})

-- | An array or endpoint objects.
lesrsEndpoints :: Lens' ListEndpointsResponse [EndpointSummary]
lesrsEndpoints = lens _lesrsEndpoints (\s a -> s {_lesrsEndpoints = a}) . _Coerce

instance NFData ListEndpointsResponse
