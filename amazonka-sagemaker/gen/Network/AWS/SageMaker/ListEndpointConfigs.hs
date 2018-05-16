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
-- Module      : Network.AWS.SageMaker.ListEndpointConfigs
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists endpoint configurations.
--
--
--
-- This operation returns paginated results.
module Network.AWS.SageMaker.ListEndpointConfigs
    (
    -- * Creating a Request
      listEndpointConfigs
    , ListEndpointConfigs
    -- * Request Lenses
    , lecNameContains
    , lecCreationTimeAfter
    , lecNextToken
    , lecSortOrder
    , lecCreationTimeBefore
    , lecMaxResults
    , lecSortBy

    -- * Destructuring the Response
    , listEndpointConfigsResponse
    , ListEndpointConfigsResponse
    -- * Response Lenses
    , lecrsNextToken
    , lecrsResponseStatus
    , lecrsEndpointConfigs
    ) where

import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SageMaker.Types
import Network.AWS.SageMaker.Types.Product

-- | /See:/ 'listEndpointConfigs' smart constructor.
data ListEndpointConfigs = ListEndpointConfigs'
  { _lecNameContains       :: !(Maybe Text)
  , _lecCreationTimeAfter  :: !(Maybe POSIX)
  , _lecNextToken          :: !(Maybe Text)
  , _lecSortOrder          :: !(Maybe OrderKey)
  , _lecCreationTimeBefore :: !(Maybe POSIX)
  , _lecMaxResults         :: !(Maybe Nat)
  , _lecSortBy             :: !(Maybe EndpointConfigSortKey)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListEndpointConfigs' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lecNameContains' - A string in the endpoint configuration name. This filter returns only endpoint configurations whose name contains the specified string.
--
-- * 'lecCreationTimeAfter' - A filter that returns only endpoint configurations created after the specified time (timestamp).
--
-- * 'lecNextToken' - If the result of the previous @ListEndpointConfig@ request was truncated, the response includes a @NextToken@ . To retrieve the next set of endpoint configurations, use the token in the next request.
--
-- * 'lecSortOrder' - The sort order for results. The default is @Ascending@ .
--
-- * 'lecCreationTimeBefore' - A filter that returns only endpoint configurations created before the specified time (timestamp).
--
-- * 'lecMaxResults' - The maximum number of training jobs to return in the response.
--
-- * 'lecSortBy' - The field to sort results by. The default is @CreationTime@ .
listEndpointConfigs
    :: ListEndpointConfigs
listEndpointConfigs =
  ListEndpointConfigs'
    { _lecNameContains = Nothing
    , _lecCreationTimeAfter = Nothing
    , _lecNextToken = Nothing
    , _lecSortOrder = Nothing
    , _lecCreationTimeBefore = Nothing
    , _lecMaxResults = Nothing
    , _lecSortBy = Nothing
    }


-- | A string in the endpoint configuration name. This filter returns only endpoint configurations whose name contains the specified string.
lecNameContains :: Lens' ListEndpointConfigs (Maybe Text)
lecNameContains = lens _lecNameContains (\ s a -> s{_lecNameContains = a})

-- | A filter that returns only endpoint configurations created after the specified time (timestamp).
lecCreationTimeAfter :: Lens' ListEndpointConfigs (Maybe UTCTime)
lecCreationTimeAfter = lens _lecCreationTimeAfter (\ s a -> s{_lecCreationTimeAfter = a}) . mapping _Time

-- | If the result of the previous @ListEndpointConfig@ request was truncated, the response includes a @NextToken@ . To retrieve the next set of endpoint configurations, use the token in the next request.
lecNextToken :: Lens' ListEndpointConfigs (Maybe Text)
lecNextToken = lens _lecNextToken (\ s a -> s{_lecNextToken = a})

-- | The sort order for results. The default is @Ascending@ .
lecSortOrder :: Lens' ListEndpointConfigs (Maybe OrderKey)
lecSortOrder = lens _lecSortOrder (\ s a -> s{_lecSortOrder = a})

-- | A filter that returns only endpoint configurations created before the specified time (timestamp).
lecCreationTimeBefore :: Lens' ListEndpointConfigs (Maybe UTCTime)
lecCreationTimeBefore = lens _lecCreationTimeBefore (\ s a -> s{_lecCreationTimeBefore = a}) . mapping _Time

-- | The maximum number of training jobs to return in the response.
lecMaxResults :: Lens' ListEndpointConfigs (Maybe Natural)
lecMaxResults = lens _lecMaxResults (\ s a -> s{_lecMaxResults = a}) . mapping _Nat

-- | The field to sort results by. The default is @CreationTime@ .
lecSortBy :: Lens' ListEndpointConfigs (Maybe EndpointConfigSortKey)
lecSortBy = lens _lecSortBy (\ s a -> s{_lecSortBy = a})

instance AWSPager ListEndpointConfigs where
        page rq rs
          | stop (rs ^. lecrsNextToken) = Nothing
          | stop (rs ^. lecrsEndpointConfigs) = Nothing
          | otherwise =
            Just $ rq & lecNextToken .~ rs ^. lecrsNextToken

instance AWSRequest ListEndpointConfigs where
        type Rs ListEndpointConfigs =
             ListEndpointConfigsResponse
        request = postJSON sageMaker
        response
          = receiveJSON
              (\ s h x ->
                 ListEndpointConfigsResponse' <$>
                   (x .?> "NextToken") <*> (pure (fromEnum s)) <*>
                     (x .?> "EndpointConfigs" .!@ mempty))

instance Hashable ListEndpointConfigs where

instance NFData ListEndpointConfigs where

instance ToHeaders ListEndpointConfigs where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("SageMaker.ListEndpointConfigs" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListEndpointConfigs where
        toJSON ListEndpointConfigs'{..}
          = object
              (catMaybes
                 [("NameContains" .=) <$> _lecNameContains,
                  ("CreationTimeAfter" .=) <$> _lecCreationTimeAfter,
                  ("NextToken" .=) <$> _lecNextToken,
                  ("SortOrder" .=) <$> _lecSortOrder,
                  ("CreationTimeBefore" .=) <$> _lecCreationTimeBefore,
                  ("MaxResults" .=) <$> _lecMaxResults,
                  ("SortBy" .=) <$> _lecSortBy])

instance ToPath ListEndpointConfigs where
        toPath = const "/"

instance ToQuery ListEndpointConfigs where
        toQuery = const mempty

-- | /See:/ 'listEndpointConfigsResponse' smart constructor.
data ListEndpointConfigsResponse = ListEndpointConfigsResponse'
  { _lecrsNextToken       :: !(Maybe Text)
  , _lecrsResponseStatus  :: !Int
  , _lecrsEndpointConfigs :: ![EndpointConfigSummary]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListEndpointConfigsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lecrsNextToken' - If the response is truncated, Amazon SageMaker returns this token. To retrieve the next set of endpoint configurations, use it in the subsequent request
--
-- * 'lecrsResponseStatus' - -- | The response status code.
--
-- * 'lecrsEndpointConfigs' - An array of endpoint configurations.
listEndpointConfigsResponse
    :: Int -- ^ 'lecrsResponseStatus'
    -> ListEndpointConfigsResponse
listEndpointConfigsResponse pResponseStatus_ =
  ListEndpointConfigsResponse'
    { _lecrsNextToken = Nothing
    , _lecrsResponseStatus = pResponseStatus_
    , _lecrsEndpointConfigs = mempty
    }


-- | If the response is truncated, Amazon SageMaker returns this token. To retrieve the next set of endpoint configurations, use it in the subsequent request
lecrsNextToken :: Lens' ListEndpointConfigsResponse (Maybe Text)
lecrsNextToken = lens _lecrsNextToken (\ s a -> s{_lecrsNextToken = a})

-- | -- | The response status code.
lecrsResponseStatus :: Lens' ListEndpointConfigsResponse Int
lecrsResponseStatus = lens _lecrsResponseStatus (\ s a -> s{_lecrsResponseStatus = a})

-- | An array of endpoint configurations.
lecrsEndpointConfigs :: Lens' ListEndpointConfigsResponse [EndpointConfigSummary]
lecrsEndpointConfigs = lens _lecrsEndpointConfigs (\ s a -> s{_lecrsEndpointConfigs = a}) . _Coerce

instance NFData ListEndpointConfigsResponse where
