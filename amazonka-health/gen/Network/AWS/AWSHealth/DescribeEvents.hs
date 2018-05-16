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
-- Module      : Network.AWS.AWSHealth.DescribeEvents
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about events that meet the specified filter criteria. Events are returned in a summary form and do not include the detailed description, any additional metadata that depends on the event type, or any affected resources. To retrieve that information, use the 'DescribeEventDetails' and 'DescribeAffectedEntities' operations.
--
--
-- If no filter criteria are specified, all events are returned. Results are sorted by @lastModifiedTime@ , starting with the most recent.
--
--
-- This operation returns paginated results.
module Network.AWS.AWSHealth.DescribeEvents
    (
    -- * Creating a Request
      describeEvents
    , DescribeEvents
    -- * Request Lenses
    , deLocale
    , deNextToken
    , deFilter
    , deMaxResults

    -- * Destructuring the Response
    , describeEventsResponse
    , DescribeEventsResponse
    -- * Response Lenses
    , dersNextToken
    , dersEvents
    , dersResponseStatus
    ) where

import Network.AWS.AWSHealth.Types
import Network.AWS.AWSHealth.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeEvents' smart constructor.
data DescribeEvents = DescribeEvents'
  { _deLocale     :: !(Maybe Text)
  , _deNextToken  :: !(Maybe Text)
  , _deFilter     :: !(Maybe EventFilter)
  , _deMaxResults :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeEvents' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'deLocale' - The locale (language) to return information in. English (en) is the default and the only supported value at this time.
--
-- * 'deNextToken' - If the results of a search are large, only a portion of the results are returned, and a @nextToken@ pagination token is returned in the response. To retrieve the next batch of results, reissue the search request and include the returned token. When all results have been returned, the response does not contain a pagination token value.
--
-- * 'deFilter' - Values to narrow the results returned.
--
-- * 'deMaxResults' - The maximum number of items to return in one batch, between 10 and 100, inclusive.
describeEvents
    :: DescribeEvents
describeEvents =
  DescribeEvents'
    { _deLocale = Nothing
    , _deNextToken = Nothing
    , _deFilter = Nothing
    , _deMaxResults = Nothing
    }


-- | The locale (language) to return information in. English (en) is the default and the only supported value at this time.
deLocale :: Lens' DescribeEvents (Maybe Text)
deLocale = lens _deLocale (\ s a -> s{_deLocale = a})

-- | If the results of a search are large, only a portion of the results are returned, and a @nextToken@ pagination token is returned in the response. To retrieve the next batch of results, reissue the search request and include the returned token. When all results have been returned, the response does not contain a pagination token value.
deNextToken :: Lens' DescribeEvents (Maybe Text)
deNextToken = lens _deNextToken (\ s a -> s{_deNextToken = a})

-- | Values to narrow the results returned.
deFilter :: Lens' DescribeEvents (Maybe EventFilter)
deFilter = lens _deFilter (\ s a -> s{_deFilter = a})

-- | The maximum number of items to return in one batch, between 10 and 100, inclusive.
deMaxResults :: Lens' DescribeEvents (Maybe Natural)
deMaxResults = lens _deMaxResults (\ s a -> s{_deMaxResults = a}) . mapping _Nat

instance AWSPager DescribeEvents where
        page rq rs
          | stop (rs ^. dersNextToken) = Nothing
          | stop (rs ^. dersEvents) = Nothing
          | otherwise =
            Just $ rq & deNextToken .~ rs ^. dersNextToken

instance AWSRequest DescribeEvents where
        type Rs DescribeEvents = DescribeEventsResponse
        request = postJSON awsHealth
        response
          = receiveJSON
              (\ s h x ->
                 DescribeEventsResponse' <$>
                   (x .?> "nextToken") <*> (x .?> "events" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable DescribeEvents where

instance NFData DescribeEvents where

instance ToHeaders DescribeEvents where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSHealth_20160804.DescribeEvents" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeEvents where
        toJSON DescribeEvents'{..}
          = object
              (catMaybes
                 [("locale" .=) <$> _deLocale,
                  ("nextToken" .=) <$> _deNextToken,
                  ("filter" .=) <$> _deFilter,
                  ("maxResults" .=) <$> _deMaxResults])

instance ToPath DescribeEvents where
        toPath = const "/"

instance ToQuery DescribeEvents where
        toQuery = const mempty

-- | /See:/ 'describeEventsResponse' smart constructor.
data DescribeEventsResponse = DescribeEventsResponse'
  { _dersNextToken      :: !(Maybe Text)
  , _dersEvents         :: !(Maybe [Event])
  , _dersResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeEventsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dersNextToken' - If the results of a search are large, only a portion of the results are returned, and a @nextToken@ pagination token is returned in the response. To retrieve the next batch of results, reissue the search request and include the returned token. When all results have been returned, the response does not contain a pagination token value.
--
-- * 'dersEvents' - The events that match the specified filter criteria.
--
-- * 'dersResponseStatus' - -- | The response status code.
describeEventsResponse
    :: Int -- ^ 'dersResponseStatus'
    -> DescribeEventsResponse
describeEventsResponse pResponseStatus_ =
  DescribeEventsResponse'
    { _dersNextToken = Nothing
    , _dersEvents = Nothing
    , _dersResponseStatus = pResponseStatus_
    }


-- | If the results of a search are large, only a portion of the results are returned, and a @nextToken@ pagination token is returned in the response. To retrieve the next batch of results, reissue the search request and include the returned token. When all results have been returned, the response does not contain a pagination token value.
dersNextToken :: Lens' DescribeEventsResponse (Maybe Text)
dersNextToken = lens _dersNextToken (\ s a -> s{_dersNextToken = a})

-- | The events that match the specified filter criteria.
dersEvents :: Lens' DescribeEventsResponse [Event]
dersEvents = lens _dersEvents (\ s a -> s{_dersEvents = a}) . _Default . _Coerce

-- | -- | The response status code.
dersResponseStatus :: Lens' DescribeEventsResponse Int
dersResponseStatus = lens _dersResponseStatus (\ s a -> s{_dersResponseStatus = a})

instance NFData DescribeEventsResponse where
