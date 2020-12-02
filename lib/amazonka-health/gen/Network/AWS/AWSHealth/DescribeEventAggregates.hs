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
-- Module      : Network.AWS.AWSHealth.DescribeEventAggregates
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the number of events of each event type (issue, scheduled change, and account notification). If no filter is specified, the counts of all events in each category are returned.
--
--
--
-- This operation returns paginated results.
module Network.AWS.AWSHealth.DescribeEventAggregates
    (
    -- * Creating a Request
      describeEventAggregates
    , DescribeEventAggregates
    -- * Request Lenses
    , deaNextToken
    , deaFilter
    , deaMaxResults
    , deaAggregateField

    -- * Destructuring the Response
    , describeEventAggregatesResponse
    , DescribeEventAggregatesResponse
    -- * Response Lenses
    , drsNextToken
    , drsEventAggregates
    , drsResponseStatus
    ) where

import Network.AWS.AWSHealth.Types
import Network.AWS.AWSHealth.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeEventAggregates' smart constructor.
data DescribeEventAggregates = DescribeEventAggregates'
  { _deaNextToken      :: !(Maybe Text)
  , _deaFilter         :: !(Maybe EventFilter)
  , _deaMaxResults     :: !(Maybe Nat)
  , _deaAggregateField :: !EventAggregateField
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeEventAggregates' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'deaNextToken' - If the results of a search are large, only a portion of the results are returned, and a @nextToken@ pagination token is returned in the response. To retrieve the next batch of results, reissue the search request and include the returned token. When all results have been returned, the response does not contain a pagination token value.
--
-- * 'deaFilter' - Values to narrow the results returned.
--
-- * 'deaMaxResults' - The maximum number of items to return in one batch, between 10 and 100, inclusive.
--
-- * 'deaAggregateField' - The only currently supported value is @eventTypeCategory@ .
describeEventAggregates
    :: EventAggregateField -- ^ 'deaAggregateField'
    -> DescribeEventAggregates
describeEventAggregates pAggregateField_ =
  DescribeEventAggregates'
    { _deaNextToken = Nothing
    , _deaFilter = Nothing
    , _deaMaxResults = Nothing
    , _deaAggregateField = pAggregateField_
    }


-- | If the results of a search are large, only a portion of the results are returned, and a @nextToken@ pagination token is returned in the response. To retrieve the next batch of results, reissue the search request and include the returned token. When all results have been returned, the response does not contain a pagination token value.
deaNextToken :: Lens' DescribeEventAggregates (Maybe Text)
deaNextToken = lens _deaNextToken (\ s a -> s{_deaNextToken = a})

-- | Values to narrow the results returned.
deaFilter :: Lens' DescribeEventAggregates (Maybe EventFilter)
deaFilter = lens _deaFilter (\ s a -> s{_deaFilter = a})

-- | The maximum number of items to return in one batch, between 10 and 100, inclusive.
deaMaxResults :: Lens' DescribeEventAggregates (Maybe Natural)
deaMaxResults = lens _deaMaxResults (\ s a -> s{_deaMaxResults = a}) . mapping _Nat

-- | The only currently supported value is @eventTypeCategory@ .
deaAggregateField :: Lens' DescribeEventAggregates EventAggregateField
deaAggregateField = lens _deaAggregateField (\ s a -> s{_deaAggregateField = a})

instance AWSPager DescribeEventAggregates where
        page rq rs
          | stop (rs ^. drsNextToken) = Nothing
          | stop (rs ^. drsEventAggregates) = Nothing
          | otherwise =
            Just $ rq & deaNextToken .~ rs ^. drsNextToken

instance AWSRequest DescribeEventAggregates where
        type Rs DescribeEventAggregates =
             DescribeEventAggregatesResponse
        request = postJSON awsHealth
        response
          = receiveJSON
              (\ s h x ->
                 DescribeEventAggregatesResponse' <$>
                   (x .?> "nextToken") <*>
                     (x .?> "eventAggregates" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable DescribeEventAggregates where

instance NFData DescribeEventAggregates where

instance ToHeaders DescribeEventAggregates where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSHealth_20160804.DescribeEventAggregates" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeEventAggregates where
        toJSON DescribeEventAggregates'{..}
          = object
              (catMaybes
                 [("nextToken" .=) <$> _deaNextToken,
                  ("filter" .=) <$> _deaFilter,
                  ("maxResults" .=) <$> _deaMaxResults,
                  Just ("aggregateField" .= _deaAggregateField)])

instance ToPath DescribeEventAggregates where
        toPath = const "/"

instance ToQuery DescribeEventAggregates where
        toQuery = const mempty

-- | /See:/ 'describeEventAggregatesResponse' smart constructor.
data DescribeEventAggregatesResponse = DescribeEventAggregatesResponse'
  { _drsNextToken       :: !(Maybe Text)
  , _drsEventAggregates :: !(Maybe [EventAggregate])
  , _drsResponseStatus  :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeEventAggregatesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drsNextToken' - If the results of a search are large, only a portion of the results are returned, and a @nextToken@ pagination token is returned in the response. To retrieve the next batch of results, reissue the search request and include the returned token. When all results have been returned, the response does not contain a pagination token value.
--
-- * 'drsEventAggregates' - The number of events in each category that meet the optional filter criteria.
--
-- * 'drsResponseStatus' - -- | The response status code.
describeEventAggregatesResponse
    :: Int -- ^ 'drsResponseStatus'
    -> DescribeEventAggregatesResponse
describeEventAggregatesResponse pResponseStatus_ =
  DescribeEventAggregatesResponse'
    { _drsNextToken = Nothing
    , _drsEventAggregates = Nothing
    , _drsResponseStatus = pResponseStatus_
    }


-- | If the results of a search are large, only a portion of the results are returned, and a @nextToken@ pagination token is returned in the response. To retrieve the next batch of results, reissue the search request and include the returned token. When all results have been returned, the response does not contain a pagination token value.
drsNextToken :: Lens' DescribeEventAggregatesResponse (Maybe Text)
drsNextToken = lens _drsNextToken (\ s a -> s{_drsNextToken = a})

-- | The number of events in each category that meet the optional filter criteria.
drsEventAggregates :: Lens' DescribeEventAggregatesResponse [EventAggregate]
drsEventAggregates = lens _drsEventAggregates (\ s a -> s{_drsEventAggregates = a}) . _Default . _Coerce

-- | -- | The response status code.
drsResponseStatus :: Lens' DescribeEventAggregatesResponse Int
drsResponseStatus = lens _drsResponseStatus (\ s a -> s{_drsResponseStatus = a})

instance NFData DescribeEventAggregatesResponse where
