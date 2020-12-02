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
-- Module      : Network.AWS.AWSHealth.DescribeEventTypes
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the event types that meet the specified filter criteria. If no filter criteria are specified, all event types are returned, in no particular order.
--
--
--
-- This operation returns paginated results.
module Network.AWS.AWSHealth.DescribeEventTypes
    (
    -- * Creating a Request
      describeEventTypes
    , DescribeEventTypes
    -- * Request Lenses
    , detLocale
    , detNextToken
    , detFilter
    , detMaxResults

    -- * Destructuring the Response
    , describeEventTypesResponse
    , DescribeEventTypesResponse
    -- * Response Lenses
    , detrsEventTypes
    , detrsNextToken
    , detrsResponseStatus
    ) where

import Network.AWS.AWSHealth.Types
import Network.AWS.AWSHealth.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeEventTypes' smart constructor.
data DescribeEventTypes = DescribeEventTypes'
  { _detLocale     :: !(Maybe Text)
  , _detNextToken  :: !(Maybe Text)
  , _detFilter     :: !(Maybe EventTypeFilter)
  , _detMaxResults :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeEventTypes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'detLocale' - The locale (language) to return information in. English (en) is the default and the only supported value at this time.
--
-- * 'detNextToken' - If the results of a search are large, only a portion of the results are returned, and a @nextToken@ pagination token is returned in the response. To retrieve the next batch of results, reissue the search request and include the returned token. When all results have been returned, the response does not contain a pagination token value.
--
-- * 'detFilter' - Values to narrow the results returned.
--
-- * 'detMaxResults' - The maximum number of items to return in one batch, between 10 and 100, inclusive.
describeEventTypes
    :: DescribeEventTypes
describeEventTypes =
  DescribeEventTypes'
    { _detLocale = Nothing
    , _detNextToken = Nothing
    , _detFilter = Nothing
    , _detMaxResults = Nothing
    }


-- | The locale (language) to return information in. English (en) is the default and the only supported value at this time.
detLocale :: Lens' DescribeEventTypes (Maybe Text)
detLocale = lens _detLocale (\ s a -> s{_detLocale = a})

-- | If the results of a search are large, only a portion of the results are returned, and a @nextToken@ pagination token is returned in the response. To retrieve the next batch of results, reissue the search request and include the returned token. When all results have been returned, the response does not contain a pagination token value.
detNextToken :: Lens' DescribeEventTypes (Maybe Text)
detNextToken = lens _detNextToken (\ s a -> s{_detNextToken = a})

-- | Values to narrow the results returned.
detFilter :: Lens' DescribeEventTypes (Maybe EventTypeFilter)
detFilter = lens _detFilter (\ s a -> s{_detFilter = a})

-- | The maximum number of items to return in one batch, between 10 and 100, inclusive.
detMaxResults :: Lens' DescribeEventTypes (Maybe Natural)
detMaxResults = lens _detMaxResults (\ s a -> s{_detMaxResults = a}) . mapping _Nat

instance AWSPager DescribeEventTypes where
        page rq rs
          | stop (rs ^. detrsNextToken) = Nothing
          | stop (rs ^. detrsEventTypes) = Nothing
          | otherwise =
            Just $ rq & detNextToken .~ rs ^. detrsNextToken

instance AWSRequest DescribeEventTypes where
        type Rs DescribeEventTypes =
             DescribeEventTypesResponse
        request = postJSON awsHealth
        response
          = receiveJSON
              (\ s h x ->
                 DescribeEventTypesResponse' <$>
                   (x .?> "eventTypes" .!@ mempty) <*>
                     (x .?> "nextToken")
                     <*> (pure (fromEnum s)))

instance Hashable DescribeEventTypes where

instance NFData DescribeEventTypes where

instance ToHeaders DescribeEventTypes where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSHealth_20160804.DescribeEventTypes" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeEventTypes where
        toJSON DescribeEventTypes'{..}
          = object
              (catMaybes
                 [("locale" .=) <$> _detLocale,
                  ("nextToken" .=) <$> _detNextToken,
                  ("filter" .=) <$> _detFilter,
                  ("maxResults" .=) <$> _detMaxResults])

instance ToPath DescribeEventTypes where
        toPath = const "/"

instance ToQuery DescribeEventTypes where
        toQuery = const mempty

-- | /See:/ 'describeEventTypesResponse' smart constructor.
data DescribeEventTypesResponse = DescribeEventTypesResponse'
  { _detrsEventTypes     :: !(Maybe [EventType])
  , _detrsNextToken      :: !(Maybe Text)
  , _detrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeEventTypesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'detrsEventTypes' - A list of event types that match the filter criteria. Event types have a category (@issue@ , @accountNotification@ , or @scheduledChange@ ), a service (for example, @EC2@ , @RDS@ , @DATAPIPELINE@ , @BILLING@ ), and a code (in the format @AWS_/SERVICE/ _/DESCRIPTION/ @ ; for example, @AWS_EC2_SYSTEM_MAINTENANCE_EVENT@ ).
--
-- * 'detrsNextToken' - If the results of a search are large, only a portion of the results are returned, and a @nextToken@ pagination token is returned in the response. To retrieve the next batch of results, reissue the search request and include the returned token. When all results have been returned, the response does not contain a pagination token value.
--
-- * 'detrsResponseStatus' - -- | The response status code.
describeEventTypesResponse
    :: Int -- ^ 'detrsResponseStatus'
    -> DescribeEventTypesResponse
describeEventTypesResponse pResponseStatus_ =
  DescribeEventTypesResponse'
    { _detrsEventTypes = Nothing
    , _detrsNextToken = Nothing
    , _detrsResponseStatus = pResponseStatus_
    }


-- | A list of event types that match the filter criteria. Event types have a category (@issue@ , @accountNotification@ , or @scheduledChange@ ), a service (for example, @EC2@ , @RDS@ , @DATAPIPELINE@ , @BILLING@ ), and a code (in the format @AWS_/SERVICE/ _/DESCRIPTION/ @ ; for example, @AWS_EC2_SYSTEM_MAINTENANCE_EVENT@ ).
detrsEventTypes :: Lens' DescribeEventTypesResponse [EventType]
detrsEventTypes = lens _detrsEventTypes (\ s a -> s{_detrsEventTypes = a}) . _Default . _Coerce

-- | If the results of a search are large, only a portion of the results are returned, and a @nextToken@ pagination token is returned in the response. To retrieve the next batch of results, reissue the search request and include the returned token. When all results have been returned, the response does not contain a pagination token value.
detrsNextToken :: Lens' DescribeEventTypesResponse (Maybe Text)
detrsNextToken = lens _detrsNextToken (\ s a -> s{_detrsNextToken = a})

-- | -- | The response status code.
detrsResponseStatus :: Lens' DescribeEventTypesResponse Int
detrsResponseStatus = lens _detrsResponseStatus (\ s a -> s{_detrsResponseStatus = a})

instance NFData DescribeEventTypesResponse where
