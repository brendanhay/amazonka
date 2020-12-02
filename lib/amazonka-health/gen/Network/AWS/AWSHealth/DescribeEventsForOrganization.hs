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
-- Module      : Network.AWS.AWSHealth.DescribeEventsForOrganization
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about events across your organization in AWS Organizations. You can use the@filters@ parameter to specify the events that you want to return. Events are returned in a summary form and don't include the affected accounts, detailed description, any additional metadata that depends on the event type, or any affected resources. To retrieve that information, use the following operations:
--
--
--     * <https://docs.aws.amazon.com/health/latest/APIReference/API_DescribeAffectedAccountsForOrganization.html DescribeAffectedAccountsForOrganization>
--
--     * <https://docs.aws.amazon.com/health/latest/APIReference/API_DescribeEventDetailsForOrganization.html DescribeEventDetailsForOrganization>
--
--     * <https://docs.aws.amazon.com/health/latest/APIReference/API_DescribeAffectedEntitiesForOrganization.html DescribeAffectedEntitiesForOrganization>
--
--
--
-- If you don't specify a @filter@ , the @DescribeEventsForOrganizations@ returns all events across your organization. Results are sorted by @lastModifiedTime@ , starting with the most recent event.
--
-- For more information about the different types of AWS Health events, see <https://docs.aws.amazon.com/health/latest/APIReference/API_Event.html Event> .
--
-- Before you can call this operation, you must first enable AWS Health to work with AWS Organizations. To do this, call the <https://docs.aws.amazon.com/health/latest/APIReference/API_EnableHealthServiceAccessForOrganization.html EnableHealthServiceAccessForOrganization> operation from your organization's master AWS account.
--
--
-- This operation returns paginated results.
module Network.AWS.AWSHealth.DescribeEventsForOrganization
  ( -- * Creating a Request
    describeEventsForOrganization,
    DescribeEventsForOrganization,

    -- * Request Lenses
    defoLocale,
    defoNextToken,
    defoFilter,
    defoMaxResults,

    -- * Destructuring the Response
    describeEventsForOrganizationResponse,
    DescribeEventsForOrganizationResponse,

    -- * Response Lenses
    deforsNextToken,
    deforsEvents,
    deforsResponseStatus,
  )
where

import Network.AWS.AWSHealth.Types
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeEventsForOrganization' smart constructor.
data DescribeEventsForOrganization = DescribeEventsForOrganization'
  { _defoLocale ::
      !(Maybe Text),
    _defoNextToken :: !(Maybe Text),
    _defoFilter ::
      !( Maybe
           OrganizationEventFilter
       ),
    _defoMaxResults :: !(Maybe Nat)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeEventsForOrganization' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'defoLocale' - The locale (language) to return information in. English (en) is the default and the only supported value at this time.
--
-- * 'defoNextToken' - If the results of a search are large, only a portion of the results are returned, and a @nextToken@ pagination token is returned in the response. To retrieve the next batch of results, reissue the search request and include the returned token. When all results have been returned, the response does not contain a pagination token value.
--
-- * 'defoFilter' - Values to narrow the results returned.
--
-- * 'defoMaxResults' - The maximum number of items to return in one batch, between 10 and 100, inclusive.
describeEventsForOrganization ::
  DescribeEventsForOrganization
describeEventsForOrganization =
  DescribeEventsForOrganization'
    { _defoLocale = Nothing,
      _defoNextToken = Nothing,
      _defoFilter = Nothing,
      _defoMaxResults = Nothing
    }

-- | The locale (language) to return information in. English (en) is the default and the only supported value at this time.
defoLocale :: Lens' DescribeEventsForOrganization (Maybe Text)
defoLocale = lens _defoLocale (\s a -> s {_defoLocale = a})

-- | If the results of a search are large, only a portion of the results are returned, and a @nextToken@ pagination token is returned in the response. To retrieve the next batch of results, reissue the search request and include the returned token. When all results have been returned, the response does not contain a pagination token value.
defoNextToken :: Lens' DescribeEventsForOrganization (Maybe Text)
defoNextToken = lens _defoNextToken (\s a -> s {_defoNextToken = a})

-- | Values to narrow the results returned.
defoFilter :: Lens' DescribeEventsForOrganization (Maybe OrganizationEventFilter)
defoFilter = lens _defoFilter (\s a -> s {_defoFilter = a})

-- | The maximum number of items to return in one batch, between 10 and 100, inclusive.
defoMaxResults :: Lens' DescribeEventsForOrganization (Maybe Natural)
defoMaxResults = lens _defoMaxResults (\s a -> s {_defoMaxResults = a}) . mapping _Nat

instance AWSPager DescribeEventsForOrganization where
  page rq rs
    | stop (rs ^. deforsNextToken) = Nothing
    | stop (rs ^. deforsEvents) = Nothing
    | otherwise = Just $ rq & defoNextToken .~ rs ^. deforsNextToken

instance AWSRequest DescribeEventsForOrganization where
  type
    Rs DescribeEventsForOrganization =
      DescribeEventsForOrganizationResponse
  request = postJSON awsHealth
  response =
    receiveJSON
      ( \s h x ->
          DescribeEventsForOrganizationResponse'
            <$> (x .?> "nextToken")
            <*> (x .?> "events" .!@ mempty)
            <*> (pure (fromEnum s))
      )

instance Hashable DescribeEventsForOrganization

instance NFData DescribeEventsForOrganization

instance ToHeaders DescribeEventsForOrganization where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("AWSHealth_20160804.DescribeEventsForOrganization" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON DescribeEventsForOrganization where
  toJSON DescribeEventsForOrganization' {..} =
    object
      ( catMaybes
          [ ("locale" .=) <$> _defoLocale,
            ("nextToken" .=) <$> _defoNextToken,
            ("filter" .=) <$> _defoFilter,
            ("maxResults" .=) <$> _defoMaxResults
          ]
      )

instance ToPath DescribeEventsForOrganization where
  toPath = const "/"

instance ToQuery DescribeEventsForOrganization where
  toQuery = const mempty

-- | /See:/ 'describeEventsForOrganizationResponse' smart constructor.
data DescribeEventsForOrganizationResponse = DescribeEventsForOrganizationResponse'
  { _deforsNextToken ::
      !(Maybe Text),
    _deforsEvents ::
      !( Maybe
           [OrganizationEvent]
       ),
    _deforsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeEventsForOrganizationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'deforsNextToken' - If the results of a search are large, only a portion of the results are returned, and a @nextToken@ pagination token is returned in the response. To retrieve the next batch of results, reissue the search request and include the returned token. When all results have been returned, the response does not contain a pagination token value.
--
-- * 'deforsEvents' - The events that match the specified filter criteria.
--
-- * 'deforsResponseStatus' - -- | The response status code.
describeEventsForOrganizationResponse ::
  -- | 'deforsResponseStatus'
  Int ->
  DescribeEventsForOrganizationResponse
describeEventsForOrganizationResponse pResponseStatus_ =
  DescribeEventsForOrganizationResponse'
    { _deforsNextToken =
        Nothing,
      _deforsEvents = Nothing,
      _deforsResponseStatus = pResponseStatus_
    }

-- | If the results of a search are large, only a portion of the results are returned, and a @nextToken@ pagination token is returned in the response. To retrieve the next batch of results, reissue the search request and include the returned token. When all results have been returned, the response does not contain a pagination token value.
deforsNextToken :: Lens' DescribeEventsForOrganizationResponse (Maybe Text)
deforsNextToken = lens _deforsNextToken (\s a -> s {_deforsNextToken = a})

-- | The events that match the specified filter criteria.
deforsEvents :: Lens' DescribeEventsForOrganizationResponse [OrganizationEvent]
deforsEvents = lens _deforsEvents (\s a -> s {_deforsEvents = a}) . _Default . _Coerce

-- | -- | The response status code.
deforsResponseStatus :: Lens' DescribeEventsForOrganizationResponse Int
deforsResponseStatus = lens _deforsResponseStatus (\s a -> s {_deforsResponseStatus = a})

instance NFData DescribeEventsForOrganizationResponse
