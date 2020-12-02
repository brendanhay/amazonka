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
-- Module      : Network.AWS.AWSHealth.DescribeAffectedEntitiesForOrganization
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of entities that have been affected by one or more events for one or more accounts in your organization in AWS Organizations, based on the filter criteria. Entities can refer to individual customer resources, groups of customer resources, or any other construct, depending on the AWS service.
--
--
-- At least one event Amazon Resource Name (ARN) and account ID are required. Results are sorted by the @lastUpdatedTime@ of the entity, starting with the most recent.
--
-- Before you can call this operation, you must first enable AWS Health to work with AWS Organizations. To do this, call the <https://docs.aws.amazon.com/health/latest/APIReference/API_EnableHealthServiceAccessForOrganization.html EnableHealthServiceAccessForOrganization> operation from your organization's master account.
--
--
-- This operation returns paginated results.
module Network.AWS.AWSHealth.DescribeAffectedEntitiesForOrganization
  ( -- * Creating a Request
    describeAffectedEntitiesForOrganization,
    DescribeAffectedEntitiesForOrganization,

    -- * Request Lenses
    daefoLocale,
    daefoNextToken,
    daefoMaxResults,
    daefoOrganizationEntityFilters,

    -- * Destructuring the Response
    describeAffectedEntitiesForOrganizationResponse,
    DescribeAffectedEntitiesForOrganizationResponse,

    -- * Response Lenses
    daeforsEntities,
    daeforsFailedSet,
    daeforsNextToken,
    daeforsResponseStatus,
  )
where

import Network.AWS.AWSHealth.Types
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeAffectedEntitiesForOrganization' smart constructor.
data DescribeAffectedEntitiesForOrganization = DescribeAffectedEntitiesForOrganization'
  { _daefoLocale ::
      !( Maybe
           Text
       ),
    _daefoNextToken ::
      !( Maybe
           Text
       ),
    _daefoMaxResults ::
      !( Maybe
           Nat
       ),
    _daefoOrganizationEntityFilters ::
      !( List1
           EventAccountFilter
       )
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeAffectedEntitiesForOrganization' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'daefoLocale' - The locale (language) to return information in. English (en) is the default and the only supported value at this time.
--
-- * 'daefoNextToken' - If the results of a search are large, only a portion of the results are returned, and a @nextToken@ pagination token is returned in the response. To retrieve the next batch of results, reissue the search request and include the returned token. When all results have been returned, the response does not contain a pagination token value.
--
-- * 'daefoMaxResults' - The maximum number of items to return in one batch, between 10 and 100, inclusive.
--
-- * 'daefoOrganizationEntityFilters' - A JSON set of elements including the @awsAccountId@ and the @eventArn@ .
describeAffectedEntitiesForOrganization ::
  -- | 'daefoOrganizationEntityFilters'
  NonEmpty EventAccountFilter ->
  DescribeAffectedEntitiesForOrganization
describeAffectedEntitiesForOrganization pOrganizationEntityFilters_ =
  DescribeAffectedEntitiesForOrganization'
    { _daefoLocale = Nothing,
      _daefoNextToken = Nothing,
      _daefoMaxResults = Nothing,
      _daefoOrganizationEntityFilters =
        _List1 # pOrganizationEntityFilters_
    }

-- | The locale (language) to return information in. English (en) is the default and the only supported value at this time.
daefoLocale :: Lens' DescribeAffectedEntitiesForOrganization (Maybe Text)
daefoLocale = lens _daefoLocale (\s a -> s {_daefoLocale = a})

-- | If the results of a search are large, only a portion of the results are returned, and a @nextToken@ pagination token is returned in the response. To retrieve the next batch of results, reissue the search request and include the returned token. When all results have been returned, the response does not contain a pagination token value.
daefoNextToken :: Lens' DescribeAffectedEntitiesForOrganization (Maybe Text)
daefoNextToken = lens _daefoNextToken (\s a -> s {_daefoNextToken = a})

-- | The maximum number of items to return in one batch, between 10 and 100, inclusive.
daefoMaxResults :: Lens' DescribeAffectedEntitiesForOrganization (Maybe Natural)
daefoMaxResults = lens _daefoMaxResults (\s a -> s {_daefoMaxResults = a}) . mapping _Nat

-- | A JSON set of elements including the @awsAccountId@ and the @eventArn@ .
daefoOrganizationEntityFilters :: Lens' DescribeAffectedEntitiesForOrganization (NonEmpty EventAccountFilter)
daefoOrganizationEntityFilters = lens _daefoOrganizationEntityFilters (\s a -> s {_daefoOrganizationEntityFilters = a}) . _List1

instance AWSPager DescribeAffectedEntitiesForOrganization where
  page rq rs
    | stop (rs ^. daeforsNextToken) = Nothing
    | stop (rs ^. daeforsEntities) = Nothing
    | otherwise = Just $ rq & daefoNextToken .~ rs ^. daeforsNextToken

instance AWSRequest DescribeAffectedEntitiesForOrganization where
  type
    Rs DescribeAffectedEntitiesForOrganization =
      DescribeAffectedEntitiesForOrganizationResponse
  request = postJSON awsHealth
  response =
    receiveJSON
      ( \s h x ->
          DescribeAffectedEntitiesForOrganizationResponse'
            <$> (x .?> "entities" .!@ mempty)
            <*> (x .?> "failedSet" .!@ mempty)
            <*> (x .?> "nextToken")
            <*> (pure (fromEnum s))
      )

instance Hashable DescribeAffectedEntitiesForOrganization

instance NFData DescribeAffectedEntitiesForOrganization

instance ToHeaders DescribeAffectedEntitiesForOrganization where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ( "AWSHealth_20160804.DescribeAffectedEntitiesForOrganization" ::
                     ByteString
                 ),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON DescribeAffectedEntitiesForOrganization where
  toJSON DescribeAffectedEntitiesForOrganization' {..} =
    object
      ( catMaybes
          [ ("locale" .=) <$> _daefoLocale,
            ("nextToken" .=) <$> _daefoNextToken,
            ("maxResults" .=) <$> _daefoMaxResults,
            Just
              ("organizationEntityFilters" .= _daefoOrganizationEntityFilters)
          ]
      )

instance ToPath DescribeAffectedEntitiesForOrganization where
  toPath = const "/"

instance ToQuery DescribeAffectedEntitiesForOrganization where
  toQuery = const mempty

-- | /See:/ 'describeAffectedEntitiesForOrganizationResponse' smart constructor.
data DescribeAffectedEntitiesForOrganizationResponse = DescribeAffectedEntitiesForOrganizationResponse'
  { _daeforsEntities ::
      !( Maybe
           [AffectedEntity]
       ),
    _daeforsFailedSet ::
      !( Maybe
           [OrganizationAffectedEntitiesErrorItem]
       ),
    _daeforsNextToken ::
      !( Maybe
           Text
       ),
    _daeforsResponseStatus ::
      !Int
  }
  deriving
    ( Eq,
      Read,
      Show,
      Data,
      Typeable,
      Generic
    )

-- | Creates a value of 'DescribeAffectedEntitiesForOrganizationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'daeforsEntities' - A JSON set of elements including the @awsAccountId@ and its @entityArn@ , @entityValue@ and its @entityArn@ , @lastUpdatedTime@ , and @statusCode@ .
--
-- * 'daeforsFailedSet' - A JSON set of elements of the failed response, including the @awsAccountId@ , @errorMessage@ , @errorName@ , and @eventArn@ .
--
-- * 'daeforsNextToken' - If the results of a search are large, only a portion of the results are returned, and a @nextToken@ pagination token is returned in the response. To retrieve the next batch of results, reissue the search request and include the returned token. When all results have been returned, the response does not contain a pagination token value.
--
-- * 'daeforsResponseStatus' - -- | The response status code.
describeAffectedEntitiesForOrganizationResponse ::
  -- | 'daeforsResponseStatus'
  Int ->
  DescribeAffectedEntitiesForOrganizationResponse
describeAffectedEntitiesForOrganizationResponse pResponseStatus_ =
  DescribeAffectedEntitiesForOrganizationResponse'
    { _daeforsEntities =
        Nothing,
      _daeforsFailedSet = Nothing,
      _daeforsNextToken = Nothing,
      _daeforsResponseStatus = pResponseStatus_
    }

-- | A JSON set of elements including the @awsAccountId@ and its @entityArn@ , @entityValue@ and its @entityArn@ , @lastUpdatedTime@ , and @statusCode@ .
daeforsEntities :: Lens' DescribeAffectedEntitiesForOrganizationResponse [AffectedEntity]
daeforsEntities = lens _daeforsEntities (\s a -> s {_daeforsEntities = a}) . _Default . _Coerce

-- | A JSON set of elements of the failed response, including the @awsAccountId@ , @errorMessage@ , @errorName@ , and @eventArn@ .
daeforsFailedSet :: Lens' DescribeAffectedEntitiesForOrganizationResponse [OrganizationAffectedEntitiesErrorItem]
daeforsFailedSet = lens _daeforsFailedSet (\s a -> s {_daeforsFailedSet = a}) . _Default . _Coerce

-- | If the results of a search are large, only a portion of the results are returned, and a @nextToken@ pagination token is returned in the response. To retrieve the next batch of results, reissue the search request and include the returned token. When all results have been returned, the response does not contain a pagination token value.
daeforsNextToken :: Lens' DescribeAffectedEntitiesForOrganizationResponse (Maybe Text)
daeforsNextToken = lens _daeforsNextToken (\s a -> s {_daeforsNextToken = a})

-- | -- | The response status code.
daeforsResponseStatus :: Lens' DescribeAffectedEntitiesForOrganizationResponse Int
daeforsResponseStatus = lens _daeforsResponseStatus (\s a -> s {_daeforsResponseStatus = a})

instance NFData DescribeAffectedEntitiesForOrganizationResponse
