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
-- Module      : Network.AWS.Config.GetAggregateDiscoveredResourceCounts
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the resource counts across accounts and regions that are present in your AWS Config aggregator. You can request the resource counts by providing filters and GroupByKey.
--
--
-- For example, if the input contains accountID 12345678910 and region us-east-1 in filters, the API returns the count of resources in account ID 12345678910 and region us-east-1. If the input contains ACCOUNT_ID as a GroupByKey, the API returns resource counts for all source accounts that are present in your aggregator.
module Network.AWS.Config.GetAggregateDiscoveredResourceCounts
  ( -- * Creating a Request
    getAggregateDiscoveredResourceCounts,
    GetAggregateDiscoveredResourceCounts,

    -- * Request Lenses
    gadrcFilters,
    gadrcNextToken,
    gadrcLimit,
    gadrcGroupByKey,
    gadrcConfigurationAggregatorName,

    -- * Destructuring the Response
    getAggregateDiscoveredResourceCountsResponse,
    GetAggregateDiscoveredResourceCountsResponse,

    -- * Response Lenses
    gadrcrsGroupedResourceCounts,
    gadrcrsNextToken,
    gadrcrsGroupByKey,
    gadrcrsResponseStatus,
    gadrcrsTotalDiscoveredResources,
  )
where

import Network.AWS.Config.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getAggregateDiscoveredResourceCounts' smart constructor.
data GetAggregateDiscoveredResourceCounts = GetAggregateDiscoveredResourceCounts'
  { _gadrcFilters ::
      !( Maybe
           ResourceCountFilters
       ),
    _gadrcNextToken ::
      !(Maybe Text),
    _gadrcLimit ::
      !(Maybe Nat),
    _gadrcGroupByKey ::
      !( Maybe
           ResourceCountGroupKey
       ),
    _gadrcConfigurationAggregatorName ::
      !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetAggregateDiscoveredResourceCounts' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gadrcFilters' - Filters the results based on the @ResourceCountFilters@ object.
--
-- * 'gadrcNextToken' - The @nextToken@ string returned on a previous page that you use to get the next page of results in a paginated response.
--
-- * 'gadrcLimit' - The maximum number of 'GroupedResourceCount' objects returned on each page. The default is 1000. You cannot specify a number greater than 1000. If you specify 0, AWS Config uses the default.
--
-- * 'gadrcGroupByKey' - The key to group the resource counts.
--
-- * 'gadrcConfigurationAggregatorName' - The name of the configuration aggregator.
getAggregateDiscoveredResourceCounts ::
  -- | 'gadrcConfigurationAggregatorName'
  Text ->
  GetAggregateDiscoveredResourceCounts
getAggregateDiscoveredResourceCounts pConfigurationAggregatorName_ =
  GetAggregateDiscoveredResourceCounts'
    { _gadrcFilters = Nothing,
      _gadrcNextToken = Nothing,
      _gadrcLimit = Nothing,
      _gadrcGroupByKey = Nothing,
      _gadrcConfigurationAggregatorName =
        pConfigurationAggregatorName_
    }

-- | Filters the results based on the @ResourceCountFilters@ object.
gadrcFilters :: Lens' GetAggregateDiscoveredResourceCounts (Maybe ResourceCountFilters)
gadrcFilters = lens _gadrcFilters (\s a -> s {_gadrcFilters = a})

-- | The @nextToken@ string returned on a previous page that you use to get the next page of results in a paginated response.
gadrcNextToken :: Lens' GetAggregateDiscoveredResourceCounts (Maybe Text)
gadrcNextToken = lens _gadrcNextToken (\s a -> s {_gadrcNextToken = a})

-- | The maximum number of 'GroupedResourceCount' objects returned on each page. The default is 1000. You cannot specify a number greater than 1000. If you specify 0, AWS Config uses the default.
gadrcLimit :: Lens' GetAggregateDiscoveredResourceCounts (Maybe Natural)
gadrcLimit = lens _gadrcLimit (\s a -> s {_gadrcLimit = a}) . mapping _Nat

-- | The key to group the resource counts.
gadrcGroupByKey :: Lens' GetAggregateDiscoveredResourceCounts (Maybe ResourceCountGroupKey)
gadrcGroupByKey = lens _gadrcGroupByKey (\s a -> s {_gadrcGroupByKey = a})

-- | The name of the configuration aggregator.
gadrcConfigurationAggregatorName :: Lens' GetAggregateDiscoveredResourceCounts Text
gadrcConfigurationAggregatorName = lens _gadrcConfigurationAggregatorName (\s a -> s {_gadrcConfigurationAggregatorName = a})

instance AWSRequest GetAggregateDiscoveredResourceCounts where
  type
    Rs GetAggregateDiscoveredResourceCounts =
      GetAggregateDiscoveredResourceCountsResponse
  request = postJSON config
  response =
    receiveJSON
      ( \s h x ->
          GetAggregateDiscoveredResourceCountsResponse'
            <$> (x .?> "GroupedResourceCounts" .!@ mempty)
            <*> (x .?> "NextToken")
            <*> (x .?> "GroupByKey")
            <*> (pure (fromEnum s))
            <*> (x .:> "TotalDiscoveredResources")
      )

instance Hashable GetAggregateDiscoveredResourceCounts

instance NFData GetAggregateDiscoveredResourceCounts

instance ToHeaders GetAggregateDiscoveredResourceCounts where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ( "StarlingDoveService.GetAggregateDiscoveredResourceCounts" ::
                     ByteString
                 ),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON GetAggregateDiscoveredResourceCounts where
  toJSON GetAggregateDiscoveredResourceCounts' {..} =
    object
      ( catMaybes
          [ ("Filters" .=) <$> _gadrcFilters,
            ("NextToken" .=) <$> _gadrcNextToken,
            ("Limit" .=) <$> _gadrcLimit,
            ("GroupByKey" .=) <$> _gadrcGroupByKey,
            Just
              ( "ConfigurationAggregatorName"
                  .= _gadrcConfigurationAggregatorName
              )
          ]
      )

instance ToPath GetAggregateDiscoveredResourceCounts where
  toPath = const "/"

instance ToQuery GetAggregateDiscoveredResourceCounts where
  toQuery = const mempty

-- | /See:/ 'getAggregateDiscoveredResourceCountsResponse' smart constructor.
data GetAggregateDiscoveredResourceCountsResponse = GetAggregateDiscoveredResourceCountsResponse'
  { _gadrcrsGroupedResourceCounts ::
      !( Maybe
           [GroupedResourceCount]
       ),
    _gadrcrsNextToken ::
      !( Maybe
           Text
       ),
    _gadrcrsGroupByKey ::
      !( Maybe
           Text
       ),
    _gadrcrsResponseStatus ::
      !Int,
    _gadrcrsTotalDiscoveredResources ::
      !Integer
  }
  deriving
    ( Eq,
      Read,
      Show,
      Data,
      Typeable,
      Generic
    )

-- | Creates a value of 'GetAggregateDiscoveredResourceCountsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gadrcrsGroupedResourceCounts' - Returns a list of GroupedResourceCount objects.
--
-- * 'gadrcrsNextToken' - The @nextToken@ string returned on a previous page that you use to get the next page of results in a paginated response.
--
-- * 'gadrcrsGroupByKey' - The key passed into the request object. If @GroupByKey@ is not provided, the result will be empty.
--
-- * 'gadrcrsResponseStatus' - -- | The response status code.
--
-- * 'gadrcrsTotalDiscoveredResources' - The total number of resources that are present in an aggregator with the filters that you provide.
getAggregateDiscoveredResourceCountsResponse ::
  -- | 'gadrcrsResponseStatus'
  Int ->
  -- | 'gadrcrsTotalDiscoveredResources'
  Integer ->
  GetAggregateDiscoveredResourceCountsResponse
getAggregateDiscoveredResourceCountsResponse
  pResponseStatus_
  pTotalDiscoveredResources_ =
    GetAggregateDiscoveredResourceCountsResponse'
      { _gadrcrsGroupedResourceCounts =
          Nothing,
        _gadrcrsNextToken = Nothing,
        _gadrcrsGroupByKey = Nothing,
        _gadrcrsResponseStatus = pResponseStatus_,
        _gadrcrsTotalDiscoveredResources =
          pTotalDiscoveredResources_
      }

-- | Returns a list of GroupedResourceCount objects.
gadrcrsGroupedResourceCounts :: Lens' GetAggregateDiscoveredResourceCountsResponse [GroupedResourceCount]
gadrcrsGroupedResourceCounts = lens _gadrcrsGroupedResourceCounts (\s a -> s {_gadrcrsGroupedResourceCounts = a}) . _Default . _Coerce

-- | The @nextToken@ string returned on a previous page that you use to get the next page of results in a paginated response.
gadrcrsNextToken :: Lens' GetAggregateDiscoveredResourceCountsResponse (Maybe Text)
gadrcrsNextToken = lens _gadrcrsNextToken (\s a -> s {_gadrcrsNextToken = a})

-- | The key passed into the request object. If @GroupByKey@ is not provided, the result will be empty.
gadrcrsGroupByKey :: Lens' GetAggregateDiscoveredResourceCountsResponse (Maybe Text)
gadrcrsGroupByKey = lens _gadrcrsGroupByKey (\s a -> s {_gadrcrsGroupByKey = a})

-- | -- | The response status code.
gadrcrsResponseStatus :: Lens' GetAggregateDiscoveredResourceCountsResponse Int
gadrcrsResponseStatus = lens _gadrcrsResponseStatus (\s a -> s {_gadrcrsResponseStatus = a})

-- | The total number of resources that are present in an aggregator with the filters that you provide.
gadrcrsTotalDiscoveredResources :: Lens' GetAggregateDiscoveredResourceCountsResponse Integer
gadrcrsTotalDiscoveredResources = lens _gadrcrsTotalDiscoveredResources (\s a -> s {_gadrcrsTotalDiscoveredResources = a})

instance NFData GetAggregateDiscoveredResourceCountsResponse
