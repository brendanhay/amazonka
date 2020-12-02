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
-- Module      : Network.AWS.Config.ListAggregateDiscoveredResources
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Accepts a resource type and returns a list of resource identifiers that are aggregated for a specific resource type across accounts and regions. A resource identifier includes the resource type, ID, (if available) the custom resource name, source account, and source region. You can narrow the results to include only resources that have specific resource IDs, or a resource name, or source account ID, or source region.
--
--
-- For example, if the input consists of accountID 12345678910 and the region is us-east-1 for resource type @AWS::EC2::Instance@ then the API returns all the EC2 instance identifiers of accountID 12345678910 and region us-east-1.
--
--
-- This operation returns paginated results.
module Network.AWS.Config.ListAggregateDiscoveredResources
  ( -- * Creating a Request
    listAggregateDiscoveredResources,
    ListAggregateDiscoveredResources,

    -- * Request Lenses
    ladrFilters,
    ladrNextToken,
    ladrLimit,
    ladrConfigurationAggregatorName,
    ladrResourceType,

    -- * Destructuring the Response
    listAggregateDiscoveredResourcesResponse,
    ListAggregateDiscoveredResourcesResponse,

    -- * Response Lenses
    ladrrsNextToken,
    ladrrsResourceIdentifiers,
    ladrrsResponseStatus,
  )
where

import Network.AWS.Config.Types
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listAggregateDiscoveredResources' smart constructor.
data ListAggregateDiscoveredResources = ListAggregateDiscoveredResources'
  { _ladrFilters ::
      !(Maybe ResourceFilters),
    _ladrNextToken ::
      !(Maybe Text),
    _ladrLimit ::
      !(Maybe Nat),
    _ladrConfigurationAggregatorName ::
      !Text,
    _ladrResourceType ::
      !ResourceType
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListAggregateDiscoveredResources' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ladrFilters' - Filters the results based on the @ResourceFilters@ object.
--
-- * 'ladrNextToken' - The @nextToken@ string returned on a previous page that you use to get the next page of results in a paginated response.
--
-- * 'ladrLimit' - The maximum number of resource identifiers returned on each page. The default is 100. You cannot specify a number greater than 100. If you specify 0, AWS Config uses the default.
--
-- * 'ladrConfigurationAggregatorName' - The name of the configuration aggregator.
--
-- * 'ladrResourceType' - The type of resources that you want AWS Config to list in the response.
listAggregateDiscoveredResources ::
  -- | 'ladrConfigurationAggregatorName'
  Text ->
  -- | 'ladrResourceType'
  ResourceType ->
  ListAggregateDiscoveredResources
listAggregateDiscoveredResources
  pConfigurationAggregatorName_
  pResourceType_ =
    ListAggregateDiscoveredResources'
      { _ladrFilters = Nothing,
        _ladrNextToken = Nothing,
        _ladrLimit = Nothing,
        _ladrConfigurationAggregatorName =
          pConfigurationAggregatorName_,
        _ladrResourceType = pResourceType_
      }

-- | Filters the results based on the @ResourceFilters@ object.
ladrFilters :: Lens' ListAggregateDiscoveredResources (Maybe ResourceFilters)
ladrFilters = lens _ladrFilters (\s a -> s {_ladrFilters = a})

-- | The @nextToken@ string returned on a previous page that you use to get the next page of results in a paginated response.
ladrNextToken :: Lens' ListAggregateDiscoveredResources (Maybe Text)
ladrNextToken = lens _ladrNextToken (\s a -> s {_ladrNextToken = a})

-- | The maximum number of resource identifiers returned on each page. The default is 100. You cannot specify a number greater than 100. If you specify 0, AWS Config uses the default.
ladrLimit :: Lens' ListAggregateDiscoveredResources (Maybe Natural)
ladrLimit = lens _ladrLimit (\s a -> s {_ladrLimit = a}) . mapping _Nat

-- | The name of the configuration aggregator.
ladrConfigurationAggregatorName :: Lens' ListAggregateDiscoveredResources Text
ladrConfigurationAggregatorName = lens _ladrConfigurationAggregatorName (\s a -> s {_ladrConfigurationAggregatorName = a})

-- | The type of resources that you want AWS Config to list in the response.
ladrResourceType :: Lens' ListAggregateDiscoveredResources ResourceType
ladrResourceType = lens _ladrResourceType (\s a -> s {_ladrResourceType = a})

instance AWSPager ListAggregateDiscoveredResources where
  page rq rs
    | stop (rs ^. ladrrsNextToken) = Nothing
    | stop (rs ^. ladrrsResourceIdentifiers) = Nothing
    | otherwise = Just $ rq & ladrNextToken .~ rs ^. ladrrsNextToken

instance AWSRequest ListAggregateDiscoveredResources where
  type
    Rs ListAggregateDiscoveredResources =
      ListAggregateDiscoveredResourcesResponse
  request = postJSON config
  response =
    receiveJSON
      ( \s h x ->
          ListAggregateDiscoveredResourcesResponse'
            <$> (x .?> "NextToken")
            <*> (x .?> "ResourceIdentifiers" .!@ mempty)
            <*> (pure (fromEnum s))
      )

instance Hashable ListAggregateDiscoveredResources

instance NFData ListAggregateDiscoveredResources

instance ToHeaders ListAggregateDiscoveredResources where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ( "StarlingDoveService.ListAggregateDiscoveredResources" ::
                     ByteString
                 ),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON ListAggregateDiscoveredResources where
  toJSON ListAggregateDiscoveredResources' {..} =
    object
      ( catMaybes
          [ ("Filters" .=) <$> _ladrFilters,
            ("NextToken" .=) <$> _ladrNextToken,
            ("Limit" .=) <$> _ladrLimit,
            Just
              ( "ConfigurationAggregatorName"
                  .= _ladrConfigurationAggregatorName
              ),
            Just ("ResourceType" .= _ladrResourceType)
          ]
      )

instance ToPath ListAggregateDiscoveredResources where
  toPath = const "/"

instance ToQuery ListAggregateDiscoveredResources where
  toQuery = const mempty

-- | /See:/ 'listAggregateDiscoveredResourcesResponse' smart constructor.
data ListAggregateDiscoveredResourcesResponse = ListAggregateDiscoveredResourcesResponse'
  { _ladrrsNextToken ::
      !( Maybe
           Text
       ),
    _ladrrsResourceIdentifiers ::
      !( Maybe
           [AggregateResourceIdentifier]
       ),
    _ladrrsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListAggregateDiscoveredResourcesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ladrrsNextToken' - The @nextToken@ string returned on a previous page that you use to get the next page of results in a paginated response.
--
-- * 'ladrrsResourceIdentifiers' - Returns a list of @ResourceIdentifiers@ objects.
--
-- * 'ladrrsResponseStatus' - -- | The response status code.
listAggregateDiscoveredResourcesResponse ::
  -- | 'ladrrsResponseStatus'
  Int ->
  ListAggregateDiscoveredResourcesResponse
listAggregateDiscoveredResourcesResponse pResponseStatus_ =
  ListAggregateDiscoveredResourcesResponse'
    { _ladrrsNextToken =
        Nothing,
      _ladrrsResourceIdentifiers = Nothing,
      _ladrrsResponseStatus = pResponseStatus_
    }

-- | The @nextToken@ string returned on a previous page that you use to get the next page of results in a paginated response.
ladrrsNextToken :: Lens' ListAggregateDiscoveredResourcesResponse (Maybe Text)
ladrrsNextToken = lens _ladrrsNextToken (\s a -> s {_ladrrsNextToken = a})

-- | Returns a list of @ResourceIdentifiers@ objects.
ladrrsResourceIdentifiers :: Lens' ListAggregateDiscoveredResourcesResponse [AggregateResourceIdentifier]
ladrrsResourceIdentifiers = lens _ladrrsResourceIdentifiers (\s a -> s {_ladrrsResourceIdentifiers = a}) . _Default . _Coerce

-- | -- | The response status code.
ladrrsResponseStatus :: Lens' ListAggregateDiscoveredResourcesResponse Int
ladrrsResponseStatus = lens _ladrrsResponseStatus (\s a -> s {_ladrrsResponseStatus = a})

instance NFData ListAggregateDiscoveredResourcesResponse
