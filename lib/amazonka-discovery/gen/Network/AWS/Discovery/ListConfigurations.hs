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
-- Module      : Network.AWS.Discovery.ListConfigurations
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list of configuration items according to criteria that you specify in a filter. The filter criteria identifies the relationship requirements.
--
--
module Network.AWS.Discovery.ListConfigurations
    (
    -- * Creating a Request
      listConfigurations
    , ListConfigurations
    -- * Request Lenses
    , lcOrderBy
    , lcFilters
    , lcNextToken
    , lcMaxResults
    , lcConfigurationType

    -- * Destructuring the Response
    , listConfigurationsResponse
    , ListConfigurationsResponse
    -- * Response Lenses
    , lcrsConfigurations
    , lcrsNextToken
    , lcrsResponseStatus
    ) where

import Network.AWS.Discovery.Types
import Network.AWS.Discovery.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listConfigurations' smart constructor.
data ListConfigurations = ListConfigurations'
  { _lcOrderBy           :: !(Maybe [OrderByElement])
  , _lcFilters           :: !(Maybe [Filter])
  , _lcNextToken         :: !(Maybe Text)
  , _lcMaxResults        :: !(Maybe Int)
  , _lcConfigurationType :: !ConfigurationItemType
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListConfigurations' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lcOrderBy' - Certain filter criteria return output that can be sorted in ascending or descending order. For a list of output characteristics for each filter, see <http://docs.aws.amazon.com/application-discovery/latest/APIReference/discovery-api-queries.html#ListConfigurations Using the ListConfigurations Action> .
--
-- * 'lcFilters' - You can filter the request using various logical operators and a /key/ -/value/ format. For example:  @{"key": "serverType", "value": "webServer"}@  For a complete list of filter options and guidance about using them with this action, see <http://docs.aws.amazon.com/application-discovery/latest/APIReference/discovery-api-queries.html#ListConfigurations Querying Discovered Configuration Items> .
--
-- * 'lcNextToken' - Token to retrieve the next set of results. For example, if a previous call to ListConfigurations returned 100 items, but you set @ListConfigurationsRequest$maxResults@ to 10, you received a set of 10 results along with a token. Use that token in this query to get the next set of 10.
--
-- * 'lcMaxResults' - The total number of items to return. The maximum value is 100.
--
-- * 'lcConfigurationType' - A valid configuration identified by Application Discovery Service.
listConfigurations
    :: ConfigurationItemType -- ^ 'lcConfigurationType'
    -> ListConfigurations
listConfigurations pConfigurationType_ =
  ListConfigurations'
    { _lcOrderBy = Nothing
    , _lcFilters = Nothing
    , _lcNextToken = Nothing
    , _lcMaxResults = Nothing
    , _lcConfigurationType = pConfigurationType_
    }


-- | Certain filter criteria return output that can be sorted in ascending or descending order. For a list of output characteristics for each filter, see <http://docs.aws.amazon.com/application-discovery/latest/APIReference/discovery-api-queries.html#ListConfigurations Using the ListConfigurations Action> .
lcOrderBy :: Lens' ListConfigurations [OrderByElement]
lcOrderBy = lens _lcOrderBy (\ s a -> s{_lcOrderBy = a}) . _Default . _Coerce

-- | You can filter the request using various logical operators and a /key/ -/value/ format. For example:  @{"key": "serverType", "value": "webServer"}@  For a complete list of filter options and guidance about using them with this action, see <http://docs.aws.amazon.com/application-discovery/latest/APIReference/discovery-api-queries.html#ListConfigurations Querying Discovered Configuration Items> .
lcFilters :: Lens' ListConfigurations [Filter]
lcFilters = lens _lcFilters (\ s a -> s{_lcFilters = a}) . _Default . _Coerce

-- | Token to retrieve the next set of results. For example, if a previous call to ListConfigurations returned 100 items, but you set @ListConfigurationsRequest$maxResults@ to 10, you received a set of 10 results along with a token. Use that token in this query to get the next set of 10.
lcNextToken :: Lens' ListConfigurations (Maybe Text)
lcNextToken = lens _lcNextToken (\ s a -> s{_lcNextToken = a})

-- | The total number of items to return. The maximum value is 100.
lcMaxResults :: Lens' ListConfigurations (Maybe Int)
lcMaxResults = lens _lcMaxResults (\ s a -> s{_lcMaxResults = a})

-- | A valid configuration identified by Application Discovery Service.
lcConfigurationType :: Lens' ListConfigurations ConfigurationItemType
lcConfigurationType = lens _lcConfigurationType (\ s a -> s{_lcConfigurationType = a})

instance AWSRequest ListConfigurations where
        type Rs ListConfigurations =
             ListConfigurationsResponse
        request = postJSON discovery
        response
          = receiveJSON
              (\ s h x ->
                 ListConfigurationsResponse' <$>
                   (x .?> "configurations" .!@ mempty) <*>
                     (x .?> "nextToken")
                     <*> (pure (fromEnum s)))

instance Hashable ListConfigurations where

instance NFData ListConfigurations where

instance ToHeaders ListConfigurations where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSPoseidonService_V2015_11_01.ListConfigurations"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListConfigurations where
        toJSON ListConfigurations'{..}
          = object
              (catMaybes
                 [("orderBy" .=) <$> _lcOrderBy,
                  ("filters" .=) <$> _lcFilters,
                  ("nextToken" .=) <$> _lcNextToken,
                  ("maxResults" .=) <$> _lcMaxResults,
                  Just ("configurationType" .= _lcConfigurationType)])

instance ToPath ListConfigurations where
        toPath = const "/"

instance ToQuery ListConfigurations where
        toQuery = const mempty

-- | /See:/ 'listConfigurationsResponse' smart constructor.
data ListConfigurationsResponse = ListConfigurationsResponse'
  { _lcrsConfigurations :: !(Maybe [Map Text Text])
  , _lcrsNextToken      :: !(Maybe Text)
  , _lcrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListConfigurationsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lcrsConfigurations' - Returns configuration details, including the configuration ID, attribute names, and attribute values.
--
-- * 'lcrsNextToken' - Token to retrieve the next set of results. For example, if your call to ListConfigurations returned 100 items, but you set @ListConfigurationsRequest$maxResults@ to 10, you received a set of 10 results along with this token. Use this token in the next query to retrieve the next set of 10.
--
-- * 'lcrsResponseStatus' - -- | The response status code.
listConfigurationsResponse
    :: Int -- ^ 'lcrsResponseStatus'
    -> ListConfigurationsResponse
listConfigurationsResponse pResponseStatus_ =
  ListConfigurationsResponse'
    { _lcrsConfigurations = Nothing
    , _lcrsNextToken = Nothing
    , _lcrsResponseStatus = pResponseStatus_
    }


-- | Returns configuration details, including the configuration ID, attribute names, and attribute values.
lcrsConfigurations :: Lens' ListConfigurationsResponse [HashMap Text Text]
lcrsConfigurations = lens _lcrsConfigurations (\ s a -> s{_lcrsConfigurations = a}) . _Default . _Coerce

-- | Token to retrieve the next set of results. For example, if your call to ListConfigurations returned 100 items, but you set @ListConfigurationsRequest$maxResults@ to 10, you received a set of 10 results along with this token. Use this token in the next query to retrieve the next set of 10.
lcrsNextToken :: Lens' ListConfigurationsResponse (Maybe Text)
lcrsNextToken = lens _lcrsNextToken (\ s a -> s{_lcrsNextToken = a})

-- | -- | The response status code.
lcrsResponseStatus :: Lens' ListConfigurationsResponse Int
lcrsResponseStatus = lens _lcrsResponseStatus (\ s a -> s{_lcrsResponseStatus = a})

instance NFData ListConfigurationsResponse where
