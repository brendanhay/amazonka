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
-- Module      : Network.AWS.Config.ListDiscoveredResources
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Accepts a resource type and returns a list of resource identifiers for the resources of that type. A resource identifier includes the resource type, ID, and (if available) the custom resource name. The results consist of resources that AWS Config has discovered, including those that AWS Config is not currently recording. You can narrow the results to include only resources that have specific resource IDs or a resource name.
--
--
-- The response is paginated. By default, AWS Config lists 100 resource identifiers on each page. You can customize this number with the @limit@ parameter. The response includes a @nextToken@ string. To get the next page of results, run the request again and specify the string for the @nextToken@ parameter.
--
--
-- This operation returns paginated results.
module Network.AWS.Config.ListDiscoveredResources
    (
    -- * Creating a Request
      listDiscoveredResources
    , ListDiscoveredResources
    -- * Request Lenses
    , ldrResourceIds
    , ldrResourceName
    , ldrIncludeDeletedResources
    , ldrNextToken
    , ldrLimit
    , ldrResourceType

    -- * Destructuring the Response
    , listDiscoveredResourcesResponse
    , ListDiscoveredResourcesResponse
    -- * Response Lenses
    , ldrrsNextToken
    , ldrrsResourceIdentifiers
    , ldrrsResponseStatus
    ) where

import Network.AWS.Config.Types
import Network.AWS.Config.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- |
--
--
--
-- /See:/ 'listDiscoveredResources' smart constructor.
data ListDiscoveredResources = ListDiscoveredResources'
  { _ldrResourceIds             :: !(Maybe [Text])
  , _ldrResourceName            :: !(Maybe Text)
  , _ldrIncludeDeletedResources :: !(Maybe Bool)
  , _ldrNextToken               :: !(Maybe Text)
  , _ldrLimit                   :: !(Maybe Nat)
  , _ldrResourceType            :: !ResourceType
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListDiscoveredResources' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ldrResourceIds' - The IDs of only those resources that you want AWS Config to list in the response. If you do not specify this parameter, AWS Config lists all resources of the specified type that it has discovered.
--
-- * 'ldrResourceName' - The custom name of only those resources that you want AWS Config to list in the response. If you do not specify this parameter, AWS Config lists all resources of the specified type that it has discovered.
--
-- * 'ldrIncludeDeletedResources' - Specifies whether AWS Config includes deleted resources in the results. By default, deleted resources are not included.
--
-- * 'ldrNextToken' - The @nextToken@ string returned on a previous page that you use to get the next page of results in a paginated response.
--
-- * 'ldrLimit' - The maximum number of resource identifiers returned on each page. The default is 100. You cannot specify a number greater than 100. If you specify 0, AWS Config uses the default.
--
-- * 'ldrResourceType' - The type of resources that you want AWS Config to list in the response.
listDiscoveredResources
    :: ResourceType -- ^ 'ldrResourceType'
    -> ListDiscoveredResources
listDiscoveredResources pResourceType_ =
  ListDiscoveredResources'
    { _ldrResourceIds = Nothing
    , _ldrResourceName = Nothing
    , _ldrIncludeDeletedResources = Nothing
    , _ldrNextToken = Nothing
    , _ldrLimit = Nothing
    , _ldrResourceType = pResourceType_
    }


-- | The IDs of only those resources that you want AWS Config to list in the response. If you do not specify this parameter, AWS Config lists all resources of the specified type that it has discovered.
ldrResourceIds :: Lens' ListDiscoveredResources [Text]
ldrResourceIds = lens _ldrResourceIds (\ s a -> s{_ldrResourceIds = a}) . _Default . _Coerce

-- | The custom name of only those resources that you want AWS Config to list in the response. If you do not specify this parameter, AWS Config lists all resources of the specified type that it has discovered.
ldrResourceName :: Lens' ListDiscoveredResources (Maybe Text)
ldrResourceName = lens _ldrResourceName (\ s a -> s{_ldrResourceName = a})

-- | Specifies whether AWS Config includes deleted resources in the results. By default, deleted resources are not included.
ldrIncludeDeletedResources :: Lens' ListDiscoveredResources (Maybe Bool)
ldrIncludeDeletedResources = lens _ldrIncludeDeletedResources (\ s a -> s{_ldrIncludeDeletedResources = a})

-- | The @nextToken@ string returned on a previous page that you use to get the next page of results in a paginated response.
ldrNextToken :: Lens' ListDiscoveredResources (Maybe Text)
ldrNextToken = lens _ldrNextToken (\ s a -> s{_ldrNextToken = a})

-- | The maximum number of resource identifiers returned on each page. The default is 100. You cannot specify a number greater than 100. If you specify 0, AWS Config uses the default.
ldrLimit :: Lens' ListDiscoveredResources (Maybe Natural)
ldrLimit = lens _ldrLimit (\ s a -> s{_ldrLimit = a}) . mapping _Nat

-- | The type of resources that you want AWS Config to list in the response.
ldrResourceType :: Lens' ListDiscoveredResources ResourceType
ldrResourceType = lens _ldrResourceType (\ s a -> s{_ldrResourceType = a})

instance AWSPager ListDiscoveredResources where
        page rq rs
          | stop (rs ^. ldrrsNextToken) = Nothing
          | stop (rs ^. ldrrsResourceIdentifiers) = Nothing
          | otherwise =
            Just $ rq & ldrNextToken .~ rs ^. ldrrsNextToken

instance AWSRequest ListDiscoveredResources where
        type Rs ListDiscoveredResources =
             ListDiscoveredResourcesResponse
        request = postJSON config
        response
          = receiveJSON
              (\ s h x ->
                 ListDiscoveredResourcesResponse' <$>
                   (x .?> "nextToken") <*>
                     (x .?> "resourceIdentifiers" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable ListDiscoveredResources where

instance NFData ListDiscoveredResources where

instance ToHeaders ListDiscoveredResources where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("StarlingDoveService.ListDiscoveredResources" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListDiscoveredResources where
        toJSON ListDiscoveredResources'{..}
          = object
              (catMaybes
                 [("resourceIds" .=) <$> _ldrResourceIds,
                  ("resourceName" .=) <$> _ldrResourceName,
                  ("includeDeletedResources" .=) <$>
                    _ldrIncludeDeletedResources,
                  ("nextToken" .=) <$> _ldrNextToken,
                  ("limit" .=) <$> _ldrLimit,
                  Just ("resourceType" .= _ldrResourceType)])

instance ToPath ListDiscoveredResources where
        toPath = const "/"

instance ToQuery ListDiscoveredResources where
        toQuery = const mempty

-- |
--
--
--
-- /See:/ 'listDiscoveredResourcesResponse' smart constructor.
data ListDiscoveredResourcesResponse = ListDiscoveredResourcesResponse'
  { _ldrrsNextToken           :: !(Maybe Text)
  , _ldrrsResourceIdentifiers :: !(Maybe [ResourceIdentifier])
  , _ldrrsResponseStatus      :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListDiscoveredResourcesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ldrrsNextToken' - The string that you use in a subsequent request to get the next page of results in a paginated response.
--
-- * 'ldrrsResourceIdentifiers' - The details that identify a resource that is discovered by AWS Config, including the resource type, ID, and (if available) the custom resource name.
--
-- * 'ldrrsResponseStatus' - -- | The response status code.
listDiscoveredResourcesResponse
    :: Int -- ^ 'ldrrsResponseStatus'
    -> ListDiscoveredResourcesResponse
listDiscoveredResourcesResponse pResponseStatus_ =
  ListDiscoveredResourcesResponse'
    { _ldrrsNextToken = Nothing
    , _ldrrsResourceIdentifiers = Nothing
    , _ldrrsResponseStatus = pResponseStatus_
    }


-- | The string that you use in a subsequent request to get the next page of results in a paginated response.
ldrrsNextToken :: Lens' ListDiscoveredResourcesResponse (Maybe Text)
ldrrsNextToken = lens _ldrrsNextToken (\ s a -> s{_ldrrsNextToken = a})

-- | The details that identify a resource that is discovered by AWS Config, including the resource type, ID, and (if available) the custom resource name.
ldrrsResourceIdentifiers :: Lens' ListDiscoveredResourcesResponse [ResourceIdentifier]
ldrrsResourceIdentifiers = lens _ldrrsResourceIdentifiers (\ s a -> s{_ldrrsResourceIdentifiers = a}) . _Default . _Coerce

-- | -- | The response status code.
ldrrsResponseStatus :: Lens' ListDiscoveredResourcesResponse Int
ldrrsResponseStatus = lens _ldrrsResponseStatus (\ s a -> s{_ldrrsResponseStatus = a})

instance NFData ListDiscoveredResourcesResponse where
