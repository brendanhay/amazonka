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
-- Module      : Network.AWS.ResourceGroups.SearchResources
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of AWS resource identifiers that matches a specified query. The query uses the same format as a resource query in a CreateGroup or UpdateGroupQuery operation.
--
--
--
-- This operation returns paginated results.
module Network.AWS.ResourceGroups.SearchResources
    (
    -- * Creating a Request
      searchResources
    , SearchResources
    -- * Request Lenses
    , srNextToken
    , srMaxResults
    , srResourceQuery

    -- * Destructuring the Response
    , searchResourcesResponse
    , SearchResourcesResponse
    -- * Response Lenses
    , srrsNextToken
    , srrsResourceIdentifiers
    , srrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.ResourceGroups.Types
import Network.AWS.ResourceGroups.Types.Product
import Network.AWS.Response

-- | /See:/ 'searchResources' smart constructor.
data SearchResources = SearchResources'
  { _srNextToken     :: !(Maybe Text)
  , _srMaxResults    :: !(Maybe Nat)
  , _srResourceQuery :: !ResourceQuery
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SearchResources' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'srNextToken' - The NextToken value that is returned in a paginated @SearchResources@ request. To get the next page of results, run the call again, add the NextToken parameter, and specify the NextToken value.
--
-- * 'srMaxResults' - The maximum number of group member ARNs returned by @SearchResources@ in paginated output. By default, this number is 50.
--
-- * 'srResourceQuery' - The search query, using the same formats that are supported for resource group definition.
searchResources
    :: ResourceQuery -- ^ 'srResourceQuery'
    -> SearchResources
searchResources pResourceQuery_ =
  SearchResources'
    { _srNextToken = Nothing
    , _srMaxResults = Nothing
    , _srResourceQuery = pResourceQuery_
    }


-- | The NextToken value that is returned in a paginated @SearchResources@ request. To get the next page of results, run the call again, add the NextToken parameter, and specify the NextToken value.
srNextToken :: Lens' SearchResources (Maybe Text)
srNextToken = lens _srNextToken (\ s a -> s{_srNextToken = a})

-- | The maximum number of group member ARNs returned by @SearchResources@ in paginated output. By default, this number is 50.
srMaxResults :: Lens' SearchResources (Maybe Natural)
srMaxResults = lens _srMaxResults (\ s a -> s{_srMaxResults = a}) . mapping _Nat

-- | The search query, using the same formats that are supported for resource group definition.
srResourceQuery :: Lens' SearchResources ResourceQuery
srResourceQuery = lens _srResourceQuery (\ s a -> s{_srResourceQuery = a})

instance AWSPager SearchResources where
        page rq rs
          | stop (rs ^. srrsNextToken) = Nothing
          | stop (rs ^. srrsResourceIdentifiers) = Nothing
          | otherwise =
            Just $ rq & srNextToken .~ rs ^. srrsNextToken

instance AWSRequest SearchResources where
        type Rs SearchResources = SearchResourcesResponse
        request = postJSON resourceGroups
        response
          = receiveJSON
              (\ s h x ->
                 SearchResourcesResponse' <$>
                   (x .?> "NextToken") <*>
                     (x .?> "ResourceIdentifiers" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable SearchResources where

instance NFData SearchResources where

instance ToHeaders SearchResources where
        toHeaders = const mempty

instance ToJSON SearchResources where
        toJSON SearchResources'{..}
          = object
              (catMaybes
                 [("NextToken" .=) <$> _srNextToken,
                  ("MaxResults" .=) <$> _srMaxResults,
                  Just ("ResourceQuery" .= _srResourceQuery)])

instance ToPath SearchResources where
        toPath = const "/resources/search"

instance ToQuery SearchResources where
        toQuery = const mempty

-- | /See:/ 'searchResourcesResponse' smart constructor.
data SearchResourcesResponse = SearchResourcesResponse'
  { _srrsNextToken           :: !(Maybe Text)
  , _srrsResourceIdentifiers :: !(Maybe [ResourceIdentifier])
  , _srrsResponseStatus      :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SearchResourcesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'srrsNextToken' - The NextToken value to include in a subsequent @SearchResources@ request, to get more results.
--
-- * 'srrsResourceIdentifiers' - The ARNs and resource types of resources that are members of the group that you specified.
--
-- * 'srrsResponseStatus' - -- | The response status code.
searchResourcesResponse
    :: Int -- ^ 'srrsResponseStatus'
    -> SearchResourcesResponse
searchResourcesResponse pResponseStatus_ =
  SearchResourcesResponse'
    { _srrsNextToken = Nothing
    , _srrsResourceIdentifiers = Nothing
    , _srrsResponseStatus = pResponseStatus_
    }


-- | The NextToken value to include in a subsequent @SearchResources@ request, to get more results.
srrsNextToken :: Lens' SearchResourcesResponse (Maybe Text)
srrsNextToken = lens _srrsNextToken (\ s a -> s{_srrsNextToken = a})

-- | The ARNs and resource types of resources that are members of the group that you specified.
srrsResourceIdentifiers :: Lens' SearchResourcesResponse [ResourceIdentifier]
srrsResourceIdentifiers = lens _srrsResourceIdentifiers (\ s a -> s{_srrsResourceIdentifiers = a}) . _Default . _Coerce

-- | -- | The response status code.
srrsResponseStatus :: Lens' SearchResourcesResponse Int
srrsResponseStatus = lens _srrsResponseStatus (\ s a -> s{_srrsResponseStatus = a})

instance NFData SearchResourcesResponse where
