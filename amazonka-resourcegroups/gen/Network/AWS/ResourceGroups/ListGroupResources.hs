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
-- Module      : Network.AWS.ResourceGroups.ListGroupResources
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of ARNs of resources that are members of a specified resource group.
--
--
--
-- This operation returns paginated results.
module Network.AWS.ResourceGroups.ListGroupResources
    (
    -- * Creating a Request
      listGroupResources
    , ListGroupResources
    -- * Request Lenses
    , lgrFilters
    , lgrNextToken
    , lgrMaxResults
    , lgrGroupName

    -- * Destructuring the Response
    , listGroupResourcesResponse
    , ListGroupResourcesResponse
    -- * Response Lenses
    , lgrrsQueryErrors
    , lgrrsNextToken
    , lgrrsResourceIdentifiers
    , lgrrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.ResourceGroups.Types
import Network.AWS.ResourceGroups.Types.Product
import Network.AWS.Response

-- | /See:/ 'listGroupResources' smart constructor.
data ListGroupResources = ListGroupResources'
  { _lgrFilters    :: !(Maybe [ResourceFilter])
  , _lgrNextToken  :: !(Maybe Text)
  , _lgrMaxResults :: !(Maybe Nat)
  , _lgrGroupName  :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListGroupResources' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lgrFilters' - Filters, formatted as ResourceFilter objects, that you want to apply to a ListGroupResources operation.     * @resource-type@ - Filter resources by their type. Specify up to five resource types in the format AWS::ServiceCode::ResourceType. For example, AWS::EC2::Instance, or AWS::S3::Bucket.
--
-- * 'lgrNextToken' - The NextToken value that is returned in a paginated ListGroupResources request. To get the next page of results, run the call again, add the NextToken parameter, and specify the NextToken value.
--
-- * 'lgrMaxResults' - The maximum number of group member ARNs that are returned in a single call by ListGroupResources, in paginated output. By default, this number is 50.
--
-- * 'lgrGroupName' - The name of the resource group.
listGroupResources
    :: Text -- ^ 'lgrGroupName'
    -> ListGroupResources
listGroupResources pGroupName_ =
  ListGroupResources'
    { _lgrFilters = Nothing
    , _lgrNextToken = Nothing
    , _lgrMaxResults = Nothing
    , _lgrGroupName = pGroupName_
    }


-- | Filters, formatted as ResourceFilter objects, that you want to apply to a ListGroupResources operation.     * @resource-type@ - Filter resources by their type. Specify up to five resource types in the format AWS::ServiceCode::ResourceType. For example, AWS::EC2::Instance, or AWS::S3::Bucket.
lgrFilters :: Lens' ListGroupResources [ResourceFilter]
lgrFilters = lens _lgrFilters (\ s a -> s{_lgrFilters = a}) . _Default . _Coerce

-- | The NextToken value that is returned in a paginated ListGroupResources request. To get the next page of results, run the call again, add the NextToken parameter, and specify the NextToken value.
lgrNextToken :: Lens' ListGroupResources (Maybe Text)
lgrNextToken = lens _lgrNextToken (\ s a -> s{_lgrNextToken = a})

-- | The maximum number of group member ARNs that are returned in a single call by ListGroupResources, in paginated output. By default, this number is 50.
lgrMaxResults :: Lens' ListGroupResources (Maybe Natural)
lgrMaxResults = lens _lgrMaxResults (\ s a -> s{_lgrMaxResults = a}) . mapping _Nat

-- | The name of the resource group.
lgrGroupName :: Lens' ListGroupResources Text
lgrGroupName = lens _lgrGroupName (\ s a -> s{_lgrGroupName = a})

instance AWSPager ListGroupResources where
        page rq rs
          | stop (rs ^. lgrrsNextToken) = Nothing
          | stop (rs ^. lgrrsResourceIdentifiers) = Nothing
          | otherwise =
            Just $ rq & lgrNextToken .~ rs ^. lgrrsNextToken

instance AWSRequest ListGroupResources where
        type Rs ListGroupResources =
             ListGroupResourcesResponse
        request = postJSON resourceGroups
        response
          = receiveJSON
              (\ s h x ->
                 ListGroupResourcesResponse' <$>
                   (x .?> "QueryErrors" .!@ mempty) <*>
                     (x .?> "NextToken")
                     <*> (x .?> "ResourceIdentifiers" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable ListGroupResources where

instance NFData ListGroupResources where

instance ToHeaders ListGroupResources where
        toHeaders = const mempty

instance ToJSON ListGroupResources where
        toJSON ListGroupResources'{..}
          = object (catMaybes [("Filters" .=) <$> _lgrFilters])

instance ToPath ListGroupResources where
        toPath ListGroupResources'{..}
          = mconcat
              ["/groups/", toBS _lgrGroupName,
               "/resource-identifiers-list"]

instance ToQuery ListGroupResources where
        toQuery ListGroupResources'{..}
          = mconcat
              ["nextToken" =: _lgrNextToken,
               "maxResults" =: _lgrMaxResults]

-- | /See:/ 'listGroupResourcesResponse' smart constructor.
data ListGroupResourcesResponse = ListGroupResourcesResponse'
  { _lgrrsQueryErrors         :: !(Maybe [QueryError])
  , _lgrrsNextToken           :: !(Maybe Text)
  , _lgrrsResourceIdentifiers :: !(Maybe [ResourceIdentifier])
  , _lgrrsResponseStatus      :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListGroupResourcesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lgrrsQueryErrors' - A list of @QueryError@ objects. Each error is an object that contains @ErrorCode@ and @Message@ structures. Possible values for @ErrorCode@ are @CLOUDFORMATION_STACK_INACTIVE@ and @CLOUDFORMATION_STACK_NOT_EXISTING@ .
--
-- * 'lgrrsNextToken' - The NextToken value to include in a subsequent @ListGroupResources@ request, to get more results.
--
-- * 'lgrrsResourceIdentifiers' - The ARNs and resource types of resources that are members of the group that you specified.
--
-- * 'lgrrsResponseStatus' - -- | The response status code.
listGroupResourcesResponse
    :: Int -- ^ 'lgrrsResponseStatus'
    -> ListGroupResourcesResponse
listGroupResourcesResponse pResponseStatus_ =
  ListGroupResourcesResponse'
    { _lgrrsQueryErrors = Nothing
    , _lgrrsNextToken = Nothing
    , _lgrrsResourceIdentifiers = Nothing
    , _lgrrsResponseStatus = pResponseStatus_
    }


-- | A list of @QueryError@ objects. Each error is an object that contains @ErrorCode@ and @Message@ structures. Possible values for @ErrorCode@ are @CLOUDFORMATION_STACK_INACTIVE@ and @CLOUDFORMATION_STACK_NOT_EXISTING@ .
lgrrsQueryErrors :: Lens' ListGroupResourcesResponse [QueryError]
lgrrsQueryErrors = lens _lgrrsQueryErrors (\ s a -> s{_lgrrsQueryErrors = a}) . _Default . _Coerce

-- | The NextToken value to include in a subsequent @ListGroupResources@ request, to get more results.
lgrrsNextToken :: Lens' ListGroupResourcesResponse (Maybe Text)
lgrrsNextToken = lens _lgrrsNextToken (\ s a -> s{_lgrrsNextToken = a})

-- | The ARNs and resource types of resources that are members of the group that you specified.
lgrrsResourceIdentifiers :: Lens' ListGroupResourcesResponse [ResourceIdentifier]
lgrrsResourceIdentifiers = lens _lgrrsResourceIdentifiers (\ s a -> s{_lgrrsResourceIdentifiers = a}) . _Default . _Coerce

-- | -- | The response status code.
lgrrsResponseStatus :: Lens' ListGroupResourcesResponse Int
lgrrsResponseStatus = lens _lgrrsResponseStatus (\ s a -> s{_lgrrsResponseStatus = a})

instance NFData ListGroupResourcesResponse where
