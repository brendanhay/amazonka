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
-- Module      : Network.AWS.Inspector.DescribeResourceGroups
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the resource groups that are specified by the ARNs of the resource groups.
--
--
module Network.AWS.Inspector.DescribeResourceGroups
    (
    -- * Creating a Request
      describeResourceGroups
    , DescribeResourceGroups
    -- * Request Lenses
    , drgResourceGroupARNs

    -- * Destructuring the Response
    , describeResourceGroupsResponse
    , DescribeResourceGroupsResponse
    -- * Response Lenses
    , drgrsResponseStatus
    , drgrsResourceGroups
    , drgrsFailedItems
    ) where

import Network.AWS.Inspector.Types
import Network.AWS.Inspector.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeResourceGroups' smart constructor.
newtype DescribeResourceGroups = DescribeResourceGroups'
  { _drgResourceGroupARNs :: List1 Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeResourceGroups' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drgResourceGroupARNs' - The ARN that specifies the resource group that you want to describe.
describeResourceGroups
    :: NonEmpty Text -- ^ 'drgResourceGroupARNs'
    -> DescribeResourceGroups
describeResourceGroups pResourceGroupARNs_ =
  DescribeResourceGroups' {_drgResourceGroupARNs = _List1 # pResourceGroupARNs_}


-- | The ARN that specifies the resource group that you want to describe.
drgResourceGroupARNs :: Lens' DescribeResourceGroups (NonEmpty Text)
drgResourceGroupARNs = lens _drgResourceGroupARNs (\ s a -> s{_drgResourceGroupARNs = a}) . _List1

instance AWSRequest DescribeResourceGroups where
        type Rs DescribeResourceGroups =
             DescribeResourceGroupsResponse
        request = postJSON inspector
        response
          = receiveJSON
              (\ s h x ->
                 DescribeResourceGroupsResponse' <$>
                   (pure (fromEnum s)) <*>
                     (x .?> "resourceGroups" .!@ mempty)
                     <*> (x .?> "failedItems" .!@ mempty))

instance Hashable DescribeResourceGroups where

instance NFData DescribeResourceGroups where

instance ToHeaders DescribeResourceGroups where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("InspectorService.DescribeResourceGroups" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeResourceGroups where
        toJSON DescribeResourceGroups'{..}
          = object
              (catMaybes
                 [Just
                    ("resourceGroupArns" .= _drgResourceGroupARNs)])

instance ToPath DescribeResourceGroups where
        toPath = const "/"

instance ToQuery DescribeResourceGroups where
        toQuery = const mempty

-- | /See:/ 'describeResourceGroupsResponse' smart constructor.
data DescribeResourceGroupsResponse = DescribeResourceGroupsResponse'
  { _drgrsResponseStatus :: !Int
  , _drgrsResourceGroups :: ![ResourceGroup]
  , _drgrsFailedItems    :: !(Map Text FailedItemDetails)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeResourceGroupsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drgrsResponseStatus' - -- | The response status code.
--
-- * 'drgrsResourceGroups' - Information about a resource group.
--
-- * 'drgrsFailedItems' - Resource group details that cannot be described. An error code is provided for each failed item.
describeResourceGroupsResponse
    :: Int -- ^ 'drgrsResponseStatus'
    -> DescribeResourceGroupsResponse
describeResourceGroupsResponse pResponseStatus_ =
  DescribeResourceGroupsResponse'
    { _drgrsResponseStatus = pResponseStatus_
    , _drgrsResourceGroups = mempty
    , _drgrsFailedItems = mempty
    }


-- | -- | The response status code.
drgrsResponseStatus :: Lens' DescribeResourceGroupsResponse Int
drgrsResponseStatus = lens _drgrsResponseStatus (\ s a -> s{_drgrsResponseStatus = a})

-- | Information about a resource group.
drgrsResourceGroups :: Lens' DescribeResourceGroupsResponse [ResourceGroup]
drgrsResourceGroups = lens _drgrsResourceGroups (\ s a -> s{_drgrsResourceGroups = a}) . _Coerce

-- | Resource group details that cannot be described. An error code is provided for each failed item.
drgrsFailedItems :: Lens' DescribeResourceGroupsResponse (HashMap Text FailedItemDetails)
drgrsFailedItems = lens _drgrsFailedItems (\ s a -> s{_drgrsFailedItems = a}) . _Map

instance NFData DescribeResourceGroupsResponse where
