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
-- Module      : Network.AWS.ResourceGroups.GroupResources
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds the specified resources to the specified group.
module Network.AWS.ResourceGroups.GroupResources
  ( -- * Creating a Request
    groupResources,
    GroupResources,

    -- * Request Lenses
    grGroup,
    grResourceARNs,

    -- * Destructuring the Response
    groupResourcesResponse,
    GroupResourcesResponse,

    -- * Response Lenses
    grrsSucceeded,
    grrsFailed,
    grrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.ResourceGroups.Types
import Network.AWS.Response

-- | /See:/ 'groupResources' smart constructor.
data GroupResources = GroupResources'
  { _grGroup :: !Text,
    _grResourceARNs :: !(List1 Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GroupResources' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'grGroup' - The name or the ARN of the resource group to add resources to.
--
-- * 'grResourceARNs' - The list of ARNs for resources to be added to the group.
groupResources ::
  -- | 'grGroup'
  Text ->
  -- | 'grResourceARNs'
  NonEmpty Text ->
  GroupResources
groupResources pGroup_ pResourceARNs_ =
  GroupResources'
    { _grGroup = pGroup_,
      _grResourceARNs = _List1 # pResourceARNs_
    }

-- | The name or the ARN of the resource group to add resources to.
grGroup :: Lens' GroupResources Text
grGroup = lens _grGroup (\s a -> s {_grGroup = a})

-- | The list of ARNs for resources to be added to the group.
grResourceARNs :: Lens' GroupResources (NonEmpty Text)
grResourceARNs = lens _grResourceARNs (\s a -> s {_grResourceARNs = a}) . _List1

instance AWSRequest GroupResources where
  type Rs GroupResources = GroupResourcesResponse
  request = postJSON resourceGroups
  response =
    receiveJSON
      ( \s h x ->
          GroupResourcesResponse'
            <$> (x .?> "Succeeded")
            <*> (x .?> "Failed" .!@ mempty)
            <*> (pure (fromEnum s))
      )

instance Hashable GroupResources

instance NFData GroupResources

instance ToHeaders GroupResources where
  toHeaders = const mempty

instance ToJSON GroupResources where
  toJSON GroupResources' {..} =
    object
      ( catMaybes
          [ Just ("Group" .= _grGroup),
            Just ("ResourceArns" .= _grResourceARNs)
          ]
      )

instance ToPath GroupResources where
  toPath = const "/group-resources"

instance ToQuery GroupResources where
  toQuery = const mempty

-- | /See:/ 'groupResourcesResponse' smart constructor.
data GroupResourcesResponse = GroupResourcesResponse'
  { _grrsSucceeded ::
      !(Maybe (List1 Text)),
    _grrsFailed :: !(Maybe [FailedResource]),
    _grrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GroupResourcesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'grrsSucceeded' - The ARNs of the resources that were successfully added to the group by this operation.
--
-- * 'grrsFailed' - The ARNs of the resources that failed to be added to the group by this operation.
--
-- * 'grrsResponseStatus' - -- | The response status code.
groupResourcesResponse ::
  -- | 'grrsResponseStatus'
  Int ->
  GroupResourcesResponse
groupResourcesResponse pResponseStatus_ =
  GroupResourcesResponse'
    { _grrsSucceeded = Nothing,
      _grrsFailed = Nothing,
      _grrsResponseStatus = pResponseStatus_
    }

-- | The ARNs of the resources that were successfully added to the group by this operation.
grrsSucceeded :: Lens' GroupResourcesResponse (Maybe (NonEmpty Text))
grrsSucceeded = lens _grrsSucceeded (\s a -> s {_grrsSucceeded = a}) . mapping _List1

-- | The ARNs of the resources that failed to be added to the group by this operation.
grrsFailed :: Lens' GroupResourcesResponse [FailedResource]
grrsFailed = lens _grrsFailed (\s a -> s {_grrsFailed = a}) . _Default . _Coerce

-- | -- | The response status code.
grrsResponseStatus :: Lens' GroupResourcesResponse Int
grrsResponseStatus = lens _grrsResponseStatus (\s a -> s {_grrsResponseStatus = a})

instance NFData GroupResourcesResponse
