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
-- Module      : Network.AWS.ResourceGroups.UngroupResources
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes the specified resources from the specified group.
module Network.AWS.ResourceGroups.UngroupResources
  ( -- * Creating a Request
    ungroupResources,
    UngroupResources,

    -- * Request Lenses
    urGroup,
    urResourceARNs,

    -- * Destructuring the Response
    ungroupResourcesResponse,
    UngroupResourcesResponse,

    -- * Response Lenses
    urrsSucceeded,
    urrsFailed,
    urrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.ResourceGroups.Types
import Network.AWS.Response

-- | /See:/ 'ungroupResources' smart constructor.
data UngroupResources = UngroupResources'
  { _urGroup :: !Text,
    _urResourceARNs :: !(List1 Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UngroupResources' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'urGroup' - The name or the ARN of the resource group from which to remove the resources.
--
-- * 'urResourceARNs' - The ARNs of the resources to be removed from the group.
ungroupResources ::
  -- | 'urGroup'
  Text ->
  -- | 'urResourceARNs'
  NonEmpty Text ->
  UngroupResources
ungroupResources pGroup_ pResourceARNs_ =
  UngroupResources'
    { _urGroup = pGroup_,
      _urResourceARNs = _List1 # pResourceARNs_
    }

-- | The name or the ARN of the resource group from which to remove the resources.
urGroup :: Lens' UngroupResources Text
urGroup = lens _urGroup (\s a -> s {_urGroup = a})

-- | The ARNs of the resources to be removed from the group.
urResourceARNs :: Lens' UngroupResources (NonEmpty Text)
urResourceARNs = lens _urResourceARNs (\s a -> s {_urResourceARNs = a}) . _List1

instance AWSRequest UngroupResources where
  type Rs UngroupResources = UngroupResourcesResponse
  request = postJSON resourceGroups
  response =
    receiveJSON
      ( \s h x ->
          UngroupResourcesResponse'
            <$> (x .?> "Succeeded")
            <*> (x .?> "Failed" .!@ mempty)
            <*> (pure (fromEnum s))
      )

instance Hashable UngroupResources

instance NFData UngroupResources

instance ToHeaders UngroupResources where
  toHeaders = const mempty

instance ToJSON UngroupResources where
  toJSON UngroupResources' {..} =
    object
      ( catMaybes
          [ Just ("Group" .= _urGroup),
            Just ("ResourceArns" .= _urResourceARNs)
          ]
      )

instance ToPath UngroupResources where
  toPath = const "/ungroup-resources"

instance ToQuery UngroupResources where
  toQuery = const mempty

-- | /See:/ 'ungroupResourcesResponse' smart constructor.
data UngroupResourcesResponse = UngroupResourcesResponse'
  { _urrsSucceeded ::
      !(Maybe (List1 Text)),
    _urrsFailed :: !(Maybe [FailedResource]),
    _urrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UngroupResourcesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'urrsSucceeded' - The ARNs of the resources that were successfully removed from the group.
--
-- * 'urrsFailed' - The resources that failed to be removed from the group.
--
-- * 'urrsResponseStatus' - -- | The response status code.
ungroupResourcesResponse ::
  -- | 'urrsResponseStatus'
  Int ->
  UngroupResourcesResponse
ungroupResourcesResponse pResponseStatus_ =
  UngroupResourcesResponse'
    { _urrsSucceeded = Nothing,
      _urrsFailed = Nothing,
      _urrsResponseStatus = pResponseStatus_
    }

-- | The ARNs of the resources that were successfully removed from the group.
urrsSucceeded :: Lens' UngroupResourcesResponse (Maybe (NonEmpty Text))
urrsSucceeded = lens _urrsSucceeded (\s a -> s {_urrsSucceeded = a}) . mapping _List1

-- | The resources that failed to be removed from the group.
urrsFailed :: Lens' UngroupResourcesResponse [FailedResource]
urrsFailed = lens _urrsFailed (\s a -> s {_urrsFailed = a}) . _Default . _Coerce

-- | -- | The response status code.
urrsResponseStatus :: Lens' UngroupResourcesResponse Int
urrsResponseStatus = lens _urrsResponseStatus (\s a -> s {_urrsResponseStatus = a})

instance NFData UngroupResourcesResponse
