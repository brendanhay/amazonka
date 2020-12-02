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
-- Module      : Network.AWS.ResourceGroups.GetGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a specified resource group.
module Network.AWS.ResourceGroups.GetGroup
  ( -- * Creating a Request
    getGroup,
    GetGroup,

    -- * Request Lenses
    ggGroup,
    ggGroupName,

    -- * Destructuring the Response
    getGroupResponse,
    GetGroupResponse,

    -- * Response Lenses
    ggrsGroup,
    ggrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.ResourceGroups.Types
import Network.AWS.Response

-- | /See:/ 'getGroup' smart constructor.
data GetGroup = GetGroup'
  { _ggGroup :: !(Maybe Text),
    _ggGroupName :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ggGroup' - The name or the ARN of the resource group to retrieve.
--
-- * 'ggGroupName' - Don't use this parameter. Use @Group@ instead.
getGroup ::
  GetGroup
getGroup = GetGroup' {_ggGroup = Nothing, _ggGroupName = Nothing}

-- | The name or the ARN of the resource group to retrieve.
ggGroup :: Lens' GetGroup (Maybe Text)
ggGroup = lens _ggGroup (\s a -> s {_ggGroup = a})

-- | Don't use this parameter. Use @Group@ instead.
ggGroupName :: Lens' GetGroup (Maybe Text)
ggGroupName = lens _ggGroupName (\s a -> s {_ggGroupName = a})

instance AWSRequest GetGroup where
  type Rs GetGroup = GetGroupResponse
  request = postJSON resourceGroups
  response =
    receiveJSON
      ( \s h x ->
          GetGroupResponse' <$> (x .?> "Group") <*> (pure (fromEnum s))
      )

instance Hashable GetGroup

instance NFData GetGroup

instance ToHeaders GetGroup where
  toHeaders = const mempty

instance ToJSON GetGroup where
  toJSON GetGroup' {..} =
    object
      ( catMaybes
          [("Group" .=) <$> _ggGroup, ("GroupName" .=) <$> _ggGroupName]
      )

instance ToPath GetGroup where
  toPath = const "/get-group"

instance ToQuery GetGroup where
  toQuery = const mempty

-- | /See:/ 'getGroupResponse' smart constructor.
data GetGroupResponse = GetGroupResponse'
  { _ggrsGroup ::
      !(Maybe Group),
    _ggrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetGroupResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ggrsGroup' - A full description of the resource group.
--
-- * 'ggrsResponseStatus' - -- | The response status code.
getGroupResponse ::
  -- | 'ggrsResponseStatus'
  Int ->
  GetGroupResponse
getGroupResponse pResponseStatus_ =
  GetGroupResponse'
    { _ggrsGroup = Nothing,
      _ggrsResponseStatus = pResponseStatus_
    }

-- | A full description of the resource group.
ggrsGroup :: Lens' GetGroupResponse (Maybe Group)
ggrsGroup = lens _ggrsGroup (\s a -> s {_ggrsGroup = a})

-- | -- | The response status code.
ggrsResponseStatus :: Lens' GetGroupResponse Int
ggrsResponseStatus = lens _ggrsResponseStatus (\s a -> s {_ggrsResponseStatus = a})

instance NFData GetGroupResponse
