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
-- Module      : Network.AWS.ResourceGroups.DeleteGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified resource group. Deleting a resource group does not delete any resources that are members of the group; it only deletes the group structure.
module Network.AWS.ResourceGroups.DeleteGroup
  ( -- * Creating a Request
    deleteGroup,
    DeleteGroup,

    -- * Request Lenses
    dgGroup,
    dgGroupName,

    -- * Destructuring the Response
    deleteGroupResponse,
    DeleteGroupResponse,

    -- * Response Lenses
    dgrsGroup,
    dgrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.ResourceGroups.Types
import Network.AWS.Response

-- | /See:/ 'deleteGroup' smart constructor.
data DeleteGroup = DeleteGroup'
  { _dgGroup :: !(Maybe Text),
    _dgGroupName :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dgGroup' - The name or the ARN of the resource group to delete.
--
-- * 'dgGroupName' - Don't use this parameter. Use @Group@ instead.
deleteGroup ::
  DeleteGroup
deleteGroup =
  DeleteGroup' {_dgGroup = Nothing, _dgGroupName = Nothing}

-- | The name or the ARN of the resource group to delete.
dgGroup :: Lens' DeleteGroup (Maybe Text)
dgGroup = lens _dgGroup (\s a -> s {_dgGroup = a})

-- | Don't use this parameter. Use @Group@ instead.
dgGroupName :: Lens' DeleteGroup (Maybe Text)
dgGroupName = lens _dgGroupName (\s a -> s {_dgGroupName = a})

instance AWSRequest DeleteGroup where
  type Rs DeleteGroup = DeleteGroupResponse
  request = postJSON resourceGroups
  response =
    receiveJSON
      ( \s h x ->
          DeleteGroupResponse' <$> (x .?> "Group") <*> (pure (fromEnum s))
      )

instance Hashable DeleteGroup

instance NFData DeleteGroup

instance ToHeaders DeleteGroup where
  toHeaders = const mempty

instance ToJSON DeleteGroup where
  toJSON DeleteGroup' {..} =
    object
      ( catMaybes
          [("Group" .=) <$> _dgGroup, ("GroupName" .=) <$> _dgGroupName]
      )

instance ToPath DeleteGroup where
  toPath = const "/delete-group"

instance ToQuery DeleteGroup where
  toQuery = const mempty

-- | /See:/ 'deleteGroupResponse' smart constructor.
data DeleteGroupResponse = DeleteGroupResponse'
  { _dgrsGroup ::
      !(Maybe Group),
    _dgrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteGroupResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dgrsGroup' - A full description of the deleted resource group.
--
-- * 'dgrsResponseStatus' - -- | The response status code.
deleteGroupResponse ::
  -- | 'dgrsResponseStatus'
  Int ->
  DeleteGroupResponse
deleteGroupResponse pResponseStatus_ =
  DeleteGroupResponse'
    { _dgrsGroup = Nothing,
      _dgrsResponseStatus = pResponseStatus_
    }

-- | A full description of the deleted resource group.
dgrsGroup :: Lens' DeleteGroupResponse (Maybe Group)
dgrsGroup = lens _dgrsGroup (\s a -> s {_dgrsGroup = a})

-- | -- | The response status code.
dgrsResponseStatus :: Lens' DeleteGroupResponse Int
dgrsResponseStatus = lens _dgrsResponseStatus (\s a -> s {_dgrsResponseStatus = a})

instance NFData DeleteGroupResponse
