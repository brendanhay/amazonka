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
-- Module      : Network.AWS.Connect.DeleteUserHierarchyGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an existing user hierarchy group. It must not be associated with any agents or have any active child groups.
module Network.AWS.Connect.DeleteUserHierarchyGroup
  ( -- * Creating a Request
    deleteUserHierarchyGroup,
    DeleteUserHierarchyGroup,

    -- * Request Lenses
    duhguHierarchyGroupId,
    duhguInstanceId,

    -- * Destructuring the Response
    deleteUserHierarchyGroupResponse,
    DeleteUserHierarchyGroupResponse,
  )
where

import Network.AWS.Connect.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteUserHierarchyGroup' smart constructor.
data DeleteUserHierarchyGroup = DeleteUserHierarchyGroup'
  { _duhguHierarchyGroupId ::
      !Text,
    _duhguInstanceId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteUserHierarchyGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'duhguHierarchyGroupId' - The identifier of the hierarchy group.
--
-- * 'duhguInstanceId' - The identifier of the Amazon Connect instance.
deleteUserHierarchyGroup ::
  -- | 'duhguHierarchyGroupId'
  Text ->
  -- | 'duhguInstanceId'
  Text ->
  DeleteUserHierarchyGroup
deleteUserHierarchyGroup pHierarchyGroupId_ pInstanceId_ =
  DeleteUserHierarchyGroup'
    { _duhguHierarchyGroupId =
        pHierarchyGroupId_,
      _duhguInstanceId = pInstanceId_
    }

-- | The identifier of the hierarchy group.
duhguHierarchyGroupId :: Lens' DeleteUserHierarchyGroup Text
duhguHierarchyGroupId = lens _duhguHierarchyGroupId (\s a -> s {_duhguHierarchyGroupId = a})

-- | The identifier of the Amazon Connect instance.
duhguInstanceId :: Lens' DeleteUserHierarchyGroup Text
duhguInstanceId = lens _duhguInstanceId (\s a -> s {_duhguInstanceId = a})

instance AWSRequest DeleteUserHierarchyGroup where
  type Rs DeleteUserHierarchyGroup = DeleteUserHierarchyGroupResponse
  request = delete connect
  response = receiveNull DeleteUserHierarchyGroupResponse'

instance Hashable DeleteUserHierarchyGroup

instance NFData DeleteUserHierarchyGroup

instance ToHeaders DeleteUserHierarchyGroup where
  toHeaders =
    const
      ( mconcat
          ["Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)]
      )

instance ToPath DeleteUserHierarchyGroup where
  toPath DeleteUserHierarchyGroup' {..} =
    mconcat
      [ "/user-hierarchy-groups/",
        toBS _duhguInstanceId,
        "/",
        toBS _duhguHierarchyGroupId
      ]

instance ToQuery DeleteUserHierarchyGroup where
  toQuery = const mempty

-- | /See:/ 'deleteUserHierarchyGroupResponse' smart constructor.
data DeleteUserHierarchyGroupResponse = DeleteUserHierarchyGroupResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteUserHierarchyGroupResponse' with the minimum fields required to make a request.
deleteUserHierarchyGroupResponse ::
  DeleteUserHierarchyGroupResponse
deleteUserHierarchyGroupResponse =
  DeleteUserHierarchyGroupResponse'

instance NFData DeleteUserHierarchyGroupResponse
