{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
  ( -- * Creating a request
    DeleteUserHierarchyGroup (..),
    mkDeleteUserHierarchyGroup,

    -- ** Request lenses
    duhguHierarchyGroupId,
    duhguInstanceId,

    -- * Destructuring the response
    DeleteUserHierarchyGroupResponse (..),
    mkDeleteUserHierarchyGroupResponse,
  )
where

import Network.AWS.Connect.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteUserHierarchyGroup' smart constructor.
data DeleteUserHierarchyGroup = DeleteUserHierarchyGroup'
  { hierarchyGroupId ::
      Lude.Text,
    instanceId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteUserHierarchyGroup' with the minimum fields required to make a request.
--
-- * 'hierarchyGroupId' - The identifier of the hierarchy group.
-- * 'instanceId' - The identifier of the Amazon Connect instance.
mkDeleteUserHierarchyGroup ::
  -- | 'hierarchyGroupId'
  Lude.Text ->
  -- | 'instanceId'
  Lude.Text ->
  DeleteUserHierarchyGroup
mkDeleteUserHierarchyGroup pHierarchyGroupId_ pInstanceId_ =
  DeleteUserHierarchyGroup'
    { hierarchyGroupId = pHierarchyGroupId_,
      instanceId = pInstanceId_
    }

-- | The identifier of the hierarchy group.
--
-- /Note:/ Consider using 'hierarchyGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
duhguHierarchyGroupId :: Lens.Lens' DeleteUserHierarchyGroup Lude.Text
duhguHierarchyGroupId = Lens.lens (hierarchyGroupId :: DeleteUserHierarchyGroup -> Lude.Text) (\s a -> s {hierarchyGroupId = a} :: DeleteUserHierarchyGroup)
{-# DEPRECATED duhguHierarchyGroupId "Use generic-lens or generic-optics with 'hierarchyGroupId' instead." #-}

-- | The identifier of the Amazon Connect instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
duhguInstanceId :: Lens.Lens' DeleteUserHierarchyGroup Lude.Text
duhguInstanceId = Lens.lens (instanceId :: DeleteUserHierarchyGroup -> Lude.Text) (\s a -> s {instanceId = a} :: DeleteUserHierarchyGroup)
{-# DEPRECATED duhguInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

instance Lude.AWSRequest DeleteUserHierarchyGroup where
  type Rs DeleteUserHierarchyGroup = DeleteUserHierarchyGroupResponse
  request = Req.delete connectService
  response = Res.receiveNull DeleteUserHierarchyGroupResponse'

instance Lude.ToHeaders DeleteUserHierarchyGroup where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath DeleteUserHierarchyGroup where
  toPath DeleteUserHierarchyGroup' {..} =
    Lude.mconcat
      [ "/user-hierarchy-groups/",
        Lude.toBS instanceId,
        "/",
        Lude.toBS hierarchyGroupId
      ]

instance Lude.ToQuery DeleteUserHierarchyGroup where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteUserHierarchyGroupResponse' smart constructor.
data DeleteUserHierarchyGroupResponse = DeleteUserHierarchyGroupResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteUserHierarchyGroupResponse' with the minimum fields required to make a request.
mkDeleteUserHierarchyGroupResponse ::
  DeleteUserHierarchyGroupResponse
mkDeleteUserHierarchyGroupResponse =
  DeleteUserHierarchyGroupResponse'
