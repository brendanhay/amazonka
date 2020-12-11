{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
  ( -- * Creating a request
    DeleteGroup (..),
    mkDeleteGroup,

    -- ** Request lenses
    dgGroup,
    dgGroupName,

    -- * Destructuring the response
    DeleteGroupResponse (..),
    mkDeleteGroupResponse,

    -- ** Response lenses
    dgrsGroup,
    dgrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import Network.AWS.ResourceGroups.Types
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteGroup' smart constructor.
data DeleteGroup = DeleteGroup'
  { group :: Lude.Maybe Lude.Text,
    groupName :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteGroup' with the minimum fields required to make a request.
--
-- * 'group' - The name or the ARN of the resource group to delete.
-- * 'groupName' - Don't use this parameter. Use @Group@ instead.
mkDeleteGroup ::
  DeleteGroup
mkDeleteGroup =
  DeleteGroup' {group = Lude.Nothing, groupName = Lude.Nothing}

-- | The name or the ARN of the resource group to delete.
--
-- /Note:/ Consider using 'group' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgGroup :: Lens.Lens' DeleteGroup (Lude.Maybe Lude.Text)
dgGroup = Lens.lens (group :: DeleteGroup -> Lude.Maybe Lude.Text) (\s a -> s {group = a} :: DeleteGroup)
{-# DEPRECATED dgGroup "Use generic-lens or generic-optics with 'group' instead." #-}

-- | Don't use this parameter. Use @Group@ instead.
--
-- /Note:/ Consider using 'groupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgGroupName :: Lens.Lens' DeleteGroup (Lude.Maybe Lude.Text)
dgGroupName = Lens.lens (groupName :: DeleteGroup -> Lude.Maybe Lude.Text) (\s a -> s {groupName = a} :: DeleteGroup)
{-# DEPRECATED dgGroupName "Use generic-lens or generic-optics with 'groupName' instead." #-}

instance Lude.AWSRequest DeleteGroup where
  type Rs DeleteGroup = DeleteGroupResponse
  request = Req.postJSON resourceGroupsService
  response =
    Res.receiveJSON
      ( \s h x ->
          DeleteGroupResponse'
            Lude.<$> (x Lude..?> "Group") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteGroup where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToJSON DeleteGroup where
  toJSON DeleteGroup' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Group" Lude..=) Lude.<$> group,
            ("GroupName" Lude..=) Lude.<$> groupName
          ]
      )

instance Lude.ToPath DeleteGroup where
  toPath = Lude.const "/delete-group"

instance Lude.ToQuery DeleteGroup where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteGroupResponse' smart constructor.
data DeleteGroupResponse = DeleteGroupResponse'
  { group ::
      Lude.Maybe Group,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteGroupResponse' with the minimum fields required to make a request.
--
-- * 'group' - A full description of the deleted resource group.
-- * 'responseStatus' - The response status code.
mkDeleteGroupResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteGroupResponse
mkDeleteGroupResponse pResponseStatus_ =
  DeleteGroupResponse'
    { group = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A full description of the deleted resource group.
--
-- /Note:/ Consider using 'group' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgrsGroup :: Lens.Lens' DeleteGroupResponse (Lude.Maybe Group)
dgrsGroup = Lens.lens (group :: DeleteGroupResponse -> Lude.Maybe Group) (\s a -> s {group = a} :: DeleteGroupResponse)
{-# DEPRECATED dgrsGroup "Use generic-lens or generic-optics with 'group' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgrsResponseStatus :: Lens.Lens' DeleteGroupResponse Lude.Int
dgrsResponseStatus = Lens.lens (responseStatus :: DeleteGroupResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteGroupResponse)
{-# DEPRECATED dgrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
