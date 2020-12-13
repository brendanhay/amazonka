{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ResourceGroups.UpdateGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the description for an existing group. You cannot update the name of a resource group.
module Network.AWS.ResourceGroups.UpdateGroup
  ( -- * Creating a request
    UpdateGroup (..),
    mkUpdateGroup,

    -- ** Request lenses
    ugGroup,
    ugGroupName,
    ugDescription,

    -- * Destructuring the response
    UpdateGroupResponse (..),
    mkUpdateGroupResponse,

    -- ** Response lenses
    ugrsGroup,
    ugrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import Network.AWS.ResourceGroups.Types
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateGroup' smart constructor.
data UpdateGroup = UpdateGroup'
  { -- | The name or the ARN of the resource group to modify.
    group :: Lude.Maybe Lude.Text,
    -- | Don't use this parameter. Use @Group@ instead.
    groupName :: Lude.Maybe Lude.Text,
    -- | The new description that you want to update the resource group with. Descriptions can contain letters, numbers, hyphens, underscores, periods, and spaces.
    description :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateGroup' with the minimum fields required to make a request.
--
-- * 'group' - The name or the ARN of the resource group to modify.
-- * 'groupName' - Don't use this parameter. Use @Group@ instead.
-- * 'description' - The new description that you want to update the resource group with. Descriptions can contain letters, numbers, hyphens, underscores, periods, and spaces.
mkUpdateGroup ::
  UpdateGroup
mkUpdateGroup =
  UpdateGroup'
    { group = Lude.Nothing,
      groupName = Lude.Nothing,
      description = Lude.Nothing
    }

-- | The name or the ARN of the resource group to modify.
--
-- /Note:/ Consider using 'group' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugGroup :: Lens.Lens' UpdateGroup (Lude.Maybe Lude.Text)
ugGroup = Lens.lens (group :: UpdateGroup -> Lude.Maybe Lude.Text) (\s a -> s {group = a} :: UpdateGroup)
{-# DEPRECATED ugGroup "Use generic-lens or generic-optics with 'group' instead." #-}

-- | Don't use this parameter. Use @Group@ instead.
--
-- /Note:/ Consider using 'groupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugGroupName :: Lens.Lens' UpdateGroup (Lude.Maybe Lude.Text)
ugGroupName = Lens.lens (groupName :: UpdateGroup -> Lude.Maybe Lude.Text) (\s a -> s {groupName = a} :: UpdateGroup)
{-# DEPRECATED ugGroupName "Use generic-lens or generic-optics with 'groupName' instead." #-}

-- | The new description that you want to update the resource group with. Descriptions can contain letters, numbers, hyphens, underscores, periods, and spaces.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugDescription :: Lens.Lens' UpdateGroup (Lude.Maybe Lude.Text)
ugDescription = Lens.lens (description :: UpdateGroup -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: UpdateGroup)
{-# DEPRECATED ugDescription "Use generic-lens or generic-optics with 'description' instead." #-}

instance Lude.AWSRequest UpdateGroup where
  type Rs UpdateGroup = UpdateGroupResponse
  request = Req.postJSON resourceGroupsService
  response =
    Res.receiveJSON
      ( \s h x ->
          UpdateGroupResponse'
            Lude.<$> (x Lude..?> "Group") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateGroup where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToJSON UpdateGroup where
  toJSON UpdateGroup' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Group" Lude..=) Lude.<$> group,
            ("GroupName" Lude..=) Lude.<$> groupName,
            ("Description" Lude..=) Lude.<$> description
          ]
      )

instance Lude.ToPath UpdateGroup where
  toPath = Lude.const "/update-group"

instance Lude.ToQuery UpdateGroup where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateGroupResponse' smart constructor.
data UpdateGroupResponse = UpdateGroupResponse'
  { -- | The update description of the resource group.
    group :: Lude.Maybe Group,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateGroupResponse' with the minimum fields required to make a request.
--
-- * 'group' - The update description of the resource group.
-- * 'responseStatus' - The response status code.
mkUpdateGroupResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateGroupResponse
mkUpdateGroupResponse pResponseStatus_ =
  UpdateGroupResponse'
    { group = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The update description of the resource group.
--
-- /Note:/ Consider using 'group' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugrsGroup :: Lens.Lens' UpdateGroupResponse (Lude.Maybe Group)
ugrsGroup = Lens.lens (group :: UpdateGroupResponse -> Lude.Maybe Group) (\s a -> s {group = a} :: UpdateGroupResponse)
{-# DEPRECATED ugrsGroup "Use generic-lens or generic-optics with 'group' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugrsResponseStatus :: Lens.Lens' UpdateGroupResponse Lude.Int
ugrsResponseStatus = Lens.lens (responseStatus :: UpdateGroupResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateGroupResponse)
{-# DEPRECATED ugrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
