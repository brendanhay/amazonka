{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.AssociateRoleToGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates a role with a group. Your Greengrass core will use the role to access AWS cloud services. The role's permissions should allow Greengrass core Lambda functions to perform actions against the cloud.
module Network.AWS.Greengrass.AssociateRoleToGroup
  ( -- * Creating a request
    AssociateRoleToGroup (..),
    mkAssociateRoleToGroup,

    -- ** Request lenses
    artgGroupId,
    artgRoleARN,

    -- * Destructuring the response
    AssociateRoleToGroupResponse (..),
    mkAssociateRoleToGroupResponse,

    -- ** Response lenses
    artgrsAssociatedAt,
    artgrsResponseStatus,
  )
where

import Network.AWS.Greengrass.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkAssociateRoleToGroup' smart constructor.
data AssociateRoleToGroup = AssociateRoleToGroup'
  { -- | The ID of the Greengrass group.
    groupId :: Lude.Text,
    -- | The ARN of the role you wish to associate with this group. The existence of the role is not validated.
    roleARN :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AssociateRoleToGroup' with the minimum fields required to make a request.
--
-- * 'groupId' - The ID of the Greengrass group.
-- * 'roleARN' - The ARN of the role you wish to associate with this group. The existence of the role is not validated.
mkAssociateRoleToGroup ::
  -- | 'groupId'
  Lude.Text ->
  -- | 'roleARN'
  Lude.Text ->
  AssociateRoleToGroup
mkAssociateRoleToGroup pGroupId_ pRoleARN_ =
  AssociateRoleToGroup' {groupId = pGroupId_, roleARN = pRoleARN_}

-- | The ID of the Greengrass group.
--
-- /Note:/ Consider using 'groupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
artgGroupId :: Lens.Lens' AssociateRoleToGroup Lude.Text
artgGroupId = Lens.lens (groupId :: AssociateRoleToGroup -> Lude.Text) (\s a -> s {groupId = a} :: AssociateRoleToGroup)
{-# DEPRECATED artgGroupId "Use generic-lens or generic-optics with 'groupId' instead." #-}

-- | The ARN of the role you wish to associate with this group. The existence of the role is not validated.
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
artgRoleARN :: Lens.Lens' AssociateRoleToGroup Lude.Text
artgRoleARN = Lens.lens (roleARN :: AssociateRoleToGroup -> Lude.Text) (\s a -> s {roleARN = a} :: AssociateRoleToGroup)
{-# DEPRECATED artgRoleARN "Use generic-lens or generic-optics with 'roleARN' instead." #-}

instance Lude.AWSRequest AssociateRoleToGroup where
  type Rs AssociateRoleToGroup = AssociateRoleToGroupResponse
  request = Req.putJSON greengrassService
  response =
    Res.receiveJSON
      ( \s h x ->
          AssociateRoleToGroupResponse'
            Lude.<$> (x Lude..?> "AssociatedAt") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders AssociateRoleToGroup where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON AssociateRoleToGroup where
  toJSON AssociateRoleToGroup' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("RoleArn" Lude..= roleARN)])

instance Lude.ToPath AssociateRoleToGroup where
  toPath AssociateRoleToGroup' {..} =
    Lude.mconcat ["/greengrass/groups/", Lude.toBS groupId, "/role"]

instance Lude.ToQuery AssociateRoleToGroup where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkAssociateRoleToGroupResponse' smart constructor.
data AssociateRoleToGroupResponse = AssociateRoleToGroupResponse'
  { -- | The time, in milliseconds since the epoch, when the role ARN was associated with the group.
    associatedAt :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AssociateRoleToGroupResponse' with the minimum fields required to make a request.
--
-- * 'associatedAt' - The time, in milliseconds since the epoch, when the role ARN was associated with the group.
-- * 'responseStatus' - The response status code.
mkAssociateRoleToGroupResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  AssociateRoleToGroupResponse
mkAssociateRoleToGroupResponse pResponseStatus_ =
  AssociateRoleToGroupResponse'
    { associatedAt = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The time, in milliseconds since the epoch, when the role ARN was associated with the group.
--
-- /Note:/ Consider using 'associatedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
artgrsAssociatedAt :: Lens.Lens' AssociateRoleToGroupResponse (Lude.Maybe Lude.Text)
artgrsAssociatedAt = Lens.lens (associatedAt :: AssociateRoleToGroupResponse -> Lude.Maybe Lude.Text) (\s a -> s {associatedAt = a} :: AssociateRoleToGroupResponse)
{-# DEPRECATED artgrsAssociatedAt "Use generic-lens or generic-optics with 'associatedAt' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
artgrsResponseStatus :: Lens.Lens' AssociateRoleToGroupResponse Lude.Int
artgrsResponseStatus = Lens.lens (responseStatus :: AssociateRoleToGroupResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: AssociateRoleToGroupResponse)
{-# DEPRECATED artgrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
