{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.GetAssociatedRole
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the role associated with a particular group.
module Network.AWS.Greengrass.GetAssociatedRole
  ( -- * Creating a request
    GetAssociatedRole (..),
    mkGetAssociatedRole,

    -- ** Request lenses
    garGroupId,

    -- * Destructuring the response
    GetAssociatedRoleResponse (..),
    mkGetAssociatedRoleResponse,

    -- ** Response lenses
    garrsAssociatedAt,
    garrsRoleARN,
    garrsResponseStatus,
  )
where

import Network.AWS.Greengrass.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetAssociatedRole' smart constructor.
newtype GetAssociatedRole = GetAssociatedRole'
  { -- | The ID of the Greengrass group.
    groupId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetAssociatedRole' with the minimum fields required to make a request.
--
-- * 'groupId' - The ID of the Greengrass group.
mkGetAssociatedRole ::
  -- | 'groupId'
  Lude.Text ->
  GetAssociatedRole
mkGetAssociatedRole pGroupId_ =
  GetAssociatedRole' {groupId = pGroupId_}

-- | The ID of the Greengrass group.
--
-- /Note:/ Consider using 'groupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
garGroupId :: Lens.Lens' GetAssociatedRole Lude.Text
garGroupId = Lens.lens (groupId :: GetAssociatedRole -> Lude.Text) (\s a -> s {groupId = a} :: GetAssociatedRole)
{-# DEPRECATED garGroupId "Use generic-lens or generic-optics with 'groupId' instead." #-}

instance Lude.AWSRequest GetAssociatedRole where
  type Rs GetAssociatedRole = GetAssociatedRoleResponse
  request = Req.get greengrassService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetAssociatedRoleResponse'
            Lude.<$> (x Lude..?> "AssociatedAt")
            Lude.<*> (x Lude..?> "RoleArn")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetAssociatedRole where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath GetAssociatedRole where
  toPath GetAssociatedRole' {..} =
    Lude.mconcat ["/greengrass/groups/", Lude.toBS groupId, "/role"]

instance Lude.ToQuery GetAssociatedRole where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetAssociatedRoleResponse' smart constructor.
data GetAssociatedRoleResponse = GetAssociatedRoleResponse'
  { -- | The time when the role was associated with the group.
    associatedAt :: Lude.Maybe Lude.Text,
    -- | The ARN of the role that is associated with the group.
    roleARN :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetAssociatedRoleResponse' with the minimum fields required to make a request.
--
-- * 'associatedAt' - The time when the role was associated with the group.
-- * 'roleARN' - The ARN of the role that is associated with the group.
-- * 'responseStatus' - The response status code.
mkGetAssociatedRoleResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetAssociatedRoleResponse
mkGetAssociatedRoleResponse pResponseStatus_ =
  GetAssociatedRoleResponse'
    { associatedAt = Lude.Nothing,
      roleARN = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The time when the role was associated with the group.
--
-- /Note:/ Consider using 'associatedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
garrsAssociatedAt :: Lens.Lens' GetAssociatedRoleResponse (Lude.Maybe Lude.Text)
garrsAssociatedAt = Lens.lens (associatedAt :: GetAssociatedRoleResponse -> Lude.Maybe Lude.Text) (\s a -> s {associatedAt = a} :: GetAssociatedRoleResponse)
{-# DEPRECATED garrsAssociatedAt "Use generic-lens or generic-optics with 'associatedAt' instead." #-}

-- | The ARN of the role that is associated with the group.
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
garrsRoleARN :: Lens.Lens' GetAssociatedRoleResponse (Lude.Maybe Lude.Text)
garrsRoleARN = Lens.lens (roleARN :: GetAssociatedRoleResponse -> Lude.Maybe Lude.Text) (\s a -> s {roleARN = a} :: GetAssociatedRoleResponse)
{-# DEPRECATED garrsRoleARN "Use generic-lens or generic-optics with 'roleARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
garrsResponseStatus :: Lens.Lens' GetAssociatedRoleResponse Lude.Int
garrsResponseStatus = Lens.lens (responseStatus :: GetAssociatedRoleResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetAssociatedRoleResponse)
{-# DEPRECATED garrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
