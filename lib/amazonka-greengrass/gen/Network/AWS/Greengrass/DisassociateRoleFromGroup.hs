{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.DisassociateRoleFromGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociates the role from a group.
module Network.AWS.Greengrass.DisassociateRoleFromGroup
  ( -- * Creating a request
    DisassociateRoleFromGroup (..),
    mkDisassociateRoleFromGroup,

    -- ** Request lenses
    drfgGroupId,

    -- * Destructuring the response
    DisassociateRoleFromGroupResponse (..),
    mkDisassociateRoleFromGroupResponse,

    -- ** Response lenses
    drfgrsDisassociatedAt,
    drfgrsResponseStatus,
  )
where

import Network.AWS.Greengrass.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDisassociateRoleFromGroup' smart constructor.
newtype DisassociateRoleFromGroup = DisassociateRoleFromGroup'
  { groupId ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DisassociateRoleFromGroup' with the minimum fields required to make a request.
--
-- * 'groupId' - The ID of the Greengrass group.
mkDisassociateRoleFromGroup ::
  -- | 'groupId'
  Lude.Text ->
  DisassociateRoleFromGroup
mkDisassociateRoleFromGroup pGroupId_ =
  DisassociateRoleFromGroup' {groupId = pGroupId_}

-- | The ID of the Greengrass group.
--
-- /Note:/ Consider using 'groupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drfgGroupId :: Lens.Lens' DisassociateRoleFromGroup Lude.Text
drfgGroupId = Lens.lens (groupId :: DisassociateRoleFromGroup -> Lude.Text) (\s a -> s {groupId = a} :: DisassociateRoleFromGroup)
{-# DEPRECATED drfgGroupId "Use generic-lens or generic-optics with 'groupId' instead." #-}

instance Lude.AWSRequest DisassociateRoleFromGroup where
  type
    Rs DisassociateRoleFromGroup =
      DisassociateRoleFromGroupResponse
  request = Req.delete greengrassService
  response =
    Res.receiveJSON
      ( \s h x ->
          DisassociateRoleFromGroupResponse'
            Lude.<$> (x Lude..?> "DisassociatedAt")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DisassociateRoleFromGroup where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath DisassociateRoleFromGroup where
  toPath DisassociateRoleFromGroup' {..} =
    Lude.mconcat ["/greengrass/groups/", Lude.toBS groupId, "/role"]

instance Lude.ToQuery DisassociateRoleFromGroup where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDisassociateRoleFromGroupResponse' smart constructor.
data DisassociateRoleFromGroupResponse = DisassociateRoleFromGroupResponse'
  { disassociatedAt ::
      Lude.Maybe Lude.Text,
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DisassociateRoleFromGroupResponse' with the minimum fields required to make a request.
--
-- * 'disassociatedAt' - The time, in milliseconds since the epoch, when the role was disassociated from the group.
-- * 'responseStatus' - The response status code.
mkDisassociateRoleFromGroupResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DisassociateRoleFromGroupResponse
mkDisassociateRoleFromGroupResponse pResponseStatus_ =
  DisassociateRoleFromGroupResponse'
    { disassociatedAt =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The time, in milliseconds since the epoch, when the role was disassociated from the group.
--
-- /Note:/ Consider using 'disassociatedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drfgrsDisassociatedAt :: Lens.Lens' DisassociateRoleFromGroupResponse (Lude.Maybe Lude.Text)
drfgrsDisassociatedAt = Lens.lens (disassociatedAt :: DisassociateRoleFromGroupResponse -> Lude.Maybe Lude.Text) (\s a -> s {disassociatedAt = a} :: DisassociateRoleFromGroupResponse)
{-# DEPRECATED drfgrsDisassociatedAt "Use generic-lens or generic-optics with 'disassociatedAt' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drfgrsResponseStatus :: Lens.Lens' DisassociateRoleFromGroupResponse Lude.Int
drfgrsResponseStatus = Lens.lens (responseStatus :: DisassociateRoleFromGroupResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DisassociateRoleFromGroupResponse)
{-# DEPRECATED drfgrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
