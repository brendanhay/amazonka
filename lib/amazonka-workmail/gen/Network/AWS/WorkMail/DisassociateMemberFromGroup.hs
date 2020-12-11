{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkMail.DisassociateMemberFromGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes a member from a group.
module Network.AWS.WorkMail.DisassociateMemberFromGroup
  ( -- * Creating a request
    DisassociateMemberFromGroup (..),
    mkDisassociateMemberFromGroup,

    -- ** Request lenses
    dmfgOrganizationId,
    dmfgGroupId,
    dmfgMemberId,

    -- * Destructuring the response
    DisassociateMemberFromGroupResponse (..),
    mkDisassociateMemberFromGroupResponse,

    -- ** Response lenses
    dmfgrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.WorkMail.Types

-- | /See:/ 'mkDisassociateMemberFromGroup' smart constructor.
data DisassociateMemberFromGroup = DisassociateMemberFromGroup'
  { organizationId ::
      Lude.Text,
    groupId :: Lude.Text,
    memberId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DisassociateMemberFromGroup' with the minimum fields required to make a request.
--
-- * 'groupId' - The identifier for the group from which members are removed.
-- * 'memberId' - The identifier for the member to be removed to the group.
-- * 'organizationId' - The identifier for the organization under which the group exists.
mkDisassociateMemberFromGroup ::
  -- | 'organizationId'
  Lude.Text ->
  -- | 'groupId'
  Lude.Text ->
  -- | 'memberId'
  Lude.Text ->
  DisassociateMemberFromGroup
mkDisassociateMemberFromGroup pOrganizationId_ pGroupId_ pMemberId_ =
  DisassociateMemberFromGroup'
    { organizationId = pOrganizationId_,
      groupId = pGroupId_,
      memberId = pMemberId_
    }

-- | The identifier for the organization under which the group exists.
--
-- /Note:/ Consider using 'organizationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmfgOrganizationId :: Lens.Lens' DisassociateMemberFromGroup Lude.Text
dmfgOrganizationId = Lens.lens (organizationId :: DisassociateMemberFromGroup -> Lude.Text) (\s a -> s {organizationId = a} :: DisassociateMemberFromGroup)
{-# DEPRECATED dmfgOrganizationId "Use generic-lens or generic-optics with 'organizationId' instead." #-}

-- | The identifier for the group from which members are removed.
--
-- /Note:/ Consider using 'groupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmfgGroupId :: Lens.Lens' DisassociateMemberFromGroup Lude.Text
dmfgGroupId = Lens.lens (groupId :: DisassociateMemberFromGroup -> Lude.Text) (\s a -> s {groupId = a} :: DisassociateMemberFromGroup)
{-# DEPRECATED dmfgGroupId "Use generic-lens or generic-optics with 'groupId' instead." #-}

-- | The identifier for the member to be removed to the group.
--
-- /Note:/ Consider using 'memberId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmfgMemberId :: Lens.Lens' DisassociateMemberFromGroup Lude.Text
dmfgMemberId = Lens.lens (memberId :: DisassociateMemberFromGroup -> Lude.Text) (\s a -> s {memberId = a} :: DisassociateMemberFromGroup)
{-# DEPRECATED dmfgMemberId "Use generic-lens or generic-optics with 'memberId' instead." #-}

instance Lude.AWSRequest DisassociateMemberFromGroup where
  type
    Rs DisassociateMemberFromGroup =
      DisassociateMemberFromGroupResponse
  request = Req.postJSON workMailService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DisassociateMemberFromGroupResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DisassociateMemberFromGroup where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("WorkMailService.DisassociateMemberFromGroup" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DisassociateMemberFromGroup where
  toJSON DisassociateMemberFromGroup' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("OrganizationId" Lude..= organizationId),
            Lude.Just ("GroupId" Lude..= groupId),
            Lude.Just ("MemberId" Lude..= memberId)
          ]
      )

instance Lude.ToPath DisassociateMemberFromGroup where
  toPath = Lude.const "/"

instance Lude.ToQuery DisassociateMemberFromGroup where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDisassociateMemberFromGroupResponse' smart constructor.
newtype DisassociateMemberFromGroupResponse = DisassociateMemberFromGroupResponse'
  { responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DisassociateMemberFromGroupResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDisassociateMemberFromGroupResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DisassociateMemberFromGroupResponse
mkDisassociateMemberFromGroupResponse pResponseStatus_ =
  DisassociateMemberFromGroupResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmfgrsResponseStatus :: Lens.Lens' DisassociateMemberFromGroupResponse Lude.Int
dmfgrsResponseStatus = Lens.lens (responseStatus :: DisassociateMemberFromGroupResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DisassociateMemberFromGroupResponse)
{-# DEPRECATED dmfgrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
