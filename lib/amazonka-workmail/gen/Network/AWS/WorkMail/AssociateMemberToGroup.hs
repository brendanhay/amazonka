{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkMail.AssociateMemberToGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds a member (user or group) to the group's set.
module Network.AWS.WorkMail.AssociateMemberToGroup
  ( -- * Creating a request
    AssociateMemberToGroup (..),
    mkAssociateMemberToGroup,

    -- ** Request lenses
    amtgOrganizationId,
    amtgGroupId,
    amtgMemberId,

    -- * Destructuring the response
    AssociateMemberToGroupResponse (..),
    mkAssociateMemberToGroupResponse,

    -- ** Response lenses
    amtgrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.WorkMail.Types

-- | /See:/ 'mkAssociateMemberToGroup' smart constructor.
data AssociateMemberToGroup = AssociateMemberToGroup'
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

-- | Creates a value of 'AssociateMemberToGroup' with the minimum fields required to make a request.
--
-- * 'groupId' - The group to which the member (user or group) is associated.
-- * 'memberId' - The member (user or group) to associate to the group.
-- * 'organizationId' - The organization under which the group exists.
mkAssociateMemberToGroup ::
  -- | 'organizationId'
  Lude.Text ->
  -- | 'groupId'
  Lude.Text ->
  -- | 'memberId'
  Lude.Text ->
  AssociateMemberToGroup
mkAssociateMemberToGroup pOrganizationId_ pGroupId_ pMemberId_ =
  AssociateMemberToGroup'
    { organizationId = pOrganizationId_,
      groupId = pGroupId_,
      memberId = pMemberId_
    }

-- | The organization under which the group exists.
--
-- /Note:/ Consider using 'organizationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
amtgOrganizationId :: Lens.Lens' AssociateMemberToGroup Lude.Text
amtgOrganizationId = Lens.lens (organizationId :: AssociateMemberToGroup -> Lude.Text) (\s a -> s {organizationId = a} :: AssociateMemberToGroup)
{-# DEPRECATED amtgOrganizationId "Use generic-lens or generic-optics with 'organizationId' instead." #-}

-- | The group to which the member (user or group) is associated.
--
-- /Note:/ Consider using 'groupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
amtgGroupId :: Lens.Lens' AssociateMemberToGroup Lude.Text
amtgGroupId = Lens.lens (groupId :: AssociateMemberToGroup -> Lude.Text) (\s a -> s {groupId = a} :: AssociateMemberToGroup)
{-# DEPRECATED amtgGroupId "Use generic-lens or generic-optics with 'groupId' instead." #-}

-- | The member (user or group) to associate to the group.
--
-- /Note:/ Consider using 'memberId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
amtgMemberId :: Lens.Lens' AssociateMemberToGroup Lude.Text
amtgMemberId = Lens.lens (memberId :: AssociateMemberToGroup -> Lude.Text) (\s a -> s {memberId = a} :: AssociateMemberToGroup)
{-# DEPRECATED amtgMemberId "Use generic-lens or generic-optics with 'memberId' instead." #-}

instance Lude.AWSRequest AssociateMemberToGroup where
  type Rs AssociateMemberToGroup = AssociateMemberToGroupResponse
  request = Req.postJSON workMailService
  response =
    Res.receiveEmpty
      ( \s h x ->
          AssociateMemberToGroupResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders AssociateMemberToGroup where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("WorkMailService.AssociateMemberToGroup" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON AssociateMemberToGroup where
  toJSON AssociateMemberToGroup' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("OrganizationId" Lude..= organizationId),
            Lude.Just ("GroupId" Lude..= groupId),
            Lude.Just ("MemberId" Lude..= memberId)
          ]
      )

instance Lude.ToPath AssociateMemberToGroup where
  toPath = Lude.const "/"

instance Lude.ToQuery AssociateMemberToGroup where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkAssociateMemberToGroupResponse' smart constructor.
newtype AssociateMemberToGroupResponse = AssociateMemberToGroupResponse'
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

-- | Creates a value of 'AssociateMemberToGroupResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkAssociateMemberToGroupResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  AssociateMemberToGroupResponse
mkAssociateMemberToGroupResponse pResponseStatus_ =
  AssociateMemberToGroupResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
amtgrsResponseStatus :: Lens.Lens' AssociateMemberToGroupResponse Lude.Int
amtgrsResponseStatus = Lens.lens (responseStatus :: AssociateMemberToGroupResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: AssociateMemberToGroupResponse)
{-# DEPRECATED amtgrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
