{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
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
    dmfgrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WorkMail.Types as Types

-- | /See:/ 'mkDisassociateMemberFromGroup' smart constructor.
data DisassociateMemberFromGroup = DisassociateMemberFromGroup'
  { -- | The identifier for the organization under which the group exists.
    organizationId :: Types.OrganizationId,
    -- | The identifier for the group from which members are removed.
    groupId :: Types.GroupId,
    -- | The identifier for the member to be removed to the group.
    memberId :: Types.MemberId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DisassociateMemberFromGroup' value with any optional fields omitted.
mkDisassociateMemberFromGroup ::
  -- | 'organizationId'
  Types.OrganizationId ->
  -- | 'groupId'
  Types.GroupId ->
  -- | 'memberId'
  Types.MemberId ->
  DisassociateMemberFromGroup
mkDisassociateMemberFromGroup organizationId groupId memberId =
  DisassociateMemberFromGroup' {organizationId, groupId, memberId}

-- | The identifier for the organization under which the group exists.
--
-- /Note:/ Consider using 'organizationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmfgOrganizationId :: Lens.Lens' DisassociateMemberFromGroup Types.OrganizationId
dmfgOrganizationId = Lens.field @"organizationId"
{-# DEPRECATED dmfgOrganizationId "Use generic-lens or generic-optics with 'organizationId' instead." #-}

-- | The identifier for the group from which members are removed.
--
-- /Note:/ Consider using 'groupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmfgGroupId :: Lens.Lens' DisassociateMemberFromGroup Types.GroupId
dmfgGroupId = Lens.field @"groupId"
{-# DEPRECATED dmfgGroupId "Use generic-lens or generic-optics with 'groupId' instead." #-}

-- | The identifier for the member to be removed to the group.
--
-- /Note:/ Consider using 'memberId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmfgMemberId :: Lens.Lens' DisassociateMemberFromGroup Types.MemberId
dmfgMemberId = Lens.field @"memberId"
{-# DEPRECATED dmfgMemberId "Use generic-lens or generic-optics with 'memberId' instead." #-}

instance Core.FromJSON DisassociateMemberFromGroup where
  toJSON DisassociateMemberFromGroup {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("OrganizationId" Core..= organizationId),
            Core.Just ("GroupId" Core..= groupId),
            Core.Just ("MemberId" Core..= memberId)
          ]
      )

instance Core.AWSRequest DisassociateMemberFromGroup where
  type
    Rs DisassociateMemberFromGroup =
      DisassociateMemberFromGroupResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "WorkMailService.DisassociateMemberFromGroup")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          DisassociateMemberFromGroupResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDisassociateMemberFromGroupResponse' smart constructor.
newtype DisassociateMemberFromGroupResponse = DisassociateMemberFromGroupResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DisassociateMemberFromGroupResponse' value with any optional fields omitted.
mkDisassociateMemberFromGroupResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DisassociateMemberFromGroupResponse
mkDisassociateMemberFromGroupResponse responseStatus =
  DisassociateMemberFromGroupResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmfgrrsResponseStatus :: Lens.Lens' DisassociateMemberFromGroupResponse Core.Int
dmfgrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dmfgrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
