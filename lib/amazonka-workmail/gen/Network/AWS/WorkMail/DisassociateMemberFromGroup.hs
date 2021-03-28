{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      DisassociateMemberFromGroup (..)
    , mkDisassociateMemberFromGroup
    -- ** Request lenses
    , dmfgOrganizationId
    , dmfgGroupId
    , dmfgMemberId

    -- * Destructuring the response
    , DisassociateMemberFromGroupResponse (..)
    , mkDisassociateMemberFromGroupResponse
    -- ** Response lenses
    , dmfgrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WorkMail.Types as Types

-- | /See:/ 'mkDisassociateMemberFromGroup' smart constructor.
data DisassociateMemberFromGroup = DisassociateMemberFromGroup'
  { organizationId :: Types.OrganizationId
    -- ^ The identifier for the organization under which the group exists.
  , groupId :: Types.GroupId
    -- ^ The identifier for the group from which members are removed.
  , memberId :: Types.MemberId
    -- ^ The identifier for the member to be removed to the group.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DisassociateMemberFromGroup' value with any optional fields omitted.
mkDisassociateMemberFromGroup
    :: Types.OrganizationId -- ^ 'organizationId'
    -> Types.GroupId -- ^ 'groupId'
    -> Types.MemberId -- ^ 'memberId'
    -> DisassociateMemberFromGroup
mkDisassociateMemberFromGroup organizationId groupId memberId
  = DisassociateMemberFromGroup'{organizationId, groupId, memberId}

-- | The identifier for the organization under which the group exists.
--
-- /Note:/ Consider using 'organizationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmfgOrganizationId :: Lens.Lens' DisassociateMemberFromGroup Types.OrganizationId
dmfgOrganizationId = Lens.field @"organizationId"
{-# INLINEABLE dmfgOrganizationId #-}
{-# DEPRECATED organizationId "Use generic-lens or generic-optics with 'organizationId' instead"  #-}

-- | The identifier for the group from which members are removed.
--
-- /Note:/ Consider using 'groupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmfgGroupId :: Lens.Lens' DisassociateMemberFromGroup Types.GroupId
dmfgGroupId = Lens.field @"groupId"
{-# INLINEABLE dmfgGroupId #-}
{-# DEPRECATED groupId "Use generic-lens or generic-optics with 'groupId' instead"  #-}

-- | The identifier for the member to be removed to the group.
--
-- /Note:/ Consider using 'memberId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmfgMemberId :: Lens.Lens' DisassociateMemberFromGroup Types.MemberId
dmfgMemberId = Lens.field @"memberId"
{-# INLINEABLE dmfgMemberId #-}
{-# DEPRECATED memberId "Use generic-lens or generic-optics with 'memberId' instead"  #-}

instance Core.ToQuery DisassociateMemberFromGroup where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DisassociateMemberFromGroup where
        toHeaders DisassociateMemberFromGroup{..}
          = Core.pure
              ("X-Amz-Target", "WorkMailService.DisassociateMemberFromGroup")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DisassociateMemberFromGroup where
        toJSON DisassociateMemberFromGroup{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("OrganizationId" Core..= organizationId),
                  Core.Just ("GroupId" Core..= groupId),
                  Core.Just ("MemberId" Core..= memberId)])

instance Core.AWSRequest DisassociateMemberFromGroup where
        type Rs DisassociateMemberFromGroup =
             DisassociateMemberFromGroupResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 DisassociateMemberFromGroupResponse' Core.<$>
                   (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDisassociateMemberFromGroupResponse' smart constructor.
newtype DisassociateMemberFromGroupResponse = DisassociateMemberFromGroupResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DisassociateMemberFromGroupResponse' value with any optional fields omitted.
mkDisassociateMemberFromGroupResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DisassociateMemberFromGroupResponse
mkDisassociateMemberFromGroupResponse responseStatus
  = DisassociateMemberFromGroupResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmfgrrsResponseStatus :: Lens.Lens' DisassociateMemberFromGroupResponse Core.Int
dmfgrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dmfgrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
