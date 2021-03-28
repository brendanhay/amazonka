{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      DisassociateRoleFromGroup (..)
    , mkDisassociateRoleFromGroup
    -- ** Request lenses
    , drfgGroupId

    -- * Destructuring the response
    , DisassociateRoleFromGroupResponse (..)
    , mkDisassociateRoleFromGroupResponse
    -- ** Response lenses
    , drfgrrsDisassociatedAt
    , drfgrrsResponseStatus
    ) where

import qualified Network.AWS.Greengrass.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDisassociateRoleFromGroup' smart constructor.
newtype DisassociateRoleFromGroup = DisassociateRoleFromGroup'
  { groupId :: Core.Text
    -- ^ The ID of the Greengrass group.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DisassociateRoleFromGroup' value with any optional fields omitted.
mkDisassociateRoleFromGroup
    :: Core.Text -- ^ 'groupId'
    -> DisassociateRoleFromGroup
mkDisassociateRoleFromGroup groupId
  = DisassociateRoleFromGroup'{groupId}

-- | The ID of the Greengrass group.
--
-- /Note:/ Consider using 'groupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drfgGroupId :: Lens.Lens' DisassociateRoleFromGroup Core.Text
drfgGroupId = Lens.field @"groupId"
{-# INLINEABLE drfgGroupId #-}
{-# DEPRECATED groupId "Use generic-lens or generic-optics with 'groupId' instead"  #-}

instance Core.ToQuery DisassociateRoleFromGroup where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DisassociateRoleFromGroup where
        toHeaders DisassociateRoleFromGroup{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.AWSRequest DisassociateRoleFromGroup where
        type Rs DisassociateRoleFromGroup =
             DisassociateRoleFromGroupResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.DELETE,
                         Core._rqPath =
                           "/greengrass/groups/" Core.<> Core.toText groupId Core.<> "/role",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DisassociateRoleFromGroupResponse' Core.<$>
                   (x Core..:? "DisassociatedAt") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDisassociateRoleFromGroupResponse' smart constructor.
data DisassociateRoleFromGroupResponse = DisassociateRoleFromGroupResponse'
  { disassociatedAt :: Core.Maybe Core.Text
    -- ^ The time, in milliseconds since the epoch, when the role was disassociated from the group.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DisassociateRoleFromGroupResponse' value with any optional fields omitted.
mkDisassociateRoleFromGroupResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DisassociateRoleFromGroupResponse
mkDisassociateRoleFromGroupResponse responseStatus
  = DisassociateRoleFromGroupResponse'{disassociatedAt =
                                         Core.Nothing,
                                       responseStatus}

-- | The time, in milliseconds since the epoch, when the role was disassociated from the group.
--
-- /Note:/ Consider using 'disassociatedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drfgrrsDisassociatedAt :: Lens.Lens' DisassociateRoleFromGroupResponse (Core.Maybe Core.Text)
drfgrrsDisassociatedAt = Lens.field @"disassociatedAt"
{-# INLINEABLE drfgrrsDisassociatedAt #-}
{-# DEPRECATED disassociatedAt "Use generic-lens or generic-optics with 'disassociatedAt' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drfgrrsResponseStatus :: Lens.Lens' DisassociateRoleFromGroupResponse Core.Int
drfgrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE drfgrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
