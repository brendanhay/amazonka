{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.RevokeIpRules
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes one or more rules from the specified IP access control group.
module Network.AWS.WorkSpaces.RevokeIpRules
    (
    -- * Creating a request
      RevokeIpRules (..)
    , mkRevokeIpRules
    -- ** Request lenses
    , rirGroupId
    , rirUserRules

    -- * Destructuring the response
    , RevokeIpRulesResponse (..)
    , mkRevokeIpRulesResponse
    -- ** Response lenses
    , rirrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WorkSpaces.Types as Types

-- | /See:/ 'mkRevokeIpRules' smart constructor.
data RevokeIpRules = RevokeIpRules'
  { groupId :: Types.IpGroupId
    -- ^ The identifier of the group.
  , userRules :: [Types.IpRule]
    -- ^ The rules to remove from the group.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RevokeIpRules' value with any optional fields omitted.
mkRevokeIpRules
    :: Types.IpGroupId -- ^ 'groupId'
    -> RevokeIpRules
mkRevokeIpRules groupId
  = RevokeIpRules'{groupId, userRules = Core.mempty}

-- | The identifier of the group.
--
-- /Note:/ Consider using 'groupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rirGroupId :: Lens.Lens' RevokeIpRules Types.IpGroupId
rirGroupId = Lens.field @"groupId"
{-# INLINEABLE rirGroupId #-}
{-# DEPRECATED groupId "Use generic-lens or generic-optics with 'groupId' instead"  #-}

-- | The rules to remove from the group.
--
-- /Note:/ Consider using 'userRules' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rirUserRules :: Lens.Lens' RevokeIpRules [Types.IpRule]
rirUserRules = Lens.field @"userRules"
{-# INLINEABLE rirUserRules #-}
{-# DEPRECATED userRules "Use generic-lens or generic-optics with 'userRules' instead"  #-}

instance Core.ToQuery RevokeIpRules where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders RevokeIpRules where
        toHeaders RevokeIpRules{..}
          = Core.pure ("X-Amz-Target", "WorkspacesService.RevokeIpRules")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON RevokeIpRules where
        toJSON RevokeIpRules{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("GroupId" Core..= groupId),
                  Core.Just ("UserRules" Core..= userRules)])

instance Core.AWSRequest RevokeIpRules where
        type Rs RevokeIpRules = RevokeIpRulesResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 RevokeIpRulesResponse' Core.<$> (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkRevokeIpRulesResponse' smart constructor.
newtype RevokeIpRulesResponse = RevokeIpRulesResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'RevokeIpRulesResponse' value with any optional fields omitted.
mkRevokeIpRulesResponse
    :: Core.Int -- ^ 'responseStatus'
    -> RevokeIpRulesResponse
mkRevokeIpRulesResponse responseStatus
  = RevokeIpRulesResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rirrrsResponseStatus :: Lens.Lens' RevokeIpRulesResponse Core.Int
rirrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE rirrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
