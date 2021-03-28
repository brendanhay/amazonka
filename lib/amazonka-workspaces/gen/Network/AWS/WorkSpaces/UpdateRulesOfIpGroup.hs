{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.UpdateRulesOfIpGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Replaces the current rules of the specified IP access control group with the specified rules.
module Network.AWS.WorkSpaces.UpdateRulesOfIpGroup
    (
    -- * Creating a request
      UpdateRulesOfIpGroup (..)
    , mkUpdateRulesOfIpGroup
    -- ** Request lenses
    , uroigGroupId
    , uroigUserRules

    -- * Destructuring the response
    , UpdateRulesOfIpGroupResponse (..)
    , mkUpdateRulesOfIpGroupResponse
    -- ** Response lenses
    , uroigrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WorkSpaces.Types as Types

-- | /See:/ 'mkUpdateRulesOfIpGroup' smart constructor.
data UpdateRulesOfIpGroup = UpdateRulesOfIpGroup'
  { groupId :: Types.GroupId
    -- ^ The identifier of the group.
  , userRules :: [Types.IpRuleItem]
    -- ^ One or more rules.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateRulesOfIpGroup' value with any optional fields omitted.
mkUpdateRulesOfIpGroup
    :: Types.GroupId -- ^ 'groupId'
    -> UpdateRulesOfIpGroup
mkUpdateRulesOfIpGroup groupId
  = UpdateRulesOfIpGroup'{groupId, userRules = Core.mempty}

-- | The identifier of the group.
--
-- /Note:/ Consider using 'groupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uroigGroupId :: Lens.Lens' UpdateRulesOfIpGroup Types.GroupId
uroigGroupId = Lens.field @"groupId"
{-# INLINEABLE uroigGroupId #-}
{-# DEPRECATED groupId "Use generic-lens or generic-optics with 'groupId' instead"  #-}

-- | One or more rules.
--
-- /Note:/ Consider using 'userRules' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uroigUserRules :: Lens.Lens' UpdateRulesOfIpGroup [Types.IpRuleItem]
uroigUserRules = Lens.field @"userRules"
{-# INLINEABLE uroigUserRules #-}
{-# DEPRECATED userRules "Use generic-lens or generic-optics with 'userRules' instead"  #-}

instance Core.ToQuery UpdateRulesOfIpGroup where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpdateRulesOfIpGroup where
        toHeaders UpdateRulesOfIpGroup{..}
          = Core.pure
              ("X-Amz-Target", "WorkspacesService.UpdateRulesOfIpGroup")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON UpdateRulesOfIpGroup where
        toJSON UpdateRulesOfIpGroup{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("GroupId" Core..= groupId),
                  Core.Just ("UserRules" Core..= userRules)])

instance Core.AWSRequest UpdateRulesOfIpGroup where
        type Rs UpdateRulesOfIpGroup = UpdateRulesOfIpGroupResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 UpdateRulesOfIpGroupResponse' Core.<$>
                   (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkUpdateRulesOfIpGroupResponse' smart constructor.
newtype UpdateRulesOfIpGroupResponse = UpdateRulesOfIpGroupResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateRulesOfIpGroupResponse' value with any optional fields omitted.
mkUpdateRulesOfIpGroupResponse
    :: Core.Int -- ^ 'responseStatus'
    -> UpdateRulesOfIpGroupResponse
mkUpdateRulesOfIpGroupResponse responseStatus
  = UpdateRulesOfIpGroupResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uroigrrsResponseStatus :: Lens.Lens' UpdateRulesOfIpGroupResponse Core.Int
uroigrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE uroigrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
