{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.AuthorizeIpRules
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds one or more rules to the specified IP access control group.
--
-- This action gives users permission to access their WorkSpaces from the CIDR address ranges specified in the rules.
module Network.AWS.WorkSpaces.AuthorizeIpRules
    (
    -- * Creating a request
      AuthorizeIpRules (..)
    , mkAuthorizeIpRules
    -- ** Request lenses
    , airGroupId
    , airUserRules

    -- * Destructuring the response
    , AuthorizeIpRulesResponse (..)
    , mkAuthorizeIpRulesResponse
    -- ** Response lenses
    , airrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WorkSpaces.Types as Types

-- | /See:/ 'mkAuthorizeIpRules' smart constructor.
data AuthorizeIpRules = AuthorizeIpRules'
  { groupId :: Types.IpGroupId
    -- ^ The identifier of the group.
  , userRules :: [Types.IpRuleItem]
    -- ^ The rules to add to the group.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AuthorizeIpRules' value with any optional fields omitted.
mkAuthorizeIpRules
    :: Types.IpGroupId -- ^ 'groupId'
    -> AuthorizeIpRules
mkAuthorizeIpRules groupId
  = AuthorizeIpRules'{groupId, userRules = Core.mempty}

-- | The identifier of the group.
--
-- /Note:/ Consider using 'groupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
airGroupId :: Lens.Lens' AuthorizeIpRules Types.IpGroupId
airGroupId = Lens.field @"groupId"
{-# INLINEABLE airGroupId #-}
{-# DEPRECATED groupId "Use generic-lens or generic-optics with 'groupId' instead"  #-}

-- | The rules to add to the group.
--
-- /Note:/ Consider using 'userRules' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
airUserRules :: Lens.Lens' AuthorizeIpRules [Types.IpRuleItem]
airUserRules = Lens.field @"userRules"
{-# INLINEABLE airUserRules #-}
{-# DEPRECATED userRules "Use generic-lens or generic-optics with 'userRules' instead"  #-}

instance Core.ToQuery AuthorizeIpRules where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders AuthorizeIpRules where
        toHeaders AuthorizeIpRules{..}
          = Core.pure ("X-Amz-Target", "WorkspacesService.AuthorizeIpRules")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON AuthorizeIpRules where
        toJSON AuthorizeIpRules{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("GroupId" Core..= groupId),
                  Core.Just ("UserRules" Core..= userRules)])

instance Core.AWSRequest AuthorizeIpRules where
        type Rs AuthorizeIpRules = AuthorizeIpRulesResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 AuthorizeIpRulesResponse' Core.<$> (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkAuthorizeIpRulesResponse' smart constructor.
newtype AuthorizeIpRulesResponse = AuthorizeIpRulesResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'AuthorizeIpRulesResponse' value with any optional fields omitted.
mkAuthorizeIpRulesResponse
    :: Core.Int -- ^ 'responseStatus'
    -> AuthorizeIpRulesResponse
mkAuthorizeIpRulesResponse responseStatus
  = AuthorizeIpRulesResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
airrrsResponseStatus :: Lens.Lens' AuthorizeIpRulesResponse Core.Int
airrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE airrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
