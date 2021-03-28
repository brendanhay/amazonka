{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAF.GetRuleGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the 'RuleGroup' that is specified by the @RuleGroupId@ that you included in the @GetRuleGroup@ request.
--
-- To view the rules in a rule group, use 'ListActivatedRulesInRuleGroup' .
module Network.AWS.WAF.GetRuleGroup
    (
    -- * Creating a request
      GetRuleGroup (..)
    , mkGetRuleGroup
    -- ** Request lenses
    , grgRuleGroupId

    -- * Destructuring the response
    , GetRuleGroupResponse (..)
    , mkGetRuleGroupResponse
    -- ** Response lenses
    , grgrrsRuleGroup
    , grgrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WAF.Types as Types

-- | /See:/ 'mkGetRuleGroup' smart constructor.
newtype GetRuleGroup = GetRuleGroup'
  { ruleGroupId :: Types.ResourceId
    -- ^ The @RuleGroupId@ of the 'RuleGroup' that you want to get. @RuleGroupId@ is returned by 'CreateRuleGroup' and by 'ListRuleGroups' .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetRuleGroup' value with any optional fields omitted.
mkGetRuleGroup
    :: Types.ResourceId -- ^ 'ruleGroupId'
    -> GetRuleGroup
mkGetRuleGroup ruleGroupId = GetRuleGroup'{ruleGroupId}

-- | The @RuleGroupId@ of the 'RuleGroup' that you want to get. @RuleGroupId@ is returned by 'CreateRuleGroup' and by 'ListRuleGroups' .
--
-- /Note:/ Consider using 'ruleGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grgRuleGroupId :: Lens.Lens' GetRuleGroup Types.ResourceId
grgRuleGroupId = Lens.field @"ruleGroupId"
{-# INLINEABLE grgRuleGroupId #-}
{-# DEPRECATED ruleGroupId "Use generic-lens or generic-optics with 'ruleGroupId' instead"  #-}

instance Core.ToQuery GetRuleGroup where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetRuleGroup where
        toHeaders GetRuleGroup{..}
          = Core.pure ("X-Amz-Target", "AWSWAF_20150824.GetRuleGroup")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON GetRuleGroup where
        toJSON GetRuleGroup{..}
          = Core.object
              (Core.catMaybes [Core.Just ("RuleGroupId" Core..= ruleGroupId)])

instance Core.AWSRequest GetRuleGroup where
        type Rs GetRuleGroup = GetRuleGroupResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetRuleGroupResponse' Core.<$>
                   (x Core..:? "RuleGroup") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetRuleGroupResponse' smart constructor.
data GetRuleGroupResponse = GetRuleGroupResponse'
  { ruleGroup :: Core.Maybe Types.RuleGroup
    -- ^ Information about the 'RuleGroup' that you specified in the @GetRuleGroup@ request. 
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetRuleGroupResponse' value with any optional fields omitted.
mkGetRuleGroupResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetRuleGroupResponse
mkGetRuleGroupResponse responseStatus
  = GetRuleGroupResponse'{ruleGroup = Core.Nothing, responseStatus}

-- | Information about the 'RuleGroup' that you specified in the @GetRuleGroup@ request. 
--
-- /Note:/ Consider using 'ruleGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grgrrsRuleGroup :: Lens.Lens' GetRuleGroupResponse (Core.Maybe Types.RuleGroup)
grgrrsRuleGroup = Lens.field @"ruleGroup"
{-# INLINEABLE grgrrsRuleGroup #-}
{-# DEPRECATED ruleGroup "Use generic-lens or generic-optics with 'ruleGroup' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grgrrsResponseStatus :: Lens.Lens' GetRuleGroupResponse Core.Int
grgrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE grgrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
