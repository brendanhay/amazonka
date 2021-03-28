{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAF.GetRateBasedRuleManagedKeys
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns an array of IP addresses currently being blocked by the 'RateBasedRule' that is specified by the @RuleId@ . The maximum number of managed keys that will be blocked is 10,000. If more than 10,000 addresses exceed the rate limit, the 10,000 addresses with the highest rates will be blocked.
--
-- This operation returns paginated results.
module Network.AWS.WAF.GetRateBasedRuleManagedKeys
    (
    -- * Creating a request
      GetRateBasedRuleManagedKeys (..)
    , mkGetRateBasedRuleManagedKeys
    -- ** Request lenses
    , grbrmkRuleId
    , grbrmkNextMarker

    -- * Destructuring the response
    , GetRateBasedRuleManagedKeysResponse (..)
    , mkGetRateBasedRuleManagedKeysResponse
    -- ** Response lenses
    , grbrmkrrsManagedKeys
    , grbrmkrrsNextMarker
    , grbrmkrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WAF.Types as Types

-- | /See:/ 'mkGetRateBasedRuleManagedKeys' smart constructor.
data GetRateBasedRuleManagedKeys = GetRateBasedRuleManagedKeys'
  { ruleId :: Types.ResourceId
    -- ^ The @RuleId@ of the 'RateBasedRule' for which you want to get a list of @ManagedKeys@ . @RuleId@ is returned by 'CreateRateBasedRule' and by 'ListRateBasedRules' .
  , nextMarker :: Core.Maybe Types.NextMarker
    -- ^ A null value and not currently used. Do not include this in your request.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetRateBasedRuleManagedKeys' value with any optional fields omitted.
mkGetRateBasedRuleManagedKeys
    :: Types.ResourceId -- ^ 'ruleId'
    -> GetRateBasedRuleManagedKeys
mkGetRateBasedRuleManagedKeys ruleId
  = GetRateBasedRuleManagedKeys'{ruleId, nextMarker = Core.Nothing}

-- | The @RuleId@ of the 'RateBasedRule' for which you want to get a list of @ManagedKeys@ . @RuleId@ is returned by 'CreateRateBasedRule' and by 'ListRateBasedRules' .
--
-- /Note:/ Consider using 'ruleId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grbrmkRuleId :: Lens.Lens' GetRateBasedRuleManagedKeys Types.ResourceId
grbrmkRuleId = Lens.field @"ruleId"
{-# INLINEABLE grbrmkRuleId #-}
{-# DEPRECATED ruleId "Use generic-lens or generic-optics with 'ruleId' instead"  #-}

-- | A null value and not currently used. Do not include this in your request.
--
-- /Note:/ Consider using 'nextMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grbrmkNextMarker :: Lens.Lens' GetRateBasedRuleManagedKeys (Core.Maybe Types.NextMarker)
grbrmkNextMarker = Lens.field @"nextMarker"
{-# INLINEABLE grbrmkNextMarker #-}
{-# DEPRECATED nextMarker "Use generic-lens or generic-optics with 'nextMarker' instead"  #-}

instance Core.ToQuery GetRateBasedRuleManagedKeys where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetRateBasedRuleManagedKeys where
        toHeaders GetRateBasedRuleManagedKeys{..}
          = Core.pure
              ("X-Amz-Target", "AWSWAF_20150824.GetRateBasedRuleManagedKeys")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON GetRateBasedRuleManagedKeys where
        toJSON GetRateBasedRuleManagedKeys{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("RuleId" Core..= ruleId),
                  ("NextMarker" Core..=) Core.<$> nextMarker])

instance Core.AWSRequest GetRateBasedRuleManagedKeys where
        type Rs GetRateBasedRuleManagedKeys =
             GetRateBasedRuleManagedKeysResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetRateBasedRuleManagedKeysResponse' Core.<$>
                   (x Core..:? "ManagedKeys") Core.<*> x Core..:? "NextMarker"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager GetRateBasedRuleManagedKeys where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextMarker") = Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"managedKeys" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextMarker" Lens..~
                   rs Lens.^. Lens.field @"nextMarker")

-- | /See:/ 'mkGetRateBasedRuleManagedKeysResponse' smart constructor.
data GetRateBasedRuleManagedKeysResponse = GetRateBasedRuleManagedKeysResponse'
  { managedKeys :: Core.Maybe [Types.ManagedKey]
    -- ^ An array of IP addresses that currently are blocked by the specified 'RateBasedRule' . 
  , nextMarker :: Core.Maybe Types.NextMarker
    -- ^ A null value and not currently used.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetRateBasedRuleManagedKeysResponse' value with any optional fields omitted.
mkGetRateBasedRuleManagedKeysResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetRateBasedRuleManagedKeysResponse
mkGetRateBasedRuleManagedKeysResponse responseStatus
  = GetRateBasedRuleManagedKeysResponse'{managedKeys = Core.Nothing,
                                         nextMarker = Core.Nothing, responseStatus}

-- | An array of IP addresses that currently are blocked by the specified 'RateBasedRule' . 
--
-- /Note:/ Consider using 'managedKeys' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grbrmkrrsManagedKeys :: Lens.Lens' GetRateBasedRuleManagedKeysResponse (Core.Maybe [Types.ManagedKey])
grbrmkrrsManagedKeys = Lens.field @"managedKeys"
{-# INLINEABLE grbrmkrrsManagedKeys #-}
{-# DEPRECATED managedKeys "Use generic-lens or generic-optics with 'managedKeys' instead"  #-}

-- | A null value and not currently used.
--
-- /Note:/ Consider using 'nextMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grbrmkrrsNextMarker :: Lens.Lens' GetRateBasedRuleManagedKeysResponse (Core.Maybe Types.NextMarker)
grbrmkrrsNextMarker = Lens.field @"nextMarker"
{-# INLINEABLE grbrmkrrsNextMarker #-}
{-# DEPRECATED nextMarker "Use generic-lens or generic-optics with 'nextMarker' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grbrmkrrsResponseStatus :: Lens.Lens' GetRateBasedRuleManagedKeysResponse Core.Int
grbrmkrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE grbrmkrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
