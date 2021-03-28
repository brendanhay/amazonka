{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.GetComplianceSummaryByConfigRule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the number of AWS Config rules that are compliant and noncompliant, up to a maximum of 25 for each.
module Network.AWS.Config.GetComplianceSummaryByConfigRule
    (
    -- * Creating a request
      GetComplianceSummaryByConfigRule (..)
    , mkGetComplianceSummaryByConfigRule

    -- * Destructuring the response
    , GetComplianceSummaryByConfigRuleResponse (..)
    , mkGetComplianceSummaryByConfigRuleResponse
    -- ** Response lenses
    , gcsbcrrrsComplianceSummary
    , gcsbcrrrsResponseStatus
    ) where

import qualified Network.AWS.Config.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetComplianceSummaryByConfigRule' smart constructor.
data GetComplianceSummaryByConfigRule = GetComplianceSummaryByConfigRule'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetComplianceSummaryByConfigRule' value with any optional fields omitted.
mkGetComplianceSummaryByConfigRule
    :: GetComplianceSummaryByConfigRule
mkGetComplianceSummaryByConfigRule
  = GetComplianceSummaryByConfigRule'

instance Core.ToQuery GetComplianceSummaryByConfigRule where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetComplianceSummaryByConfigRule where
        toHeaders GetComplianceSummaryByConfigRule{..}
          = Core.pure
              ("X-Amz-Target",
               "StarlingDoveService.GetComplianceSummaryByConfigRule")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON GetComplianceSummaryByConfigRule where
        toJSON _ = Core.Object Core.mempty

instance Core.AWSRequest GetComplianceSummaryByConfigRule where
        type Rs GetComplianceSummaryByConfigRule =
             GetComplianceSummaryByConfigRuleResponse
        toRequest x@_
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetComplianceSummaryByConfigRuleResponse' Core.<$>
                   (x Core..:? "ComplianceSummary") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | 
--
-- /See:/ 'mkGetComplianceSummaryByConfigRuleResponse' smart constructor.
data GetComplianceSummaryByConfigRuleResponse = GetComplianceSummaryByConfigRuleResponse'
  { complianceSummary :: Core.Maybe Types.ComplianceSummary
    -- ^ The number of AWS Config rules that are compliant and the number that are noncompliant, up to a maximum of 25 for each.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'GetComplianceSummaryByConfigRuleResponse' value with any optional fields omitted.
mkGetComplianceSummaryByConfigRuleResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetComplianceSummaryByConfigRuleResponse
mkGetComplianceSummaryByConfigRuleResponse responseStatus
  = GetComplianceSummaryByConfigRuleResponse'{complianceSummary =
                                                Core.Nothing,
                                              responseStatus}

-- | The number of AWS Config rules that are compliant and the number that are noncompliant, up to a maximum of 25 for each.
--
-- /Note:/ Consider using 'complianceSummary' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcsbcrrrsComplianceSummary :: Lens.Lens' GetComplianceSummaryByConfigRuleResponse (Core.Maybe Types.ComplianceSummary)
gcsbcrrrsComplianceSummary = Lens.field @"complianceSummary"
{-# INLINEABLE gcsbcrrrsComplianceSummary #-}
{-# DEPRECATED complianceSummary "Use generic-lens or generic-optics with 'complianceSummary' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcsbcrrrsResponseStatus :: Lens.Lens' GetComplianceSummaryByConfigRuleResponse Core.Int
gcsbcrrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gcsbcrrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
