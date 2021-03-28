{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.CreateCostCategoryDefinition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new Cost Category with the requested name and rules.
module Network.AWS.CostExplorer.CreateCostCategoryDefinition
    (
    -- * Creating a request
      CreateCostCategoryDefinition (..)
    , mkCreateCostCategoryDefinition
    -- ** Request lenses
    , cccdName
    , cccdRuleVersion
    , cccdRules

    -- * Destructuring the response
    , CreateCostCategoryDefinitionResponse (..)
    , mkCreateCostCategoryDefinitionResponse
    -- ** Response lenses
    , cccdrrsCostCategoryArn
    , cccdrrsEffectiveStart
    , cccdrrsResponseStatus
    ) where

import qualified Network.AWS.CostExplorer.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateCostCategoryDefinition' smart constructor.
data CreateCostCategoryDefinition = CreateCostCategoryDefinition'
  { name :: Types.Name
  , ruleVersion :: Types.CostCategoryRuleVersion
  , rules :: Core.NonEmpty Types.CostCategoryRule
    -- ^ The Cost Category rules used to categorize costs. For more information, see <https://docs.aws.amazon.com/aws-cost-management/latest/APIReference/API_CostCategoryRule.html CostCategoryRule> .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateCostCategoryDefinition' value with any optional fields omitted.
mkCreateCostCategoryDefinition
    :: Types.Name -- ^ 'name'
    -> Types.CostCategoryRuleVersion -- ^ 'ruleVersion'
    -> Core.NonEmpty Types.CostCategoryRule -- ^ 'rules'
    -> CreateCostCategoryDefinition
mkCreateCostCategoryDefinition name ruleVersion rules
  = CreateCostCategoryDefinition'{name, ruleVersion, rules}

-- | Undocumented field.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cccdName :: Lens.Lens' CreateCostCategoryDefinition Types.Name
cccdName = Lens.field @"name"
{-# INLINEABLE cccdName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'ruleVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cccdRuleVersion :: Lens.Lens' CreateCostCategoryDefinition Types.CostCategoryRuleVersion
cccdRuleVersion = Lens.field @"ruleVersion"
{-# INLINEABLE cccdRuleVersion #-}
{-# DEPRECATED ruleVersion "Use generic-lens or generic-optics with 'ruleVersion' instead"  #-}

-- | The Cost Category rules used to categorize costs. For more information, see <https://docs.aws.amazon.com/aws-cost-management/latest/APIReference/API_CostCategoryRule.html CostCategoryRule> .
--
-- /Note:/ Consider using 'rules' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cccdRules :: Lens.Lens' CreateCostCategoryDefinition (Core.NonEmpty Types.CostCategoryRule)
cccdRules = Lens.field @"rules"
{-# INLINEABLE cccdRules #-}
{-# DEPRECATED rules "Use generic-lens or generic-optics with 'rules' instead"  #-}

instance Core.ToQuery CreateCostCategoryDefinition where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateCostCategoryDefinition where
        toHeaders CreateCostCategoryDefinition{..}
          = Core.pure
              ("X-Amz-Target",
               "AWSInsightsIndexService.CreateCostCategoryDefinition")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CreateCostCategoryDefinition where
        toJSON CreateCostCategoryDefinition{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("Name" Core..= name),
                  Core.Just ("RuleVersion" Core..= ruleVersion),
                  Core.Just ("Rules" Core..= rules)])

instance Core.AWSRequest CreateCostCategoryDefinition where
        type Rs CreateCostCategoryDefinition =
             CreateCostCategoryDefinitionResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CreateCostCategoryDefinitionResponse' Core.<$>
                   (x Core..:? "CostCategoryArn") Core.<*> x Core..:? "EffectiveStart"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateCostCategoryDefinitionResponse' smart constructor.
data CreateCostCategoryDefinitionResponse = CreateCostCategoryDefinitionResponse'
  { costCategoryArn :: Core.Maybe Types.Arn
    -- ^ The unique identifier for your newly created Cost Category. 
  , effectiveStart :: Core.Maybe Types.ZonedDateTime
    -- ^ The Cost Category's effective start date. 
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateCostCategoryDefinitionResponse' value with any optional fields omitted.
mkCreateCostCategoryDefinitionResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateCostCategoryDefinitionResponse
mkCreateCostCategoryDefinitionResponse responseStatus
  = CreateCostCategoryDefinitionResponse'{costCategoryArn =
                                            Core.Nothing,
                                          effectiveStart = Core.Nothing, responseStatus}

-- | The unique identifier for your newly created Cost Category. 
--
-- /Note:/ Consider using 'costCategoryArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cccdrrsCostCategoryArn :: Lens.Lens' CreateCostCategoryDefinitionResponse (Core.Maybe Types.Arn)
cccdrrsCostCategoryArn = Lens.field @"costCategoryArn"
{-# INLINEABLE cccdrrsCostCategoryArn #-}
{-# DEPRECATED costCategoryArn "Use generic-lens or generic-optics with 'costCategoryArn' instead"  #-}

-- | The Cost Category's effective start date. 
--
-- /Note:/ Consider using 'effectiveStart' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cccdrrsEffectiveStart :: Lens.Lens' CreateCostCategoryDefinitionResponse (Core.Maybe Types.ZonedDateTime)
cccdrrsEffectiveStart = Lens.field @"effectiveStart"
{-# INLINEABLE cccdrrsEffectiveStart #-}
{-# DEPRECATED effectiveStart "Use generic-lens or generic-optics with 'effectiveStart' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cccdrrsResponseStatus :: Lens.Lens' CreateCostCategoryDefinitionResponse Core.Int
cccdrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE cccdrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
