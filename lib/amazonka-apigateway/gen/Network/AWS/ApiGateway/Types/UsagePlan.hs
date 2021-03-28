{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ApiGateway.Types.UsagePlan
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ApiGateway.Types.UsagePlan
  ( UsagePlan (..)
  -- * Smart constructor
  , mkUsagePlan
  -- * Lenses
  , upApiStages
  , upDescription
  , upId
  , upName
  , upProductCode
  , upQuota
  , upTags
  , upThrottle
  ) where

import qualified Network.AWS.ApiGateway.Types.ApiStage as Types
import qualified Network.AWS.ApiGateway.Types.QuotaSettings as Types
import qualified Network.AWS.ApiGateway.Types.ThrottleSettings as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents a usage plan than can specify who can assess associated API stages with specified request limits and quotas.
--
-- In a usage plan, you associate an API by specifying the API's Id and a stage name of the specified API. You add plan customers by adding API keys to the plan. 
-- <https://docs.aws.amazon.com/apigateway/latest/developerguide/api-gateway-api-usage-plans.html Create and Use Usage Plans> 
--
-- /See:/ 'mkUsagePlan' smart constructor.
data UsagePlan = UsagePlan'
  { apiStages :: Core.Maybe [Types.ApiStage]
    -- ^ The associated API stages of a usage plan.
  , description :: Core.Maybe Core.Text
    -- ^ The description of a usage plan.
  , id :: Core.Maybe Core.Text
    -- ^ The identifier of a 'UsagePlan' resource.
  , name :: Core.Maybe Core.Text
    -- ^ The name of a usage plan.
  , productCode :: Core.Maybe Core.Text
    -- ^ The AWS Markeplace product identifier to associate with the usage plan as a SaaS product on AWS Marketplace.
  , quota :: Core.Maybe Types.QuotaSettings
    -- ^ The maximum number of permitted requests per a given unit time interval.
  , tags :: Core.Maybe (Core.HashMap Core.Text Core.Text)
    -- ^ The collection of tags. Each tag element is associated with a given resource.
  , throttle :: Core.Maybe Types.ThrottleSettings
    -- ^ The request throttle limits of a usage plan.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UsagePlan' value with any optional fields omitted.
mkUsagePlan
    :: UsagePlan
mkUsagePlan
  = UsagePlan'{apiStages = Core.Nothing, description = Core.Nothing,
               id = Core.Nothing, name = Core.Nothing, productCode = Core.Nothing,
               quota = Core.Nothing, tags = Core.Nothing, throttle = Core.Nothing}

-- | The associated API stages of a usage plan.
--
-- /Note:/ Consider using 'apiStages' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upApiStages :: Lens.Lens' UsagePlan (Core.Maybe [Types.ApiStage])
upApiStages = Lens.field @"apiStages"
{-# INLINEABLE upApiStages #-}
{-# DEPRECATED apiStages "Use generic-lens or generic-optics with 'apiStages' instead"  #-}

-- | The description of a usage plan.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upDescription :: Lens.Lens' UsagePlan (Core.Maybe Core.Text)
upDescription = Lens.field @"description"
{-# INLINEABLE upDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | The identifier of a 'UsagePlan' resource.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upId :: Lens.Lens' UsagePlan (Core.Maybe Core.Text)
upId = Lens.field @"id"
{-# INLINEABLE upId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

-- | The name of a usage plan.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upName :: Lens.Lens' UsagePlan (Core.Maybe Core.Text)
upName = Lens.field @"name"
{-# INLINEABLE upName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The AWS Markeplace product identifier to associate with the usage plan as a SaaS product on AWS Marketplace.
--
-- /Note:/ Consider using 'productCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upProductCode :: Lens.Lens' UsagePlan (Core.Maybe Core.Text)
upProductCode = Lens.field @"productCode"
{-# INLINEABLE upProductCode #-}
{-# DEPRECATED productCode "Use generic-lens or generic-optics with 'productCode' instead"  #-}

-- | The maximum number of permitted requests per a given unit time interval.
--
-- /Note:/ Consider using 'quota' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upQuota :: Lens.Lens' UsagePlan (Core.Maybe Types.QuotaSettings)
upQuota = Lens.field @"quota"
{-# INLINEABLE upQuota #-}
{-# DEPRECATED quota "Use generic-lens or generic-optics with 'quota' instead"  #-}

-- | The collection of tags. Each tag element is associated with a given resource.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upTags :: Lens.Lens' UsagePlan (Core.Maybe (Core.HashMap Core.Text Core.Text))
upTags = Lens.field @"tags"
{-# INLINEABLE upTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

-- | The request throttle limits of a usage plan.
--
-- /Note:/ Consider using 'throttle' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upThrottle :: Lens.Lens' UsagePlan (Core.Maybe Types.ThrottleSettings)
upThrottle = Lens.field @"throttle"
{-# INLINEABLE upThrottle #-}
{-# DEPRECATED throttle "Use generic-lens or generic-optics with 'throttle' instead"  #-}

instance Core.FromJSON UsagePlan where
        parseJSON
          = Core.withObject "UsagePlan" Core.$
              \ x ->
                UsagePlan' Core.<$>
                  (x Core..:? "apiStages") Core.<*> x Core..:? "description" Core.<*>
                    x Core..:? "id"
                    Core.<*> x Core..:? "name"
                    Core.<*> x Core..:? "productCode"
                    Core.<*> x Core..:? "quota"
                    Core.<*> x Core..:? "tags"
                    Core.<*> x Core..:? "throttle"
