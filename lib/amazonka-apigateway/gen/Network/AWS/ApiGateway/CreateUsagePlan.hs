{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ApiGateway.CreateUsagePlan
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a usage plan with the throttle and quota limits, as well as the associated API stages, specified in the payload. 
module Network.AWS.ApiGateway.CreateUsagePlan
    (
    -- * Creating a request
      CreateUsagePlan (..)
    , mkCreateUsagePlan
    -- ** Request lenses
    , cupName
    , cupApiStages
    , cupDescription
    , cupQuota
    , cupTags
    , cupThrottle

     -- * Destructuring the response
    , Types.UsagePlan (..)
    , Types.mkUsagePlan
    -- ** Response lenses
    , Types.upApiStages
    , Types.upDescription
    , Types.upId
    , Types.upName
    , Types.upProductCode
    , Types.upQuota
    , Types.upTags
    , Types.upThrottle
    ) where

import qualified Network.AWS.ApiGateway.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The POST request to create a usage plan with the name, description, throttle limits and quota limits, as well as the associated API stages, specified in the payload.
--
-- /See:/ 'mkCreateUsagePlan' smart constructor.
data CreateUsagePlan = CreateUsagePlan'
  { name :: Core.Text
    -- ^ [Required] The name of the usage plan.
  , apiStages :: Core.Maybe [Types.ApiStage]
    -- ^ The associated API stages of the usage plan.
  , description :: Core.Maybe Core.Text
    -- ^ The description of the usage plan.
  , quota :: Core.Maybe Types.QuotaSettings
    -- ^ The quota of the usage plan.
  , tags :: Core.Maybe (Core.HashMap Core.Text Core.Text)
    -- ^ The key-value map of strings. The valid character set is [a-zA-Z+-=._:/]. The tag key can be up to 128 characters and must not start with @aws:@ . The tag value can be up to 256 characters.
  , throttle :: Core.Maybe Types.ThrottleSettings
    -- ^ The throttling limits of the usage plan.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateUsagePlan' value with any optional fields omitted.
mkCreateUsagePlan
    :: Core.Text -- ^ 'name'
    -> CreateUsagePlan
mkCreateUsagePlan name
  = CreateUsagePlan'{name, apiStages = Core.Nothing,
                     description = Core.Nothing, quota = Core.Nothing,
                     tags = Core.Nothing, throttle = Core.Nothing}

-- | [Required] The name of the usage plan.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cupName :: Lens.Lens' CreateUsagePlan Core.Text
cupName = Lens.field @"name"
{-# INLINEABLE cupName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The associated API stages of the usage plan.
--
-- /Note:/ Consider using 'apiStages' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cupApiStages :: Lens.Lens' CreateUsagePlan (Core.Maybe [Types.ApiStage])
cupApiStages = Lens.field @"apiStages"
{-# INLINEABLE cupApiStages #-}
{-# DEPRECATED apiStages "Use generic-lens or generic-optics with 'apiStages' instead"  #-}

-- | The description of the usage plan.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cupDescription :: Lens.Lens' CreateUsagePlan (Core.Maybe Core.Text)
cupDescription = Lens.field @"description"
{-# INLINEABLE cupDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | The quota of the usage plan.
--
-- /Note:/ Consider using 'quota' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cupQuota :: Lens.Lens' CreateUsagePlan (Core.Maybe Types.QuotaSettings)
cupQuota = Lens.field @"quota"
{-# INLINEABLE cupQuota #-}
{-# DEPRECATED quota "Use generic-lens or generic-optics with 'quota' instead"  #-}

-- | The key-value map of strings. The valid character set is [a-zA-Z+-=._:/]. The tag key can be up to 128 characters and must not start with @aws:@ . The tag value can be up to 256 characters.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cupTags :: Lens.Lens' CreateUsagePlan (Core.Maybe (Core.HashMap Core.Text Core.Text))
cupTags = Lens.field @"tags"
{-# INLINEABLE cupTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

-- | The throttling limits of the usage plan.
--
-- /Note:/ Consider using 'throttle' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cupThrottle :: Lens.Lens' CreateUsagePlan (Core.Maybe Types.ThrottleSettings)
cupThrottle = Lens.field @"throttle"
{-# INLINEABLE cupThrottle #-}
{-# DEPRECATED throttle "Use generic-lens or generic-optics with 'throttle' instead"  #-}

instance Core.ToQuery CreateUsagePlan where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateUsagePlan where
        toHeaders CreateUsagePlan{..}
          = Core.pure ("Accept", "application/json")

instance Core.FromJSON CreateUsagePlan where
        toJSON CreateUsagePlan{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("name" Core..= name),
                  ("apiStages" Core..=) Core.<$> apiStages,
                  ("description" Core..=) Core.<$> description,
                  ("quota" Core..=) Core.<$> quota, ("tags" Core..=) Core.<$> tags,
                  ("throttle" Core..=) Core.<$> throttle])

instance Core.AWSRequest CreateUsagePlan where
        type Rs CreateUsagePlan = Types.UsagePlan
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/usageplans",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON (\ s h x -> Core.eitherParseJSON x)
        
        {-# INLINE parseResponse #-}
