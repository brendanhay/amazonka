{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ApiGateway.Types.ApiStage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ApiGateway.Types.ApiStage
  ( ApiStage (..)
  -- * Smart constructor
  , mkApiStage
  -- * Lenses
  , asApiId
  , asStage
  , asThrottle
  ) where

import qualified Network.AWS.ApiGateway.Types.ThrottleSettings as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | API stage name of the associated API stage in a usage plan.
--
-- /See:/ 'mkApiStage' smart constructor.
data ApiStage = ApiStage'
  { apiId :: Core.Maybe Core.Text
    -- ^ API Id of the associated API stage in a usage plan.
  , stage :: Core.Maybe Core.Text
    -- ^ API stage name of the associated API stage in a usage plan.
  , throttle :: Core.Maybe (Core.HashMap Core.Text Types.ThrottleSettings)
    -- ^ Map containing method level throttling information for API stage in a usage plan.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ApiStage' value with any optional fields omitted.
mkApiStage
    :: ApiStage
mkApiStage
  = ApiStage'{apiId = Core.Nothing, stage = Core.Nothing,
              throttle = Core.Nothing}

-- | API Id of the associated API stage in a usage plan.
--
-- /Note:/ Consider using 'apiId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asApiId :: Lens.Lens' ApiStage (Core.Maybe Core.Text)
asApiId = Lens.field @"apiId"
{-# INLINEABLE asApiId #-}
{-# DEPRECATED apiId "Use generic-lens or generic-optics with 'apiId' instead"  #-}

-- | API stage name of the associated API stage in a usage plan.
--
-- /Note:/ Consider using 'stage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asStage :: Lens.Lens' ApiStage (Core.Maybe Core.Text)
asStage = Lens.field @"stage"
{-# INLINEABLE asStage #-}
{-# DEPRECATED stage "Use generic-lens or generic-optics with 'stage' instead"  #-}

-- | Map containing method level throttling information for API stage in a usage plan.
--
-- /Note:/ Consider using 'throttle' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asThrottle :: Lens.Lens' ApiStage (Core.Maybe (Core.HashMap Core.Text Types.ThrottleSettings))
asThrottle = Lens.field @"throttle"
{-# INLINEABLE asThrottle #-}
{-# DEPRECATED throttle "Use generic-lens or generic-optics with 'throttle' instead"  #-}

instance Core.FromJSON ApiStage where
        toJSON ApiStage{..}
          = Core.object
              (Core.catMaybes
                 [("apiId" Core..=) Core.<$> apiId,
                  ("stage" Core..=) Core.<$> stage,
                  ("throttle" Core..=) Core.<$> throttle])

instance Core.FromJSON ApiStage where
        parseJSON
          = Core.withObject "ApiStage" Core.$
              \ x ->
                ApiStage' Core.<$>
                  (x Core..:? "apiId") Core.<*> x Core..:? "stage" Core.<*>
                    x Core..:? "throttle"
