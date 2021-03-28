{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ApiGateway.Types.CanarySettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ApiGateway.Types.CanarySettings
  ( CanarySettings (..)
  -- * Smart constructor
  , mkCanarySettings
  -- * Lenses
  , csDeploymentId
  , csPercentTraffic
  , csStageVariableOverrides
  , csUseStageCache
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Configuration settings of a canary deployment.
--
-- /See:/ 'mkCanarySettings' smart constructor.
data CanarySettings = CanarySettings'
  { deploymentId :: Core.Maybe Core.Text
    -- ^ The ID of the canary deployment.
  , percentTraffic :: Core.Maybe Core.Double
    -- ^ The percent (0-100) of traffic diverted to a canary deployment.
  , stageVariableOverrides :: Core.Maybe (Core.HashMap Core.Text Core.Text)
    -- ^ Stage variables overridden for a canary release deployment, including new stage variables introduced in the canary. These stage variables are represented as a string-to-string map between stage variable names and their values.
  , useStageCache :: Core.Maybe Core.Bool
    -- ^ A Boolean flag to indicate whether the canary deployment uses the stage cache or not.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CanarySettings' value with any optional fields omitted.
mkCanarySettings
    :: CanarySettings
mkCanarySettings
  = CanarySettings'{deploymentId = Core.Nothing,
                    percentTraffic = Core.Nothing,
                    stageVariableOverrides = Core.Nothing,
                    useStageCache = Core.Nothing}

-- | The ID of the canary deployment.
--
-- /Note:/ Consider using 'deploymentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csDeploymentId :: Lens.Lens' CanarySettings (Core.Maybe Core.Text)
csDeploymentId = Lens.field @"deploymentId"
{-# INLINEABLE csDeploymentId #-}
{-# DEPRECATED deploymentId "Use generic-lens or generic-optics with 'deploymentId' instead"  #-}

-- | The percent (0-100) of traffic diverted to a canary deployment.
--
-- /Note:/ Consider using 'percentTraffic' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csPercentTraffic :: Lens.Lens' CanarySettings (Core.Maybe Core.Double)
csPercentTraffic = Lens.field @"percentTraffic"
{-# INLINEABLE csPercentTraffic #-}
{-# DEPRECATED percentTraffic "Use generic-lens or generic-optics with 'percentTraffic' instead"  #-}

-- | Stage variables overridden for a canary release deployment, including new stage variables introduced in the canary. These stage variables are represented as a string-to-string map between stage variable names and their values.
--
-- /Note:/ Consider using 'stageVariableOverrides' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csStageVariableOverrides :: Lens.Lens' CanarySettings (Core.Maybe (Core.HashMap Core.Text Core.Text))
csStageVariableOverrides = Lens.field @"stageVariableOverrides"
{-# INLINEABLE csStageVariableOverrides #-}
{-# DEPRECATED stageVariableOverrides "Use generic-lens or generic-optics with 'stageVariableOverrides' instead"  #-}

-- | A Boolean flag to indicate whether the canary deployment uses the stage cache or not.
--
-- /Note:/ Consider using 'useStageCache' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csUseStageCache :: Lens.Lens' CanarySettings (Core.Maybe Core.Bool)
csUseStageCache = Lens.field @"useStageCache"
{-# INLINEABLE csUseStageCache #-}
{-# DEPRECATED useStageCache "Use generic-lens or generic-optics with 'useStageCache' instead"  #-}

instance Core.FromJSON CanarySettings where
        toJSON CanarySettings{..}
          = Core.object
              (Core.catMaybes
                 [("deploymentId" Core..=) Core.<$> deploymentId,
                  ("percentTraffic" Core..=) Core.<$> percentTraffic,
                  ("stageVariableOverrides" Core..=) Core.<$> stageVariableOverrides,
                  ("useStageCache" Core..=) Core.<$> useStageCache])

instance Core.FromJSON CanarySettings where
        parseJSON
          = Core.withObject "CanarySettings" Core.$
              \ x ->
                CanarySettings' Core.<$>
                  (x Core..:? "deploymentId") Core.<*> x Core..:? "percentTraffic"
                    Core.<*> x Core..:? "stageVariableOverrides"
                    Core.<*> x Core..:? "useStageCache"
