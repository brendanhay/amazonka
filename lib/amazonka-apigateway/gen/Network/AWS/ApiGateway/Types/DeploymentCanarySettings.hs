{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ApiGateway.Types.DeploymentCanarySettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ApiGateway.Types.DeploymentCanarySettings
  ( DeploymentCanarySettings (..)
  -- * Smart constructor
  , mkDeploymentCanarySettings
  -- * Lenses
  , dcsPercentTraffic
  , dcsStageVariableOverrides
  , dcsUseStageCache
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The input configuration for a canary deployment.
--
-- /See:/ 'mkDeploymentCanarySettings' smart constructor.
data DeploymentCanarySettings = DeploymentCanarySettings'
  { percentTraffic :: Core.Maybe Core.Double
    -- ^ The percentage (0.0-100.0) of traffic routed to the canary deployment.
  , stageVariableOverrides :: Core.Maybe (Core.HashMap Core.Text Core.Text)
    -- ^ A stage variable overrides used for the canary release deployment. They can override existing stage variables or add new stage variables for the canary release deployment. These stage variables are represented as a string-to-string map between stage variable names and their values.
  , useStageCache :: Core.Maybe Core.Bool
    -- ^ A Boolean flag to indicate whether the canary release deployment uses the stage cache or not.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeploymentCanarySettings' value with any optional fields omitted.
mkDeploymentCanarySettings
    :: DeploymentCanarySettings
mkDeploymentCanarySettings
  = DeploymentCanarySettings'{percentTraffic = Core.Nothing,
                              stageVariableOverrides = Core.Nothing,
                              useStageCache = Core.Nothing}

-- | The percentage (0.0-100.0) of traffic routed to the canary deployment.
--
-- /Note:/ Consider using 'percentTraffic' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsPercentTraffic :: Lens.Lens' DeploymentCanarySettings (Core.Maybe Core.Double)
dcsPercentTraffic = Lens.field @"percentTraffic"
{-# INLINEABLE dcsPercentTraffic #-}
{-# DEPRECATED percentTraffic "Use generic-lens or generic-optics with 'percentTraffic' instead"  #-}

-- | A stage variable overrides used for the canary release deployment. They can override existing stage variables or add new stage variables for the canary release deployment. These stage variables are represented as a string-to-string map between stage variable names and their values.
--
-- /Note:/ Consider using 'stageVariableOverrides' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsStageVariableOverrides :: Lens.Lens' DeploymentCanarySettings (Core.Maybe (Core.HashMap Core.Text Core.Text))
dcsStageVariableOverrides = Lens.field @"stageVariableOverrides"
{-# INLINEABLE dcsStageVariableOverrides #-}
{-# DEPRECATED stageVariableOverrides "Use generic-lens or generic-optics with 'stageVariableOverrides' instead"  #-}

-- | A Boolean flag to indicate whether the canary release deployment uses the stage cache or not.
--
-- /Note:/ Consider using 'useStageCache' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsUseStageCache :: Lens.Lens' DeploymentCanarySettings (Core.Maybe Core.Bool)
dcsUseStageCache = Lens.field @"useStageCache"
{-# INLINEABLE dcsUseStageCache #-}
{-# DEPRECATED useStageCache "Use generic-lens or generic-optics with 'useStageCache' instead"  #-}

instance Core.FromJSON DeploymentCanarySettings where
        toJSON DeploymentCanarySettings{..}
          = Core.object
              (Core.catMaybes
                 [("percentTraffic" Core..=) Core.<$> percentTraffic,
                  ("stageVariableOverrides" Core..=) Core.<$> stageVariableOverrides,
                  ("useStageCache" Core..=) Core.<$> useStageCache])
