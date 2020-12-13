{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway.Types.DeploymentCanarySettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.APIGateway.Types.DeploymentCanarySettings
  ( DeploymentCanarySettings (..),

    -- * Smart constructor
    mkDeploymentCanarySettings,

    -- * Lenses
    dcsStageVariableOverrides,
    dcsUseStageCache,
    dcsPercentTraffic,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The input configuration for a canary deployment.
--
-- /See:/ 'mkDeploymentCanarySettings' smart constructor.
data DeploymentCanarySettings = DeploymentCanarySettings'
  { -- | A stage variable overrides used for the canary release deployment. They can override existing stage variables or add new stage variables for the canary release deployment. These stage variables are represented as a string-to-string map between stage variable names and their values.
    stageVariableOverrides :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    -- | A Boolean flag to indicate whether the canary release deployment uses the stage cache or not.
    useStageCache :: Lude.Maybe Lude.Bool,
    -- | The percentage (0.0-100.0) of traffic routed to the canary deployment.
    percentTraffic :: Lude.Maybe Lude.Double
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeploymentCanarySettings' with the minimum fields required to make a request.
--
-- * 'stageVariableOverrides' - A stage variable overrides used for the canary release deployment. They can override existing stage variables or add new stage variables for the canary release deployment. These stage variables are represented as a string-to-string map between stage variable names and their values.
-- * 'useStageCache' - A Boolean flag to indicate whether the canary release deployment uses the stage cache or not.
-- * 'percentTraffic' - The percentage (0.0-100.0) of traffic routed to the canary deployment.
mkDeploymentCanarySettings ::
  DeploymentCanarySettings
mkDeploymentCanarySettings =
  DeploymentCanarySettings'
    { stageVariableOverrides = Lude.Nothing,
      useStageCache = Lude.Nothing,
      percentTraffic = Lude.Nothing
    }

-- | A stage variable overrides used for the canary release deployment. They can override existing stage variables or add new stage variables for the canary release deployment. These stage variables are represented as a string-to-string map between stage variable names and their values.
--
-- /Note:/ Consider using 'stageVariableOverrides' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsStageVariableOverrides :: Lens.Lens' DeploymentCanarySettings (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
dcsStageVariableOverrides = Lens.lens (stageVariableOverrides :: DeploymentCanarySettings -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {stageVariableOverrides = a} :: DeploymentCanarySettings)
{-# DEPRECATED dcsStageVariableOverrides "Use generic-lens or generic-optics with 'stageVariableOverrides' instead." #-}

-- | A Boolean flag to indicate whether the canary release deployment uses the stage cache or not.
--
-- /Note:/ Consider using 'useStageCache' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsUseStageCache :: Lens.Lens' DeploymentCanarySettings (Lude.Maybe Lude.Bool)
dcsUseStageCache = Lens.lens (useStageCache :: DeploymentCanarySettings -> Lude.Maybe Lude.Bool) (\s a -> s {useStageCache = a} :: DeploymentCanarySettings)
{-# DEPRECATED dcsUseStageCache "Use generic-lens or generic-optics with 'useStageCache' instead." #-}

-- | The percentage (0.0-100.0) of traffic routed to the canary deployment.
--
-- /Note:/ Consider using 'percentTraffic' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsPercentTraffic :: Lens.Lens' DeploymentCanarySettings (Lude.Maybe Lude.Double)
dcsPercentTraffic = Lens.lens (percentTraffic :: DeploymentCanarySettings -> Lude.Maybe Lude.Double) (\s a -> s {percentTraffic = a} :: DeploymentCanarySettings)
{-# DEPRECATED dcsPercentTraffic "Use generic-lens or generic-optics with 'percentTraffic' instead." #-}

instance Lude.ToJSON DeploymentCanarySettings where
  toJSON DeploymentCanarySettings' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("stageVariableOverrides" Lude..=)
              Lude.<$> stageVariableOverrides,
            ("useStageCache" Lude..=) Lude.<$> useStageCache,
            ("percentTraffic" Lude..=) Lude.<$> percentTraffic
          ]
      )
