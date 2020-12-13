{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway.Types.CanarySettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.APIGateway.Types.CanarySettings
  ( CanarySettings (..),

    -- * Smart constructor
    mkCanarySettings,

    -- * Lenses
    csDeploymentId,
    csStageVariableOverrides,
    csUseStageCache,
    csPercentTraffic,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Configuration settings of a canary deployment.
--
-- /See:/ 'mkCanarySettings' smart constructor.
data CanarySettings = CanarySettings'
  { -- | The ID of the canary deployment.
    deploymentId :: Lude.Maybe Lude.Text,
    -- | Stage variables overridden for a canary release deployment, including new stage variables introduced in the canary. These stage variables are represented as a string-to-string map between stage variable names and their values.
    stageVariableOverrides :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    -- | A Boolean flag to indicate whether the canary deployment uses the stage cache or not.
    useStageCache :: Lude.Maybe Lude.Bool,
    -- | The percent (0-100) of traffic diverted to a canary deployment.
    percentTraffic :: Lude.Maybe Lude.Double
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CanarySettings' with the minimum fields required to make a request.
--
-- * 'deploymentId' - The ID of the canary deployment.
-- * 'stageVariableOverrides' - Stage variables overridden for a canary release deployment, including new stage variables introduced in the canary. These stage variables are represented as a string-to-string map between stage variable names and their values.
-- * 'useStageCache' - A Boolean flag to indicate whether the canary deployment uses the stage cache or not.
-- * 'percentTraffic' - The percent (0-100) of traffic diverted to a canary deployment.
mkCanarySettings ::
  CanarySettings
mkCanarySettings =
  CanarySettings'
    { deploymentId = Lude.Nothing,
      stageVariableOverrides = Lude.Nothing,
      useStageCache = Lude.Nothing,
      percentTraffic = Lude.Nothing
    }

-- | The ID of the canary deployment.
--
-- /Note:/ Consider using 'deploymentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csDeploymentId :: Lens.Lens' CanarySettings (Lude.Maybe Lude.Text)
csDeploymentId = Lens.lens (deploymentId :: CanarySettings -> Lude.Maybe Lude.Text) (\s a -> s {deploymentId = a} :: CanarySettings)
{-# DEPRECATED csDeploymentId "Use generic-lens or generic-optics with 'deploymentId' instead." #-}

-- | Stage variables overridden for a canary release deployment, including new stage variables introduced in the canary. These stage variables are represented as a string-to-string map between stage variable names and their values.
--
-- /Note:/ Consider using 'stageVariableOverrides' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csStageVariableOverrides :: Lens.Lens' CanarySettings (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
csStageVariableOverrides = Lens.lens (stageVariableOverrides :: CanarySettings -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {stageVariableOverrides = a} :: CanarySettings)
{-# DEPRECATED csStageVariableOverrides "Use generic-lens or generic-optics with 'stageVariableOverrides' instead." #-}

-- | A Boolean flag to indicate whether the canary deployment uses the stage cache or not.
--
-- /Note:/ Consider using 'useStageCache' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csUseStageCache :: Lens.Lens' CanarySettings (Lude.Maybe Lude.Bool)
csUseStageCache = Lens.lens (useStageCache :: CanarySettings -> Lude.Maybe Lude.Bool) (\s a -> s {useStageCache = a} :: CanarySettings)
{-# DEPRECATED csUseStageCache "Use generic-lens or generic-optics with 'useStageCache' instead." #-}

-- | The percent (0-100) of traffic diverted to a canary deployment.
--
-- /Note:/ Consider using 'percentTraffic' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csPercentTraffic :: Lens.Lens' CanarySettings (Lude.Maybe Lude.Double)
csPercentTraffic = Lens.lens (percentTraffic :: CanarySettings -> Lude.Maybe Lude.Double) (\s a -> s {percentTraffic = a} :: CanarySettings)
{-# DEPRECATED csPercentTraffic "Use generic-lens or generic-optics with 'percentTraffic' instead." #-}

instance Lude.FromJSON CanarySettings where
  parseJSON =
    Lude.withObject
      "CanarySettings"
      ( \x ->
          CanarySettings'
            Lude.<$> (x Lude..:? "deploymentId")
            Lude.<*> (x Lude..:? "stageVariableOverrides" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "useStageCache")
            Lude.<*> (x Lude..:? "percentTraffic")
      )

instance Lude.ToJSON CanarySettings where
  toJSON CanarySettings' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("deploymentId" Lude..=) Lude.<$> deploymentId,
            ("stageVariableOverrides" Lude..=) Lude.<$> stageVariableOverrides,
            ("useStageCache" Lude..=) Lude.<$> useStageCache,
            ("percentTraffic" Lude..=) Lude.<$> percentTraffic
          ]
      )
