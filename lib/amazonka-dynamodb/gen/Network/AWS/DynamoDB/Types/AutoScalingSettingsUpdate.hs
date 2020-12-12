{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.AutoScalingSettingsUpdate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.AutoScalingSettingsUpdate
  ( AutoScalingSettingsUpdate (..),

    -- * Smart constructor
    mkAutoScalingSettingsUpdate,

    -- * Lenses
    assuAutoScalingDisabled,
    assuMinimumUnits,
    assuScalingPolicyUpdate,
    assuMaximumUnits,
    assuAutoScalingRoleARN,
  )
where

import Network.AWS.DynamoDB.Types.AutoScalingPolicyUpdate
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents the auto scaling settings to be modified for a global table or global secondary index.
--
-- /See:/ 'mkAutoScalingSettingsUpdate' smart constructor.
data AutoScalingSettingsUpdate = AutoScalingSettingsUpdate'
  { autoScalingDisabled ::
      Lude.Maybe Lude.Bool,
    minimumUnits :: Lude.Maybe Lude.Natural,
    scalingPolicyUpdate ::
      Lude.Maybe AutoScalingPolicyUpdate,
    maximumUnits :: Lude.Maybe Lude.Natural,
    autoScalingRoleARN ::
      Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AutoScalingSettingsUpdate' with the minimum fields required to make a request.
--
-- * 'autoScalingDisabled' - Disabled auto scaling for this global table or global secondary index.
-- * 'autoScalingRoleARN' - Role ARN used for configuring auto scaling policy.
-- * 'maximumUnits' - The maximum capacity units that a global table or global secondary index should be scaled up to.
-- * 'minimumUnits' - The minimum capacity units that a global table or global secondary index should be scaled down to.
-- * 'scalingPolicyUpdate' - The scaling policy to apply for scaling target global table or global secondary index capacity units.
mkAutoScalingSettingsUpdate ::
  AutoScalingSettingsUpdate
mkAutoScalingSettingsUpdate =
  AutoScalingSettingsUpdate'
    { autoScalingDisabled = Lude.Nothing,
      minimumUnits = Lude.Nothing,
      scalingPolicyUpdate = Lude.Nothing,
      maximumUnits = Lude.Nothing,
      autoScalingRoleARN = Lude.Nothing
    }

-- | Disabled auto scaling for this global table or global secondary index.
--
-- /Note:/ Consider using 'autoScalingDisabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
assuAutoScalingDisabled :: Lens.Lens' AutoScalingSettingsUpdate (Lude.Maybe Lude.Bool)
assuAutoScalingDisabled = Lens.lens (autoScalingDisabled :: AutoScalingSettingsUpdate -> Lude.Maybe Lude.Bool) (\s a -> s {autoScalingDisabled = a} :: AutoScalingSettingsUpdate)
{-# DEPRECATED assuAutoScalingDisabled "Use generic-lens or generic-optics with 'autoScalingDisabled' instead." #-}

-- | The minimum capacity units that a global table or global secondary index should be scaled down to.
--
-- /Note:/ Consider using 'minimumUnits' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
assuMinimumUnits :: Lens.Lens' AutoScalingSettingsUpdate (Lude.Maybe Lude.Natural)
assuMinimumUnits = Lens.lens (minimumUnits :: AutoScalingSettingsUpdate -> Lude.Maybe Lude.Natural) (\s a -> s {minimumUnits = a} :: AutoScalingSettingsUpdate)
{-# DEPRECATED assuMinimumUnits "Use generic-lens or generic-optics with 'minimumUnits' instead." #-}

-- | The scaling policy to apply for scaling target global table or global secondary index capacity units.
--
-- /Note:/ Consider using 'scalingPolicyUpdate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
assuScalingPolicyUpdate :: Lens.Lens' AutoScalingSettingsUpdate (Lude.Maybe AutoScalingPolicyUpdate)
assuScalingPolicyUpdate = Lens.lens (scalingPolicyUpdate :: AutoScalingSettingsUpdate -> Lude.Maybe AutoScalingPolicyUpdate) (\s a -> s {scalingPolicyUpdate = a} :: AutoScalingSettingsUpdate)
{-# DEPRECATED assuScalingPolicyUpdate "Use generic-lens or generic-optics with 'scalingPolicyUpdate' instead." #-}

-- | The maximum capacity units that a global table or global secondary index should be scaled up to.
--
-- /Note:/ Consider using 'maximumUnits' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
assuMaximumUnits :: Lens.Lens' AutoScalingSettingsUpdate (Lude.Maybe Lude.Natural)
assuMaximumUnits = Lens.lens (maximumUnits :: AutoScalingSettingsUpdate -> Lude.Maybe Lude.Natural) (\s a -> s {maximumUnits = a} :: AutoScalingSettingsUpdate)
{-# DEPRECATED assuMaximumUnits "Use generic-lens or generic-optics with 'maximumUnits' instead." #-}

-- | Role ARN used for configuring auto scaling policy.
--
-- /Note:/ Consider using 'autoScalingRoleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
assuAutoScalingRoleARN :: Lens.Lens' AutoScalingSettingsUpdate (Lude.Maybe Lude.Text)
assuAutoScalingRoleARN = Lens.lens (autoScalingRoleARN :: AutoScalingSettingsUpdate -> Lude.Maybe Lude.Text) (\s a -> s {autoScalingRoleARN = a} :: AutoScalingSettingsUpdate)
{-# DEPRECATED assuAutoScalingRoleARN "Use generic-lens or generic-optics with 'autoScalingRoleARN' instead." #-}

instance Lude.ToJSON AutoScalingSettingsUpdate where
  toJSON AutoScalingSettingsUpdate' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("AutoScalingDisabled" Lude..=) Lude.<$> autoScalingDisabled,
            ("MinimumUnits" Lude..=) Lude.<$> minimumUnits,
            ("ScalingPolicyUpdate" Lude..=) Lude.<$> scalingPolicyUpdate,
            ("MaximumUnits" Lude..=) Lude.<$> maximumUnits,
            ("AutoScalingRoleArn" Lude..=) Lude.<$> autoScalingRoleARN
          ]
      )
