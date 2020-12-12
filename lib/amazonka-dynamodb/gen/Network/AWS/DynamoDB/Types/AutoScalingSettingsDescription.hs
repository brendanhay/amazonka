{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.AutoScalingSettingsDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.AutoScalingSettingsDescription
  ( AutoScalingSettingsDescription (..),

    -- * Smart constructor
    mkAutoScalingSettingsDescription,

    -- * Lenses
    assdAutoScalingDisabled,
    assdMinimumUnits,
    assdMaximumUnits,
    assdScalingPolicies,
    assdAutoScalingRoleARN,
  )
where

import Network.AWS.DynamoDB.Types.AutoScalingPolicyDescription
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents the auto scaling settings for a global table or global secondary index.
--
-- /See:/ 'mkAutoScalingSettingsDescription' smart constructor.
data AutoScalingSettingsDescription = AutoScalingSettingsDescription'
  { autoScalingDisabled ::
      Lude.Maybe Lude.Bool,
    minimumUnits ::
      Lude.Maybe Lude.Natural,
    maximumUnits ::
      Lude.Maybe Lude.Natural,
    scalingPolicies ::
      Lude.Maybe
        [AutoScalingPolicyDescription],
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

-- | Creates a value of 'AutoScalingSettingsDescription' with the minimum fields required to make a request.
--
-- * 'autoScalingDisabled' - Disabled auto scaling for this global table or global secondary index.
-- * 'autoScalingRoleARN' - Role ARN used for configuring the auto scaling policy.
-- * 'maximumUnits' - The maximum capacity units that a global table or global secondary index should be scaled up to.
-- * 'minimumUnits' - The minimum capacity units that a global table or global secondary index should be scaled down to.
-- * 'scalingPolicies' - Information about the scaling policies.
mkAutoScalingSettingsDescription ::
  AutoScalingSettingsDescription
mkAutoScalingSettingsDescription =
  AutoScalingSettingsDescription'
    { autoScalingDisabled =
        Lude.Nothing,
      minimumUnits = Lude.Nothing,
      maximumUnits = Lude.Nothing,
      scalingPolicies = Lude.Nothing,
      autoScalingRoleARN = Lude.Nothing
    }

-- | Disabled auto scaling for this global table or global secondary index.
--
-- /Note:/ Consider using 'autoScalingDisabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
assdAutoScalingDisabled :: Lens.Lens' AutoScalingSettingsDescription (Lude.Maybe Lude.Bool)
assdAutoScalingDisabled = Lens.lens (autoScalingDisabled :: AutoScalingSettingsDescription -> Lude.Maybe Lude.Bool) (\s a -> s {autoScalingDisabled = a} :: AutoScalingSettingsDescription)
{-# DEPRECATED assdAutoScalingDisabled "Use generic-lens or generic-optics with 'autoScalingDisabled' instead." #-}

-- | The minimum capacity units that a global table or global secondary index should be scaled down to.
--
-- /Note:/ Consider using 'minimumUnits' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
assdMinimumUnits :: Lens.Lens' AutoScalingSettingsDescription (Lude.Maybe Lude.Natural)
assdMinimumUnits = Lens.lens (minimumUnits :: AutoScalingSettingsDescription -> Lude.Maybe Lude.Natural) (\s a -> s {minimumUnits = a} :: AutoScalingSettingsDescription)
{-# DEPRECATED assdMinimumUnits "Use generic-lens or generic-optics with 'minimumUnits' instead." #-}

-- | The maximum capacity units that a global table or global secondary index should be scaled up to.
--
-- /Note:/ Consider using 'maximumUnits' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
assdMaximumUnits :: Lens.Lens' AutoScalingSettingsDescription (Lude.Maybe Lude.Natural)
assdMaximumUnits = Lens.lens (maximumUnits :: AutoScalingSettingsDescription -> Lude.Maybe Lude.Natural) (\s a -> s {maximumUnits = a} :: AutoScalingSettingsDescription)
{-# DEPRECATED assdMaximumUnits "Use generic-lens or generic-optics with 'maximumUnits' instead." #-}

-- | Information about the scaling policies.
--
-- /Note:/ Consider using 'scalingPolicies' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
assdScalingPolicies :: Lens.Lens' AutoScalingSettingsDescription (Lude.Maybe [AutoScalingPolicyDescription])
assdScalingPolicies = Lens.lens (scalingPolicies :: AutoScalingSettingsDescription -> Lude.Maybe [AutoScalingPolicyDescription]) (\s a -> s {scalingPolicies = a} :: AutoScalingSettingsDescription)
{-# DEPRECATED assdScalingPolicies "Use generic-lens or generic-optics with 'scalingPolicies' instead." #-}

-- | Role ARN used for configuring the auto scaling policy.
--
-- /Note:/ Consider using 'autoScalingRoleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
assdAutoScalingRoleARN :: Lens.Lens' AutoScalingSettingsDescription (Lude.Maybe Lude.Text)
assdAutoScalingRoleARN = Lens.lens (autoScalingRoleARN :: AutoScalingSettingsDescription -> Lude.Maybe Lude.Text) (\s a -> s {autoScalingRoleARN = a} :: AutoScalingSettingsDescription)
{-# DEPRECATED assdAutoScalingRoleARN "Use generic-lens or generic-optics with 'autoScalingRoleARN' instead." #-}

instance Lude.FromJSON AutoScalingSettingsDescription where
  parseJSON =
    Lude.withObject
      "AutoScalingSettingsDescription"
      ( \x ->
          AutoScalingSettingsDescription'
            Lude.<$> (x Lude..:? "AutoScalingDisabled")
            Lude.<*> (x Lude..:? "MinimumUnits")
            Lude.<*> (x Lude..:? "MaximumUnits")
            Lude.<*> (x Lude..:? "ScalingPolicies" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "AutoScalingRoleArn")
      )
