-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.AutoScalingPolicyDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.AutoScalingPolicyDescription
  ( AutoScalingPolicyDescription (..),

    -- * Smart constructor
    mkAutoScalingPolicyDescription,

    -- * Lenses
    aspdPolicyName,
    aspdTargetTrackingScalingPolicyConfiguration,
  )
where

import Network.AWS.DynamoDB.Types.AutoScalingTargetTrackingScalingPolicyConfigurationDescription
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents the properties of the scaling policy.
--
-- /See:/ 'mkAutoScalingPolicyDescription' smart constructor.
data AutoScalingPolicyDescription = AutoScalingPolicyDescription'
  { policyName ::
      Lude.Maybe Lude.Text,
    targetTrackingScalingPolicyConfiguration ::
      Lude.Maybe
        AutoScalingTargetTrackingScalingPolicyConfigurationDescription
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AutoScalingPolicyDescription' with the minimum fields required to make a request.
--
-- * 'policyName' - The name of the scaling policy.
-- * 'targetTrackingScalingPolicyConfiguration' - Represents a target tracking scaling policy configuration.
mkAutoScalingPolicyDescription ::
  AutoScalingPolicyDescription
mkAutoScalingPolicyDescription =
  AutoScalingPolicyDescription'
    { policyName = Lude.Nothing,
      targetTrackingScalingPolicyConfiguration = Lude.Nothing
    }

-- | The name of the scaling policy.
--
-- /Note:/ Consider using 'policyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aspdPolicyName :: Lens.Lens' AutoScalingPolicyDescription (Lude.Maybe Lude.Text)
aspdPolicyName = Lens.lens (policyName :: AutoScalingPolicyDescription -> Lude.Maybe Lude.Text) (\s a -> s {policyName = a} :: AutoScalingPolicyDescription)
{-# DEPRECATED aspdPolicyName "Use generic-lens or generic-optics with 'policyName' instead." #-}

-- | Represents a target tracking scaling policy configuration.
--
-- /Note:/ Consider using 'targetTrackingScalingPolicyConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aspdTargetTrackingScalingPolicyConfiguration :: Lens.Lens' AutoScalingPolicyDescription (Lude.Maybe AutoScalingTargetTrackingScalingPolicyConfigurationDescription)
aspdTargetTrackingScalingPolicyConfiguration = Lens.lens (targetTrackingScalingPolicyConfiguration :: AutoScalingPolicyDescription -> Lude.Maybe AutoScalingTargetTrackingScalingPolicyConfigurationDescription) (\s a -> s {targetTrackingScalingPolicyConfiguration = a} :: AutoScalingPolicyDescription)
{-# DEPRECATED aspdTargetTrackingScalingPolicyConfiguration "Use generic-lens or generic-optics with 'targetTrackingScalingPolicyConfiguration' instead." #-}

instance Lude.FromJSON AutoScalingPolicyDescription where
  parseJSON =
    Lude.withObject
      "AutoScalingPolicyDescription"
      ( \x ->
          AutoScalingPolicyDescription'
            Lude.<$> (x Lude..:? "PolicyName")
            Lude.<*> (x Lude..:? "TargetTrackingScalingPolicyConfiguration")
      )
