{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.AutoScalingPolicyUpdate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.AutoScalingPolicyUpdate
  ( AutoScalingPolicyUpdate (..),

    -- * Smart constructor
    mkAutoScalingPolicyUpdate,

    -- * Lenses
    aspuPolicyName,
    aspuTargetTrackingScalingPolicyConfiguration,
  )
where

import Network.AWS.DynamoDB.Types.AutoScalingTargetTrackingScalingPolicyConfigurationUpdate
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents the auto scaling policy to be modified.
--
-- /See:/ 'mkAutoScalingPolicyUpdate' smart constructor.
data AutoScalingPolicyUpdate = AutoScalingPolicyUpdate'
  { policyName ::
      Lude.Maybe Lude.Text,
    targetTrackingScalingPolicyConfiguration ::
      AutoScalingTargetTrackingScalingPolicyConfigurationUpdate
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AutoScalingPolicyUpdate' with the minimum fields required to make a request.
--
-- * 'policyName' - The name of the scaling policy.
-- * 'targetTrackingScalingPolicyConfiguration' - Represents a target tracking scaling policy configuration.
mkAutoScalingPolicyUpdate ::
  -- | 'targetTrackingScalingPolicyConfiguration'
  AutoScalingTargetTrackingScalingPolicyConfigurationUpdate ->
  AutoScalingPolicyUpdate
mkAutoScalingPolicyUpdate
  pTargetTrackingScalingPolicyConfiguration_ =
    AutoScalingPolicyUpdate'
      { policyName = Lude.Nothing,
        targetTrackingScalingPolicyConfiguration =
          pTargetTrackingScalingPolicyConfiguration_
      }

-- | The name of the scaling policy.
--
-- /Note:/ Consider using 'policyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aspuPolicyName :: Lens.Lens' AutoScalingPolicyUpdate (Lude.Maybe Lude.Text)
aspuPolicyName = Lens.lens (policyName :: AutoScalingPolicyUpdate -> Lude.Maybe Lude.Text) (\s a -> s {policyName = a} :: AutoScalingPolicyUpdate)
{-# DEPRECATED aspuPolicyName "Use generic-lens or generic-optics with 'policyName' instead." #-}

-- | Represents a target tracking scaling policy configuration.
--
-- /Note:/ Consider using 'targetTrackingScalingPolicyConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aspuTargetTrackingScalingPolicyConfiguration :: Lens.Lens' AutoScalingPolicyUpdate AutoScalingTargetTrackingScalingPolicyConfigurationUpdate
aspuTargetTrackingScalingPolicyConfiguration = Lens.lens (targetTrackingScalingPolicyConfiguration :: AutoScalingPolicyUpdate -> AutoScalingTargetTrackingScalingPolicyConfigurationUpdate) (\s a -> s {targetTrackingScalingPolicyConfiguration = a} :: AutoScalingPolicyUpdate)
{-# DEPRECATED aspuTargetTrackingScalingPolicyConfiguration "Use generic-lens or generic-optics with 'targetTrackingScalingPolicyConfiguration' instead." #-}

instance Lude.ToJSON AutoScalingPolicyUpdate where
  toJSON AutoScalingPolicyUpdate' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("PolicyName" Lude..=) Lude.<$> policyName,
            Lude.Just
              ( "TargetTrackingScalingPolicyConfiguration"
                  Lude..= targetTrackingScalingPolicyConfiguration
              )
          ]
      )
