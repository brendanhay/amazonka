-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScalingPlans.Types.ScalingPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AutoScalingPlans.Types.ScalingPolicy
  ( ScalingPolicy (..),

    -- * Smart constructor
    mkScalingPolicy,

    -- * Lenses
    spTargetTrackingConfiguration,
    spPolicyName,
    spPolicyType,
  )
where

import Network.AWS.AutoScalingPlans.Types.PolicyType
import Network.AWS.AutoScalingPlans.Types.TargetTrackingConfiguration
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents a scaling policy.
--
-- /See:/ 'mkScalingPolicy' smart constructor.
data ScalingPolicy = ScalingPolicy'
  { targetTrackingConfiguration ::
      Lude.Maybe TargetTrackingConfiguration,
    policyName :: Lude.Text,
    policyType :: PolicyType
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ScalingPolicy' with the minimum fields required to make a request.
--
-- * 'policyName' - The name of the scaling policy.
-- * 'policyType' - The type of scaling policy.
-- * 'targetTrackingConfiguration' - The target tracking scaling policy. Includes support for predefined or customized metrics.
mkScalingPolicy ::
  -- | 'policyName'
  Lude.Text ->
  -- | 'policyType'
  PolicyType ->
  ScalingPolicy
mkScalingPolicy pPolicyName_ pPolicyType_ =
  ScalingPolicy'
    { targetTrackingConfiguration = Lude.Nothing,
      policyName = pPolicyName_,
      policyType = pPolicyType_
    }

-- | The target tracking scaling policy. Includes support for predefined or customized metrics.
--
-- /Note:/ Consider using 'targetTrackingConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spTargetTrackingConfiguration :: Lens.Lens' ScalingPolicy (Lude.Maybe TargetTrackingConfiguration)
spTargetTrackingConfiguration = Lens.lens (targetTrackingConfiguration :: ScalingPolicy -> Lude.Maybe TargetTrackingConfiguration) (\s a -> s {targetTrackingConfiguration = a} :: ScalingPolicy)
{-# DEPRECATED spTargetTrackingConfiguration "Use generic-lens or generic-optics with 'targetTrackingConfiguration' instead." #-}

-- | The name of the scaling policy.
--
-- /Note:/ Consider using 'policyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spPolicyName :: Lens.Lens' ScalingPolicy Lude.Text
spPolicyName = Lens.lens (policyName :: ScalingPolicy -> Lude.Text) (\s a -> s {policyName = a} :: ScalingPolicy)
{-# DEPRECATED spPolicyName "Use generic-lens or generic-optics with 'policyName' instead." #-}

-- | The type of scaling policy.
--
-- /Note:/ Consider using 'policyType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spPolicyType :: Lens.Lens' ScalingPolicy PolicyType
spPolicyType = Lens.lens (policyType :: ScalingPolicy -> PolicyType) (\s a -> s {policyType = a} :: ScalingPolicy)
{-# DEPRECATED spPolicyType "Use generic-lens or generic-optics with 'policyType' instead." #-}

instance Lude.FromJSON ScalingPolicy where
  parseJSON =
    Lude.withObject
      "ScalingPolicy"
      ( \x ->
          ScalingPolicy'
            Lude.<$> (x Lude..:? "TargetTrackingConfiguration")
            Lude.<*> (x Lude..: "PolicyName")
            Lude.<*> (x Lude..: "PolicyType")
      )
