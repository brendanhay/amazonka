-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.AutoScalingPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.AutoScalingPolicy
  ( AutoScalingPolicy (..),

    -- * Smart constructor
    mkAutoScalingPolicy,

    -- * Lenses
    aspConstraints,
    aspRules,
  )
where

import Network.AWS.EMR.Types.ScalingConstraints
import Network.AWS.EMR.Types.ScalingRule
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | An automatic scaling policy for a core instance group or task instance group in an Amazon EMR cluster. An automatic scaling policy defines how an instance group dynamically adds and terminates EC2 instances in response to the value of a CloudWatch metric. See 'PutAutoScalingPolicy' .
--
-- /See:/ 'mkAutoScalingPolicy' smart constructor.
data AutoScalingPolicy = AutoScalingPolicy'
  { constraints ::
      ScalingConstraints,
    rules :: [ScalingRule]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AutoScalingPolicy' with the minimum fields required to make a request.
--
-- * 'constraints' - The upper and lower EC2 instance limits for an automatic scaling policy. Automatic scaling activity will not cause an instance group to grow above or below these limits.
-- * 'rules' - The scale-in and scale-out rules that comprise the automatic scaling policy.
mkAutoScalingPolicy ::
  -- | 'constraints'
  ScalingConstraints ->
  AutoScalingPolicy
mkAutoScalingPolicy pConstraints_ =
  AutoScalingPolicy'
    { constraints = pConstraints_,
      rules = Lude.mempty
    }

-- | The upper and lower EC2 instance limits for an automatic scaling policy. Automatic scaling activity will not cause an instance group to grow above or below these limits.
--
-- /Note:/ Consider using 'constraints' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aspConstraints :: Lens.Lens' AutoScalingPolicy ScalingConstraints
aspConstraints = Lens.lens (constraints :: AutoScalingPolicy -> ScalingConstraints) (\s a -> s {constraints = a} :: AutoScalingPolicy)
{-# DEPRECATED aspConstraints "Use generic-lens or generic-optics with 'constraints' instead." #-}

-- | The scale-in and scale-out rules that comprise the automatic scaling policy.
--
-- /Note:/ Consider using 'rules' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aspRules :: Lens.Lens' AutoScalingPolicy [ScalingRule]
aspRules = Lens.lens (rules :: AutoScalingPolicy -> [ScalingRule]) (\s a -> s {rules = a} :: AutoScalingPolicy)
{-# DEPRECATED aspRules "Use generic-lens or generic-optics with 'rules' instead." #-}

instance Lude.ToJSON AutoScalingPolicy where
  toJSON AutoScalingPolicy' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("Constraints" Lude..= constraints),
            Lude.Just ("Rules" Lude..= rules)
          ]
      )
