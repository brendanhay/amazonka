-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.AutoScalingPolicyDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.AutoScalingPolicyDescription
  ( AutoScalingPolicyDescription (..),

    -- * Smart constructor
    mkAutoScalingPolicyDescription,

    -- * Lenses
    aspdStatus,
    aspdRules,
    aspdConstraints,
  )
where

import Network.AWS.EMR.Types.AutoScalingPolicyStatus
import Network.AWS.EMR.Types.ScalingConstraints
import Network.AWS.EMR.Types.ScalingRule
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | An automatic scaling policy for a core instance group or task instance group in an Amazon EMR cluster. The automatic scaling policy defines how an instance group dynamically adds and terminates EC2 instances in response to the value of a CloudWatch metric. See 'PutAutoScalingPolicy' .
--
-- /See:/ 'mkAutoScalingPolicyDescription' smart constructor.
data AutoScalingPolicyDescription = AutoScalingPolicyDescription'
  { status ::
      Lude.Maybe
        AutoScalingPolicyStatus,
    rules :: Lude.Maybe [ScalingRule],
    constraints ::
      Lude.Maybe ScalingConstraints
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
-- * 'constraints' - The upper and lower EC2 instance limits for an automatic scaling policy. Automatic scaling activity will not cause an instance group to grow above or below these limits.
-- * 'rules' - The scale-in and scale-out rules that comprise the automatic scaling policy.
-- * 'status' - The status of an automatic scaling policy.
mkAutoScalingPolicyDescription ::
  AutoScalingPolicyDescription
mkAutoScalingPolicyDescription =
  AutoScalingPolicyDescription'
    { status = Lude.Nothing,
      rules = Lude.Nothing,
      constraints = Lude.Nothing
    }

-- | The status of an automatic scaling policy.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aspdStatus :: Lens.Lens' AutoScalingPolicyDescription (Lude.Maybe AutoScalingPolicyStatus)
aspdStatus = Lens.lens (status :: AutoScalingPolicyDescription -> Lude.Maybe AutoScalingPolicyStatus) (\s a -> s {status = a} :: AutoScalingPolicyDescription)
{-# DEPRECATED aspdStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The scale-in and scale-out rules that comprise the automatic scaling policy.
--
-- /Note:/ Consider using 'rules' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aspdRules :: Lens.Lens' AutoScalingPolicyDescription (Lude.Maybe [ScalingRule])
aspdRules = Lens.lens (rules :: AutoScalingPolicyDescription -> Lude.Maybe [ScalingRule]) (\s a -> s {rules = a} :: AutoScalingPolicyDescription)
{-# DEPRECATED aspdRules "Use generic-lens or generic-optics with 'rules' instead." #-}

-- | The upper and lower EC2 instance limits for an automatic scaling policy. Automatic scaling activity will not cause an instance group to grow above or below these limits.
--
-- /Note:/ Consider using 'constraints' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aspdConstraints :: Lens.Lens' AutoScalingPolicyDescription (Lude.Maybe ScalingConstraints)
aspdConstraints = Lens.lens (constraints :: AutoScalingPolicyDescription -> Lude.Maybe ScalingConstraints) (\s a -> s {constraints = a} :: AutoScalingPolicyDescription)
{-# DEPRECATED aspdConstraints "Use generic-lens or generic-optics with 'constraints' instead." #-}

instance Lude.FromJSON AutoScalingPolicyDescription where
  parseJSON =
    Lude.withObject
      "AutoScalingPolicyDescription"
      ( \x ->
          AutoScalingPolicyDescription'
            Lude.<$> (x Lude..:? "Status")
            Lude.<*> (x Lude..:? "Rules" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "Constraints")
      )
