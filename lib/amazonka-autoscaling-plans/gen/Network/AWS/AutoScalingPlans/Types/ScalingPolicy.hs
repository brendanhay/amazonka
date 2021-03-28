{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScalingPlans.Types.ScalingPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AutoScalingPlans.Types.ScalingPolicy
  ( ScalingPolicy (..)
  -- * Smart constructor
  , mkScalingPolicy
  -- * Lenses
  , spPolicyName
  , spPolicyType
  , spTargetTrackingConfiguration
  ) where

import qualified Network.AWS.AutoScalingPlans.Types.PolicyName as Types
import qualified Network.AWS.AutoScalingPlans.Types.PolicyType as Types
import qualified Network.AWS.AutoScalingPlans.Types.TargetTrackingConfiguration as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents a scaling policy.
--
-- /See:/ 'mkScalingPolicy' smart constructor.
data ScalingPolicy = ScalingPolicy'
  { policyName :: Types.PolicyName
    -- ^ The name of the scaling policy.
  , policyType :: Types.PolicyType
    -- ^ The type of scaling policy.
  , targetTrackingConfiguration :: Core.Maybe Types.TargetTrackingConfiguration
    -- ^ The target tracking scaling policy. Includes support for predefined or customized metrics.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ScalingPolicy' value with any optional fields omitted.
mkScalingPolicy
    :: Types.PolicyName -- ^ 'policyName'
    -> Types.PolicyType -- ^ 'policyType'
    -> ScalingPolicy
mkScalingPolicy policyName policyType
  = ScalingPolicy'{policyName, policyType,
                   targetTrackingConfiguration = Core.Nothing}

-- | The name of the scaling policy.
--
-- /Note:/ Consider using 'policyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spPolicyName :: Lens.Lens' ScalingPolicy Types.PolicyName
spPolicyName = Lens.field @"policyName"
{-# INLINEABLE spPolicyName #-}
{-# DEPRECATED policyName "Use generic-lens or generic-optics with 'policyName' instead"  #-}

-- | The type of scaling policy.
--
-- /Note:/ Consider using 'policyType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spPolicyType :: Lens.Lens' ScalingPolicy Types.PolicyType
spPolicyType = Lens.field @"policyType"
{-# INLINEABLE spPolicyType #-}
{-# DEPRECATED policyType "Use generic-lens or generic-optics with 'policyType' instead"  #-}

-- | The target tracking scaling policy. Includes support for predefined or customized metrics.
--
-- /Note:/ Consider using 'targetTrackingConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spTargetTrackingConfiguration :: Lens.Lens' ScalingPolicy (Core.Maybe Types.TargetTrackingConfiguration)
spTargetTrackingConfiguration = Lens.field @"targetTrackingConfiguration"
{-# INLINEABLE spTargetTrackingConfiguration #-}
{-# DEPRECATED targetTrackingConfiguration "Use generic-lens or generic-optics with 'targetTrackingConfiguration' instead"  #-}

instance Core.FromJSON ScalingPolicy where
        parseJSON
          = Core.withObject "ScalingPolicy" Core.$
              \ x ->
                ScalingPolicy' Core.<$>
                  (x Core..: "PolicyName") Core.<*> x Core..: "PolicyType" Core.<*>
                    x Core..:? "TargetTrackingConfiguration"
