{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.AutoScalingPolicyDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DynamoDB.Types.AutoScalingPolicyDescription
  ( AutoScalingPolicyDescription (..)
  -- * Smart constructor
  , mkAutoScalingPolicyDescription
  -- * Lenses
  , aspdPolicyName
  , aspdTargetTrackingScalingPolicyConfiguration
  ) where

import qualified Network.AWS.DynamoDB.Types.AutoScalingPolicyName as Types
import qualified Network.AWS.DynamoDB.Types.AutoScalingTargetTrackingScalingPolicyConfigurationDescription as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents the properties of the scaling policy.
--
-- /See:/ 'mkAutoScalingPolicyDescription' smart constructor.
data AutoScalingPolicyDescription = AutoScalingPolicyDescription'
  { policyName :: Core.Maybe Types.AutoScalingPolicyName
    -- ^ The name of the scaling policy.
  , targetTrackingScalingPolicyConfiguration :: Core.Maybe Types.AutoScalingTargetTrackingScalingPolicyConfigurationDescription
    -- ^ Represents a target tracking scaling policy configuration.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AutoScalingPolicyDescription' value with any optional fields omitted.
mkAutoScalingPolicyDescription
    :: AutoScalingPolicyDescription
mkAutoScalingPolicyDescription
  = AutoScalingPolicyDescription'{policyName = Core.Nothing,
                                  targetTrackingScalingPolicyConfiguration = Core.Nothing}

-- | The name of the scaling policy.
--
-- /Note:/ Consider using 'policyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aspdPolicyName :: Lens.Lens' AutoScalingPolicyDescription (Core.Maybe Types.AutoScalingPolicyName)
aspdPolicyName = Lens.field @"policyName"
{-# INLINEABLE aspdPolicyName #-}
{-# DEPRECATED policyName "Use generic-lens or generic-optics with 'policyName' instead"  #-}

-- | Represents a target tracking scaling policy configuration.
--
-- /Note:/ Consider using 'targetTrackingScalingPolicyConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aspdTargetTrackingScalingPolicyConfiguration :: Lens.Lens' AutoScalingPolicyDescription (Core.Maybe Types.AutoScalingTargetTrackingScalingPolicyConfigurationDescription)
aspdTargetTrackingScalingPolicyConfiguration = Lens.field @"targetTrackingScalingPolicyConfiguration"
{-# INLINEABLE aspdTargetTrackingScalingPolicyConfiguration #-}
{-# DEPRECATED targetTrackingScalingPolicyConfiguration "Use generic-lens or generic-optics with 'targetTrackingScalingPolicyConfiguration' instead"  #-}

instance Core.FromJSON AutoScalingPolicyDescription where
        parseJSON
          = Core.withObject "AutoScalingPolicyDescription" Core.$
              \ x ->
                AutoScalingPolicyDescription' Core.<$>
                  (x Core..:? "PolicyName") Core.<*>
                    x Core..:? "TargetTrackingScalingPolicyConfiguration"
