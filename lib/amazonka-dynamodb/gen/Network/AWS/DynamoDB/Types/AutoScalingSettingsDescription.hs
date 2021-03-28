{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.AutoScalingSettingsDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DynamoDB.Types.AutoScalingSettingsDescription
  ( AutoScalingSettingsDescription (..)
  -- * Smart constructor
  , mkAutoScalingSettingsDescription
  -- * Lenses
  , assdAutoScalingDisabled
  , assdAutoScalingRoleArn
  , assdMaximumUnits
  , assdMinimumUnits
  , assdScalingPolicies
  ) where

import qualified Network.AWS.DynamoDB.Types.AutoScalingPolicyDescription as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents the auto scaling settings for a global table or global secondary index.
--
-- /See:/ 'mkAutoScalingSettingsDescription' smart constructor.
data AutoScalingSettingsDescription = AutoScalingSettingsDescription'
  { autoScalingDisabled :: Core.Maybe Core.Bool
    -- ^ Disabled auto scaling for this global table or global secondary index.
  , autoScalingRoleArn :: Core.Maybe Core.Text
    -- ^ Role ARN used for configuring the auto scaling policy.
  , maximumUnits :: Core.Maybe Core.Natural
    -- ^ The maximum capacity units that a global table or global secondary index should be scaled up to.
  , minimumUnits :: Core.Maybe Core.Natural
    -- ^ The minimum capacity units that a global table or global secondary index should be scaled down to.
  , scalingPolicies :: Core.Maybe [Types.AutoScalingPolicyDescription]
    -- ^ Information about the scaling policies.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AutoScalingSettingsDescription' value with any optional fields omitted.
mkAutoScalingSettingsDescription
    :: AutoScalingSettingsDescription
mkAutoScalingSettingsDescription
  = AutoScalingSettingsDescription'{autoScalingDisabled =
                                      Core.Nothing,
                                    autoScalingRoleArn = Core.Nothing, maximumUnits = Core.Nothing,
                                    minimumUnits = Core.Nothing, scalingPolicies = Core.Nothing}

-- | Disabled auto scaling for this global table or global secondary index.
--
-- /Note:/ Consider using 'autoScalingDisabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
assdAutoScalingDisabled :: Lens.Lens' AutoScalingSettingsDescription (Core.Maybe Core.Bool)
assdAutoScalingDisabled = Lens.field @"autoScalingDisabled"
{-# INLINEABLE assdAutoScalingDisabled #-}
{-# DEPRECATED autoScalingDisabled "Use generic-lens or generic-optics with 'autoScalingDisabled' instead"  #-}

-- | Role ARN used for configuring the auto scaling policy.
--
-- /Note:/ Consider using 'autoScalingRoleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
assdAutoScalingRoleArn :: Lens.Lens' AutoScalingSettingsDescription (Core.Maybe Core.Text)
assdAutoScalingRoleArn = Lens.field @"autoScalingRoleArn"
{-# INLINEABLE assdAutoScalingRoleArn #-}
{-# DEPRECATED autoScalingRoleArn "Use generic-lens or generic-optics with 'autoScalingRoleArn' instead"  #-}

-- | The maximum capacity units that a global table or global secondary index should be scaled up to.
--
-- /Note:/ Consider using 'maximumUnits' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
assdMaximumUnits :: Lens.Lens' AutoScalingSettingsDescription (Core.Maybe Core.Natural)
assdMaximumUnits = Lens.field @"maximumUnits"
{-# INLINEABLE assdMaximumUnits #-}
{-# DEPRECATED maximumUnits "Use generic-lens or generic-optics with 'maximumUnits' instead"  #-}

-- | The minimum capacity units that a global table or global secondary index should be scaled down to.
--
-- /Note:/ Consider using 'minimumUnits' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
assdMinimumUnits :: Lens.Lens' AutoScalingSettingsDescription (Core.Maybe Core.Natural)
assdMinimumUnits = Lens.field @"minimumUnits"
{-# INLINEABLE assdMinimumUnits #-}
{-# DEPRECATED minimumUnits "Use generic-lens or generic-optics with 'minimumUnits' instead"  #-}

-- | Information about the scaling policies.
--
-- /Note:/ Consider using 'scalingPolicies' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
assdScalingPolicies :: Lens.Lens' AutoScalingSettingsDescription (Core.Maybe [Types.AutoScalingPolicyDescription])
assdScalingPolicies = Lens.field @"scalingPolicies"
{-# INLINEABLE assdScalingPolicies #-}
{-# DEPRECATED scalingPolicies "Use generic-lens or generic-optics with 'scalingPolicies' instead"  #-}

instance Core.FromJSON AutoScalingSettingsDescription where
        parseJSON
          = Core.withObject "AutoScalingSettingsDescription" Core.$
              \ x ->
                AutoScalingSettingsDescription' Core.<$>
                  (x Core..:? "AutoScalingDisabled") Core.<*>
                    x Core..:? "AutoScalingRoleArn"
                    Core.<*> x Core..:? "MaximumUnits"
                    Core.<*> x Core..:? "MinimumUnits"
                    Core.<*> x Core..:? "ScalingPolicies"
