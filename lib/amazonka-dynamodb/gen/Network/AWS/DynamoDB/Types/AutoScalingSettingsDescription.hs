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
    assdAutoScalingRoleArn,
    assdMaximumUnits,
    assdMinimumUnits,
    assdScalingPolicies,
  )
where

import qualified Network.AWS.DynamoDB.Types.AutoScalingPolicyDescription as Types
import qualified Network.AWS.DynamoDB.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents the auto scaling settings for a global table or global secondary index.
--
-- /See:/ 'mkAutoScalingSettingsDescription' smart constructor.
data AutoScalingSettingsDescription = AutoScalingSettingsDescription'
  { -- | Disabled auto scaling for this global table or global secondary index.
    autoScalingDisabled :: Core.Maybe Core.Bool,
    -- | Role ARN used for configuring the auto scaling policy.
    autoScalingRoleArn :: Core.Maybe Types.String,
    -- | The maximum capacity units that a global table or global secondary index should be scaled up to.
    maximumUnits :: Core.Maybe Core.Natural,
    -- | The minimum capacity units that a global table or global secondary index should be scaled down to.
    minimumUnits :: Core.Maybe Core.Natural,
    -- | Information about the scaling policies.
    scalingPolicies :: Core.Maybe [Types.AutoScalingPolicyDescription]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AutoScalingSettingsDescription' value with any optional fields omitted.
mkAutoScalingSettingsDescription ::
  AutoScalingSettingsDescription
mkAutoScalingSettingsDescription =
  AutoScalingSettingsDescription'
    { autoScalingDisabled =
        Core.Nothing,
      autoScalingRoleArn = Core.Nothing,
      maximumUnits = Core.Nothing,
      minimumUnits = Core.Nothing,
      scalingPolicies = Core.Nothing
    }

-- | Disabled auto scaling for this global table or global secondary index.
--
-- /Note:/ Consider using 'autoScalingDisabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
assdAutoScalingDisabled :: Lens.Lens' AutoScalingSettingsDescription (Core.Maybe Core.Bool)
assdAutoScalingDisabled = Lens.field @"autoScalingDisabled"
{-# DEPRECATED assdAutoScalingDisabled "Use generic-lens or generic-optics with 'autoScalingDisabled' instead." #-}

-- | Role ARN used for configuring the auto scaling policy.
--
-- /Note:/ Consider using 'autoScalingRoleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
assdAutoScalingRoleArn :: Lens.Lens' AutoScalingSettingsDescription (Core.Maybe Types.String)
assdAutoScalingRoleArn = Lens.field @"autoScalingRoleArn"
{-# DEPRECATED assdAutoScalingRoleArn "Use generic-lens or generic-optics with 'autoScalingRoleArn' instead." #-}

-- | The maximum capacity units that a global table or global secondary index should be scaled up to.
--
-- /Note:/ Consider using 'maximumUnits' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
assdMaximumUnits :: Lens.Lens' AutoScalingSettingsDescription (Core.Maybe Core.Natural)
assdMaximumUnits = Lens.field @"maximumUnits"
{-# DEPRECATED assdMaximumUnits "Use generic-lens or generic-optics with 'maximumUnits' instead." #-}

-- | The minimum capacity units that a global table or global secondary index should be scaled down to.
--
-- /Note:/ Consider using 'minimumUnits' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
assdMinimumUnits :: Lens.Lens' AutoScalingSettingsDescription (Core.Maybe Core.Natural)
assdMinimumUnits = Lens.field @"minimumUnits"
{-# DEPRECATED assdMinimumUnits "Use generic-lens or generic-optics with 'minimumUnits' instead." #-}

-- | Information about the scaling policies.
--
-- /Note:/ Consider using 'scalingPolicies' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
assdScalingPolicies :: Lens.Lens' AutoScalingSettingsDescription (Core.Maybe [Types.AutoScalingPolicyDescription])
assdScalingPolicies = Lens.field @"scalingPolicies"
{-# DEPRECATED assdScalingPolicies "Use generic-lens or generic-optics with 'scalingPolicies' instead." #-}

instance Core.FromJSON AutoScalingSettingsDescription where
  parseJSON =
    Core.withObject "AutoScalingSettingsDescription" Core.$
      \x ->
        AutoScalingSettingsDescription'
          Core.<$> (x Core..:? "AutoScalingDisabled")
          Core.<*> (x Core..:? "AutoScalingRoleArn")
          Core.<*> (x Core..:? "MaximumUnits")
          Core.<*> (x Core..:? "MinimumUnits")
          Core.<*> (x Core..:? "ScalingPolicies")
