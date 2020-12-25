{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.AutoScalingSettingsUpdate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.AutoScalingSettingsUpdate
  ( AutoScalingSettingsUpdate (..),

    -- * Smart constructor
    mkAutoScalingSettingsUpdate,

    -- * Lenses
    assuAutoScalingDisabled,
    assuAutoScalingRoleArn,
    assuMaximumUnits,
    assuMinimumUnits,
    assuScalingPolicyUpdate,
  )
where

import qualified Network.AWS.DynamoDB.Types.AutoScalingPolicyUpdate as Types
import qualified Network.AWS.DynamoDB.Types.AutoScalingRoleArn as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents the auto scaling settings to be modified for a global table or global secondary index.
--
-- /See:/ 'mkAutoScalingSettingsUpdate' smart constructor.
data AutoScalingSettingsUpdate = AutoScalingSettingsUpdate'
  { -- | Disabled auto scaling for this global table or global secondary index.
    autoScalingDisabled :: Core.Maybe Core.Bool,
    -- | Role ARN used for configuring auto scaling policy.
    autoScalingRoleArn :: Core.Maybe Types.AutoScalingRoleArn,
    -- | The maximum capacity units that a global table or global secondary index should be scaled up to.
    maximumUnits :: Core.Maybe Core.Natural,
    -- | The minimum capacity units that a global table or global secondary index should be scaled down to.
    minimumUnits :: Core.Maybe Core.Natural,
    -- | The scaling policy to apply for scaling target global table or global secondary index capacity units.
    scalingPolicyUpdate :: Core.Maybe Types.AutoScalingPolicyUpdate
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AutoScalingSettingsUpdate' value with any optional fields omitted.
mkAutoScalingSettingsUpdate ::
  AutoScalingSettingsUpdate
mkAutoScalingSettingsUpdate =
  AutoScalingSettingsUpdate'
    { autoScalingDisabled = Core.Nothing,
      autoScalingRoleArn = Core.Nothing,
      maximumUnits = Core.Nothing,
      minimumUnits = Core.Nothing,
      scalingPolicyUpdate = Core.Nothing
    }

-- | Disabled auto scaling for this global table or global secondary index.
--
-- /Note:/ Consider using 'autoScalingDisabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
assuAutoScalingDisabled :: Lens.Lens' AutoScalingSettingsUpdate (Core.Maybe Core.Bool)
assuAutoScalingDisabled = Lens.field @"autoScalingDisabled"
{-# DEPRECATED assuAutoScalingDisabled "Use generic-lens or generic-optics with 'autoScalingDisabled' instead." #-}

-- | Role ARN used for configuring auto scaling policy.
--
-- /Note:/ Consider using 'autoScalingRoleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
assuAutoScalingRoleArn :: Lens.Lens' AutoScalingSettingsUpdate (Core.Maybe Types.AutoScalingRoleArn)
assuAutoScalingRoleArn = Lens.field @"autoScalingRoleArn"
{-# DEPRECATED assuAutoScalingRoleArn "Use generic-lens or generic-optics with 'autoScalingRoleArn' instead." #-}

-- | The maximum capacity units that a global table or global secondary index should be scaled up to.
--
-- /Note:/ Consider using 'maximumUnits' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
assuMaximumUnits :: Lens.Lens' AutoScalingSettingsUpdate (Core.Maybe Core.Natural)
assuMaximumUnits = Lens.field @"maximumUnits"
{-# DEPRECATED assuMaximumUnits "Use generic-lens or generic-optics with 'maximumUnits' instead." #-}

-- | The minimum capacity units that a global table or global secondary index should be scaled down to.
--
-- /Note:/ Consider using 'minimumUnits' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
assuMinimumUnits :: Lens.Lens' AutoScalingSettingsUpdate (Core.Maybe Core.Natural)
assuMinimumUnits = Lens.field @"minimumUnits"
{-# DEPRECATED assuMinimumUnits "Use generic-lens or generic-optics with 'minimumUnits' instead." #-}

-- | The scaling policy to apply for scaling target global table or global secondary index capacity units.
--
-- /Note:/ Consider using 'scalingPolicyUpdate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
assuScalingPolicyUpdate :: Lens.Lens' AutoScalingSettingsUpdate (Core.Maybe Types.AutoScalingPolicyUpdate)
assuScalingPolicyUpdate = Lens.field @"scalingPolicyUpdate"
{-# DEPRECATED assuScalingPolicyUpdate "Use generic-lens or generic-optics with 'scalingPolicyUpdate' instead." #-}

instance Core.FromJSON AutoScalingSettingsUpdate where
  toJSON AutoScalingSettingsUpdate {..} =
    Core.object
      ( Core.catMaybes
          [ ("AutoScalingDisabled" Core..=) Core.<$> autoScalingDisabled,
            ("AutoScalingRoleArn" Core..=) Core.<$> autoScalingRoleArn,
            ("MaximumUnits" Core..=) Core.<$> maximumUnits,
            ("MinimumUnits" Core..=) Core.<$> minimumUnits,
            ("ScalingPolicyUpdate" Core..=) Core.<$> scalingPolicyUpdate
          ]
      )
