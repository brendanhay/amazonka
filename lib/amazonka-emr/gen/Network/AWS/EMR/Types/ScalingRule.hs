{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.ScalingRule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.ScalingRule
  ( ScalingRule (..),

    -- * Smart constructor
    mkScalingRule,

    -- * Lenses
    srName,
    srAction,
    srTrigger,
    srDescription,
  )
where

import qualified Network.AWS.EMR.Types.ScalingAction as Types
import qualified Network.AWS.EMR.Types.ScalingTrigger as Types
import qualified Network.AWS.EMR.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A scale-in or scale-out rule that defines scaling activity, including the CloudWatch metric alarm that triggers activity, how EC2 instances are added or removed, and the periodicity of adjustments. The automatic scaling policy for an instance group can comprise one or more automatic scaling rules.
--
-- /See:/ 'mkScalingRule' smart constructor.
data ScalingRule = ScalingRule'
  { -- | The name used to identify an automatic scaling rule. Rule names must be unique within a scaling policy.
    name :: Types.String,
    -- | The conditions that trigger an automatic scaling activity.
    action :: Types.ScalingAction,
    -- | The CloudWatch alarm definition that determines when automatic scaling activity is triggered.
    trigger :: Types.ScalingTrigger,
    -- | A friendly, more verbose description of the automatic scaling rule.
    description :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ScalingRule' value with any optional fields omitted.
mkScalingRule ::
  -- | 'name'
  Types.String ->
  -- | 'action'
  Types.ScalingAction ->
  -- | 'trigger'
  Types.ScalingTrigger ->
  ScalingRule
mkScalingRule name action trigger =
  ScalingRule' {name, action, trigger, description = Core.Nothing}

-- | The name used to identify an automatic scaling rule. Rule names must be unique within a scaling policy.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srName :: Lens.Lens' ScalingRule Types.String
srName = Lens.field @"name"
{-# DEPRECATED srName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The conditions that trigger an automatic scaling activity.
--
-- /Note:/ Consider using 'action' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srAction :: Lens.Lens' ScalingRule Types.ScalingAction
srAction = Lens.field @"action"
{-# DEPRECATED srAction "Use generic-lens or generic-optics with 'action' instead." #-}

-- | The CloudWatch alarm definition that determines when automatic scaling activity is triggered.
--
-- /Note:/ Consider using 'trigger' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srTrigger :: Lens.Lens' ScalingRule Types.ScalingTrigger
srTrigger = Lens.field @"trigger"
{-# DEPRECATED srTrigger "Use generic-lens or generic-optics with 'trigger' instead." #-}

-- | A friendly, more verbose description of the automatic scaling rule.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srDescription :: Lens.Lens' ScalingRule (Core.Maybe Types.String)
srDescription = Lens.field @"description"
{-# DEPRECATED srDescription "Use generic-lens or generic-optics with 'description' instead." #-}

instance Core.FromJSON ScalingRule where
  toJSON ScalingRule {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Name" Core..= name),
            Core.Just ("Action" Core..= action),
            Core.Just ("Trigger" Core..= trigger),
            ("Description" Core..=) Core.<$> description
          ]
      )

instance Core.FromJSON ScalingRule where
  parseJSON =
    Core.withObject "ScalingRule" Core.$
      \x ->
        ScalingRule'
          Core.<$> (x Core..: "Name")
          Core.<*> (x Core..: "Action")
          Core.<*> (x Core..: "Trigger")
          Core.<*> (x Core..:? "Description")
