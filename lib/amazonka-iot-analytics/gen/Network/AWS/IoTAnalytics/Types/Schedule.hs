{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.Types.Schedule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTAnalytics.Types.Schedule
  ( Schedule (..),

    -- * Smart constructor
    mkSchedule,

    -- * Lenses
    sExpression,
  )
where

import qualified Network.AWS.IoTAnalytics.Types.Expression as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The schedule for when to trigger an update.
--
-- /See:/ 'mkSchedule' smart constructor.
newtype Schedule = Schedule'
  { -- | The expression that defines when to trigger an update. For more information, see <https://docs.aws.amazon.com/AmazonCloudWatch/latest/events/ScheduledEvents.html Schedule Expressions for Rules> in the /Amazon CloudWatch Events User Guide/ .
    expression :: Core.Maybe Types.Expression
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'Schedule' value with any optional fields omitted.
mkSchedule ::
  Schedule
mkSchedule = Schedule' {expression = Core.Nothing}

-- | The expression that defines when to trigger an update. For more information, see <https://docs.aws.amazon.com/AmazonCloudWatch/latest/events/ScheduledEvents.html Schedule Expressions for Rules> in the /Amazon CloudWatch Events User Guide/ .
--
-- /Note:/ Consider using 'expression' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sExpression :: Lens.Lens' Schedule (Core.Maybe Types.Expression)
sExpression = Lens.field @"expression"
{-# DEPRECATED sExpression "Use generic-lens or generic-optics with 'expression' instead." #-}

instance Core.FromJSON Schedule where
  toJSON Schedule {..} =
    Core.object
      (Core.catMaybes [("expression" Core..=) Core.<$> expression])

instance Core.FromJSON Schedule where
  parseJSON =
    Core.withObject "Schedule" Core.$
      \x -> Schedule' Core.<$> (x Core..:? "expression")
