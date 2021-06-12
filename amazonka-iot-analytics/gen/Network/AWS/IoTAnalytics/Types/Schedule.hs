{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.Types.Schedule
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTAnalytics.Types.Schedule where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | The schedule for when to trigger an update.
--
-- /See:/ 'newSchedule' smart constructor.
data Schedule = Schedule'
  { -- | The expression that defines when to trigger an update. For more
    -- information, see
    -- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/events/ScheduledEvents.html Schedule Expressions for Rules>
    -- in the /Amazon CloudWatch Events User Guide/.
    expression :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Schedule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'expression', 'schedule_expression' - The expression that defines when to trigger an update. For more
-- information, see
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/events/ScheduledEvents.html Schedule Expressions for Rules>
-- in the /Amazon CloudWatch Events User Guide/.
newSchedule ::
  Schedule
newSchedule = Schedule' {expression = Core.Nothing}

-- | The expression that defines when to trigger an update. For more
-- information, see
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/events/ScheduledEvents.html Schedule Expressions for Rules>
-- in the /Amazon CloudWatch Events User Guide/.
schedule_expression :: Lens.Lens' Schedule (Core.Maybe Core.Text)
schedule_expression = Lens.lens (\Schedule' {expression} -> expression) (\s@Schedule' {} a -> s {expression = a} :: Schedule)

instance Core.FromJSON Schedule where
  parseJSON =
    Core.withObject
      "Schedule"
      (\x -> Schedule' Core.<$> (x Core..:? "expression"))

instance Core.Hashable Schedule

instance Core.NFData Schedule

instance Core.ToJSON Schedule where
  toJSON Schedule' {..} =
    Core.object
      ( Core.catMaybes
          [("expression" Core..=) Core.<$> expression]
      )
