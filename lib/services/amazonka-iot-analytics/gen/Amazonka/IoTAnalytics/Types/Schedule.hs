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
-- Module      : Amazonka.IoTAnalytics.Types.Schedule
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTAnalytics.Types.Schedule where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The schedule for when to trigger an update.
--
-- /See:/ 'newSchedule' smart constructor.
data Schedule = Schedule'
  { -- | The expression that defines when to trigger an update. For more
    -- information, see
    -- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/events/ScheduledEvents.html Schedule Expressions for Rules>
    -- in the /Amazon CloudWatch Events User Guide/.
    expression :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
newSchedule = Schedule' {expression = Prelude.Nothing}

-- | The expression that defines when to trigger an update. For more
-- information, see
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/events/ScheduledEvents.html Schedule Expressions for Rules>
-- in the /Amazon CloudWatch Events User Guide/.
schedule_expression :: Lens.Lens' Schedule (Prelude.Maybe Prelude.Text)
schedule_expression = Lens.lens (\Schedule' {expression} -> expression) (\s@Schedule' {} a -> s {expression = a} :: Schedule)

instance Data.FromJSON Schedule where
  parseJSON =
    Data.withObject
      "Schedule"
      ( \x ->
          Schedule' Prelude.<$> (x Data..:? "expression")
      )

instance Prelude.Hashable Schedule where
  hashWithSalt _salt Schedule' {..} =
    _salt `Prelude.hashWithSalt` expression

instance Prelude.NFData Schedule where
  rnf Schedule' {..} = Prelude.rnf expression

instance Data.ToJSON Schedule where
  toJSON Schedule' {..} =
    Data.object
      ( Prelude.catMaybes
          [("expression" Data..=) Prelude.<$> expression]
      )
