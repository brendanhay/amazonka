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
-- Module      : Amazonka.TimeStreamQuery.Types.ScheduleConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.TimeStreamQuery.Types.ScheduleConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Configuration of the schedule of the query.
--
-- /See:/ 'newScheduleConfiguration' smart constructor.
data ScheduleConfiguration = ScheduleConfiguration'
  { -- | An expression that denotes when to trigger the scheduled query run. This
    -- can be a cron expression or a rate expression.
    scheduleExpression :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ScheduleConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'scheduleExpression', 'scheduleConfiguration_scheduleExpression' - An expression that denotes when to trigger the scheduled query run. This
-- can be a cron expression or a rate expression.
newScheduleConfiguration ::
  -- | 'scheduleExpression'
  Prelude.Text ->
  ScheduleConfiguration
newScheduleConfiguration pScheduleExpression_ =
  ScheduleConfiguration'
    { scheduleExpression =
        pScheduleExpression_
    }

-- | An expression that denotes when to trigger the scheduled query run. This
-- can be a cron expression or a rate expression.
scheduleConfiguration_scheduleExpression :: Lens.Lens' ScheduleConfiguration Prelude.Text
scheduleConfiguration_scheduleExpression = Lens.lens (\ScheduleConfiguration' {scheduleExpression} -> scheduleExpression) (\s@ScheduleConfiguration' {} a -> s {scheduleExpression = a} :: ScheduleConfiguration)

instance Core.FromJSON ScheduleConfiguration where
  parseJSON =
    Core.withObject
      "ScheduleConfiguration"
      ( \x ->
          ScheduleConfiguration'
            Prelude.<$> (x Core..: "ScheduleExpression")
      )

instance Prelude.Hashable ScheduleConfiguration where
  hashWithSalt _salt ScheduleConfiguration' {..} =
    _salt `Prelude.hashWithSalt` scheduleExpression

instance Prelude.NFData ScheduleConfiguration where
  rnf ScheduleConfiguration' {..} =
    Prelude.rnf scheduleExpression

instance Core.ToJSON ScheduleConfiguration where
  toJSON ScheduleConfiguration' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("ScheduleExpression" Core..= scheduleExpression)
          ]
      )
