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
-- Module      : Amazonka.Scheduler.Types.ScheduleGroupSummary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Scheduler.Types.ScheduleGroupSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.Scheduler.Types.ScheduleGroupState

-- | The details of a schedule group.
--
-- /See:/ 'newScheduleGroupSummary' smart constructor.
data ScheduleGroupSummary = ScheduleGroupSummary'
  { -- | The name of the schedule group.
    name :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the schedule group.
    arn :: Prelude.Maybe Prelude.Text,
    -- | Specifies the state of the schedule group.
    state :: Prelude.Maybe ScheduleGroupState,
    -- | The time at which the schedule group was created.
    creationDate :: Prelude.Maybe Core.POSIX,
    -- | The time at which the schedule group was last modified.
    lastModificationDate :: Prelude.Maybe Core.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ScheduleGroupSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'scheduleGroupSummary_name' - The name of the schedule group.
--
-- 'arn', 'scheduleGroupSummary_arn' - The Amazon Resource Name (ARN) of the schedule group.
--
-- 'state', 'scheduleGroupSummary_state' - Specifies the state of the schedule group.
--
-- 'creationDate', 'scheduleGroupSummary_creationDate' - The time at which the schedule group was created.
--
-- 'lastModificationDate', 'scheduleGroupSummary_lastModificationDate' - The time at which the schedule group was last modified.
newScheduleGroupSummary ::
  ScheduleGroupSummary
newScheduleGroupSummary =
  ScheduleGroupSummary'
    { name = Prelude.Nothing,
      arn = Prelude.Nothing,
      state = Prelude.Nothing,
      creationDate = Prelude.Nothing,
      lastModificationDate = Prelude.Nothing
    }

-- | The name of the schedule group.
scheduleGroupSummary_name :: Lens.Lens' ScheduleGroupSummary (Prelude.Maybe Prelude.Text)
scheduleGroupSummary_name = Lens.lens (\ScheduleGroupSummary' {name} -> name) (\s@ScheduleGroupSummary' {} a -> s {name = a} :: ScheduleGroupSummary)

-- | The Amazon Resource Name (ARN) of the schedule group.
scheduleGroupSummary_arn :: Lens.Lens' ScheduleGroupSummary (Prelude.Maybe Prelude.Text)
scheduleGroupSummary_arn = Lens.lens (\ScheduleGroupSummary' {arn} -> arn) (\s@ScheduleGroupSummary' {} a -> s {arn = a} :: ScheduleGroupSummary)

-- | Specifies the state of the schedule group.
scheduleGroupSummary_state :: Lens.Lens' ScheduleGroupSummary (Prelude.Maybe ScheduleGroupState)
scheduleGroupSummary_state = Lens.lens (\ScheduleGroupSummary' {state} -> state) (\s@ScheduleGroupSummary' {} a -> s {state = a} :: ScheduleGroupSummary)

-- | The time at which the schedule group was created.
scheduleGroupSummary_creationDate :: Lens.Lens' ScheduleGroupSummary (Prelude.Maybe Prelude.UTCTime)
scheduleGroupSummary_creationDate = Lens.lens (\ScheduleGroupSummary' {creationDate} -> creationDate) (\s@ScheduleGroupSummary' {} a -> s {creationDate = a} :: ScheduleGroupSummary) Prelude.. Lens.mapping Core._Time

-- | The time at which the schedule group was last modified.
scheduleGroupSummary_lastModificationDate :: Lens.Lens' ScheduleGroupSummary (Prelude.Maybe Prelude.UTCTime)
scheduleGroupSummary_lastModificationDate = Lens.lens (\ScheduleGroupSummary' {lastModificationDate} -> lastModificationDate) (\s@ScheduleGroupSummary' {} a -> s {lastModificationDate = a} :: ScheduleGroupSummary) Prelude.. Lens.mapping Core._Time

instance Core.FromJSON ScheduleGroupSummary where
  parseJSON =
    Core.withObject
      "ScheduleGroupSummary"
      ( \x ->
          ScheduleGroupSummary'
            Prelude.<$> (x Core..:? "Name")
            Prelude.<*> (x Core..:? "Arn")
            Prelude.<*> (x Core..:? "State")
            Prelude.<*> (x Core..:? "CreationDate")
            Prelude.<*> (x Core..:? "LastModificationDate")
      )

instance Prelude.Hashable ScheduleGroupSummary where
  hashWithSalt _salt ScheduleGroupSummary' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` creationDate
      `Prelude.hashWithSalt` lastModificationDate

instance Prelude.NFData ScheduleGroupSummary where
  rnf ScheduleGroupSummary' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf creationDate
      `Prelude.seq` Prelude.rnf lastModificationDate
