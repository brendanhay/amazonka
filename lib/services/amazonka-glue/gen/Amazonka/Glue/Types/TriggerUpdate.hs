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
-- Module      : Amazonka.Glue.Types.TriggerUpdate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.TriggerUpdate where

import qualified Amazonka.Core as Core
import Amazonka.Glue.Types.Action
import Amazonka.Glue.Types.EventBatchingCondition
import Amazonka.Glue.Types.Predicate
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | A structure used to provide information used to update a trigger. This
-- object updates the previous trigger definition by overwriting it
-- completely.
--
-- /See:/ 'newTriggerUpdate' smart constructor.
data TriggerUpdate = TriggerUpdate'
  { -- | The actions initiated by this trigger.
    actions :: Prelude.Maybe [Action],
    -- | A @cron@ expression used to specify the schedule (see
    -- <https://docs.aws.amazon.com/glue/latest/dg/monitor-data-warehouse-schedule.html Time-Based Schedules for Jobs and Crawlers>.
    -- For example, to run something every day at 12:15 UTC, you would specify:
    -- @cron(15 12 * * ? *)@.
    schedule :: Prelude.Maybe Prelude.Text,
    -- | The predicate of this trigger, which defines when it will fire.
    predicate :: Prelude.Maybe Predicate,
    -- | Reserved for future use.
    name :: Prelude.Maybe Prelude.Text,
    -- | Batch condition that must be met (specified number of events received or
    -- batch time window expired) before EventBridge event trigger fires.
    eventBatchingCondition :: Prelude.Maybe EventBatchingCondition,
    -- | A description of this trigger.
    description :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TriggerUpdate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'actions', 'triggerUpdate_actions' - The actions initiated by this trigger.
--
-- 'schedule', 'triggerUpdate_schedule' - A @cron@ expression used to specify the schedule (see
-- <https://docs.aws.amazon.com/glue/latest/dg/monitor-data-warehouse-schedule.html Time-Based Schedules for Jobs and Crawlers>.
-- For example, to run something every day at 12:15 UTC, you would specify:
-- @cron(15 12 * * ? *)@.
--
-- 'predicate', 'triggerUpdate_predicate' - The predicate of this trigger, which defines when it will fire.
--
-- 'name', 'triggerUpdate_name' - Reserved for future use.
--
-- 'eventBatchingCondition', 'triggerUpdate_eventBatchingCondition' - Batch condition that must be met (specified number of events received or
-- batch time window expired) before EventBridge event trigger fires.
--
-- 'description', 'triggerUpdate_description' - A description of this trigger.
newTriggerUpdate ::
  TriggerUpdate
newTriggerUpdate =
  TriggerUpdate'
    { actions = Prelude.Nothing,
      schedule = Prelude.Nothing,
      predicate = Prelude.Nothing,
      name = Prelude.Nothing,
      eventBatchingCondition = Prelude.Nothing,
      description = Prelude.Nothing
    }

-- | The actions initiated by this trigger.
triggerUpdate_actions :: Lens.Lens' TriggerUpdate (Prelude.Maybe [Action])
triggerUpdate_actions = Lens.lens (\TriggerUpdate' {actions} -> actions) (\s@TriggerUpdate' {} a -> s {actions = a} :: TriggerUpdate) Prelude.. Lens.mapping Lens.coerced

-- | A @cron@ expression used to specify the schedule (see
-- <https://docs.aws.amazon.com/glue/latest/dg/monitor-data-warehouse-schedule.html Time-Based Schedules for Jobs and Crawlers>.
-- For example, to run something every day at 12:15 UTC, you would specify:
-- @cron(15 12 * * ? *)@.
triggerUpdate_schedule :: Lens.Lens' TriggerUpdate (Prelude.Maybe Prelude.Text)
triggerUpdate_schedule = Lens.lens (\TriggerUpdate' {schedule} -> schedule) (\s@TriggerUpdate' {} a -> s {schedule = a} :: TriggerUpdate)

-- | The predicate of this trigger, which defines when it will fire.
triggerUpdate_predicate :: Lens.Lens' TriggerUpdate (Prelude.Maybe Predicate)
triggerUpdate_predicate = Lens.lens (\TriggerUpdate' {predicate} -> predicate) (\s@TriggerUpdate' {} a -> s {predicate = a} :: TriggerUpdate)

-- | Reserved for future use.
triggerUpdate_name :: Lens.Lens' TriggerUpdate (Prelude.Maybe Prelude.Text)
triggerUpdate_name = Lens.lens (\TriggerUpdate' {name} -> name) (\s@TriggerUpdate' {} a -> s {name = a} :: TriggerUpdate)

-- | Batch condition that must be met (specified number of events received or
-- batch time window expired) before EventBridge event trigger fires.
triggerUpdate_eventBatchingCondition :: Lens.Lens' TriggerUpdate (Prelude.Maybe EventBatchingCondition)
triggerUpdate_eventBatchingCondition = Lens.lens (\TriggerUpdate' {eventBatchingCondition} -> eventBatchingCondition) (\s@TriggerUpdate' {} a -> s {eventBatchingCondition = a} :: TriggerUpdate)

-- | A description of this trigger.
triggerUpdate_description :: Lens.Lens' TriggerUpdate (Prelude.Maybe Prelude.Text)
triggerUpdate_description = Lens.lens (\TriggerUpdate' {description} -> description) (\s@TriggerUpdate' {} a -> s {description = a} :: TriggerUpdate)

instance Prelude.Hashable TriggerUpdate where
  hashWithSalt _salt TriggerUpdate' {..} =
    _salt `Prelude.hashWithSalt` actions
      `Prelude.hashWithSalt` schedule
      `Prelude.hashWithSalt` predicate
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` eventBatchingCondition
      `Prelude.hashWithSalt` description

instance Prelude.NFData TriggerUpdate where
  rnf TriggerUpdate' {..} =
    Prelude.rnf actions
      `Prelude.seq` Prelude.rnf schedule
      `Prelude.seq` Prelude.rnf predicate
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf eventBatchingCondition
      `Prelude.seq` Prelude.rnf description

instance Core.ToJSON TriggerUpdate where
  toJSON TriggerUpdate' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Actions" Core..=) Prelude.<$> actions,
            ("Schedule" Core..=) Prelude.<$> schedule,
            ("Predicate" Core..=) Prelude.<$> predicate,
            ("Name" Core..=) Prelude.<$> name,
            ("EventBatchingCondition" Core..=)
              Prelude.<$> eventBatchingCondition,
            ("Description" Core..=) Prelude.<$> description
          ]
      )
