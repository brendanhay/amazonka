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
-- Module      : Amazonka.Glue.Types.Trigger
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.Trigger where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glue.Types.Action
import Amazonka.Glue.Types.EventBatchingCondition
import Amazonka.Glue.Types.Predicate
import Amazonka.Glue.Types.TriggerState
import Amazonka.Glue.Types.TriggerType
import qualified Amazonka.Prelude as Prelude

-- | Information about a specific trigger.
--
-- /See:/ 'newTrigger' smart constructor.
data Trigger = Trigger'
  { -- | The actions initiated by this trigger.
    actions :: Prelude.Maybe [Action],
    -- | A description of this trigger.
    description :: Prelude.Maybe Prelude.Text,
    -- | Batch condition that must be met (specified number of events received or
    -- batch time window expired) before EventBridge event trigger fires.
    eventBatchingCondition :: Prelude.Maybe EventBatchingCondition,
    -- | Reserved for future use.
    id :: Prelude.Maybe Prelude.Text,
    -- | The name of the trigger.
    name :: Prelude.Maybe Prelude.Text,
    -- | The predicate of this trigger, which defines when it will fire.
    predicate :: Prelude.Maybe Predicate,
    -- | A @cron@ expression used to specify the schedule (see
    -- <https://docs.aws.amazon.com/glue/latest/dg/monitor-data-warehouse-schedule.html Time-Based Schedules for Jobs and Crawlers>.
    -- For example, to run something every day at 12:15 UTC, you would specify:
    -- @cron(15 12 * * ? *)@.
    schedule :: Prelude.Maybe Prelude.Text,
    -- | The current state of the trigger.
    state :: Prelude.Maybe TriggerState,
    -- | The type of trigger that this is.
    type' :: Prelude.Maybe TriggerType,
    -- | The name of the workflow associated with the trigger.
    workflowName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Trigger' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'actions', 'trigger_actions' - The actions initiated by this trigger.
--
-- 'description', 'trigger_description' - A description of this trigger.
--
-- 'eventBatchingCondition', 'trigger_eventBatchingCondition' - Batch condition that must be met (specified number of events received or
-- batch time window expired) before EventBridge event trigger fires.
--
-- 'id', 'trigger_id' - Reserved for future use.
--
-- 'name', 'trigger_name' - The name of the trigger.
--
-- 'predicate', 'trigger_predicate' - The predicate of this trigger, which defines when it will fire.
--
-- 'schedule', 'trigger_schedule' - A @cron@ expression used to specify the schedule (see
-- <https://docs.aws.amazon.com/glue/latest/dg/monitor-data-warehouse-schedule.html Time-Based Schedules for Jobs and Crawlers>.
-- For example, to run something every day at 12:15 UTC, you would specify:
-- @cron(15 12 * * ? *)@.
--
-- 'state', 'trigger_state' - The current state of the trigger.
--
-- 'type'', 'trigger_type' - The type of trigger that this is.
--
-- 'workflowName', 'trigger_workflowName' - The name of the workflow associated with the trigger.
newTrigger ::
  Trigger
newTrigger =
  Trigger'
    { actions = Prelude.Nothing,
      description = Prelude.Nothing,
      eventBatchingCondition = Prelude.Nothing,
      id = Prelude.Nothing,
      name = Prelude.Nothing,
      predicate = Prelude.Nothing,
      schedule = Prelude.Nothing,
      state = Prelude.Nothing,
      type' = Prelude.Nothing,
      workflowName = Prelude.Nothing
    }

-- | The actions initiated by this trigger.
trigger_actions :: Lens.Lens' Trigger (Prelude.Maybe [Action])
trigger_actions = Lens.lens (\Trigger' {actions} -> actions) (\s@Trigger' {} a -> s {actions = a} :: Trigger) Prelude.. Lens.mapping Lens.coerced

-- | A description of this trigger.
trigger_description :: Lens.Lens' Trigger (Prelude.Maybe Prelude.Text)
trigger_description = Lens.lens (\Trigger' {description} -> description) (\s@Trigger' {} a -> s {description = a} :: Trigger)

-- | Batch condition that must be met (specified number of events received or
-- batch time window expired) before EventBridge event trigger fires.
trigger_eventBatchingCondition :: Lens.Lens' Trigger (Prelude.Maybe EventBatchingCondition)
trigger_eventBatchingCondition = Lens.lens (\Trigger' {eventBatchingCondition} -> eventBatchingCondition) (\s@Trigger' {} a -> s {eventBatchingCondition = a} :: Trigger)

-- | Reserved for future use.
trigger_id :: Lens.Lens' Trigger (Prelude.Maybe Prelude.Text)
trigger_id = Lens.lens (\Trigger' {id} -> id) (\s@Trigger' {} a -> s {id = a} :: Trigger)

-- | The name of the trigger.
trigger_name :: Lens.Lens' Trigger (Prelude.Maybe Prelude.Text)
trigger_name = Lens.lens (\Trigger' {name} -> name) (\s@Trigger' {} a -> s {name = a} :: Trigger)

-- | The predicate of this trigger, which defines when it will fire.
trigger_predicate :: Lens.Lens' Trigger (Prelude.Maybe Predicate)
trigger_predicate = Lens.lens (\Trigger' {predicate} -> predicate) (\s@Trigger' {} a -> s {predicate = a} :: Trigger)

-- | A @cron@ expression used to specify the schedule (see
-- <https://docs.aws.amazon.com/glue/latest/dg/monitor-data-warehouse-schedule.html Time-Based Schedules for Jobs and Crawlers>.
-- For example, to run something every day at 12:15 UTC, you would specify:
-- @cron(15 12 * * ? *)@.
trigger_schedule :: Lens.Lens' Trigger (Prelude.Maybe Prelude.Text)
trigger_schedule = Lens.lens (\Trigger' {schedule} -> schedule) (\s@Trigger' {} a -> s {schedule = a} :: Trigger)

-- | The current state of the trigger.
trigger_state :: Lens.Lens' Trigger (Prelude.Maybe TriggerState)
trigger_state = Lens.lens (\Trigger' {state} -> state) (\s@Trigger' {} a -> s {state = a} :: Trigger)

-- | The type of trigger that this is.
trigger_type :: Lens.Lens' Trigger (Prelude.Maybe TriggerType)
trigger_type = Lens.lens (\Trigger' {type'} -> type') (\s@Trigger' {} a -> s {type' = a} :: Trigger)

-- | The name of the workflow associated with the trigger.
trigger_workflowName :: Lens.Lens' Trigger (Prelude.Maybe Prelude.Text)
trigger_workflowName = Lens.lens (\Trigger' {workflowName} -> workflowName) (\s@Trigger' {} a -> s {workflowName = a} :: Trigger)

instance Data.FromJSON Trigger where
  parseJSON =
    Data.withObject
      "Trigger"
      ( \x ->
          Trigger'
            Prelude.<$> (x Data..:? "Actions" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "Description")
            Prelude.<*> (x Data..:? "EventBatchingCondition")
            Prelude.<*> (x Data..:? "Id")
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "Predicate")
            Prelude.<*> (x Data..:? "Schedule")
            Prelude.<*> (x Data..:? "State")
            Prelude.<*> (x Data..:? "Type")
            Prelude.<*> (x Data..:? "WorkflowName")
      )

instance Prelude.Hashable Trigger where
  hashWithSalt _salt Trigger' {..} =
    _salt `Prelude.hashWithSalt` actions
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` eventBatchingCondition
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` predicate
      `Prelude.hashWithSalt` schedule
      `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` workflowName

instance Prelude.NFData Trigger where
  rnf Trigger' {..} =
    Prelude.rnf actions
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf eventBatchingCondition
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf predicate
      `Prelude.seq` Prelude.rnf schedule
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf workflowName
