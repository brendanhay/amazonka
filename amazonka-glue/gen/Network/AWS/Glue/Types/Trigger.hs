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
-- Module      : Network.AWS.Glue.Types.Trigger
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.Trigger where

import qualified Network.AWS.Core as Core
import Network.AWS.Glue.Types.Action
import Network.AWS.Glue.Types.Predicate
import Network.AWS.Glue.Types.TriggerState
import Network.AWS.Glue.Types.TriggerType
import qualified Network.AWS.Lens as Lens

-- | Information about a specific trigger.
--
-- /See:/ 'newTrigger' smart constructor.
data Trigger = Trigger'
  { -- | The name of the workflow associated with the trigger.
    workflowName :: Core.Maybe Core.Text,
    -- | Reserved for future use.
    id :: Core.Maybe Core.Text,
    -- | The actions initiated by this trigger.
    actions :: Core.Maybe [Action],
    -- | The current state of the trigger.
    state :: Core.Maybe TriggerState,
    -- | The name of the trigger.
    name :: Core.Maybe Core.Text,
    -- | The predicate of this trigger, which defines when it will fire.
    predicate :: Core.Maybe Predicate,
    -- | A description of this trigger.
    description :: Core.Maybe Core.Text,
    -- | The type of trigger that this is.
    type' :: Core.Maybe TriggerType,
    -- | A @cron@ expression used to specify the schedule (see
    -- <https://docs.aws.amazon.com/glue/latest/dg/monitor-data-warehouse-schedule.html Time-Based Schedules for Jobs and Crawlers>.
    -- For example, to run something every day at 12:15 UTC, you would specify:
    -- @cron(15 12 * * ? *)@.
    schedule :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Trigger' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'workflowName', 'trigger_workflowName' - The name of the workflow associated with the trigger.
--
-- 'id', 'trigger_id' - Reserved for future use.
--
-- 'actions', 'trigger_actions' - The actions initiated by this trigger.
--
-- 'state', 'trigger_state' - The current state of the trigger.
--
-- 'name', 'trigger_name' - The name of the trigger.
--
-- 'predicate', 'trigger_predicate' - The predicate of this trigger, which defines when it will fire.
--
-- 'description', 'trigger_description' - A description of this trigger.
--
-- 'type'', 'trigger_type' - The type of trigger that this is.
--
-- 'schedule', 'trigger_schedule' - A @cron@ expression used to specify the schedule (see
-- <https://docs.aws.amazon.com/glue/latest/dg/monitor-data-warehouse-schedule.html Time-Based Schedules for Jobs and Crawlers>.
-- For example, to run something every day at 12:15 UTC, you would specify:
-- @cron(15 12 * * ? *)@.
newTrigger ::
  Trigger
newTrigger =
  Trigger'
    { workflowName = Core.Nothing,
      id = Core.Nothing,
      actions = Core.Nothing,
      state = Core.Nothing,
      name = Core.Nothing,
      predicate = Core.Nothing,
      description = Core.Nothing,
      type' = Core.Nothing,
      schedule = Core.Nothing
    }

-- | The name of the workflow associated with the trigger.
trigger_workflowName :: Lens.Lens' Trigger (Core.Maybe Core.Text)
trigger_workflowName = Lens.lens (\Trigger' {workflowName} -> workflowName) (\s@Trigger' {} a -> s {workflowName = a} :: Trigger)

-- | Reserved for future use.
trigger_id :: Lens.Lens' Trigger (Core.Maybe Core.Text)
trigger_id = Lens.lens (\Trigger' {id} -> id) (\s@Trigger' {} a -> s {id = a} :: Trigger)

-- | The actions initiated by this trigger.
trigger_actions :: Lens.Lens' Trigger (Core.Maybe [Action])
trigger_actions = Lens.lens (\Trigger' {actions} -> actions) (\s@Trigger' {} a -> s {actions = a} :: Trigger) Core.. Lens.mapping Lens._Coerce

-- | The current state of the trigger.
trigger_state :: Lens.Lens' Trigger (Core.Maybe TriggerState)
trigger_state = Lens.lens (\Trigger' {state} -> state) (\s@Trigger' {} a -> s {state = a} :: Trigger)

-- | The name of the trigger.
trigger_name :: Lens.Lens' Trigger (Core.Maybe Core.Text)
trigger_name = Lens.lens (\Trigger' {name} -> name) (\s@Trigger' {} a -> s {name = a} :: Trigger)

-- | The predicate of this trigger, which defines when it will fire.
trigger_predicate :: Lens.Lens' Trigger (Core.Maybe Predicate)
trigger_predicate = Lens.lens (\Trigger' {predicate} -> predicate) (\s@Trigger' {} a -> s {predicate = a} :: Trigger)

-- | A description of this trigger.
trigger_description :: Lens.Lens' Trigger (Core.Maybe Core.Text)
trigger_description = Lens.lens (\Trigger' {description} -> description) (\s@Trigger' {} a -> s {description = a} :: Trigger)

-- | The type of trigger that this is.
trigger_type :: Lens.Lens' Trigger (Core.Maybe TriggerType)
trigger_type = Lens.lens (\Trigger' {type'} -> type') (\s@Trigger' {} a -> s {type' = a} :: Trigger)

-- | A @cron@ expression used to specify the schedule (see
-- <https://docs.aws.amazon.com/glue/latest/dg/monitor-data-warehouse-schedule.html Time-Based Schedules for Jobs and Crawlers>.
-- For example, to run something every day at 12:15 UTC, you would specify:
-- @cron(15 12 * * ? *)@.
trigger_schedule :: Lens.Lens' Trigger (Core.Maybe Core.Text)
trigger_schedule = Lens.lens (\Trigger' {schedule} -> schedule) (\s@Trigger' {} a -> s {schedule = a} :: Trigger)

instance Core.FromJSON Trigger where
  parseJSON =
    Core.withObject
      "Trigger"
      ( \x ->
          Trigger'
            Core.<$> (x Core..:? "WorkflowName")
            Core.<*> (x Core..:? "Id")
            Core.<*> (x Core..:? "Actions" Core..!= Core.mempty)
            Core.<*> (x Core..:? "State")
            Core.<*> (x Core..:? "Name")
            Core.<*> (x Core..:? "Predicate")
            Core.<*> (x Core..:? "Description")
            Core.<*> (x Core..:? "Type")
            Core.<*> (x Core..:? "Schedule")
      )

instance Core.Hashable Trigger

instance Core.NFData Trigger
