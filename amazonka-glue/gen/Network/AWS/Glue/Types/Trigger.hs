{-# LANGUAGE DeriveDataTypeable #-}
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

import Network.AWS.Glue.Types.Action
import Network.AWS.Glue.Types.Predicate
import Network.AWS.Glue.Types.TriggerState
import Network.AWS.Glue.Types.TriggerType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Information about a specific trigger.
--
-- /See:/ 'newTrigger' smart constructor.
data Trigger = Trigger'
  { -- | The name of the workflow associated with the trigger.
    workflowName :: Prelude.Maybe Prelude.Text,
    -- | Reserved for future use.
    id :: Prelude.Maybe Prelude.Text,
    -- | The actions initiated by this trigger.
    actions :: Prelude.Maybe [Action],
    -- | The current state of the trigger.
    state :: Prelude.Maybe TriggerState,
    -- | The name of the trigger.
    name :: Prelude.Maybe Prelude.Text,
    -- | The predicate of this trigger, which defines when it will fire.
    predicate :: Prelude.Maybe Predicate,
    -- | A description of this trigger.
    description :: Prelude.Maybe Prelude.Text,
    -- | The type of trigger that this is.
    type' :: Prelude.Maybe TriggerType,
    -- | A @cron@ expression used to specify the schedule (see
    -- <https://docs.aws.amazon.com/glue/latest/dg/monitor-data-warehouse-schedule.html Time-Based Schedules for Jobs and Crawlers>.
    -- For example, to run something every day at 12:15 UTC, you would specify:
    -- @cron(15 12 * * ? *)@.
    schedule :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { workflowName = Prelude.Nothing,
      id = Prelude.Nothing,
      actions = Prelude.Nothing,
      state = Prelude.Nothing,
      name = Prelude.Nothing,
      predicate = Prelude.Nothing,
      description = Prelude.Nothing,
      type' = Prelude.Nothing,
      schedule = Prelude.Nothing
    }

-- | The name of the workflow associated with the trigger.
trigger_workflowName :: Lens.Lens' Trigger (Prelude.Maybe Prelude.Text)
trigger_workflowName = Lens.lens (\Trigger' {workflowName} -> workflowName) (\s@Trigger' {} a -> s {workflowName = a} :: Trigger)

-- | Reserved for future use.
trigger_id :: Lens.Lens' Trigger (Prelude.Maybe Prelude.Text)
trigger_id = Lens.lens (\Trigger' {id} -> id) (\s@Trigger' {} a -> s {id = a} :: Trigger)

-- | The actions initiated by this trigger.
trigger_actions :: Lens.Lens' Trigger (Prelude.Maybe [Action])
trigger_actions = Lens.lens (\Trigger' {actions} -> actions) (\s@Trigger' {} a -> s {actions = a} :: Trigger) Prelude.. Lens.mapping Prelude._Coerce

-- | The current state of the trigger.
trigger_state :: Lens.Lens' Trigger (Prelude.Maybe TriggerState)
trigger_state = Lens.lens (\Trigger' {state} -> state) (\s@Trigger' {} a -> s {state = a} :: Trigger)

-- | The name of the trigger.
trigger_name :: Lens.Lens' Trigger (Prelude.Maybe Prelude.Text)
trigger_name = Lens.lens (\Trigger' {name} -> name) (\s@Trigger' {} a -> s {name = a} :: Trigger)

-- | The predicate of this trigger, which defines when it will fire.
trigger_predicate :: Lens.Lens' Trigger (Prelude.Maybe Predicate)
trigger_predicate = Lens.lens (\Trigger' {predicate} -> predicate) (\s@Trigger' {} a -> s {predicate = a} :: Trigger)

-- | A description of this trigger.
trigger_description :: Lens.Lens' Trigger (Prelude.Maybe Prelude.Text)
trigger_description = Lens.lens (\Trigger' {description} -> description) (\s@Trigger' {} a -> s {description = a} :: Trigger)

-- | The type of trigger that this is.
trigger_type :: Lens.Lens' Trigger (Prelude.Maybe TriggerType)
trigger_type = Lens.lens (\Trigger' {type'} -> type') (\s@Trigger' {} a -> s {type' = a} :: Trigger)

-- | A @cron@ expression used to specify the schedule (see
-- <https://docs.aws.amazon.com/glue/latest/dg/monitor-data-warehouse-schedule.html Time-Based Schedules for Jobs and Crawlers>.
-- For example, to run something every day at 12:15 UTC, you would specify:
-- @cron(15 12 * * ? *)@.
trigger_schedule :: Lens.Lens' Trigger (Prelude.Maybe Prelude.Text)
trigger_schedule = Lens.lens (\Trigger' {schedule} -> schedule) (\s@Trigger' {} a -> s {schedule = a} :: Trigger)

instance Prelude.FromJSON Trigger where
  parseJSON =
    Prelude.withObject
      "Trigger"
      ( \x ->
          Trigger'
            Prelude.<$> (x Prelude..:? "WorkflowName")
            Prelude.<*> (x Prelude..:? "Id")
            Prelude.<*> (x Prelude..:? "Actions" Prelude..!= Prelude.mempty)
            Prelude.<*> (x Prelude..:? "State")
            Prelude.<*> (x Prelude..:? "Name")
            Prelude.<*> (x Prelude..:? "Predicate")
            Prelude.<*> (x Prelude..:? "Description")
            Prelude.<*> (x Prelude..:? "Type")
            Prelude.<*> (x Prelude..:? "Schedule")
      )

instance Prelude.Hashable Trigger

instance Prelude.NFData Trigger
