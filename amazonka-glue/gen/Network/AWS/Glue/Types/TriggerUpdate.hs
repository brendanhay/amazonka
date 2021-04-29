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
-- Module      : Network.AWS.Glue.Types.TriggerUpdate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.TriggerUpdate where

import Network.AWS.Glue.Types.Action
import Network.AWS.Glue.Types.Predicate
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | A structure used to provide information used to update a trigger. This
-- object updates the previous trigger definition by overwriting it
-- completely.
--
-- /See:/ 'newTriggerUpdate' smart constructor.
data TriggerUpdate = TriggerUpdate'
  { -- | The actions initiated by this trigger.
    actions :: Prelude.Maybe [Action],
    -- | Reserved for future use.
    name :: Prelude.Maybe Prelude.Text,
    -- | The predicate of this trigger, which defines when it will fire.
    predicate :: Prelude.Maybe Predicate,
    -- | A description of this trigger.
    description :: Prelude.Maybe Prelude.Text,
    -- | A @cron@ expression used to specify the schedule (see
    -- <https://docs.aws.amazon.com/glue/latest/dg/monitor-data-warehouse-schedule.html Time-Based Schedules for Jobs and Crawlers>.
    -- For example, to run something every day at 12:15 UTC, you would specify:
    -- @cron(15 12 * * ? *)@.
    schedule :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
-- 'name', 'triggerUpdate_name' - Reserved for future use.
--
-- 'predicate', 'triggerUpdate_predicate' - The predicate of this trigger, which defines when it will fire.
--
-- 'description', 'triggerUpdate_description' - A description of this trigger.
--
-- 'schedule', 'triggerUpdate_schedule' - A @cron@ expression used to specify the schedule (see
-- <https://docs.aws.amazon.com/glue/latest/dg/monitor-data-warehouse-schedule.html Time-Based Schedules for Jobs and Crawlers>.
-- For example, to run something every day at 12:15 UTC, you would specify:
-- @cron(15 12 * * ? *)@.
newTriggerUpdate ::
  TriggerUpdate
newTriggerUpdate =
  TriggerUpdate'
    { actions = Prelude.Nothing,
      name = Prelude.Nothing,
      predicate = Prelude.Nothing,
      description = Prelude.Nothing,
      schedule = Prelude.Nothing
    }

-- | The actions initiated by this trigger.
triggerUpdate_actions :: Lens.Lens' TriggerUpdate (Prelude.Maybe [Action])
triggerUpdate_actions = Lens.lens (\TriggerUpdate' {actions} -> actions) (\s@TriggerUpdate' {} a -> s {actions = a} :: TriggerUpdate) Prelude.. Lens.mapping Prelude._Coerce

-- | Reserved for future use.
triggerUpdate_name :: Lens.Lens' TriggerUpdate (Prelude.Maybe Prelude.Text)
triggerUpdate_name = Lens.lens (\TriggerUpdate' {name} -> name) (\s@TriggerUpdate' {} a -> s {name = a} :: TriggerUpdate)

-- | The predicate of this trigger, which defines when it will fire.
triggerUpdate_predicate :: Lens.Lens' TriggerUpdate (Prelude.Maybe Predicate)
triggerUpdate_predicate = Lens.lens (\TriggerUpdate' {predicate} -> predicate) (\s@TriggerUpdate' {} a -> s {predicate = a} :: TriggerUpdate)

-- | A description of this trigger.
triggerUpdate_description :: Lens.Lens' TriggerUpdate (Prelude.Maybe Prelude.Text)
triggerUpdate_description = Lens.lens (\TriggerUpdate' {description} -> description) (\s@TriggerUpdate' {} a -> s {description = a} :: TriggerUpdate)

-- | A @cron@ expression used to specify the schedule (see
-- <https://docs.aws.amazon.com/glue/latest/dg/monitor-data-warehouse-schedule.html Time-Based Schedules for Jobs and Crawlers>.
-- For example, to run something every day at 12:15 UTC, you would specify:
-- @cron(15 12 * * ? *)@.
triggerUpdate_schedule :: Lens.Lens' TriggerUpdate (Prelude.Maybe Prelude.Text)
triggerUpdate_schedule = Lens.lens (\TriggerUpdate' {schedule} -> schedule) (\s@TriggerUpdate' {} a -> s {schedule = a} :: TriggerUpdate)

instance Prelude.Hashable TriggerUpdate

instance Prelude.NFData TriggerUpdate

instance Prelude.ToJSON TriggerUpdate where
  toJSON TriggerUpdate' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("Actions" Prelude..=) Prelude.<$> actions,
            ("Name" Prelude..=) Prelude.<$> name,
            ("Predicate" Prelude..=) Prelude.<$> predicate,
            ("Description" Prelude..=) Prelude.<$> description,
            ("Schedule" Prelude..=) Prelude.<$> schedule
          ]
      )
