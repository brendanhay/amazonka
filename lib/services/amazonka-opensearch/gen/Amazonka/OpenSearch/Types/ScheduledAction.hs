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
-- Module      : Amazonka.OpenSearch.Types.ScheduledAction
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.OpenSearch.Types.ScheduledAction where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.OpenSearch.Types.ActionSeverity
import Amazonka.OpenSearch.Types.ActionStatus
import Amazonka.OpenSearch.Types.ActionType
import Amazonka.OpenSearch.Types.ScheduledBy
import qualified Amazonka.Prelude as Prelude

-- | Information about a scheduled configuration change for an OpenSearch
-- Service domain. This actions can be a
-- <https://docs.aws.amazon.com/opensearch-service/latest/developerguide/service-software.html service software update>
-- or a
-- <https://docs.aws.amazon.com/opensearch-service/latest/developerguide/auto-tune.html#auto-tune-types blue\/green Auto-Tune enhancement>.
--
-- /See:/ 'newScheduledAction' smart constructor.
data ScheduledAction = ScheduledAction'
  { -- | Whether or not the scheduled action is cancellable.
    cancellable :: Prelude.Maybe Prelude.Bool,
    -- | A description of the action to be taken.
    description :: Prelude.Maybe Prelude.Text,
    -- | Whether the action is required or optional.
    mandatory :: Prelude.Maybe Prelude.Bool,
    -- | Whether the action was scheduled manually (@CUSTOMER@, or by OpenSearch
    -- Service automatically (@SYSTEM@).
    scheduledBy :: Prelude.Maybe ScheduledBy,
    -- | The current status of the scheduled action.
    status :: Prelude.Maybe ActionStatus,
    -- | The unique identifier of the scheduled action.
    id :: Prelude.Text,
    -- | The type of action that will be taken on the domain.
    type' :: ActionType,
    -- | The severity of the action.
    severity :: ActionSeverity,
    -- | The time when the change is scheduled to happen.
    scheduledTime :: Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ScheduledAction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cancellable', 'scheduledAction_cancellable' - Whether or not the scheduled action is cancellable.
--
-- 'description', 'scheduledAction_description' - A description of the action to be taken.
--
-- 'mandatory', 'scheduledAction_mandatory' - Whether the action is required or optional.
--
-- 'scheduledBy', 'scheduledAction_scheduledBy' - Whether the action was scheduled manually (@CUSTOMER@, or by OpenSearch
-- Service automatically (@SYSTEM@).
--
-- 'status', 'scheduledAction_status' - The current status of the scheduled action.
--
-- 'id', 'scheduledAction_id' - The unique identifier of the scheduled action.
--
-- 'type'', 'scheduledAction_type' - The type of action that will be taken on the domain.
--
-- 'severity', 'scheduledAction_severity' - The severity of the action.
--
-- 'scheduledTime', 'scheduledAction_scheduledTime' - The time when the change is scheduled to happen.
newScheduledAction ::
  -- | 'id'
  Prelude.Text ->
  -- | 'type''
  ActionType ->
  -- | 'severity'
  ActionSeverity ->
  -- | 'scheduledTime'
  Prelude.Integer ->
  ScheduledAction
newScheduledAction
  pId_
  pType_
  pSeverity_
  pScheduledTime_ =
    ScheduledAction'
      { cancellable = Prelude.Nothing,
        description = Prelude.Nothing,
        mandatory = Prelude.Nothing,
        scheduledBy = Prelude.Nothing,
        status = Prelude.Nothing,
        id = pId_,
        type' = pType_,
        severity = pSeverity_,
        scheduledTime = pScheduledTime_
      }

-- | Whether or not the scheduled action is cancellable.
scheduledAction_cancellable :: Lens.Lens' ScheduledAction (Prelude.Maybe Prelude.Bool)
scheduledAction_cancellable = Lens.lens (\ScheduledAction' {cancellable} -> cancellable) (\s@ScheduledAction' {} a -> s {cancellable = a} :: ScheduledAction)

-- | A description of the action to be taken.
scheduledAction_description :: Lens.Lens' ScheduledAction (Prelude.Maybe Prelude.Text)
scheduledAction_description = Lens.lens (\ScheduledAction' {description} -> description) (\s@ScheduledAction' {} a -> s {description = a} :: ScheduledAction)

-- | Whether the action is required or optional.
scheduledAction_mandatory :: Lens.Lens' ScheduledAction (Prelude.Maybe Prelude.Bool)
scheduledAction_mandatory = Lens.lens (\ScheduledAction' {mandatory} -> mandatory) (\s@ScheduledAction' {} a -> s {mandatory = a} :: ScheduledAction)

-- | Whether the action was scheduled manually (@CUSTOMER@, or by OpenSearch
-- Service automatically (@SYSTEM@).
scheduledAction_scheduledBy :: Lens.Lens' ScheduledAction (Prelude.Maybe ScheduledBy)
scheduledAction_scheduledBy = Lens.lens (\ScheduledAction' {scheduledBy} -> scheduledBy) (\s@ScheduledAction' {} a -> s {scheduledBy = a} :: ScheduledAction)

-- | The current status of the scheduled action.
scheduledAction_status :: Lens.Lens' ScheduledAction (Prelude.Maybe ActionStatus)
scheduledAction_status = Lens.lens (\ScheduledAction' {status} -> status) (\s@ScheduledAction' {} a -> s {status = a} :: ScheduledAction)

-- | The unique identifier of the scheduled action.
scheduledAction_id :: Lens.Lens' ScheduledAction Prelude.Text
scheduledAction_id = Lens.lens (\ScheduledAction' {id} -> id) (\s@ScheduledAction' {} a -> s {id = a} :: ScheduledAction)

-- | The type of action that will be taken on the domain.
scheduledAction_type :: Lens.Lens' ScheduledAction ActionType
scheduledAction_type = Lens.lens (\ScheduledAction' {type'} -> type') (\s@ScheduledAction' {} a -> s {type' = a} :: ScheduledAction)

-- | The severity of the action.
scheduledAction_severity :: Lens.Lens' ScheduledAction ActionSeverity
scheduledAction_severity = Lens.lens (\ScheduledAction' {severity} -> severity) (\s@ScheduledAction' {} a -> s {severity = a} :: ScheduledAction)

-- | The time when the change is scheduled to happen.
scheduledAction_scheduledTime :: Lens.Lens' ScheduledAction Prelude.Integer
scheduledAction_scheduledTime = Lens.lens (\ScheduledAction' {scheduledTime} -> scheduledTime) (\s@ScheduledAction' {} a -> s {scheduledTime = a} :: ScheduledAction)

instance Data.FromJSON ScheduledAction where
  parseJSON =
    Data.withObject
      "ScheduledAction"
      ( \x ->
          ScheduledAction'
            Prelude.<$> (x Data..:? "Cancellable")
            Prelude.<*> (x Data..:? "Description")
            Prelude.<*> (x Data..:? "Mandatory")
            Prelude.<*> (x Data..:? "ScheduledBy")
            Prelude.<*> (x Data..:? "Status")
            Prelude.<*> (x Data..: "Id")
            Prelude.<*> (x Data..: "Type")
            Prelude.<*> (x Data..: "Severity")
            Prelude.<*> (x Data..: "ScheduledTime")
      )

instance Prelude.Hashable ScheduledAction where
  hashWithSalt _salt ScheduledAction' {..} =
    _salt
      `Prelude.hashWithSalt` cancellable
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` mandatory
      `Prelude.hashWithSalt` scheduledBy
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` severity
      `Prelude.hashWithSalt` scheduledTime

instance Prelude.NFData ScheduledAction where
  rnf ScheduledAction' {..} =
    Prelude.rnf cancellable
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf mandatory
      `Prelude.seq` Prelude.rnf scheduledBy
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf severity
      `Prelude.seq` Prelude.rnf scheduledTime
