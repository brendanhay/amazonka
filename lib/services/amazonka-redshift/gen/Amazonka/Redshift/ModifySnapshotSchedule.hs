{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Redshift.ModifySnapshotSchedule
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies a snapshot schedule. Any schedule associated with a cluster is
-- modified asynchronously.
module Amazonka.Redshift.ModifySnapshotSchedule
  ( -- * Creating a Request
    ModifySnapshotSchedule (..),
    newModifySnapshotSchedule,

    -- * Request Lenses
    modifySnapshotSchedule_scheduleIdentifier,
    modifySnapshotSchedule_scheduleDefinitions,

    -- * Destructuring the Response
    SnapshotSchedule (..),
    newSnapshotSchedule,

    -- * Response Lenses
    snapshotSchedule_tags,
    snapshotSchedule_scheduleDescription,
    snapshotSchedule_associatedClusters,
    snapshotSchedule_scheduleIdentifier,
    snapshotSchedule_nextInvocations,
    snapshotSchedule_associatedClusterCount,
    snapshotSchedule_scheduleDefinitions,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.Redshift.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newModifySnapshotSchedule' smart constructor.
data ModifySnapshotSchedule = ModifySnapshotSchedule'
  { -- | A unique alphanumeric identifier of the schedule to modify.
    scheduleIdentifier :: Prelude.Text,
    -- | An updated list of schedule definitions. A schedule definition is made
    -- up of schedule expressions, for example, \"cron(30 12 *)\" or \"rate(12
    -- hours)\".
    scheduleDefinitions :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModifySnapshotSchedule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'scheduleIdentifier', 'modifySnapshotSchedule_scheduleIdentifier' - A unique alphanumeric identifier of the schedule to modify.
--
-- 'scheduleDefinitions', 'modifySnapshotSchedule_scheduleDefinitions' - An updated list of schedule definitions. A schedule definition is made
-- up of schedule expressions, for example, \"cron(30 12 *)\" or \"rate(12
-- hours)\".
newModifySnapshotSchedule ::
  -- | 'scheduleIdentifier'
  Prelude.Text ->
  ModifySnapshotSchedule
newModifySnapshotSchedule pScheduleIdentifier_ =
  ModifySnapshotSchedule'
    { scheduleIdentifier =
        pScheduleIdentifier_,
      scheduleDefinitions = Prelude.mempty
    }

-- | A unique alphanumeric identifier of the schedule to modify.
modifySnapshotSchedule_scheduleIdentifier :: Lens.Lens' ModifySnapshotSchedule Prelude.Text
modifySnapshotSchedule_scheduleIdentifier = Lens.lens (\ModifySnapshotSchedule' {scheduleIdentifier} -> scheduleIdentifier) (\s@ModifySnapshotSchedule' {} a -> s {scheduleIdentifier = a} :: ModifySnapshotSchedule)

-- | An updated list of schedule definitions. A schedule definition is made
-- up of schedule expressions, for example, \"cron(30 12 *)\" or \"rate(12
-- hours)\".
modifySnapshotSchedule_scheduleDefinitions :: Lens.Lens' ModifySnapshotSchedule [Prelude.Text]
modifySnapshotSchedule_scheduleDefinitions = Lens.lens (\ModifySnapshotSchedule' {scheduleDefinitions} -> scheduleDefinitions) (\s@ModifySnapshotSchedule' {} a -> s {scheduleDefinitions = a} :: ModifySnapshotSchedule) Prelude.. Lens.coerced

instance Core.AWSRequest ModifySnapshotSchedule where
  type
    AWSResponse ModifySnapshotSchedule =
      SnapshotSchedule
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "ModifySnapshotScheduleResult"
      (\s h x -> Core.parseXML x)

instance Prelude.Hashable ModifySnapshotSchedule where
  hashWithSalt _salt ModifySnapshotSchedule' {..} =
    _salt `Prelude.hashWithSalt` scheduleIdentifier
      `Prelude.hashWithSalt` scheduleDefinitions

instance Prelude.NFData ModifySnapshotSchedule where
  rnf ModifySnapshotSchedule' {..} =
    Prelude.rnf scheduleIdentifier
      `Prelude.seq` Prelude.rnf scheduleDefinitions

instance Core.ToHeaders ModifySnapshotSchedule where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath ModifySnapshotSchedule where
  toPath = Prelude.const "/"

instance Core.ToQuery ModifySnapshotSchedule where
  toQuery ModifySnapshotSchedule' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("ModifySnapshotSchedule" :: Prelude.ByteString),
        "Version"
          Core.=: ("2012-12-01" :: Prelude.ByteString),
        "ScheduleIdentifier" Core.=: scheduleIdentifier,
        "ScheduleDefinitions"
          Core.=: Core.toQueryList
            "ScheduleDefinition"
            scheduleDefinitions
      ]
