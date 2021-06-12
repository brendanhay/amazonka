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
-- Module      : Network.AWS.Redshift.ModifySnapshotSchedule
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies a snapshot schedule. Any schedule associated with a cluster is
-- modified asynchronously.
module Network.AWS.Redshift.ModifySnapshotSchedule
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
    snapshotSchedule_nextInvocations,
    snapshotSchedule_associatedClusters,
    snapshotSchedule_scheduleIdentifier,
    snapshotSchedule_scheduleDescription,
    snapshotSchedule_scheduleDefinitions,
    snapshotSchedule_associatedClusterCount,
    snapshotSchedule_tags,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Redshift.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newModifySnapshotSchedule' smart constructor.
data ModifySnapshotSchedule = ModifySnapshotSchedule'
  { -- | A unique alphanumeric identifier of the schedule to modify.
    scheduleIdentifier :: Core.Text,
    -- | An updated list of schedule definitions. A schedule definition is made
    -- up of schedule expressions, for example, \"cron(30 12 *)\" or \"rate(12
    -- hours)\".
    scheduleDefinitions :: [Core.Text]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  ModifySnapshotSchedule
newModifySnapshotSchedule pScheduleIdentifier_ =
  ModifySnapshotSchedule'
    { scheduleIdentifier =
        pScheduleIdentifier_,
      scheduleDefinitions = Core.mempty
    }

-- | A unique alphanumeric identifier of the schedule to modify.
modifySnapshotSchedule_scheduleIdentifier :: Lens.Lens' ModifySnapshotSchedule Core.Text
modifySnapshotSchedule_scheduleIdentifier = Lens.lens (\ModifySnapshotSchedule' {scheduleIdentifier} -> scheduleIdentifier) (\s@ModifySnapshotSchedule' {} a -> s {scheduleIdentifier = a} :: ModifySnapshotSchedule)

-- | An updated list of schedule definitions. A schedule definition is made
-- up of schedule expressions, for example, \"cron(30 12 *)\" or \"rate(12
-- hours)\".
modifySnapshotSchedule_scheduleDefinitions :: Lens.Lens' ModifySnapshotSchedule [Core.Text]
modifySnapshotSchedule_scheduleDefinitions = Lens.lens (\ModifySnapshotSchedule' {scheduleDefinitions} -> scheduleDefinitions) (\s@ModifySnapshotSchedule' {} a -> s {scheduleDefinitions = a} :: ModifySnapshotSchedule) Core.. Lens._Coerce

instance Core.AWSRequest ModifySnapshotSchedule where
  type
    AWSResponse ModifySnapshotSchedule =
      SnapshotSchedule
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "ModifySnapshotScheduleResult"
      (\s h x -> Core.parseXML x)

instance Core.Hashable ModifySnapshotSchedule

instance Core.NFData ModifySnapshotSchedule

instance Core.ToHeaders ModifySnapshotSchedule where
  toHeaders = Core.const Core.mempty

instance Core.ToPath ModifySnapshotSchedule where
  toPath = Core.const "/"

instance Core.ToQuery ModifySnapshotSchedule where
  toQuery ModifySnapshotSchedule' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("ModifySnapshotSchedule" :: Core.ByteString),
        "Version" Core.=: ("2012-12-01" :: Core.ByteString),
        "ScheduleIdentifier" Core.=: scheduleIdentifier,
        "ScheduleDefinitions"
          Core.=: Core.toQueryList
            "ScheduleDefinition"
            scheduleDefinitions
      ]
