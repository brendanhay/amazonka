{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.Redshift.ModifyClusterSnapshotSchedule
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies a snapshot schedule for a cluster.
module Network.AWS.Redshift.ModifyClusterSnapshotSchedule
  ( -- * Creating a Request
    ModifyClusterSnapshotSchedule (..),
    newModifyClusterSnapshotSchedule,

    -- * Request Lenses
    modifyClusterSnapshotSchedule_disassociateSchedule,
    modifyClusterSnapshotSchedule_scheduleIdentifier,
    modifyClusterSnapshotSchedule_clusterIdentifier,

    -- * Destructuring the Response
    ModifyClusterSnapshotScheduleResponse (..),
    newModifyClusterSnapshotScheduleResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.Redshift.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newModifyClusterSnapshotSchedule' smart constructor.
data ModifyClusterSnapshotSchedule = ModifyClusterSnapshotSchedule'
  { -- | A boolean to indicate whether to remove the assoiciation between the
    -- cluster and the schedule.
    disassociateSchedule :: Prelude.Maybe Prelude.Bool,
    -- | A unique alphanumeric identifier for the schedule that you want to
    -- associate with the cluster.
    scheduleIdentifier :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier for the cluster whose snapshot schedule you want to
    -- modify.
    clusterIdentifier :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ModifyClusterSnapshotSchedule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'disassociateSchedule', 'modifyClusterSnapshotSchedule_disassociateSchedule' - A boolean to indicate whether to remove the assoiciation between the
-- cluster and the schedule.
--
-- 'scheduleIdentifier', 'modifyClusterSnapshotSchedule_scheduleIdentifier' - A unique alphanumeric identifier for the schedule that you want to
-- associate with the cluster.
--
-- 'clusterIdentifier', 'modifyClusterSnapshotSchedule_clusterIdentifier' - A unique identifier for the cluster whose snapshot schedule you want to
-- modify.
newModifyClusterSnapshotSchedule ::
  -- | 'clusterIdentifier'
  Prelude.Text ->
  ModifyClusterSnapshotSchedule
newModifyClusterSnapshotSchedule pClusterIdentifier_ =
  ModifyClusterSnapshotSchedule'
    { disassociateSchedule =
        Prelude.Nothing,
      scheduleIdentifier = Prelude.Nothing,
      clusterIdentifier = pClusterIdentifier_
    }

-- | A boolean to indicate whether to remove the assoiciation between the
-- cluster and the schedule.
modifyClusterSnapshotSchedule_disassociateSchedule :: Lens.Lens' ModifyClusterSnapshotSchedule (Prelude.Maybe Prelude.Bool)
modifyClusterSnapshotSchedule_disassociateSchedule = Lens.lens (\ModifyClusterSnapshotSchedule' {disassociateSchedule} -> disassociateSchedule) (\s@ModifyClusterSnapshotSchedule' {} a -> s {disassociateSchedule = a} :: ModifyClusterSnapshotSchedule)

-- | A unique alphanumeric identifier for the schedule that you want to
-- associate with the cluster.
modifyClusterSnapshotSchedule_scheduleIdentifier :: Lens.Lens' ModifyClusterSnapshotSchedule (Prelude.Maybe Prelude.Text)
modifyClusterSnapshotSchedule_scheduleIdentifier = Lens.lens (\ModifyClusterSnapshotSchedule' {scheduleIdentifier} -> scheduleIdentifier) (\s@ModifyClusterSnapshotSchedule' {} a -> s {scheduleIdentifier = a} :: ModifyClusterSnapshotSchedule)

-- | A unique identifier for the cluster whose snapshot schedule you want to
-- modify.
modifyClusterSnapshotSchedule_clusterIdentifier :: Lens.Lens' ModifyClusterSnapshotSchedule Prelude.Text
modifyClusterSnapshotSchedule_clusterIdentifier = Lens.lens (\ModifyClusterSnapshotSchedule' {clusterIdentifier} -> clusterIdentifier) (\s@ModifyClusterSnapshotSchedule' {} a -> s {clusterIdentifier = a} :: ModifyClusterSnapshotSchedule)

instance
  Prelude.AWSRequest
    ModifyClusterSnapshotSchedule
  where
  type
    Rs ModifyClusterSnapshotSchedule =
      ModifyClusterSnapshotScheduleResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveNull
      ModifyClusterSnapshotScheduleResponse'

instance
  Prelude.Hashable
    ModifyClusterSnapshotSchedule

instance Prelude.NFData ModifyClusterSnapshotSchedule

instance
  Prelude.ToHeaders
    ModifyClusterSnapshotSchedule
  where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath ModifyClusterSnapshotSchedule where
  toPath = Prelude.const "/"

instance
  Prelude.ToQuery
    ModifyClusterSnapshotSchedule
  where
  toQuery ModifyClusterSnapshotSchedule' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ( "ModifyClusterSnapshotSchedule" ::
                         Prelude.ByteString
                     ),
        "Version"
          Prelude.=: ("2012-12-01" :: Prelude.ByteString),
        "DisassociateSchedule"
          Prelude.=: disassociateSchedule,
        "ScheduleIdentifier" Prelude.=: scheduleIdentifier,
        "ClusterIdentifier" Prelude.=: clusterIdentifier
      ]

-- | /See:/ 'newModifyClusterSnapshotScheduleResponse' smart constructor.
data ModifyClusterSnapshotScheduleResponse = ModifyClusterSnapshotScheduleResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ModifyClusterSnapshotScheduleResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newModifyClusterSnapshotScheduleResponse ::
  ModifyClusterSnapshotScheduleResponse
newModifyClusterSnapshotScheduleResponse =
  ModifyClusterSnapshotScheduleResponse'

instance
  Prelude.NFData
    ModifyClusterSnapshotScheduleResponse
