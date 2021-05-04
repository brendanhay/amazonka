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
-- Module      : Network.AWS.Redshift.DeleteSnapshotSchedule
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a snapshot schedule.
module Network.AWS.Redshift.DeleteSnapshotSchedule
  ( -- * Creating a Request
    DeleteSnapshotSchedule (..),
    newDeleteSnapshotSchedule,

    -- * Request Lenses
    deleteSnapshotSchedule_scheduleIdentifier,

    -- * Destructuring the Response
    DeleteSnapshotScheduleResponse (..),
    newDeleteSnapshotScheduleResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.Redshift.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteSnapshotSchedule' smart constructor.
data DeleteSnapshotSchedule = DeleteSnapshotSchedule'
  { -- | A unique identifier of the snapshot schedule to delete.
    scheduleIdentifier :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteSnapshotSchedule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'scheduleIdentifier', 'deleteSnapshotSchedule_scheduleIdentifier' - A unique identifier of the snapshot schedule to delete.
newDeleteSnapshotSchedule ::
  -- | 'scheduleIdentifier'
  Prelude.Text ->
  DeleteSnapshotSchedule
newDeleteSnapshotSchedule pScheduleIdentifier_ =
  DeleteSnapshotSchedule'
    { scheduleIdentifier =
        pScheduleIdentifier_
    }

-- | A unique identifier of the snapshot schedule to delete.
deleteSnapshotSchedule_scheduleIdentifier :: Lens.Lens' DeleteSnapshotSchedule Prelude.Text
deleteSnapshotSchedule_scheduleIdentifier = Lens.lens (\DeleteSnapshotSchedule' {scheduleIdentifier} -> scheduleIdentifier) (\s@DeleteSnapshotSchedule' {} a -> s {scheduleIdentifier = a} :: DeleteSnapshotSchedule)

instance Prelude.AWSRequest DeleteSnapshotSchedule where
  type
    Rs DeleteSnapshotSchedule =
      DeleteSnapshotScheduleResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveNull
      DeleteSnapshotScheduleResponse'

instance Prelude.Hashable DeleteSnapshotSchedule

instance Prelude.NFData DeleteSnapshotSchedule

instance Prelude.ToHeaders DeleteSnapshotSchedule where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath DeleteSnapshotSchedule where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DeleteSnapshotSchedule where
  toQuery DeleteSnapshotSchedule' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("DeleteSnapshotSchedule" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2012-12-01" :: Prelude.ByteString),
        "ScheduleIdentifier" Prelude.=: scheduleIdentifier
      ]

-- | /See:/ 'newDeleteSnapshotScheduleResponse' smart constructor.
data DeleteSnapshotScheduleResponse = DeleteSnapshotScheduleResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteSnapshotScheduleResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteSnapshotScheduleResponse ::
  DeleteSnapshotScheduleResponse
newDeleteSnapshotScheduleResponse =
  DeleteSnapshotScheduleResponse'

instance
  Prelude.NFData
    DeleteSnapshotScheduleResponse
