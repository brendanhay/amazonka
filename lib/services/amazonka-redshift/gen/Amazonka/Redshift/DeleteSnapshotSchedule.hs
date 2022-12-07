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
-- Module      : Amazonka.Redshift.DeleteSnapshotSchedule
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a snapshot schedule.
module Amazonka.Redshift.DeleteSnapshotSchedule
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Redshift.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteSnapshotSchedule' smart constructor.
data DeleteSnapshotSchedule = DeleteSnapshotSchedule'
  { -- | A unique identifier of the snapshot schedule to delete.
    scheduleIdentifier :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Core.AWSRequest DeleteSnapshotSchedule where
  type
    AWSResponse DeleteSnapshotSchedule =
      DeleteSnapshotScheduleResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveNull
      DeleteSnapshotScheduleResponse'

instance Prelude.Hashable DeleteSnapshotSchedule where
  hashWithSalt _salt DeleteSnapshotSchedule' {..} =
    _salt `Prelude.hashWithSalt` scheduleIdentifier

instance Prelude.NFData DeleteSnapshotSchedule where
  rnf DeleteSnapshotSchedule' {..} =
    Prelude.rnf scheduleIdentifier

instance Data.ToHeaders DeleteSnapshotSchedule where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DeleteSnapshotSchedule where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteSnapshotSchedule where
  toQuery DeleteSnapshotSchedule' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("DeleteSnapshotSchedule" :: Prelude.ByteString),
        "Version"
          Data.=: ("2012-12-01" :: Prelude.ByteString),
        "ScheduleIdentifier" Data.=: scheduleIdentifier
      ]

-- | /See:/ 'newDeleteSnapshotScheduleResponse' smart constructor.
data DeleteSnapshotScheduleResponse = DeleteSnapshotScheduleResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  where
  rnf _ = ()
