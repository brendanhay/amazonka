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
-- Module      : Network.AWS.SageMaker.DeleteMonitoringSchedule
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a monitoring schedule. Also stops the schedule had not already
-- been stopped. This does not delete the job execution history of the
-- monitoring schedule.
module Network.AWS.SageMaker.DeleteMonitoringSchedule
  ( -- * Creating a Request
    DeleteMonitoringSchedule (..),
    newDeleteMonitoringSchedule,

    -- * Request Lenses
    deleteMonitoringSchedule_monitoringScheduleName,

    -- * Destructuring the Response
    DeleteMonitoringScheduleResponse (..),
    newDeleteMonitoringScheduleResponse,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newDeleteMonitoringSchedule' smart constructor.
data DeleteMonitoringSchedule = DeleteMonitoringSchedule'
  { -- | The name of the monitoring schedule to delete.
    monitoringScheduleName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteMonitoringSchedule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'monitoringScheduleName', 'deleteMonitoringSchedule_monitoringScheduleName' - The name of the monitoring schedule to delete.
newDeleteMonitoringSchedule ::
  -- | 'monitoringScheduleName'
  Prelude.Text ->
  DeleteMonitoringSchedule
newDeleteMonitoringSchedule pMonitoringScheduleName_ =
  DeleteMonitoringSchedule'
    { monitoringScheduleName =
        pMonitoringScheduleName_
    }

-- | The name of the monitoring schedule to delete.
deleteMonitoringSchedule_monitoringScheduleName :: Lens.Lens' DeleteMonitoringSchedule Prelude.Text
deleteMonitoringSchedule_monitoringScheduleName = Lens.lens (\DeleteMonitoringSchedule' {monitoringScheduleName} -> monitoringScheduleName) (\s@DeleteMonitoringSchedule' {} a -> s {monitoringScheduleName = a} :: DeleteMonitoringSchedule)

instance Core.AWSRequest DeleteMonitoringSchedule where
  type
    AWSResponse DeleteMonitoringSchedule =
      DeleteMonitoringScheduleResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull
      DeleteMonitoringScheduleResponse'

instance Prelude.Hashable DeleteMonitoringSchedule

instance Prelude.NFData DeleteMonitoringSchedule

instance Core.ToHeaders DeleteMonitoringSchedule where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "SageMaker.DeleteMonitoringSchedule" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DeleteMonitoringSchedule where
  toJSON DeleteMonitoringSchedule' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "MonitoringScheduleName"
                  Core..= monitoringScheduleName
              )
          ]
      )

instance Core.ToPath DeleteMonitoringSchedule where
  toPath = Prelude.const "/"

instance Core.ToQuery DeleteMonitoringSchedule where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteMonitoringScheduleResponse' smart constructor.
data DeleteMonitoringScheduleResponse = DeleteMonitoringScheduleResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteMonitoringScheduleResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteMonitoringScheduleResponse ::
  DeleteMonitoringScheduleResponse
newDeleteMonitoringScheduleResponse =
  DeleteMonitoringScheduleResponse'

instance
  Prelude.NFData
    DeleteMonitoringScheduleResponse
