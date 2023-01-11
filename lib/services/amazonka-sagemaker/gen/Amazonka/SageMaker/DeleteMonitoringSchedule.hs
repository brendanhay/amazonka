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
-- Module      : Amazonka.SageMaker.DeleteMonitoringSchedule
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a monitoring schedule. Also stops the schedule had not already
-- been stopped. This does not delete the job execution history of the
-- monitoring schedule.
module Amazonka.SageMaker.DeleteMonitoringSchedule
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

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
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull
      DeleteMonitoringScheduleResponse'

instance Prelude.Hashable DeleteMonitoringSchedule where
  hashWithSalt _salt DeleteMonitoringSchedule' {..} =
    _salt `Prelude.hashWithSalt` monitoringScheduleName

instance Prelude.NFData DeleteMonitoringSchedule where
  rnf DeleteMonitoringSchedule' {..} =
    Prelude.rnf monitoringScheduleName

instance Data.ToHeaders DeleteMonitoringSchedule where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "SageMaker.DeleteMonitoringSchedule" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteMonitoringSchedule where
  toJSON DeleteMonitoringSchedule' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "MonitoringScheduleName"
                  Data..= monitoringScheduleName
              )
          ]
      )

instance Data.ToPath DeleteMonitoringSchedule where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteMonitoringSchedule where
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
  where
  rnf _ = ()
