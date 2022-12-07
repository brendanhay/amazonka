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
-- Module      : Amazonka.AlexaBusiness.DeleteBusinessReportSchedule
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the recurring report delivery schedule with the specified
-- schedule ARN.
module Amazonka.AlexaBusiness.DeleteBusinessReportSchedule
  ( -- * Creating a Request
    DeleteBusinessReportSchedule (..),
    newDeleteBusinessReportSchedule,

    -- * Request Lenses
    deleteBusinessReportSchedule_scheduleArn,

    -- * Destructuring the Response
    DeleteBusinessReportScheduleResponse (..),
    newDeleteBusinessReportScheduleResponse,

    -- * Response Lenses
    deleteBusinessReportScheduleResponse_httpStatus,
  )
where

import Amazonka.AlexaBusiness.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteBusinessReportSchedule' smart constructor.
data DeleteBusinessReportSchedule = DeleteBusinessReportSchedule'
  { -- | The ARN of the business report schedule.
    scheduleArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteBusinessReportSchedule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'scheduleArn', 'deleteBusinessReportSchedule_scheduleArn' - The ARN of the business report schedule.
newDeleteBusinessReportSchedule ::
  -- | 'scheduleArn'
  Prelude.Text ->
  DeleteBusinessReportSchedule
newDeleteBusinessReportSchedule pScheduleArn_ =
  DeleteBusinessReportSchedule'
    { scheduleArn =
        pScheduleArn_
    }

-- | The ARN of the business report schedule.
deleteBusinessReportSchedule_scheduleArn :: Lens.Lens' DeleteBusinessReportSchedule Prelude.Text
deleteBusinessReportSchedule_scheduleArn = Lens.lens (\DeleteBusinessReportSchedule' {scheduleArn} -> scheduleArn) (\s@DeleteBusinessReportSchedule' {} a -> s {scheduleArn = a} :: DeleteBusinessReportSchedule)

instance Core.AWSRequest DeleteBusinessReportSchedule where
  type
    AWSResponse DeleteBusinessReportSchedule =
      DeleteBusinessReportScheduleResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteBusinessReportScheduleResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DeleteBusinessReportSchedule
  where
  hashWithSalt _salt DeleteBusinessReportSchedule' {..} =
    _salt `Prelude.hashWithSalt` scheduleArn

instance Prelude.NFData DeleteBusinessReportSchedule where
  rnf DeleteBusinessReportSchedule' {..} =
    Prelude.rnf scheduleArn

instance Data.ToHeaders DeleteBusinessReportSchedule where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AlexaForBusiness.DeleteBusinessReportSchedule" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteBusinessReportSchedule where
  toJSON DeleteBusinessReportSchedule' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("ScheduleArn" Data..= scheduleArn)]
      )

instance Data.ToPath DeleteBusinessReportSchedule where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteBusinessReportSchedule where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteBusinessReportScheduleResponse' smart constructor.
data DeleteBusinessReportScheduleResponse = DeleteBusinessReportScheduleResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteBusinessReportScheduleResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteBusinessReportScheduleResponse_httpStatus' - The response's http status code.
newDeleteBusinessReportScheduleResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteBusinessReportScheduleResponse
newDeleteBusinessReportScheduleResponse pHttpStatus_ =
  DeleteBusinessReportScheduleResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteBusinessReportScheduleResponse_httpStatus :: Lens.Lens' DeleteBusinessReportScheduleResponse Prelude.Int
deleteBusinessReportScheduleResponse_httpStatus = Lens.lens (\DeleteBusinessReportScheduleResponse' {httpStatus} -> httpStatus) (\s@DeleteBusinessReportScheduleResponse' {} a -> s {httpStatus = a} :: DeleteBusinessReportScheduleResponse)

instance
  Prelude.NFData
    DeleteBusinessReportScheduleResponse
  where
  rnf DeleteBusinessReportScheduleResponse' {..} =
    Prelude.rnf httpStatus
