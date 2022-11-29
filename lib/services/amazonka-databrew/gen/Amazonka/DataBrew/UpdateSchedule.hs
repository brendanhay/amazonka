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
-- Module      : Amazonka.DataBrew.UpdateSchedule
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the definition of an existing DataBrew schedule.
module Amazonka.DataBrew.UpdateSchedule
  ( -- * Creating a Request
    UpdateSchedule (..),
    newUpdateSchedule,

    -- * Request Lenses
    updateSchedule_jobNames,
    updateSchedule_cronExpression,
    updateSchedule_name,

    -- * Destructuring the Response
    UpdateScheduleResponse (..),
    newUpdateScheduleResponse,

    -- * Response Lenses
    updateScheduleResponse_httpStatus,
    updateScheduleResponse_name,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.DataBrew.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateSchedule' smart constructor.
data UpdateSchedule = UpdateSchedule'
  { -- | The name or names of one or more jobs to be run for this schedule.
    jobNames :: Prelude.Maybe [Prelude.Text],
    -- | The date or dates and time or times when the jobs are to be run. For
    -- more information, see
    -- <https://docs.aws.amazon.com/databrew/latest/dg/jobs.cron.html Cron expressions>
    -- in the /Glue DataBrew Developer Guide/.
    cronExpression :: Prelude.Text,
    -- | The name of the schedule to update.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateSchedule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobNames', 'updateSchedule_jobNames' - The name or names of one or more jobs to be run for this schedule.
--
-- 'cronExpression', 'updateSchedule_cronExpression' - The date or dates and time or times when the jobs are to be run. For
-- more information, see
-- <https://docs.aws.amazon.com/databrew/latest/dg/jobs.cron.html Cron expressions>
-- in the /Glue DataBrew Developer Guide/.
--
-- 'name', 'updateSchedule_name' - The name of the schedule to update.
newUpdateSchedule ::
  -- | 'cronExpression'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  UpdateSchedule
newUpdateSchedule pCronExpression_ pName_ =
  UpdateSchedule'
    { jobNames = Prelude.Nothing,
      cronExpression = pCronExpression_,
      name = pName_
    }

-- | The name or names of one or more jobs to be run for this schedule.
updateSchedule_jobNames :: Lens.Lens' UpdateSchedule (Prelude.Maybe [Prelude.Text])
updateSchedule_jobNames = Lens.lens (\UpdateSchedule' {jobNames} -> jobNames) (\s@UpdateSchedule' {} a -> s {jobNames = a} :: UpdateSchedule) Prelude.. Lens.mapping Lens.coerced

-- | The date or dates and time or times when the jobs are to be run. For
-- more information, see
-- <https://docs.aws.amazon.com/databrew/latest/dg/jobs.cron.html Cron expressions>
-- in the /Glue DataBrew Developer Guide/.
updateSchedule_cronExpression :: Lens.Lens' UpdateSchedule Prelude.Text
updateSchedule_cronExpression = Lens.lens (\UpdateSchedule' {cronExpression} -> cronExpression) (\s@UpdateSchedule' {} a -> s {cronExpression = a} :: UpdateSchedule)

-- | The name of the schedule to update.
updateSchedule_name :: Lens.Lens' UpdateSchedule Prelude.Text
updateSchedule_name = Lens.lens (\UpdateSchedule' {name} -> name) (\s@UpdateSchedule' {} a -> s {name = a} :: UpdateSchedule)

instance Core.AWSRequest UpdateSchedule where
  type
    AWSResponse UpdateSchedule =
      UpdateScheduleResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateScheduleResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..:> "Name")
      )

instance Prelude.Hashable UpdateSchedule where
  hashWithSalt _salt UpdateSchedule' {..} =
    _salt `Prelude.hashWithSalt` jobNames
      `Prelude.hashWithSalt` cronExpression
      `Prelude.hashWithSalt` name

instance Prelude.NFData UpdateSchedule where
  rnf UpdateSchedule' {..} =
    Prelude.rnf jobNames
      `Prelude.seq` Prelude.rnf cronExpression
      `Prelude.seq` Prelude.rnf name

instance Core.ToHeaders UpdateSchedule where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateSchedule where
  toJSON UpdateSchedule' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("JobNames" Core..=) Prelude.<$> jobNames,
            Prelude.Just
              ("CronExpression" Core..= cronExpression)
          ]
      )

instance Core.ToPath UpdateSchedule where
  toPath UpdateSchedule' {..} =
    Prelude.mconcat ["/schedules/", Core.toBS name]

instance Core.ToQuery UpdateSchedule where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateScheduleResponse' smart constructor.
data UpdateScheduleResponse = UpdateScheduleResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The name of the schedule that was updated.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateScheduleResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateScheduleResponse_httpStatus' - The response's http status code.
--
-- 'name', 'updateScheduleResponse_name' - The name of the schedule that was updated.
newUpdateScheduleResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'name'
  Prelude.Text ->
  UpdateScheduleResponse
newUpdateScheduleResponse pHttpStatus_ pName_ =
  UpdateScheduleResponse'
    { httpStatus = pHttpStatus_,
      name = pName_
    }

-- | The response's http status code.
updateScheduleResponse_httpStatus :: Lens.Lens' UpdateScheduleResponse Prelude.Int
updateScheduleResponse_httpStatus = Lens.lens (\UpdateScheduleResponse' {httpStatus} -> httpStatus) (\s@UpdateScheduleResponse' {} a -> s {httpStatus = a} :: UpdateScheduleResponse)

-- | The name of the schedule that was updated.
updateScheduleResponse_name :: Lens.Lens' UpdateScheduleResponse Prelude.Text
updateScheduleResponse_name = Lens.lens (\UpdateScheduleResponse' {name} -> name) (\s@UpdateScheduleResponse' {} a -> s {name = a} :: UpdateScheduleResponse)

instance Prelude.NFData UpdateScheduleResponse where
  rnf UpdateScheduleResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf name
