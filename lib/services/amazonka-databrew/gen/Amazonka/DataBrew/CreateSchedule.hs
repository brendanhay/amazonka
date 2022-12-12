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
-- Module      : Amazonka.DataBrew.CreateSchedule
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new schedule for one or more DataBrew jobs. Jobs can be run at
-- a specific date and time, or at regular intervals.
module Amazonka.DataBrew.CreateSchedule
  ( -- * Creating a Request
    CreateSchedule (..),
    newCreateSchedule,

    -- * Request Lenses
    createSchedule_jobNames,
    createSchedule_tags,
    createSchedule_cronExpression,
    createSchedule_name,

    -- * Destructuring the Response
    CreateScheduleResponse (..),
    newCreateScheduleResponse,

    -- * Response Lenses
    createScheduleResponse_httpStatus,
    createScheduleResponse_name,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DataBrew.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateSchedule' smart constructor.
data CreateSchedule = CreateSchedule'
  { -- | The name or names of one or more jobs to be run.
    jobNames :: Prelude.Maybe [Prelude.Text],
    -- | Metadata tags to apply to this schedule.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The date or dates and time or times when the jobs are to be run. For
    -- more information, see
    -- <https://docs.aws.amazon.com/databrew/latest/dg/jobs.cron.html Cron expressions>
    -- in the /Glue DataBrew Developer Guide/.
    cronExpression :: Prelude.Text,
    -- | A unique name for the schedule. Valid characters are alphanumeric (A-Z,
    -- a-z, 0-9), hyphen (-), period (.), and space.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateSchedule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobNames', 'createSchedule_jobNames' - The name or names of one or more jobs to be run.
--
-- 'tags', 'createSchedule_tags' - Metadata tags to apply to this schedule.
--
-- 'cronExpression', 'createSchedule_cronExpression' - The date or dates and time or times when the jobs are to be run. For
-- more information, see
-- <https://docs.aws.amazon.com/databrew/latest/dg/jobs.cron.html Cron expressions>
-- in the /Glue DataBrew Developer Guide/.
--
-- 'name', 'createSchedule_name' - A unique name for the schedule. Valid characters are alphanumeric (A-Z,
-- a-z, 0-9), hyphen (-), period (.), and space.
newCreateSchedule ::
  -- | 'cronExpression'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  CreateSchedule
newCreateSchedule pCronExpression_ pName_ =
  CreateSchedule'
    { jobNames = Prelude.Nothing,
      tags = Prelude.Nothing,
      cronExpression = pCronExpression_,
      name = pName_
    }

-- | The name or names of one or more jobs to be run.
createSchedule_jobNames :: Lens.Lens' CreateSchedule (Prelude.Maybe [Prelude.Text])
createSchedule_jobNames = Lens.lens (\CreateSchedule' {jobNames} -> jobNames) (\s@CreateSchedule' {} a -> s {jobNames = a} :: CreateSchedule) Prelude.. Lens.mapping Lens.coerced

-- | Metadata tags to apply to this schedule.
createSchedule_tags :: Lens.Lens' CreateSchedule (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createSchedule_tags = Lens.lens (\CreateSchedule' {tags} -> tags) (\s@CreateSchedule' {} a -> s {tags = a} :: CreateSchedule) Prelude.. Lens.mapping Lens.coerced

-- | The date or dates and time or times when the jobs are to be run. For
-- more information, see
-- <https://docs.aws.amazon.com/databrew/latest/dg/jobs.cron.html Cron expressions>
-- in the /Glue DataBrew Developer Guide/.
createSchedule_cronExpression :: Lens.Lens' CreateSchedule Prelude.Text
createSchedule_cronExpression = Lens.lens (\CreateSchedule' {cronExpression} -> cronExpression) (\s@CreateSchedule' {} a -> s {cronExpression = a} :: CreateSchedule)

-- | A unique name for the schedule. Valid characters are alphanumeric (A-Z,
-- a-z, 0-9), hyphen (-), period (.), and space.
createSchedule_name :: Lens.Lens' CreateSchedule Prelude.Text
createSchedule_name = Lens.lens (\CreateSchedule' {name} -> name) (\s@CreateSchedule' {} a -> s {name = a} :: CreateSchedule)

instance Core.AWSRequest CreateSchedule where
  type
    AWSResponse CreateSchedule =
      CreateScheduleResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateScheduleResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "Name")
      )

instance Prelude.Hashable CreateSchedule where
  hashWithSalt _salt CreateSchedule' {..} =
    _salt `Prelude.hashWithSalt` jobNames
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` cronExpression
      `Prelude.hashWithSalt` name

instance Prelude.NFData CreateSchedule where
  rnf CreateSchedule' {..} =
    Prelude.rnf jobNames
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf cronExpression
      `Prelude.seq` Prelude.rnf name

instance Data.ToHeaders CreateSchedule where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateSchedule where
  toJSON CreateSchedule' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("JobNames" Data..=) Prelude.<$> jobNames,
            ("Tags" Data..=) Prelude.<$> tags,
            Prelude.Just
              ("CronExpression" Data..= cronExpression),
            Prelude.Just ("Name" Data..= name)
          ]
      )

instance Data.ToPath CreateSchedule where
  toPath = Prelude.const "/schedules"

instance Data.ToQuery CreateSchedule where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateScheduleResponse' smart constructor.
data CreateScheduleResponse = CreateScheduleResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The name of the schedule that was created.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateScheduleResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createScheduleResponse_httpStatus' - The response's http status code.
--
-- 'name', 'createScheduleResponse_name' - The name of the schedule that was created.
newCreateScheduleResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'name'
  Prelude.Text ->
  CreateScheduleResponse
newCreateScheduleResponse pHttpStatus_ pName_ =
  CreateScheduleResponse'
    { httpStatus = pHttpStatus_,
      name = pName_
    }

-- | The response's http status code.
createScheduleResponse_httpStatus :: Lens.Lens' CreateScheduleResponse Prelude.Int
createScheduleResponse_httpStatus = Lens.lens (\CreateScheduleResponse' {httpStatus} -> httpStatus) (\s@CreateScheduleResponse' {} a -> s {httpStatus = a} :: CreateScheduleResponse)

-- | The name of the schedule that was created.
createScheduleResponse_name :: Lens.Lens' CreateScheduleResponse Prelude.Text
createScheduleResponse_name = Lens.lens (\CreateScheduleResponse' {name} -> name) (\s@CreateScheduleResponse' {} a -> s {name = a} :: CreateScheduleResponse)

instance Prelude.NFData CreateScheduleResponse where
  rnf CreateScheduleResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf name
