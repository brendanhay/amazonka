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
-- Module      : Amazonka.DataBrew.DescribeSchedule
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the definition of a specific DataBrew schedule.
module Amazonka.DataBrew.DescribeSchedule
  ( -- * Creating a Request
    DescribeSchedule (..),
    newDescribeSchedule,

    -- * Request Lenses
    describeSchedule_name,

    -- * Destructuring the Response
    DescribeScheduleResponse (..),
    newDescribeScheduleResponse,

    -- * Response Lenses
    describeScheduleResponse_tags,
    describeScheduleResponse_lastModifiedDate,
    describeScheduleResponse_createDate,
    describeScheduleResponse_lastModifiedBy,
    describeScheduleResponse_resourceArn,
    describeScheduleResponse_jobNames,
    describeScheduleResponse_cronExpression,
    describeScheduleResponse_createdBy,
    describeScheduleResponse_httpStatus,
    describeScheduleResponse_name,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DataBrew.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeSchedule' smart constructor.
data DescribeSchedule = DescribeSchedule'
  { -- | The name of the schedule to be described.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeSchedule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'describeSchedule_name' - The name of the schedule to be described.
newDescribeSchedule ::
  -- | 'name'
  Prelude.Text ->
  DescribeSchedule
newDescribeSchedule pName_ =
  DescribeSchedule' {name = pName_}

-- | The name of the schedule to be described.
describeSchedule_name :: Lens.Lens' DescribeSchedule Prelude.Text
describeSchedule_name = Lens.lens (\DescribeSchedule' {name} -> name) (\s@DescribeSchedule' {} a -> s {name = a} :: DescribeSchedule)

instance Core.AWSRequest DescribeSchedule where
  type
    AWSResponse DescribeSchedule =
      DescribeScheduleResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeScheduleResponse'
            Prelude.<$> (x Data..?> "Tags" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "LastModifiedDate")
            Prelude.<*> (x Data..?> "CreateDate")
            Prelude.<*> (x Data..?> "LastModifiedBy")
            Prelude.<*> (x Data..?> "ResourceArn")
            Prelude.<*> (x Data..?> "JobNames" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "CronExpression")
            Prelude.<*> (x Data..?> "CreatedBy")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "Name")
      )

instance Prelude.Hashable DescribeSchedule where
  hashWithSalt _salt DescribeSchedule' {..} =
    _salt `Prelude.hashWithSalt` name

instance Prelude.NFData DescribeSchedule where
  rnf DescribeSchedule' {..} = Prelude.rnf name

instance Data.ToHeaders DescribeSchedule where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DescribeSchedule where
  toPath DescribeSchedule' {..} =
    Prelude.mconcat ["/schedules/", Data.toBS name]

instance Data.ToQuery DescribeSchedule where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeScheduleResponse' smart constructor.
data DescribeScheduleResponse = DescribeScheduleResponse'
  { -- | Metadata tags associated with this schedule.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The date and time that the schedule was last modified.
    lastModifiedDate :: Prelude.Maybe Data.POSIX,
    -- | The date and time that the schedule was created.
    createDate :: Prelude.Maybe Data.POSIX,
    -- | The identifier (user name) of the user who last modified the schedule.
    lastModifiedBy :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the schedule.
    resourceArn :: Prelude.Maybe Prelude.Text,
    -- | The name or names of one or more jobs to be run by using the schedule.
    jobNames :: Prelude.Maybe [Prelude.Text],
    -- | The date or dates and time or times when the jobs are to be run for the
    -- schedule. For more information, see
    -- <https://docs.aws.amazon.com/databrew/latest/dg/jobs.cron.html Cron expressions>
    -- in the /Glue DataBrew Developer Guide/.
    cronExpression :: Prelude.Maybe Prelude.Text,
    -- | The identifier (user name) of the user who created the schedule.
    createdBy :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The name of the schedule.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeScheduleResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'describeScheduleResponse_tags' - Metadata tags associated with this schedule.
--
-- 'lastModifiedDate', 'describeScheduleResponse_lastModifiedDate' - The date and time that the schedule was last modified.
--
-- 'createDate', 'describeScheduleResponse_createDate' - The date and time that the schedule was created.
--
-- 'lastModifiedBy', 'describeScheduleResponse_lastModifiedBy' - The identifier (user name) of the user who last modified the schedule.
--
-- 'resourceArn', 'describeScheduleResponse_resourceArn' - The Amazon Resource Name (ARN) of the schedule.
--
-- 'jobNames', 'describeScheduleResponse_jobNames' - The name or names of one or more jobs to be run by using the schedule.
--
-- 'cronExpression', 'describeScheduleResponse_cronExpression' - The date or dates and time or times when the jobs are to be run for the
-- schedule. For more information, see
-- <https://docs.aws.amazon.com/databrew/latest/dg/jobs.cron.html Cron expressions>
-- in the /Glue DataBrew Developer Guide/.
--
-- 'createdBy', 'describeScheduleResponse_createdBy' - The identifier (user name) of the user who created the schedule.
--
-- 'httpStatus', 'describeScheduleResponse_httpStatus' - The response's http status code.
--
-- 'name', 'describeScheduleResponse_name' - The name of the schedule.
newDescribeScheduleResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'name'
  Prelude.Text ->
  DescribeScheduleResponse
newDescribeScheduleResponse pHttpStatus_ pName_ =
  DescribeScheduleResponse'
    { tags = Prelude.Nothing,
      lastModifiedDate = Prelude.Nothing,
      createDate = Prelude.Nothing,
      lastModifiedBy = Prelude.Nothing,
      resourceArn = Prelude.Nothing,
      jobNames = Prelude.Nothing,
      cronExpression = Prelude.Nothing,
      createdBy = Prelude.Nothing,
      httpStatus = pHttpStatus_,
      name = pName_
    }

-- | Metadata tags associated with this schedule.
describeScheduleResponse_tags :: Lens.Lens' DescribeScheduleResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
describeScheduleResponse_tags = Lens.lens (\DescribeScheduleResponse' {tags} -> tags) (\s@DescribeScheduleResponse' {} a -> s {tags = a} :: DescribeScheduleResponse) Prelude.. Lens.mapping Lens.coerced

-- | The date and time that the schedule was last modified.
describeScheduleResponse_lastModifiedDate :: Lens.Lens' DescribeScheduleResponse (Prelude.Maybe Prelude.UTCTime)
describeScheduleResponse_lastModifiedDate = Lens.lens (\DescribeScheduleResponse' {lastModifiedDate} -> lastModifiedDate) (\s@DescribeScheduleResponse' {} a -> s {lastModifiedDate = a} :: DescribeScheduleResponse) Prelude.. Lens.mapping Data._Time

-- | The date and time that the schedule was created.
describeScheduleResponse_createDate :: Lens.Lens' DescribeScheduleResponse (Prelude.Maybe Prelude.UTCTime)
describeScheduleResponse_createDate = Lens.lens (\DescribeScheduleResponse' {createDate} -> createDate) (\s@DescribeScheduleResponse' {} a -> s {createDate = a} :: DescribeScheduleResponse) Prelude.. Lens.mapping Data._Time

-- | The identifier (user name) of the user who last modified the schedule.
describeScheduleResponse_lastModifiedBy :: Lens.Lens' DescribeScheduleResponse (Prelude.Maybe Prelude.Text)
describeScheduleResponse_lastModifiedBy = Lens.lens (\DescribeScheduleResponse' {lastModifiedBy} -> lastModifiedBy) (\s@DescribeScheduleResponse' {} a -> s {lastModifiedBy = a} :: DescribeScheduleResponse)

-- | The Amazon Resource Name (ARN) of the schedule.
describeScheduleResponse_resourceArn :: Lens.Lens' DescribeScheduleResponse (Prelude.Maybe Prelude.Text)
describeScheduleResponse_resourceArn = Lens.lens (\DescribeScheduleResponse' {resourceArn} -> resourceArn) (\s@DescribeScheduleResponse' {} a -> s {resourceArn = a} :: DescribeScheduleResponse)

-- | The name or names of one or more jobs to be run by using the schedule.
describeScheduleResponse_jobNames :: Lens.Lens' DescribeScheduleResponse (Prelude.Maybe [Prelude.Text])
describeScheduleResponse_jobNames = Lens.lens (\DescribeScheduleResponse' {jobNames} -> jobNames) (\s@DescribeScheduleResponse' {} a -> s {jobNames = a} :: DescribeScheduleResponse) Prelude.. Lens.mapping Lens.coerced

-- | The date or dates and time or times when the jobs are to be run for the
-- schedule. For more information, see
-- <https://docs.aws.amazon.com/databrew/latest/dg/jobs.cron.html Cron expressions>
-- in the /Glue DataBrew Developer Guide/.
describeScheduleResponse_cronExpression :: Lens.Lens' DescribeScheduleResponse (Prelude.Maybe Prelude.Text)
describeScheduleResponse_cronExpression = Lens.lens (\DescribeScheduleResponse' {cronExpression} -> cronExpression) (\s@DescribeScheduleResponse' {} a -> s {cronExpression = a} :: DescribeScheduleResponse)

-- | The identifier (user name) of the user who created the schedule.
describeScheduleResponse_createdBy :: Lens.Lens' DescribeScheduleResponse (Prelude.Maybe Prelude.Text)
describeScheduleResponse_createdBy = Lens.lens (\DescribeScheduleResponse' {createdBy} -> createdBy) (\s@DescribeScheduleResponse' {} a -> s {createdBy = a} :: DescribeScheduleResponse)

-- | The response's http status code.
describeScheduleResponse_httpStatus :: Lens.Lens' DescribeScheduleResponse Prelude.Int
describeScheduleResponse_httpStatus = Lens.lens (\DescribeScheduleResponse' {httpStatus} -> httpStatus) (\s@DescribeScheduleResponse' {} a -> s {httpStatus = a} :: DescribeScheduleResponse)

-- | The name of the schedule.
describeScheduleResponse_name :: Lens.Lens' DescribeScheduleResponse Prelude.Text
describeScheduleResponse_name = Lens.lens (\DescribeScheduleResponse' {name} -> name) (\s@DescribeScheduleResponse' {} a -> s {name = a} :: DescribeScheduleResponse)

instance Prelude.NFData DescribeScheduleResponse where
  rnf DescribeScheduleResponse' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf lastModifiedDate
      `Prelude.seq` Prelude.rnf createDate
      `Prelude.seq` Prelude.rnf lastModifiedBy
      `Prelude.seq` Prelude.rnf resourceArn
      `Prelude.seq` Prelude.rnf jobNames
      `Prelude.seq` Prelude.rnf cronExpression
      `Prelude.seq` Prelude.rnf createdBy
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf name
