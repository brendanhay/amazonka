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
-- Module      : Amazonka.Backup.DescribeReportJob
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the details associated with creating a report as specified by
-- its @ReportJobId@.
module Amazonka.Backup.DescribeReportJob
  ( -- * Creating a Request
    DescribeReportJob (..),
    newDescribeReportJob,

    -- * Request Lenses
    describeReportJob_reportJobId,

    -- * Destructuring the Response
    DescribeReportJobResponse (..),
    newDescribeReportJobResponse,

    -- * Response Lenses
    describeReportJobResponse_reportJob,
    describeReportJobResponse_httpStatus,
  )
where

import Amazonka.Backup.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeReportJob' smart constructor.
data DescribeReportJob = DescribeReportJob'
  { -- | The identifier of the report job. A unique, randomly generated, Unicode,
    -- UTF-8 encoded string that is at most 1,024 bytes long. The report job ID
    -- cannot be edited.
    reportJobId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeReportJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'reportJobId', 'describeReportJob_reportJobId' - The identifier of the report job. A unique, randomly generated, Unicode,
-- UTF-8 encoded string that is at most 1,024 bytes long. The report job ID
-- cannot be edited.
newDescribeReportJob ::
  -- | 'reportJobId'
  Prelude.Text ->
  DescribeReportJob
newDescribeReportJob pReportJobId_ =
  DescribeReportJob' {reportJobId = pReportJobId_}

-- | The identifier of the report job. A unique, randomly generated, Unicode,
-- UTF-8 encoded string that is at most 1,024 bytes long. The report job ID
-- cannot be edited.
describeReportJob_reportJobId :: Lens.Lens' DescribeReportJob Prelude.Text
describeReportJob_reportJobId = Lens.lens (\DescribeReportJob' {reportJobId} -> reportJobId) (\s@DescribeReportJob' {} a -> s {reportJobId = a} :: DescribeReportJob)

instance Core.AWSRequest DescribeReportJob where
  type
    AWSResponse DescribeReportJob =
      DescribeReportJobResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeReportJobResponse'
            Prelude.<$> (x Data..?> "ReportJob")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeReportJob where
  hashWithSalt _salt DescribeReportJob' {..} =
    _salt `Prelude.hashWithSalt` reportJobId

instance Prelude.NFData DescribeReportJob where
  rnf DescribeReportJob' {..} = Prelude.rnf reportJobId

instance Data.ToHeaders DescribeReportJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DescribeReportJob where
  toPath DescribeReportJob' {..} =
    Prelude.mconcat
      ["/audit/report-jobs/", Data.toBS reportJobId]

instance Data.ToQuery DescribeReportJob where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeReportJobResponse' smart constructor.
data DescribeReportJobResponse = DescribeReportJobResponse'
  { -- | A list of information about a report job, including its completion and
    -- creation times, report destination, unique report job ID, Amazon
    -- Resource Name (ARN), report template, status, and status message.
    reportJob :: Prelude.Maybe ReportJob,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeReportJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'reportJob', 'describeReportJobResponse_reportJob' - A list of information about a report job, including its completion and
-- creation times, report destination, unique report job ID, Amazon
-- Resource Name (ARN), report template, status, and status message.
--
-- 'httpStatus', 'describeReportJobResponse_httpStatus' - The response's http status code.
newDescribeReportJobResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeReportJobResponse
newDescribeReportJobResponse pHttpStatus_ =
  DescribeReportJobResponse'
    { reportJob =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of information about a report job, including its completion and
-- creation times, report destination, unique report job ID, Amazon
-- Resource Name (ARN), report template, status, and status message.
describeReportJobResponse_reportJob :: Lens.Lens' DescribeReportJobResponse (Prelude.Maybe ReportJob)
describeReportJobResponse_reportJob = Lens.lens (\DescribeReportJobResponse' {reportJob} -> reportJob) (\s@DescribeReportJobResponse' {} a -> s {reportJob = a} :: DescribeReportJobResponse)

-- | The response's http status code.
describeReportJobResponse_httpStatus :: Lens.Lens' DescribeReportJobResponse Prelude.Int
describeReportJobResponse_httpStatus = Lens.lens (\DescribeReportJobResponse' {httpStatus} -> httpStatus) (\s@DescribeReportJobResponse' {} a -> s {httpStatus = a} :: DescribeReportJobResponse)

instance Prelude.NFData DescribeReportJobResponse where
  rnf DescribeReportJobResponse' {..} =
    Prelude.rnf reportJob
      `Prelude.seq` Prelude.rnf httpStatus
