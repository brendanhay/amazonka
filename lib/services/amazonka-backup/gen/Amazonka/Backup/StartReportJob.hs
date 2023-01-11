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
-- Module      : Amazonka.Backup.StartReportJob
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts an on-demand report job for the specified report plan.
module Amazonka.Backup.StartReportJob
  ( -- * Creating a Request
    StartReportJob (..),
    newStartReportJob,

    -- * Request Lenses
    startReportJob_idempotencyToken,
    startReportJob_reportPlanName,

    -- * Destructuring the Response
    StartReportJobResponse (..),
    newStartReportJobResponse,

    -- * Response Lenses
    startReportJobResponse_reportJobId,
    startReportJobResponse_httpStatus,
  )
where

import Amazonka.Backup.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStartReportJob' smart constructor.
data StartReportJob = StartReportJob'
  { -- | A customer-chosen string that you can use to distinguish between
    -- otherwise identical calls to @StartReportJobInput@. Retrying a
    -- successful request with the same idempotency token results in a success
    -- message with no action taken.
    idempotencyToken :: Prelude.Maybe Prelude.Text,
    -- | The unique name of a report plan.
    reportPlanName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartReportJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'idempotencyToken', 'startReportJob_idempotencyToken' - A customer-chosen string that you can use to distinguish between
-- otherwise identical calls to @StartReportJobInput@. Retrying a
-- successful request with the same idempotency token results in a success
-- message with no action taken.
--
-- 'reportPlanName', 'startReportJob_reportPlanName' - The unique name of a report plan.
newStartReportJob ::
  -- | 'reportPlanName'
  Prelude.Text ->
  StartReportJob
newStartReportJob pReportPlanName_ =
  StartReportJob'
    { idempotencyToken = Prelude.Nothing,
      reportPlanName = pReportPlanName_
    }

-- | A customer-chosen string that you can use to distinguish between
-- otherwise identical calls to @StartReportJobInput@. Retrying a
-- successful request with the same idempotency token results in a success
-- message with no action taken.
startReportJob_idempotencyToken :: Lens.Lens' StartReportJob (Prelude.Maybe Prelude.Text)
startReportJob_idempotencyToken = Lens.lens (\StartReportJob' {idempotencyToken} -> idempotencyToken) (\s@StartReportJob' {} a -> s {idempotencyToken = a} :: StartReportJob)

-- | The unique name of a report plan.
startReportJob_reportPlanName :: Lens.Lens' StartReportJob Prelude.Text
startReportJob_reportPlanName = Lens.lens (\StartReportJob' {reportPlanName} -> reportPlanName) (\s@StartReportJob' {} a -> s {reportPlanName = a} :: StartReportJob)

instance Core.AWSRequest StartReportJob where
  type
    AWSResponse StartReportJob =
      StartReportJobResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          StartReportJobResponse'
            Prelude.<$> (x Data..?> "ReportJobId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StartReportJob where
  hashWithSalt _salt StartReportJob' {..} =
    _salt `Prelude.hashWithSalt` idempotencyToken
      `Prelude.hashWithSalt` reportPlanName

instance Prelude.NFData StartReportJob where
  rnf StartReportJob' {..} =
    Prelude.rnf idempotencyToken
      `Prelude.seq` Prelude.rnf reportPlanName

instance Data.ToHeaders StartReportJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON StartReportJob where
  toJSON StartReportJob' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("IdempotencyToken" Data..=)
              Prelude.<$> idempotencyToken
          ]
      )

instance Data.ToPath StartReportJob where
  toPath StartReportJob' {..} =
    Prelude.mconcat
      ["/audit/report-jobs/", Data.toBS reportPlanName]

instance Data.ToQuery StartReportJob where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartReportJobResponse' smart constructor.
data StartReportJobResponse = StartReportJobResponse'
  { -- | The identifier of the report job. A unique, randomly generated, Unicode,
    -- UTF-8 encoded string that is at most 1,024 bytes long. The report job ID
    -- cannot be edited.
    reportJobId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartReportJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'reportJobId', 'startReportJobResponse_reportJobId' - The identifier of the report job. A unique, randomly generated, Unicode,
-- UTF-8 encoded string that is at most 1,024 bytes long. The report job ID
-- cannot be edited.
--
-- 'httpStatus', 'startReportJobResponse_httpStatus' - The response's http status code.
newStartReportJobResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StartReportJobResponse
newStartReportJobResponse pHttpStatus_ =
  StartReportJobResponse'
    { reportJobId =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The identifier of the report job. A unique, randomly generated, Unicode,
-- UTF-8 encoded string that is at most 1,024 bytes long. The report job ID
-- cannot be edited.
startReportJobResponse_reportJobId :: Lens.Lens' StartReportJobResponse (Prelude.Maybe Prelude.Text)
startReportJobResponse_reportJobId = Lens.lens (\StartReportJobResponse' {reportJobId} -> reportJobId) (\s@StartReportJobResponse' {} a -> s {reportJobId = a} :: StartReportJobResponse)

-- | The response's http status code.
startReportJobResponse_httpStatus :: Lens.Lens' StartReportJobResponse Prelude.Int
startReportJobResponse_httpStatus = Lens.lens (\StartReportJobResponse' {httpStatus} -> httpStatus) (\s@StartReportJobResponse' {} a -> s {httpStatus = a} :: StartReportJobResponse)

instance Prelude.NFData StartReportJobResponse where
  rnf StartReportJobResponse' {..} =
    Prelude.rnf reportJobId
      `Prelude.seq` Prelude.rnf httpStatus
