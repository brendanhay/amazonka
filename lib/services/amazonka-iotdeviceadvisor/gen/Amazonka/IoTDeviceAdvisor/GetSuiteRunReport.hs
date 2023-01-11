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
-- Module      : Amazonka.IoTDeviceAdvisor.GetSuiteRunReport
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a report download link for a successful Device Advisor qualifying
-- test suite run.
--
-- Requires permission to access the
-- <https://docs.aws.amazon.com/service-authorization/latest/reference/list_awsiot.html#awsiot-actions-as-permissions GetSuiteRunReport>
-- action.
module Amazonka.IoTDeviceAdvisor.GetSuiteRunReport
  ( -- * Creating a Request
    GetSuiteRunReport (..),
    newGetSuiteRunReport,

    -- * Request Lenses
    getSuiteRunReport_suiteDefinitionId,
    getSuiteRunReport_suiteRunId,

    -- * Destructuring the Response
    GetSuiteRunReportResponse (..),
    newGetSuiteRunReportResponse,

    -- * Response Lenses
    getSuiteRunReportResponse_qualificationReportDownloadUrl,
    getSuiteRunReportResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTDeviceAdvisor.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetSuiteRunReport' smart constructor.
data GetSuiteRunReport = GetSuiteRunReport'
  { -- | Suite definition ID of the test suite.
    suiteDefinitionId :: Prelude.Text,
    -- | Suite run ID of the test suite run.
    suiteRunId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetSuiteRunReport' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'suiteDefinitionId', 'getSuiteRunReport_suiteDefinitionId' - Suite definition ID of the test suite.
--
-- 'suiteRunId', 'getSuiteRunReport_suiteRunId' - Suite run ID of the test suite run.
newGetSuiteRunReport ::
  -- | 'suiteDefinitionId'
  Prelude.Text ->
  -- | 'suiteRunId'
  Prelude.Text ->
  GetSuiteRunReport
newGetSuiteRunReport pSuiteDefinitionId_ pSuiteRunId_ =
  GetSuiteRunReport'
    { suiteDefinitionId =
        pSuiteDefinitionId_,
      suiteRunId = pSuiteRunId_
    }

-- | Suite definition ID of the test suite.
getSuiteRunReport_suiteDefinitionId :: Lens.Lens' GetSuiteRunReport Prelude.Text
getSuiteRunReport_suiteDefinitionId = Lens.lens (\GetSuiteRunReport' {suiteDefinitionId} -> suiteDefinitionId) (\s@GetSuiteRunReport' {} a -> s {suiteDefinitionId = a} :: GetSuiteRunReport)

-- | Suite run ID of the test suite run.
getSuiteRunReport_suiteRunId :: Lens.Lens' GetSuiteRunReport Prelude.Text
getSuiteRunReport_suiteRunId = Lens.lens (\GetSuiteRunReport' {suiteRunId} -> suiteRunId) (\s@GetSuiteRunReport' {} a -> s {suiteRunId = a} :: GetSuiteRunReport)

instance Core.AWSRequest GetSuiteRunReport where
  type
    AWSResponse GetSuiteRunReport =
      GetSuiteRunReportResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetSuiteRunReportResponse'
            Prelude.<$> (x Data..?> "qualificationReportDownloadUrl")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetSuiteRunReport where
  hashWithSalt _salt GetSuiteRunReport' {..} =
    _salt `Prelude.hashWithSalt` suiteDefinitionId
      `Prelude.hashWithSalt` suiteRunId

instance Prelude.NFData GetSuiteRunReport where
  rnf GetSuiteRunReport' {..} =
    Prelude.rnf suiteDefinitionId
      `Prelude.seq` Prelude.rnf suiteRunId

instance Data.ToHeaders GetSuiteRunReport where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetSuiteRunReport where
  toPath GetSuiteRunReport' {..} =
    Prelude.mconcat
      [ "/suiteDefinitions/",
        Data.toBS suiteDefinitionId,
        "/suiteRuns/",
        Data.toBS suiteRunId,
        "/report"
      ]

instance Data.ToQuery GetSuiteRunReport where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetSuiteRunReportResponse' smart constructor.
data GetSuiteRunReportResponse = GetSuiteRunReportResponse'
  { -- | Download URL of the qualification report.
    qualificationReportDownloadUrl :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetSuiteRunReportResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'qualificationReportDownloadUrl', 'getSuiteRunReportResponse_qualificationReportDownloadUrl' - Download URL of the qualification report.
--
-- 'httpStatus', 'getSuiteRunReportResponse_httpStatus' - The response's http status code.
newGetSuiteRunReportResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetSuiteRunReportResponse
newGetSuiteRunReportResponse pHttpStatus_ =
  GetSuiteRunReportResponse'
    { qualificationReportDownloadUrl =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Download URL of the qualification report.
getSuiteRunReportResponse_qualificationReportDownloadUrl :: Lens.Lens' GetSuiteRunReportResponse (Prelude.Maybe Prelude.Text)
getSuiteRunReportResponse_qualificationReportDownloadUrl = Lens.lens (\GetSuiteRunReportResponse' {qualificationReportDownloadUrl} -> qualificationReportDownloadUrl) (\s@GetSuiteRunReportResponse' {} a -> s {qualificationReportDownloadUrl = a} :: GetSuiteRunReportResponse)

-- | The response's http status code.
getSuiteRunReportResponse_httpStatus :: Lens.Lens' GetSuiteRunReportResponse Prelude.Int
getSuiteRunReportResponse_httpStatus = Lens.lens (\GetSuiteRunReportResponse' {httpStatus} -> httpStatus) (\s@GetSuiteRunReportResponse' {} a -> s {httpStatus = a} :: GetSuiteRunReportResponse)

instance Prelude.NFData GetSuiteRunReportResponse where
  rnf GetSuiteRunReportResponse' {..} =
    Prelude.rnf qualificationReportDownloadUrl
      `Prelude.seq` Prelude.rnf httpStatus
