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
-- Module      : Amazonka.Inspector2.GetFindingsReportStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the status of a findings report.
module Amazonka.Inspector2.GetFindingsReportStatus
  ( -- * Creating a Request
    GetFindingsReportStatus (..),
    newGetFindingsReportStatus,

    -- * Request Lenses
    getFindingsReportStatus_reportId,

    -- * Destructuring the Response
    GetFindingsReportStatusResponse (..),
    newGetFindingsReportStatusResponse,

    -- * Response Lenses
    getFindingsReportStatusResponse_destination,
    getFindingsReportStatusResponse_errorMessage,
    getFindingsReportStatusResponse_status,
    getFindingsReportStatusResponse_filterCriteria,
    getFindingsReportStatusResponse_reportId,
    getFindingsReportStatusResponse_errorCode,
    getFindingsReportStatusResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Inspector2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetFindingsReportStatus' smart constructor.
data GetFindingsReportStatus = GetFindingsReportStatus'
  { -- | The ID of the report to retrieve the status of.
    reportId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetFindingsReportStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'reportId', 'getFindingsReportStatus_reportId' - The ID of the report to retrieve the status of.
newGetFindingsReportStatus ::
  GetFindingsReportStatus
newGetFindingsReportStatus =
  GetFindingsReportStatus'
    { reportId =
        Prelude.Nothing
    }

-- | The ID of the report to retrieve the status of.
getFindingsReportStatus_reportId :: Lens.Lens' GetFindingsReportStatus (Prelude.Maybe Prelude.Text)
getFindingsReportStatus_reportId = Lens.lens (\GetFindingsReportStatus' {reportId} -> reportId) (\s@GetFindingsReportStatus' {} a -> s {reportId = a} :: GetFindingsReportStatus)

instance Core.AWSRequest GetFindingsReportStatus where
  type
    AWSResponse GetFindingsReportStatus =
      GetFindingsReportStatusResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetFindingsReportStatusResponse'
            Prelude.<$> (x Data..?> "destination")
            Prelude.<*> (x Data..?> "errorMessage")
            Prelude.<*> (x Data..?> "status")
            Prelude.<*> (x Data..?> "filterCriteria")
            Prelude.<*> (x Data..?> "reportId")
            Prelude.<*> (x Data..?> "errorCode")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetFindingsReportStatus where
  hashWithSalt _salt GetFindingsReportStatus' {..} =
    _salt `Prelude.hashWithSalt` reportId

instance Prelude.NFData GetFindingsReportStatus where
  rnf GetFindingsReportStatus' {..} =
    Prelude.rnf reportId

instance Data.ToHeaders GetFindingsReportStatus where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetFindingsReportStatus where
  toJSON GetFindingsReportStatus' {..} =
    Data.object
      ( Prelude.catMaybes
          [("reportId" Data..=) Prelude.<$> reportId]
      )

instance Data.ToPath GetFindingsReportStatus where
  toPath = Prelude.const "/reporting/status/get"

instance Data.ToQuery GetFindingsReportStatus where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetFindingsReportStatusResponse' smart constructor.
data GetFindingsReportStatusResponse = GetFindingsReportStatusResponse'
  { -- | The destination of the report.
    destination :: Prelude.Maybe Destination,
    -- | The error message of the report.
    errorMessage :: Prelude.Maybe Prelude.Text,
    -- | The status of the report.
    status :: Prelude.Maybe ExternalReportStatus,
    -- | The filter criteria associated with the report.
    filterCriteria :: Prelude.Maybe FilterCriteria,
    -- | The ID of the report.
    reportId :: Prelude.Maybe Prelude.Text,
    -- | The error code of the report.
    errorCode :: Prelude.Maybe ReportingErrorCode,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetFindingsReportStatusResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'destination', 'getFindingsReportStatusResponse_destination' - The destination of the report.
--
-- 'errorMessage', 'getFindingsReportStatusResponse_errorMessage' - The error message of the report.
--
-- 'status', 'getFindingsReportStatusResponse_status' - The status of the report.
--
-- 'filterCriteria', 'getFindingsReportStatusResponse_filterCriteria' - The filter criteria associated with the report.
--
-- 'reportId', 'getFindingsReportStatusResponse_reportId' - The ID of the report.
--
-- 'errorCode', 'getFindingsReportStatusResponse_errorCode' - The error code of the report.
--
-- 'httpStatus', 'getFindingsReportStatusResponse_httpStatus' - The response's http status code.
newGetFindingsReportStatusResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetFindingsReportStatusResponse
newGetFindingsReportStatusResponse pHttpStatus_ =
  GetFindingsReportStatusResponse'
    { destination =
        Prelude.Nothing,
      errorMessage = Prelude.Nothing,
      status = Prelude.Nothing,
      filterCriteria = Prelude.Nothing,
      reportId = Prelude.Nothing,
      errorCode = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The destination of the report.
getFindingsReportStatusResponse_destination :: Lens.Lens' GetFindingsReportStatusResponse (Prelude.Maybe Destination)
getFindingsReportStatusResponse_destination = Lens.lens (\GetFindingsReportStatusResponse' {destination} -> destination) (\s@GetFindingsReportStatusResponse' {} a -> s {destination = a} :: GetFindingsReportStatusResponse)

-- | The error message of the report.
getFindingsReportStatusResponse_errorMessage :: Lens.Lens' GetFindingsReportStatusResponse (Prelude.Maybe Prelude.Text)
getFindingsReportStatusResponse_errorMessage = Lens.lens (\GetFindingsReportStatusResponse' {errorMessage} -> errorMessage) (\s@GetFindingsReportStatusResponse' {} a -> s {errorMessage = a} :: GetFindingsReportStatusResponse)

-- | The status of the report.
getFindingsReportStatusResponse_status :: Lens.Lens' GetFindingsReportStatusResponse (Prelude.Maybe ExternalReportStatus)
getFindingsReportStatusResponse_status = Lens.lens (\GetFindingsReportStatusResponse' {status} -> status) (\s@GetFindingsReportStatusResponse' {} a -> s {status = a} :: GetFindingsReportStatusResponse)

-- | The filter criteria associated with the report.
getFindingsReportStatusResponse_filterCriteria :: Lens.Lens' GetFindingsReportStatusResponse (Prelude.Maybe FilterCriteria)
getFindingsReportStatusResponse_filterCriteria = Lens.lens (\GetFindingsReportStatusResponse' {filterCriteria} -> filterCriteria) (\s@GetFindingsReportStatusResponse' {} a -> s {filterCriteria = a} :: GetFindingsReportStatusResponse)

-- | The ID of the report.
getFindingsReportStatusResponse_reportId :: Lens.Lens' GetFindingsReportStatusResponse (Prelude.Maybe Prelude.Text)
getFindingsReportStatusResponse_reportId = Lens.lens (\GetFindingsReportStatusResponse' {reportId} -> reportId) (\s@GetFindingsReportStatusResponse' {} a -> s {reportId = a} :: GetFindingsReportStatusResponse)

-- | The error code of the report.
getFindingsReportStatusResponse_errorCode :: Lens.Lens' GetFindingsReportStatusResponse (Prelude.Maybe ReportingErrorCode)
getFindingsReportStatusResponse_errorCode = Lens.lens (\GetFindingsReportStatusResponse' {errorCode} -> errorCode) (\s@GetFindingsReportStatusResponse' {} a -> s {errorCode = a} :: GetFindingsReportStatusResponse)

-- | The response's http status code.
getFindingsReportStatusResponse_httpStatus :: Lens.Lens' GetFindingsReportStatusResponse Prelude.Int
getFindingsReportStatusResponse_httpStatus = Lens.lens (\GetFindingsReportStatusResponse' {httpStatus} -> httpStatus) (\s@GetFindingsReportStatusResponse' {} a -> s {httpStatus = a} :: GetFindingsReportStatusResponse)

instance
  Prelude.NFData
    GetFindingsReportStatusResponse
  where
  rnf GetFindingsReportStatusResponse' {..} =
    Prelude.rnf destination
      `Prelude.seq` Prelude.rnf errorMessage
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf filterCriteria
      `Prelude.seq` Prelude.rnf reportId
      `Prelude.seq` Prelude.rnf errorCode
      `Prelude.seq` Prelude.rnf httpStatus
