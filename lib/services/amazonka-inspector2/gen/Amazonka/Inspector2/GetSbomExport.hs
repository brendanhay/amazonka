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
-- Module      : Amazonka.Inspector2.GetSbomExport
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets details of a software bill of materials (SBOM) report.
module Amazonka.Inspector2.GetSbomExport
  ( -- * Creating a Request
    GetSbomExport (..),
    newGetSbomExport,

    -- * Request Lenses
    getSbomExport_reportId,

    -- * Destructuring the Response
    GetSbomExportResponse (..),
    newGetSbomExportResponse,

    -- * Response Lenses
    getSbomExportResponse_errorCode,
    getSbomExportResponse_errorMessage,
    getSbomExportResponse_filterCriteria,
    getSbomExportResponse_format,
    getSbomExportResponse_reportId,
    getSbomExportResponse_s3Destination,
    getSbomExportResponse_status,
    getSbomExportResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Inspector2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetSbomExport' smart constructor.
data GetSbomExport = GetSbomExport'
  { -- | The report ID of the SBOM export to get details for.
    reportId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetSbomExport' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'reportId', 'getSbomExport_reportId' - The report ID of the SBOM export to get details for.
newGetSbomExport ::
  -- | 'reportId'
  Prelude.Text ->
  GetSbomExport
newGetSbomExport pReportId_ =
  GetSbomExport' {reportId = pReportId_}

-- | The report ID of the SBOM export to get details for.
getSbomExport_reportId :: Lens.Lens' GetSbomExport Prelude.Text
getSbomExport_reportId = Lens.lens (\GetSbomExport' {reportId} -> reportId) (\s@GetSbomExport' {} a -> s {reportId = a} :: GetSbomExport)

instance Core.AWSRequest GetSbomExport where
  type
    AWSResponse GetSbomExport =
      GetSbomExportResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetSbomExportResponse'
            Prelude.<$> (x Data..?> "errorCode")
            Prelude.<*> (x Data..?> "errorMessage")
            Prelude.<*> (x Data..?> "filterCriteria")
            Prelude.<*> (x Data..?> "format")
            Prelude.<*> (x Data..?> "reportId")
            Prelude.<*> (x Data..?> "s3Destination")
            Prelude.<*> (x Data..?> "status")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetSbomExport where
  hashWithSalt _salt GetSbomExport' {..} =
    _salt `Prelude.hashWithSalt` reportId

instance Prelude.NFData GetSbomExport where
  rnf GetSbomExport' {..} = Prelude.rnf reportId

instance Data.ToHeaders GetSbomExport where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetSbomExport where
  toJSON GetSbomExport' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("reportId" Data..= reportId)]
      )

instance Data.ToPath GetSbomExport where
  toPath = Prelude.const "/sbomexport/get"

instance Data.ToQuery GetSbomExport where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetSbomExportResponse' smart constructor.
data GetSbomExportResponse = GetSbomExportResponse'
  { -- | An error code.
    errorCode :: Prelude.Maybe ReportingErrorCode,
    -- | An error message.
    errorMessage :: Prelude.Maybe Prelude.Text,
    -- | Contains details about the resource filter criteria used for the
    -- software bill of materials (SBOM) report.
    filterCriteria :: Prelude.Maybe ResourceFilterCriteria,
    -- | The format of the software bill of materials (SBOM) report.
    format :: Prelude.Maybe SbomReportFormat,
    -- | The report ID of the software bill of materials (SBOM) report.
    reportId :: Prelude.Maybe Prelude.Text,
    s3Destination :: Prelude.Maybe Destination,
    -- | The status of the software bill of materials (SBOM) report.
    status :: Prelude.Maybe ExternalReportStatus,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetSbomExportResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'errorCode', 'getSbomExportResponse_errorCode' - An error code.
--
-- 'errorMessage', 'getSbomExportResponse_errorMessage' - An error message.
--
-- 'filterCriteria', 'getSbomExportResponse_filterCriteria' - Contains details about the resource filter criteria used for the
-- software bill of materials (SBOM) report.
--
-- 'format', 'getSbomExportResponse_format' - The format of the software bill of materials (SBOM) report.
--
-- 'reportId', 'getSbomExportResponse_reportId' - The report ID of the software bill of materials (SBOM) report.
--
-- 's3Destination', 'getSbomExportResponse_s3Destination' - Undocumented member.
--
-- 'status', 'getSbomExportResponse_status' - The status of the software bill of materials (SBOM) report.
--
-- 'httpStatus', 'getSbomExportResponse_httpStatus' - The response's http status code.
newGetSbomExportResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetSbomExportResponse
newGetSbomExportResponse pHttpStatus_ =
  GetSbomExportResponse'
    { errorCode = Prelude.Nothing,
      errorMessage = Prelude.Nothing,
      filterCriteria = Prelude.Nothing,
      format = Prelude.Nothing,
      reportId = Prelude.Nothing,
      s3Destination = Prelude.Nothing,
      status = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An error code.
getSbomExportResponse_errorCode :: Lens.Lens' GetSbomExportResponse (Prelude.Maybe ReportingErrorCode)
getSbomExportResponse_errorCode = Lens.lens (\GetSbomExportResponse' {errorCode} -> errorCode) (\s@GetSbomExportResponse' {} a -> s {errorCode = a} :: GetSbomExportResponse)

-- | An error message.
getSbomExportResponse_errorMessage :: Lens.Lens' GetSbomExportResponse (Prelude.Maybe Prelude.Text)
getSbomExportResponse_errorMessage = Lens.lens (\GetSbomExportResponse' {errorMessage} -> errorMessage) (\s@GetSbomExportResponse' {} a -> s {errorMessage = a} :: GetSbomExportResponse)

-- | Contains details about the resource filter criteria used for the
-- software bill of materials (SBOM) report.
getSbomExportResponse_filterCriteria :: Lens.Lens' GetSbomExportResponse (Prelude.Maybe ResourceFilterCriteria)
getSbomExportResponse_filterCriteria = Lens.lens (\GetSbomExportResponse' {filterCriteria} -> filterCriteria) (\s@GetSbomExportResponse' {} a -> s {filterCriteria = a} :: GetSbomExportResponse)

-- | The format of the software bill of materials (SBOM) report.
getSbomExportResponse_format :: Lens.Lens' GetSbomExportResponse (Prelude.Maybe SbomReportFormat)
getSbomExportResponse_format = Lens.lens (\GetSbomExportResponse' {format} -> format) (\s@GetSbomExportResponse' {} a -> s {format = a} :: GetSbomExportResponse)

-- | The report ID of the software bill of materials (SBOM) report.
getSbomExportResponse_reportId :: Lens.Lens' GetSbomExportResponse (Prelude.Maybe Prelude.Text)
getSbomExportResponse_reportId = Lens.lens (\GetSbomExportResponse' {reportId} -> reportId) (\s@GetSbomExportResponse' {} a -> s {reportId = a} :: GetSbomExportResponse)

-- | Undocumented member.
getSbomExportResponse_s3Destination :: Lens.Lens' GetSbomExportResponse (Prelude.Maybe Destination)
getSbomExportResponse_s3Destination = Lens.lens (\GetSbomExportResponse' {s3Destination} -> s3Destination) (\s@GetSbomExportResponse' {} a -> s {s3Destination = a} :: GetSbomExportResponse)

-- | The status of the software bill of materials (SBOM) report.
getSbomExportResponse_status :: Lens.Lens' GetSbomExportResponse (Prelude.Maybe ExternalReportStatus)
getSbomExportResponse_status = Lens.lens (\GetSbomExportResponse' {status} -> status) (\s@GetSbomExportResponse' {} a -> s {status = a} :: GetSbomExportResponse)

-- | The response's http status code.
getSbomExportResponse_httpStatus :: Lens.Lens' GetSbomExportResponse Prelude.Int
getSbomExportResponse_httpStatus = Lens.lens (\GetSbomExportResponse' {httpStatus} -> httpStatus) (\s@GetSbomExportResponse' {} a -> s {httpStatus = a} :: GetSbomExportResponse)

instance Prelude.NFData GetSbomExportResponse where
  rnf GetSbomExportResponse' {..} =
    Prelude.rnf errorCode
      `Prelude.seq` Prelude.rnf errorMessage
      `Prelude.seq` Prelude.rnf filterCriteria
      `Prelude.seq` Prelude.rnf format
      `Prelude.seq` Prelude.rnf reportId
      `Prelude.seq` Prelude.rnf s3Destination
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf httpStatus
