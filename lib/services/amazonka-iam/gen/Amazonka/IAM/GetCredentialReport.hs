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
-- Module      : Amazonka.IAM.GetCredentialReport
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a credential report for the Amazon Web Services account. For
-- more information about the credential report, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/credential-reports.html Getting credential reports>
-- in the /IAM User Guide/.
module Amazonka.IAM.GetCredentialReport
  ( -- * Creating a Request
    GetCredentialReport (..),
    newGetCredentialReport,

    -- * Destructuring the Response
    GetCredentialReportResponse (..),
    newGetCredentialReportResponse,

    -- * Response Lenses
    getCredentialReportResponse_reportFormat,
    getCredentialReportResponse_content,
    getCredentialReportResponse_generatedTime,
    getCredentialReportResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IAM.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetCredentialReport' smart constructor.
data GetCredentialReport = GetCredentialReport'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetCredentialReport' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newGetCredentialReport ::
  GetCredentialReport
newGetCredentialReport = GetCredentialReport'

instance Core.AWSRequest GetCredentialReport where
  type
    AWSResponse GetCredentialReport =
      GetCredentialReportResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "GetCredentialReportResult"
      ( \s h x ->
          GetCredentialReportResponse'
            Prelude.<$> (x Data..@? "ReportFormat")
            Prelude.<*> (x Data..@? "Content")
            Prelude.<*> (x Data..@? "GeneratedTime")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetCredentialReport where
  hashWithSalt _salt _ =
    _salt `Prelude.hashWithSalt` ()

instance Prelude.NFData GetCredentialReport where
  rnf _ = ()

instance Data.ToHeaders GetCredentialReport where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath GetCredentialReport where
  toPath = Prelude.const "/"

instance Data.ToQuery GetCredentialReport where
  toQuery =
    Prelude.const
      ( Prelude.mconcat
          [ "Action"
              Data.=: ("GetCredentialReport" :: Prelude.ByteString),
            "Version"
              Data.=: ("2010-05-08" :: Prelude.ByteString)
          ]
      )

-- | Contains the response to a successful GetCredentialReport request.
--
-- /See:/ 'newGetCredentialReportResponse' smart constructor.
data GetCredentialReportResponse = GetCredentialReportResponse'
  { -- | The format (MIME type) of the credential report.
    reportFormat :: Prelude.Maybe ReportFormatType,
    -- | Contains the credential report. The report is Base64-encoded.
    content :: Prelude.Maybe Data.Base64,
    -- | The date and time when the credential report was created, in
    -- <http://www.iso.org/iso/iso8601 ISO 8601 date-time format>.
    generatedTime :: Prelude.Maybe Data.ISO8601,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetCredentialReportResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'reportFormat', 'getCredentialReportResponse_reportFormat' - The format (MIME type) of the credential report.
--
-- 'content', 'getCredentialReportResponse_content' - Contains the credential report. The report is Base64-encoded.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
--
-- 'generatedTime', 'getCredentialReportResponse_generatedTime' - The date and time when the credential report was created, in
-- <http://www.iso.org/iso/iso8601 ISO 8601 date-time format>.
--
-- 'httpStatus', 'getCredentialReportResponse_httpStatus' - The response's http status code.
newGetCredentialReportResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetCredentialReportResponse
newGetCredentialReportResponse pHttpStatus_ =
  GetCredentialReportResponse'
    { reportFormat =
        Prelude.Nothing,
      content = Prelude.Nothing,
      generatedTime = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The format (MIME type) of the credential report.
getCredentialReportResponse_reportFormat :: Lens.Lens' GetCredentialReportResponse (Prelude.Maybe ReportFormatType)
getCredentialReportResponse_reportFormat = Lens.lens (\GetCredentialReportResponse' {reportFormat} -> reportFormat) (\s@GetCredentialReportResponse' {} a -> s {reportFormat = a} :: GetCredentialReportResponse)

-- | Contains the credential report. The report is Base64-encoded.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
getCredentialReportResponse_content :: Lens.Lens' GetCredentialReportResponse (Prelude.Maybe Prelude.ByteString)
getCredentialReportResponse_content = Lens.lens (\GetCredentialReportResponse' {content} -> content) (\s@GetCredentialReportResponse' {} a -> s {content = a} :: GetCredentialReportResponse) Prelude.. Lens.mapping Data._Base64

-- | The date and time when the credential report was created, in
-- <http://www.iso.org/iso/iso8601 ISO 8601 date-time format>.
getCredentialReportResponse_generatedTime :: Lens.Lens' GetCredentialReportResponse (Prelude.Maybe Prelude.UTCTime)
getCredentialReportResponse_generatedTime = Lens.lens (\GetCredentialReportResponse' {generatedTime} -> generatedTime) (\s@GetCredentialReportResponse' {} a -> s {generatedTime = a} :: GetCredentialReportResponse) Prelude.. Lens.mapping Data._Time

-- | The response's http status code.
getCredentialReportResponse_httpStatus :: Lens.Lens' GetCredentialReportResponse Prelude.Int
getCredentialReportResponse_httpStatus = Lens.lens (\GetCredentialReportResponse' {httpStatus} -> httpStatus) (\s@GetCredentialReportResponse' {} a -> s {httpStatus = a} :: GetCredentialReportResponse)

instance Prelude.NFData GetCredentialReportResponse where
  rnf GetCredentialReportResponse' {..} =
    Prelude.rnf reportFormat
      `Prelude.seq` Prelude.rnf content
      `Prelude.seq` Prelude.rnf generatedTime
      `Prelude.seq` Prelude.rnf httpStatus
