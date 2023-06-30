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
-- Module      : Amazonka.Inspector2.CreateFindingsReport
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a finding report.
module Amazonka.Inspector2.CreateFindingsReport
  ( -- * Creating a Request
    CreateFindingsReport (..),
    newCreateFindingsReport,

    -- * Request Lenses
    createFindingsReport_filterCriteria,
    createFindingsReport_reportFormat,
    createFindingsReport_s3Destination,

    -- * Destructuring the Response
    CreateFindingsReportResponse (..),
    newCreateFindingsReportResponse,

    -- * Response Lenses
    createFindingsReportResponse_reportId,
    createFindingsReportResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Inspector2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateFindingsReport' smart constructor.
data CreateFindingsReport = CreateFindingsReport'
  { -- | The filter criteria to apply to the results of the finding report.
    filterCriteria :: Prelude.Maybe FilterCriteria,
    -- | The format to generate the report in.
    reportFormat :: ReportFormat,
    -- | The Amazon S3 export destination for the report.
    s3Destination :: Destination
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateFindingsReport' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filterCriteria', 'createFindingsReport_filterCriteria' - The filter criteria to apply to the results of the finding report.
--
-- 'reportFormat', 'createFindingsReport_reportFormat' - The format to generate the report in.
--
-- 's3Destination', 'createFindingsReport_s3Destination' - The Amazon S3 export destination for the report.
newCreateFindingsReport ::
  -- | 'reportFormat'
  ReportFormat ->
  -- | 's3Destination'
  Destination ->
  CreateFindingsReport
newCreateFindingsReport
  pReportFormat_
  pS3Destination_ =
    CreateFindingsReport'
      { filterCriteria =
          Prelude.Nothing,
        reportFormat = pReportFormat_,
        s3Destination = pS3Destination_
      }

-- | The filter criteria to apply to the results of the finding report.
createFindingsReport_filterCriteria :: Lens.Lens' CreateFindingsReport (Prelude.Maybe FilterCriteria)
createFindingsReport_filterCriteria = Lens.lens (\CreateFindingsReport' {filterCriteria} -> filterCriteria) (\s@CreateFindingsReport' {} a -> s {filterCriteria = a} :: CreateFindingsReport)

-- | The format to generate the report in.
createFindingsReport_reportFormat :: Lens.Lens' CreateFindingsReport ReportFormat
createFindingsReport_reportFormat = Lens.lens (\CreateFindingsReport' {reportFormat} -> reportFormat) (\s@CreateFindingsReport' {} a -> s {reportFormat = a} :: CreateFindingsReport)

-- | The Amazon S3 export destination for the report.
createFindingsReport_s3Destination :: Lens.Lens' CreateFindingsReport Destination
createFindingsReport_s3Destination = Lens.lens (\CreateFindingsReport' {s3Destination} -> s3Destination) (\s@CreateFindingsReport' {} a -> s {s3Destination = a} :: CreateFindingsReport)

instance Core.AWSRequest CreateFindingsReport where
  type
    AWSResponse CreateFindingsReport =
      CreateFindingsReportResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateFindingsReportResponse'
            Prelude.<$> (x Data..?> "reportId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateFindingsReport where
  hashWithSalt _salt CreateFindingsReport' {..} =
    _salt
      `Prelude.hashWithSalt` filterCriteria
      `Prelude.hashWithSalt` reportFormat
      `Prelude.hashWithSalt` s3Destination

instance Prelude.NFData CreateFindingsReport where
  rnf CreateFindingsReport' {..} =
    Prelude.rnf filterCriteria
      `Prelude.seq` Prelude.rnf reportFormat
      `Prelude.seq` Prelude.rnf s3Destination

instance Data.ToHeaders CreateFindingsReport where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateFindingsReport where
  toJSON CreateFindingsReport' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("filterCriteria" Data..=)
              Prelude.<$> filterCriteria,
            Prelude.Just ("reportFormat" Data..= reportFormat),
            Prelude.Just
              ("s3Destination" Data..= s3Destination)
          ]
      )

instance Data.ToPath CreateFindingsReport where
  toPath = Prelude.const "/reporting/create"

instance Data.ToQuery CreateFindingsReport where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateFindingsReportResponse' smart constructor.
data CreateFindingsReportResponse = CreateFindingsReportResponse'
  { -- | The ID of the report.
    reportId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateFindingsReportResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'reportId', 'createFindingsReportResponse_reportId' - The ID of the report.
--
-- 'httpStatus', 'createFindingsReportResponse_httpStatus' - The response's http status code.
newCreateFindingsReportResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateFindingsReportResponse
newCreateFindingsReportResponse pHttpStatus_ =
  CreateFindingsReportResponse'
    { reportId =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ID of the report.
createFindingsReportResponse_reportId :: Lens.Lens' CreateFindingsReportResponse (Prelude.Maybe Prelude.Text)
createFindingsReportResponse_reportId = Lens.lens (\CreateFindingsReportResponse' {reportId} -> reportId) (\s@CreateFindingsReportResponse' {} a -> s {reportId = a} :: CreateFindingsReportResponse)

-- | The response's http status code.
createFindingsReportResponse_httpStatus :: Lens.Lens' CreateFindingsReportResponse Prelude.Int
createFindingsReportResponse_httpStatus = Lens.lens (\CreateFindingsReportResponse' {httpStatus} -> httpStatus) (\s@CreateFindingsReportResponse' {} a -> s {httpStatus = a} :: CreateFindingsReportResponse)

instance Prelude.NFData CreateFindingsReportResponse where
  rnf CreateFindingsReportResponse' {..} =
    Prelude.rnf reportId
      `Prelude.seq` Prelude.rnf httpStatus
