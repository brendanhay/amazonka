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
-- Module      : Amazonka.ApplicationCostProfiler.PutReportDefinition
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates the report definition for a report in Application Cost Profiler.
module Amazonka.ApplicationCostProfiler.PutReportDefinition
  ( -- * Creating a Request
    PutReportDefinition (..),
    newPutReportDefinition,

    -- * Request Lenses
    putReportDefinition_reportId,
    putReportDefinition_reportDescription,
    putReportDefinition_reportFrequency,
    putReportDefinition_format,
    putReportDefinition_destinationS3Location,

    -- * Destructuring the Response
    PutReportDefinitionResponse (..),
    newPutReportDefinitionResponse,

    -- * Response Lenses
    putReportDefinitionResponse_reportId,
    putReportDefinitionResponse_httpStatus,
  )
where

import Amazonka.ApplicationCostProfiler.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newPutReportDefinition' smart constructor.
data PutReportDefinition = PutReportDefinition'
  { -- | Required. ID of the report. You can choose any valid string matching the
    -- pattern for the ID.
    reportId :: Prelude.Text,
    -- | Required. Description of the report.
    reportDescription :: Prelude.Text,
    -- | Required. The cadence to generate the report.
    reportFrequency :: ReportFrequency,
    -- | Required. The format to use for the generated report.
    format :: Format,
    -- | Required. Amazon Simple Storage Service (Amazon S3) location where
    -- Application Cost Profiler uploads the report.
    destinationS3Location :: S3Location
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutReportDefinition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'reportId', 'putReportDefinition_reportId' - Required. ID of the report. You can choose any valid string matching the
-- pattern for the ID.
--
-- 'reportDescription', 'putReportDefinition_reportDescription' - Required. Description of the report.
--
-- 'reportFrequency', 'putReportDefinition_reportFrequency' - Required. The cadence to generate the report.
--
-- 'format', 'putReportDefinition_format' - Required. The format to use for the generated report.
--
-- 'destinationS3Location', 'putReportDefinition_destinationS3Location' - Required. Amazon Simple Storage Service (Amazon S3) location where
-- Application Cost Profiler uploads the report.
newPutReportDefinition ::
  -- | 'reportId'
  Prelude.Text ->
  -- | 'reportDescription'
  Prelude.Text ->
  -- | 'reportFrequency'
  ReportFrequency ->
  -- | 'format'
  Format ->
  -- | 'destinationS3Location'
  S3Location ->
  PutReportDefinition
newPutReportDefinition
  pReportId_
  pReportDescription_
  pReportFrequency_
  pFormat_
  pDestinationS3Location_ =
    PutReportDefinition'
      { reportId = pReportId_,
        reportDescription = pReportDescription_,
        reportFrequency = pReportFrequency_,
        format = pFormat_,
        destinationS3Location = pDestinationS3Location_
      }

-- | Required. ID of the report. You can choose any valid string matching the
-- pattern for the ID.
putReportDefinition_reportId :: Lens.Lens' PutReportDefinition Prelude.Text
putReportDefinition_reportId = Lens.lens (\PutReportDefinition' {reportId} -> reportId) (\s@PutReportDefinition' {} a -> s {reportId = a} :: PutReportDefinition)

-- | Required. Description of the report.
putReportDefinition_reportDescription :: Lens.Lens' PutReportDefinition Prelude.Text
putReportDefinition_reportDescription = Lens.lens (\PutReportDefinition' {reportDescription} -> reportDescription) (\s@PutReportDefinition' {} a -> s {reportDescription = a} :: PutReportDefinition)

-- | Required. The cadence to generate the report.
putReportDefinition_reportFrequency :: Lens.Lens' PutReportDefinition ReportFrequency
putReportDefinition_reportFrequency = Lens.lens (\PutReportDefinition' {reportFrequency} -> reportFrequency) (\s@PutReportDefinition' {} a -> s {reportFrequency = a} :: PutReportDefinition)

-- | Required. The format to use for the generated report.
putReportDefinition_format :: Lens.Lens' PutReportDefinition Format
putReportDefinition_format = Lens.lens (\PutReportDefinition' {format} -> format) (\s@PutReportDefinition' {} a -> s {format = a} :: PutReportDefinition)

-- | Required. Amazon Simple Storage Service (Amazon S3) location where
-- Application Cost Profiler uploads the report.
putReportDefinition_destinationS3Location :: Lens.Lens' PutReportDefinition S3Location
putReportDefinition_destinationS3Location = Lens.lens (\PutReportDefinition' {destinationS3Location} -> destinationS3Location) (\s@PutReportDefinition' {} a -> s {destinationS3Location = a} :: PutReportDefinition)

instance Core.AWSRequest PutReportDefinition where
  type
    AWSResponse PutReportDefinition =
      PutReportDefinitionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          PutReportDefinitionResponse'
            Prelude.<$> (x Data..?> "reportId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable PutReportDefinition where
  hashWithSalt _salt PutReportDefinition' {..} =
    _salt `Prelude.hashWithSalt` reportId
      `Prelude.hashWithSalt` reportDescription
      `Prelude.hashWithSalt` reportFrequency
      `Prelude.hashWithSalt` format
      `Prelude.hashWithSalt` destinationS3Location

instance Prelude.NFData PutReportDefinition where
  rnf PutReportDefinition' {..} =
    Prelude.rnf reportId
      `Prelude.seq` Prelude.rnf reportDescription
      `Prelude.seq` Prelude.rnf reportFrequency
      `Prelude.seq` Prelude.rnf format
      `Prelude.seq` Prelude.rnf destinationS3Location

instance Data.ToHeaders PutReportDefinition where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON PutReportDefinition where
  toJSON PutReportDefinition' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("reportId" Data..= reportId),
            Prelude.Just
              ("reportDescription" Data..= reportDescription),
            Prelude.Just
              ("reportFrequency" Data..= reportFrequency),
            Prelude.Just ("format" Data..= format),
            Prelude.Just
              ( "destinationS3Location"
                  Data..= destinationS3Location
              )
          ]
      )

instance Data.ToPath PutReportDefinition where
  toPath = Prelude.const "/reportDefinition"

instance Data.ToQuery PutReportDefinition where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPutReportDefinitionResponse' smart constructor.
data PutReportDefinitionResponse = PutReportDefinitionResponse'
  { -- | ID of the report.
    reportId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutReportDefinitionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'reportId', 'putReportDefinitionResponse_reportId' - ID of the report.
--
-- 'httpStatus', 'putReportDefinitionResponse_httpStatus' - The response's http status code.
newPutReportDefinitionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  PutReportDefinitionResponse
newPutReportDefinitionResponse pHttpStatus_ =
  PutReportDefinitionResponse'
    { reportId =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | ID of the report.
putReportDefinitionResponse_reportId :: Lens.Lens' PutReportDefinitionResponse (Prelude.Maybe Prelude.Text)
putReportDefinitionResponse_reportId = Lens.lens (\PutReportDefinitionResponse' {reportId} -> reportId) (\s@PutReportDefinitionResponse' {} a -> s {reportId = a} :: PutReportDefinitionResponse)

-- | The response's http status code.
putReportDefinitionResponse_httpStatus :: Lens.Lens' PutReportDefinitionResponse Prelude.Int
putReportDefinitionResponse_httpStatus = Lens.lens (\PutReportDefinitionResponse' {httpStatus} -> httpStatus) (\s@PutReportDefinitionResponse' {} a -> s {httpStatus = a} :: PutReportDefinitionResponse)

instance Prelude.NFData PutReportDefinitionResponse where
  rnf PutReportDefinitionResponse' {..} =
    Prelude.rnf reportId
      `Prelude.seq` Prelude.rnf httpStatus
