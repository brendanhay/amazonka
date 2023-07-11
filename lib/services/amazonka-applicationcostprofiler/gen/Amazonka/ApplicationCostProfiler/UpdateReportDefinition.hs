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
-- Module      : Amazonka.ApplicationCostProfiler.UpdateReportDefinition
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates existing report in AWS Application Cost Profiler.
module Amazonka.ApplicationCostProfiler.UpdateReportDefinition
  ( -- * Creating a Request
    UpdateReportDefinition (..),
    newUpdateReportDefinition,

    -- * Request Lenses
    updateReportDefinition_reportId,
    updateReportDefinition_reportDescription,
    updateReportDefinition_reportFrequency,
    updateReportDefinition_format,
    updateReportDefinition_destinationS3Location,

    -- * Destructuring the Response
    UpdateReportDefinitionResponse (..),
    newUpdateReportDefinitionResponse,

    -- * Response Lenses
    updateReportDefinitionResponse_reportId,
    updateReportDefinitionResponse_httpStatus,
  )
where

import Amazonka.ApplicationCostProfiler.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateReportDefinition' smart constructor.
data UpdateReportDefinition = UpdateReportDefinition'
  { -- | Required. ID of the report to update.
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
-- Create a value of 'UpdateReportDefinition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'reportId', 'updateReportDefinition_reportId' - Required. ID of the report to update.
--
-- 'reportDescription', 'updateReportDefinition_reportDescription' - Required. Description of the report.
--
-- 'reportFrequency', 'updateReportDefinition_reportFrequency' - Required. The cadence to generate the report.
--
-- 'format', 'updateReportDefinition_format' - Required. The format to use for the generated report.
--
-- 'destinationS3Location', 'updateReportDefinition_destinationS3Location' - Required. Amazon Simple Storage Service (Amazon S3) location where
-- Application Cost Profiler uploads the report.
newUpdateReportDefinition ::
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
  UpdateReportDefinition
newUpdateReportDefinition
  pReportId_
  pReportDescription_
  pReportFrequency_
  pFormat_
  pDestinationS3Location_ =
    UpdateReportDefinition'
      { reportId = pReportId_,
        reportDescription = pReportDescription_,
        reportFrequency = pReportFrequency_,
        format = pFormat_,
        destinationS3Location = pDestinationS3Location_
      }

-- | Required. ID of the report to update.
updateReportDefinition_reportId :: Lens.Lens' UpdateReportDefinition Prelude.Text
updateReportDefinition_reportId = Lens.lens (\UpdateReportDefinition' {reportId} -> reportId) (\s@UpdateReportDefinition' {} a -> s {reportId = a} :: UpdateReportDefinition)

-- | Required. Description of the report.
updateReportDefinition_reportDescription :: Lens.Lens' UpdateReportDefinition Prelude.Text
updateReportDefinition_reportDescription = Lens.lens (\UpdateReportDefinition' {reportDescription} -> reportDescription) (\s@UpdateReportDefinition' {} a -> s {reportDescription = a} :: UpdateReportDefinition)

-- | Required. The cadence to generate the report.
updateReportDefinition_reportFrequency :: Lens.Lens' UpdateReportDefinition ReportFrequency
updateReportDefinition_reportFrequency = Lens.lens (\UpdateReportDefinition' {reportFrequency} -> reportFrequency) (\s@UpdateReportDefinition' {} a -> s {reportFrequency = a} :: UpdateReportDefinition)

-- | Required. The format to use for the generated report.
updateReportDefinition_format :: Lens.Lens' UpdateReportDefinition Format
updateReportDefinition_format = Lens.lens (\UpdateReportDefinition' {format} -> format) (\s@UpdateReportDefinition' {} a -> s {format = a} :: UpdateReportDefinition)

-- | Required. Amazon Simple Storage Service (Amazon S3) location where
-- Application Cost Profiler uploads the report.
updateReportDefinition_destinationS3Location :: Lens.Lens' UpdateReportDefinition S3Location
updateReportDefinition_destinationS3Location = Lens.lens (\UpdateReportDefinition' {destinationS3Location} -> destinationS3Location) (\s@UpdateReportDefinition' {} a -> s {destinationS3Location = a} :: UpdateReportDefinition)

instance Core.AWSRequest UpdateReportDefinition where
  type
    AWSResponse UpdateReportDefinition =
      UpdateReportDefinitionResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateReportDefinitionResponse'
            Prelude.<$> (x Data..?> "reportId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateReportDefinition where
  hashWithSalt _salt UpdateReportDefinition' {..} =
    _salt
      `Prelude.hashWithSalt` reportId
      `Prelude.hashWithSalt` reportDescription
      `Prelude.hashWithSalt` reportFrequency
      `Prelude.hashWithSalt` format
      `Prelude.hashWithSalt` destinationS3Location

instance Prelude.NFData UpdateReportDefinition where
  rnf UpdateReportDefinition' {..} =
    Prelude.rnf reportId
      `Prelude.seq` Prelude.rnf reportDescription
      `Prelude.seq` Prelude.rnf reportFrequency
      `Prelude.seq` Prelude.rnf format
      `Prelude.seq` Prelude.rnf destinationS3Location

instance Data.ToHeaders UpdateReportDefinition where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateReportDefinition where
  toJSON UpdateReportDefinition' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
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

instance Data.ToPath UpdateReportDefinition where
  toPath UpdateReportDefinition' {..} =
    Prelude.mconcat
      ["/reportDefinition/", Data.toBS reportId]

instance Data.ToQuery UpdateReportDefinition where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateReportDefinitionResponse' smart constructor.
data UpdateReportDefinitionResponse = UpdateReportDefinitionResponse'
  { -- | ID of the report.
    reportId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateReportDefinitionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'reportId', 'updateReportDefinitionResponse_reportId' - ID of the report.
--
-- 'httpStatus', 'updateReportDefinitionResponse_httpStatus' - The response's http status code.
newUpdateReportDefinitionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateReportDefinitionResponse
newUpdateReportDefinitionResponse pHttpStatus_ =
  UpdateReportDefinitionResponse'
    { reportId =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | ID of the report.
updateReportDefinitionResponse_reportId :: Lens.Lens' UpdateReportDefinitionResponse (Prelude.Maybe Prelude.Text)
updateReportDefinitionResponse_reportId = Lens.lens (\UpdateReportDefinitionResponse' {reportId} -> reportId) (\s@UpdateReportDefinitionResponse' {} a -> s {reportId = a} :: UpdateReportDefinitionResponse)

-- | The response's http status code.
updateReportDefinitionResponse_httpStatus :: Lens.Lens' UpdateReportDefinitionResponse Prelude.Int
updateReportDefinitionResponse_httpStatus = Lens.lens (\UpdateReportDefinitionResponse' {httpStatus} -> httpStatus) (\s@UpdateReportDefinitionResponse' {} a -> s {httpStatus = a} :: UpdateReportDefinitionResponse)

instance
  Prelude.NFData
    UpdateReportDefinitionResponse
  where
  rnf UpdateReportDefinitionResponse' {..} =
    Prelude.rnf reportId
      `Prelude.seq` Prelude.rnf httpStatus
