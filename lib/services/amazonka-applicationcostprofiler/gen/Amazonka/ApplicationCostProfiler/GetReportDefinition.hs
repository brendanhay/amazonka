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
-- Module      : Amazonka.ApplicationCostProfiler.GetReportDefinition
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the definition of a report already configured in AWS
-- Application Cost Profiler.
module Amazonka.ApplicationCostProfiler.GetReportDefinition
  ( -- * Creating a Request
    GetReportDefinition (..),
    newGetReportDefinition,

    -- * Request Lenses
    getReportDefinition_reportId,

    -- * Destructuring the Response
    GetReportDefinitionResponse (..),
    newGetReportDefinitionResponse,

    -- * Response Lenses
    getReportDefinitionResponse_httpStatus,
    getReportDefinitionResponse_reportId,
    getReportDefinitionResponse_reportDescription,
    getReportDefinitionResponse_reportFrequency,
    getReportDefinitionResponse_format,
    getReportDefinitionResponse_destinationS3Location,
    getReportDefinitionResponse_createdAt,
    getReportDefinitionResponse_lastUpdated,
  )
where

import Amazonka.ApplicationCostProfiler.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetReportDefinition' smart constructor.
data GetReportDefinition = GetReportDefinition'
  { -- | ID of the report to retrieve.
    reportId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetReportDefinition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'reportId', 'getReportDefinition_reportId' - ID of the report to retrieve.
newGetReportDefinition ::
  -- | 'reportId'
  Prelude.Text ->
  GetReportDefinition
newGetReportDefinition pReportId_ =
  GetReportDefinition' {reportId = pReportId_}

-- | ID of the report to retrieve.
getReportDefinition_reportId :: Lens.Lens' GetReportDefinition Prelude.Text
getReportDefinition_reportId = Lens.lens (\GetReportDefinition' {reportId} -> reportId) (\s@GetReportDefinition' {} a -> s {reportId = a} :: GetReportDefinition)

instance Core.AWSRequest GetReportDefinition where
  type
    AWSResponse GetReportDefinition =
      GetReportDefinitionResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetReportDefinitionResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..:> "reportId")
            Prelude.<*> (x Core..:> "reportDescription")
            Prelude.<*> (x Core..:> "reportFrequency")
            Prelude.<*> (x Core..:> "format")
            Prelude.<*> (x Core..:> "destinationS3Location")
            Prelude.<*> (x Core..:> "createdAt")
            Prelude.<*> (x Core..:> "lastUpdated")
      )

instance Prelude.Hashable GetReportDefinition where
  hashWithSalt _salt GetReportDefinition' {..} =
    _salt `Prelude.hashWithSalt` reportId

instance Prelude.NFData GetReportDefinition where
  rnf GetReportDefinition' {..} = Prelude.rnf reportId

instance Core.ToHeaders GetReportDefinition where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath GetReportDefinition where
  toPath GetReportDefinition' {..} =
    Prelude.mconcat
      ["/reportDefinition/", Core.toBS reportId]

instance Core.ToQuery GetReportDefinition where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetReportDefinitionResponse' smart constructor.
data GetReportDefinitionResponse = GetReportDefinitionResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | ID of the report retrieved.
    reportId :: Prelude.Text,
    -- | Description of the report.
    reportDescription :: Prelude.Text,
    -- | Cadence used to generate the report.
    reportFrequency :: ReportFrequency,
    -- | Format of the generated report.
    format :: Format,
    -- | Amazon Simple Storage Service (Amazon S3) location where the report is
    -- uploaded.
    destinationS3Location :: S3Location,
    -- | Timestamp (milliseconds) when this report definition was created.
    createdAt :: Core.POSIX,
    -- | Timestamp (milliseconds) when this report definition was last updated.
    lastUpdated :: Core.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetReportDefinitionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'getReportDefinitionResponse_httpStatus' - The response's http status code.
--
-- 'reportId', 'getReportDefinitionResponse_reportId' - ID of the report retrieved.
--
-- 'reportDescription', 'getReportDefinitionResponse_reportDescription' - Description of the report.
--
-- 'reportFrequency', 'getReportDefinitionResponse_reportFrequency' - Cadence used to generate the report.
--
-- 'format', 'getReportDefinitionResponse_format' - Format of the generated report.
--
-- 'destinationS3Location', 'getReportDefinitionResponse_destinationS3Location' - Amazon Simple Storage Service (Amazon S3) location where the report is
-- uploaded.
--
-- 'createdAt', 'getReportDefinitionResponse_createdAt' - Timestamp (milliseconds) when this report definition was created.
--
-- 'lastUpdated', 'getReportDefinitionResponse_lastUpdated' - Timestamp (milliseconds) when this report definition was last updated.
newGetReportDefinitionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
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
  -- | 'createdAt'
  Prelude.UTCTime ->
  -- | 'lastUpdated'
  Prelude.UTCTime ->
  GetReportDefinitionResponse
newGetReportDefinitionResponse
  pHttpStatus_
  pReportId_
  pReportDescription_
  pReportFrequency_
  pFormat_
  pDestinationS3Location_
  pCreatedAt_
  pLastUpdated_ =
    GetReportDefinitionResponse'
      { httpStatus =
          pHttpStatus_,
        reportId = pReportId_,
        reportDescription = pReportDescription_,
        reportFrequency = pReportFrequency_,
        format = pFormat_,
        destinationS3Location =
          pDestinationS3Location_,
        createdAt = Core._Time Lens.# pCreatedAt_,
        lastUpdated = Core._Time Lens.# pLastUpdated_
      }

-- | The response's http status code.
getReportDefinitionResponse_httpStatus :: Lens.Lens' GetReportDefinitionResponse Prelude.Int
getReportDefinitionResponse_httpStatus = Lens.lens (\GetReportDefinitionResponse' {httpStatus} -> httpStatus) (\s@GetReportDefinitionResponse' {} a -> s {httpStatus = a} :: GetReportDefinitionResponse)

-- | ID of the report retrieved.
getReportDefinitionResponse_reportId :: Lens.Lens' GetReportDefinitionResponse Prelude.Text
getReportDefinitionResponse_reportId = Lens.lens (\GetReportDefinitionResponse' {reportId} -> reportId) (\s@GetReportDefinitionResponse' {} a -> s {reportId = a} :: GetReportDefinitionResponse)

-- | Description of the report.
getReportDefinitionResponse_reportDescription :: Lens.Lens' GetReportDefinitionResponse Prelude.Text
getReportDefinitionResponse_reportDescription = Lens.lens (\GetReportDefinitionResponse' {reportDescription} -> reportDescription) (\s@GetReportDefinitionResponse' {} a -> s {reportDescription = a} :: GetReportDefinitionResponse)

-- | Cadence used to generate the report.
getReportDefinitionResponse_reportFrequency :: Lens.Lens' GetReportDefinitionResponse ReportFrequency
getReportDefinitionResponse_reportFrequency = Lens.lens (\GetReportDefinitionResponse' {reportFrequency} -> reportFrequency) (\s@GetReportDefinitionResponse' {} a -> s {reportFrequency = a} :: GetReportDefinitionResponse)

-- | Format of the generated report.
getReportDefinitionResponse_format :: Lens.Lens' GetReportDefinitionResponse Format
getReportDefinitionResponse_format = Lens.lens (\GetReportDefinitionResponse' {format} -> format) (\s@GetReportDefinitionResponse' {} a -> s {format = a} :: GetReportDefinitionResponse)

-- | Amazon Simple Storage Service (Amazon S3) location where the report is
-- uploaded.
getReportDefinitionResponse_destinationS3Location :: Lens.Lens' GetReportDefinitionResponse S3Location
getReportDefinitionResponse_destinationS3Location = Lens.lens (\GetReportDefinitionResponse' {destinationS3Location} -> destinationS3Location) (\s@GetReportDefinitionResponse' {} a -> s {destinationS3Location = a} :: GetReportDefinitionResponse)

-- | Timestamp (milliseconds) when this report definition was created.
getReportDefinitionResponse_createdAt :: Lens.Lens' GetReportDefinitionResponse Prelude.UTCTime
getReportDefinitionResponse_createdAt = Lens.lens (\GetReportDefinitionResponse' {createdAt} -> createdAt) (\s@GetReportDefinitionResponse' {} a -> s {createdAt = a} :: GetReportDefinitionResponse) Prelude.. Core._Time

-- | Timestamp (milliseconds) when this report definition was last updated.
getReportDefinitionResponse_lastUpdated :: Lens.Lens' GetReportDefinitionResponse Prelude.UTCTime
getReportDefinitionResponse_lastUpdated = Lens.lens (\GetReportDefinitionResponse' {lastUpdated} -> lastUpdated) (\s@GetReportDefinitionResponse' {} a -> s {lastUpdated = a} :: GetReportDefinitionResponse) Prelude.. Core._Time

instance Prelude.NFData GetReportDefinitionResponse where
  rnf GetReportDefinitionResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf reportId
      `Prelude.seq` Prelude.rnf reportDescription
      `Prelude.seq` Prelude.rnf reportFrequency
      `Prelude.seq` Prelude.rnf format
      `Prelude.seq` Prelude.rnf destinationS3Location
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf lastUpdated
