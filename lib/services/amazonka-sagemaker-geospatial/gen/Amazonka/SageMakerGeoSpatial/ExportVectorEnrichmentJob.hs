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
-- Module      : Amazonka.SageMakerGeoSpatial.ExportVectorEnrichmentJob
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Use this operation to copy results of a Vector Enrichment job to an
-- Amazon S3 location.
module Amazonka.SageMakerGeoSpatial.ExportVectorEnrichmentJob
  ( -- * Creating a Request
    ExportVectorEnrichmentJob (..),
    newExportVectorEnrichmentJob,

    -- * Request Lenses
    exportVectorEnrichmentJob_clientToken,
    exportVectorEnrichmentJob_arn,
    exportVectorEnrichmentJob_executionRoleArn,
    exportVectorEnrichmentJob_outputConfig,

    -- * Destructuring the Response
    ExportVectorEnrichmentJobResponse (..),
    newExportVectorEnrichmentJobResponse,

    -- * Response Lenses
    exportVectorEnrichmentJobResponse_httpStatus,
    exportVectorEnrichmentJobResponse_arn,
    exportVectorEnrichmentJobResponse_creationTime,
    exportVectorEnrichmentJobResponse_executionRoleArn,
    exportVectorEnrichmentJobResponse_exportStatus,
    exportVectorEnrichmentJobResponse_outputConfig,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMakerGeoSpatial.Types

-- | /See:/ 'newExportVectorEnrichmentJob' smart constructor.
data ExportVectorEnrichmentJob = ExportVectorEnrichmentJob'
  { -- | A unique token that guarantees that the call to this API is idempotent.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the Vector Enrichment job.
    arn :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the IAM rolewith permission to upload
    -- to the location in OutputConfig.
    executionRoleArn :: Prelude.Text,
    -- | Output location information for exporting Vector Enrichment Job results.
    outputConfig :: ExportVectorEnrichmentJobOutputConfig
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ExportVectorEnrichmentJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'exportVectorEnrichmentJob_clientToken' - A unique token that guarantees that the call to this API is idempotent.
--
-- 'arn', 'exportVectorEnrichmentJob_arn' - The Amazon Resource Name (ARN) of the Vector Enrichment job.
--
-- 'executionRoleArn', 'exportVectorEnrichmentJob_executionRoleArn' - The Amazon Resource Name (ARN) of the IAM rolewith permission to upload
-- to the location in OutputConfig.
--
-- 'outputConfig', 'exportVectorEnrichmentJob_outputConfig' - Output location information for exporting Vector Enrichment Job results.
newExportVectorEnrichmentJob ::
  -- | 'arn'
  Prelude.Text ->
  -- | 'executionRoleArn'
  Prelude.Text ->
  -- | 'outputConfig'
  ExportVectorEnrichmentJobOutputConfig ->
  ExportVectorEnrichmentJob
newExportVectorEnrichmentJob
  pArn_
  pExecutionRoleArn_
  pOutputConfig_ =
    ExportVectorEnrichmentJob'
      { clientToken =
          Prelude.Nothing,
        arn = pArn_,
        executionRoleArn = pExecutionRoleArn_,
        outputConfig = pOutputConfig_
      }

-- | A unique token that guarantees that the call to this API is idempotent.
exportVectorEnrichmentJob_clientToken :: Lens.Lens' ExportVectorEnrichmentJob (Prelude.Maybe Prelude.Text)
exportVectorEnrichmentJob_clientToken = Lens.lens (\ExportVectorEnrichmentJob' {clientToken} -> clientToken) (\s@ExportVectorEnrichmentJob' {} a -> s {clientToken = a} :: ExportVectorEnrichmentJob)

-- | The Amazon Resource Name (ARN) of the Vector Enrichment job.
exportVectorEnrichmentJob_arn :: Lens.Lens' ExportVectorEnrichmentJob Prelude.Text
exportVectorEnrichmentJob_arn = Lens.lens (\ExportVectorEnrichmentJob' {arn} -> arn) (\s@ExportVectorEnrichmentJob' {} a -> s {arn = a} :: ExportVectorEnrichmentJob)

-- | The Amazon Resource Name (ARN) of the IAM rolewith permission to upload
-- to the location in OutputConfig.
exportVectorEnrichmentJob_executionRoleArn :: Lens.Lens' ExportVectorEnrichmentJob Prelude.Text
exportVectorEnrichmentJob_executionRoleArn = Lens.lens (\ExportVectorEnrichmentJob' {executionRoleArn} -> executionRoleArn) (\s@ExportVectorEnrichmentJob' {} a -> s {executionRoleArn = a} :: ExportVectorEnrichmentJob)

-- | Output location information for exporting Vector Enrichment Job results.
exportVectorEnrichmentJob_outputConfig :: Lens.Lens' ExportVectorEnrichmentJob ExportVectorEnrichmentJobOutputConfig
exportVectorEnrichmentJob_outputConfig = Lens.lens (\ExportVectorEnrichmentJob' {outputConfig} -> outputConfig) (\s@ExportVectorEnrichmentJob' {} a -> s {outputConfig = a} :: ExportVectorEnrichmentJob)

instance Core.AWSRequest ExportVectorEnrichmentJob where
  type
    AWSResponse ExportVectorEnrichmentJob =
      ExportVectorEnrichmentJobResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ExportVectorEnrichmentJobResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "Arn")
            Prelude.<*> (x Data..:> "CreationTime")
            Prelude.<*> (x Data..:> "ExecutionRoleArn")
            Prelude.<*> (x Data..:> "ExportStatus")
            Prelude.<*> (x Data..:> "OutputConfig")
      )

instance Prelude.Hashable ExportVectorEnrichmentJob where
  hashWithSalt _salt ExportVectorEnrichmentJob' {..} =
    _salt
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` executionRoleArn
      `Prelude.hashWithSalt` outputConfig

instance Prelude.NFData ExportVectorEnrichmentJob where
  rnf ExportVectorEnrichmentJob' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf executionRoleArn
      `Prelude.seq` Prelude.rnf outputConfig

instance Data.ToHeaders ExportVectorEnrichmentJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ExportVectorEnrichmentJob where
  toJSON ExportVectorEnrichmentJob' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ClientToken" Data..=) Prelude.<$> clientToken,
            Prelude.Just ("Arn" Data..= arn),
            Prelude.Just
              ("ExecutionRoleArn" Data..= executionRoleArn),
            Prelude.Just ("OutputConfig" Data..= outputConfig)
          ]
      )

instance Data.ToPath ExportVectorEnrichmentJob where
  toPath =
    Prelude.const "/export-vector-enrichment-jobs"

instance Data.ToQuery ExportVectorEnrichmentJob where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newExportVectorEnrichmentJobResponse' smart constructor.
data ExportVectorEnrichmentJobResponse = ExportVectorEnrichmentJobResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The Amazon Resource Name (ARN) of the Vector Enrichment job being
    -- exported.
    arn :: Prelude.Text,
    -- | The creation time.
    creationTime :: Data.ISO8601,
    -- | The Amazon Resource Name (ARN) of the IAM role with permission to upload
    -- to the location in OutputConfig.
    executionRoleArn :: Prelude.Text,
    -- | The status of the results the Vector Enrichment job being exported.
    exportStatus :: VectorEnrichmentJobExportStatus,
    -- | Output location information for exporting Vector Enrichment Job results.
    outputConfig :: ExportVectorEnrichmentJobOutputConfig
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ExportVectorEnrichmentJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'exportVectorEnrichmentJobResponse_httpStatus' - The response's http status code.
--
-- 'arn', 'exportVectorEnrichmentJobResponse_arn' - The Amazon Resource Name (ARN) of the Vector Enrichment job being
-- exported.
--
-- 'creationTime', 'exportVectorEnrichmentJobResponse_creationTime' - The creation time.
--
-- 'executionRoleArn', 'exportVectorEnrichmentJobResponse_executionRoleArn' - The Amazon Resource Name (ARN) of the IAM role with permission to upload
-- to the location in OutputConfig.
--
-- 'exportStatus', 'exportVectorEnrichmentJobResponse_exportStatus' - The status of the results the Vector Enrichment job being exported.
--
-- 'outputConfig', 'exportVectorEnrichmentJobResponse_outputConfig' - Output location information for exporting Vector Enrichment Job results.
newExportVectorEnrichmentJobResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'arn'
  Prelude.Text ->
  -- | 'creationTime'
  Prelude.UTCTime ->
  -- | 'executionRoleArn'
  Prelude.Text ->
  -- | 'exportStatus'
  VectorEnrichmentJobExportStatus ->
  -- | 'outputConfig'
  ExportVectorEnrichmentJobOutputConfig ->
  ExportVectorEnrichmentJobResponse
newExportVectorEnrichmentJobResponse
  pHttpStatus_
  pArn_
  pCreationTime_
  pExecutionRoleArn_
  pExportStatus_
  pOutputConfig_ =
    ExportVectorEnrichmentJobResponse'
      { httpStatus =
          pHttpStatus_,
        arn = pArn_,
        creationTime =
          Data._Time Lens.# pCreationTime_,
        executionRoleArn = pExecutionRoleArn_,
        exportStatus = pExportStatus_,
        outputConfig = pOutputConfig_
      }

-- | The response's http status code.
exportVectorEnrichmentJobResponse_httpStatus :: Lens.Lens' ExportVectorEnrichmentJobResponse Prelude.Int
exportVectorEnrichmentJobResponse_httpStatus = Lens.lens (\ExportVectorEnrichmentJobResponse' {httpStatus} -> httpStatus) (\s@ExportVectorEnrichmentJobResponse' {} a -> s {httpStatus = a} :: ExportVectorEnrichmentJobResponse)

-- | The Amazon Resource Name (ARN) of the Vector Enrichment job being
-- exported.
exportVectorEnrichmentJobResponse_arn :: Lens.Lens' ExportVectorEnrichmentJobResponse Prelude.Text
exportVectorEnrichmentJobResponse_arn = Lens.lens (\ExportVectorEnrichmentJobResponse' {arn} -> arn) (\s@ExportVectorEnrichmentJobResponse' {} a -> s {arn = a} :: ExportVectorEnrichmentJobResponse)

-- | The creation time.
exportVectorEnrichmentJobResponse_creationTime :: Lens.Lens' ExportVectorEnrichmentJobResponse Prelude.UTCTime
exportVectorEnrichmentJobResponse_creationTime = Lens.lens (\ExportVectorEnrichmentJobResponse' {creationTime} -> creationTime) (\s@ExportVectorEnrichmentJobResponse' {} a -> s {creationTime = a} :: ExportVectorEnrichmentJobResponse) Prelude.. Data._Time

-- | The Amazon Resource Name (ARN) of the IAM role with permission to upload
-- to the location in OutputConfig.
exportVectorEnrichmentJobResponse_executionRoleArn :: Lens.Lens' ExportVectorEnrichmentJobResponse Prelude.Text
exportVectorEnrichmentJobResponse_executionRoleArn = Lens.lens (\ExportVectorEnrichmentJobResponse' {executionRoleArn} -> executionRoleArn) (\s@ExportVectorEnrichmentJobResponse' {} a -> s {executionRoleArn = a} :: ExportVectorEnrichmentJobResponse)

-- | The status of the results the Vector Enrichment job being exported.
exportVectorEnrichmentJobResponse_exportStatus :: Lens.Lens' ExportVectorEnrichmentJobResponse VectorEnrichmentJobExportStatus
exportVectorEnrichmentJobResponse_exportStatus = Lens.lens (\ExportVectorEnrichmentJobResponse' {exportStatus} -> exportStatus) (\s@ExportVectorEnrichmentJobResponse' {} a -> s {exportStatus = a} :: ExportVectorEnrichmentJobResponse)

-- | Output location information for exporting Vector Enrichment Job results.
exportVectorEnrichmentJobResponse_outputConfig :: Lens.Lens' ExportVectorEnrichmentJobResponse ExportVectorEnrichmentJobOutputConfig
exportVectorEnrichmentJobResponse_outputConfig = Lens.lens (\ExportVectorEnrichmentJobResponse' {outputConfig} -> outputConfig) (\s@ExportVectorEnrichmentJobResponse' {} a -> s {outputConfig = a} :: ExportVectorEnrichmentJobResponse)

instance
  Prelude.NFData
    ExportVectorEnrichmentJobResponse
  where
  rnf ExportVectorEnrichmentJobResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf executionRoleArn
      `Prelude.seq` Prelude.rnf exportStatus
      `Prelude.seq` Prelude.rnf outputConfig
