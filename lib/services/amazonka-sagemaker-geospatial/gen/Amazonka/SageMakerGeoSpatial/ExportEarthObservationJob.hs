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
-- Module      : Amazonka.SageMakerGeoSpatial.ExportEarthObservationJob
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Use this operation to export results of an Earth Observation job and
-- optionally source images used as input to the EOJ to an S3 location.
module Amazonka.SageMakerGeoSpatial.ExportEarthObservationJob
  ( -- * Creating a Request
    ExportEarthObservationJob (..),
    newExportEarthObservationJob,

    -- * Request Lenses
    exportEarthObservationJob_exportSourceImages,
    exportEarthObservationJob_arn,
    exportEarthObservationJob_executionRoleArn,
    exportEarthObservationJob_outputConfig,

    -- * Destructuring the Response
    ExportEarthObservationJobResponse (..),
    newExportEarthObservationJobResponse,

    -- * Response Lenses
    exportEarthObservationJobResponse_exportSourceImages,
    exportEarthObservationJobResponse_httpStatus,
    exportEarthObservationJobResponse_arn,
    exportEarthObservationJobResponse_creationTime,
    exportEarthObservationJobResponse_executionRoleArn,
    exportEarthObservationJobResponse_exportStatus,
    exportEarthObservationJobResponse_outputConfig,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMakerGeoSpatial.Types

-- | /See:/ 'newExportEarthObservationJob' smart constructor.
data ExportEarthObservationJob = ExportEarthObservationJob'
  { -- | The source images provided to the Earth Observation job being exported.
    exportSourceImages :: Prelude.Maybe Prelude.Bool,
    -- | The input Amazon Resource Name (ARN) of the Earth Observation job being
    -- exported.
    arn :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the IAM role that you specified for
    -- the job.
    executionRoleArn :: Prelude.Text,
    -- | An object containing information about the output file.
    outputConfig :: OutputConfigInput
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ExportEarthObservationJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'exportSourceImages', 'exportEarthObservationJob_exportSourceImages' - The source images provided to the Earth Observation job being exported.
--
-- 'arn', 'exportEarthObservationJob_arn' - The input Amazon Resource Name (ARN) of the Earth Observation job being
-- exported.
--
-- 'executionRoleArn', 'exportEarthObservationJob_executionRoleArn' - The Amazon Resource Name (ARN) of the IAM role that you specified for
-- the job.
--
-- 'outputConfig', 'exportEarthObservationJob_outputConfig' - An object containing information about the output file.
newExportEarthObservationJob ::
  -- | 'arn'
  Prelude.Text ->
  -- | 'executionRoleArn'
  Prelude.Text ->
  -- | 'outputConfig'
  OutputConfigInput ->
  ExportEarthObservationJob
newExportEarthObservationJob
  pArn_
  pExecutionRoleArn_
  pOutputConfig_ =
    ExportEarthObservationJob'
      { exportSourceImages =
          Prelude.Nothing,
        arn = pArn_,
        executionRoleArn = pExecutionRoleArn_,
        outputConfig = pOutputConfig_
      }

-- | The source images provided to the Earth Observation job being exported.
exportEarthObservationJob_exportSourceImages :: Lens.Lens' ExportEarthObservationJob (Prelude.Maybe Prelude.Bool)
exportEarthObservationJob_exportSourceImages = Lens.lens (\ExportEarthObservationJob' {exportSourceImages} -> exportSourceImages) (\s@ExportEarthObservationJob' {} a -> s {exportSourceImages = a} :: ExportEarthObservationJob)

-- | The input Amazon Resource Name (ARN) of the Earth Observation job being
-- exported.
exportEarthObservationJob_arn :: Lens.Lens' ExportEarthObservationJob Prelude.Text
exportEarthObservationJob_arn = Lens.lens (\ExportEarthObservationJob' {arn} -> arn) (\s@ExportEarthObservationJob' {} a -> s {arn = a} :: ExportEarthObservationJob)

-- | The Amazon Resource Name (ARN) of the IAM role that you specified for
-- the job.
exportEarthObservationJob_executionRoleArn :: Lens.Lens' ExportEarthObservationJob Prelude.Text
exportEarthObservationJob_executionRoleArn = Lens.lens (\ExportEarthObservationJob' {executionRoleArn} -> executionRoleArn) (\s@ExportEarthObservationJob' {} a -> s {executionRoleArn = a} :: ExportEarthObservationJob)

-- | An object containing information about the output file.
exportEarthObservationJob_outputConfig :: Lens.Lens' ExportEarthObservationJob OutputConfigInput
exportEarthObservationJob_outputConfig = Lens.lens (\ExportEarthObservationJob' {outputConfig} -> outputConfig) (\s@ExportEarthObservationJob' {} a -> s {outputConfig = a} :: ExportEarthObservationJob)

instance Core.AWSRequest ExportEarthObservationJob where
  type
    AWSResponse ExportEarthObservationJob =
      ExportEarthObservationJobResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ExportEarthObservationJobResponse'
            Prelude.<$> (x Data..?> "ExportSourceImages")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "Arn")
            Prelude.<*> (x Data..:> "CreationTime")
            Prelude.<*> (x Data..:> "ExecutionRoleArn")
            Prelude.<*> (x Data..:> "ExportStatus")
            Prelude.<*> (x Data..:> "OutputConfig")
      )

instance Prelude.Hashable ExportEarthObservationJob where
  hashWithSalt _salt ExportEarthObservationJob' {..} =
    _salt
      `Prelude.hashWithSalt` exportSourceImages
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` executionRoleArn
      `Prelude.hashWithSalt` outputConfig

instance Prelude.NFData ExportEarthObservationJob where
  rnf ExportEarthObservationJob' {..} =
    Prelude.rnf exportSourceImages
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf executionRoleArn
      `Prelude.seq` Prelude.rnf outputConfig

instance Data.ToHeaders ExportEarthObservationJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ExportEarthObservationJob where
  toJSON ExportEarthObservationJob' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ExportSourceImages" Data..=)
              Prelude.<$> exportSourceImages,
            Prelude.Just ("Arn" Data..= arn),
            Prelude.Just
              ("ExecutionRoleArn" Data..= executionRoleArn),
            Prelude.Just ("OutputConfig" Data..= outputConfig)
          ]
      )

instance Data.ToPath ExportEarthObservationJob where
  toPath =
    Prelude.const "/export-earth-observation-job"

instance Data.ToQuery ExportEarthObservationJob where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newExportEarthObservationJobResponse' smart constructor.
data ExportEarthObservationJobResponse = ExportEarthObservationJobResponse'
  { -- | The source images provided to the Earth Observation job being exported.
    exportSourceImages :: Prelude.Maybe Prelude.Bool,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The output Amazon Resource Name (ARN) of the Earth Observation job being
    -- exported.
    arn :: Prelude.Text,
    -- | The creation time.
    creationTime :: Data.POSIX,
    -- | The Amazon Resource Name (ARN) of the IAM role that you specified for
    -- the job.
    executionRoleArn :: Prelude.Text,
    -- | The status of the results of the Earth Observation job being exported.
    exportStatus :: EarthObservationJobExportStatus,
    -- | An object containing information about the output file.
    outputConfig :: OutputConfigInput
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ExportEarthObservationJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'exportSourceImages', 'exportEarthObservationJobResponse_exportSourceImages' - The source images provided to the Earth Observation job being exported.
--
-- 'httpStatus', 'exportEarthObservationJobResponse_httpStatus' - The response's http status code.
--
-- 'arn', 'exportEarthObservationJobResponse_arn' - The output Amazon Resource Name (ARN) of the Earth Observation job being
-- exported.
--
-- 'creationTime', 'exportEarthObservationJobResponse_creationTime' - The creation time.
--
-- 'executionRoleArn', 'exportEarthObservationJobResponse_executionRoleArn' - The Amazon Resource Name (ARN) of the IAM role that you specified for
-- the job.
--
-- 'exportStatus', 'exportEarthObservationJobResponse_exportStatus' - The status of the results of the Earth Observation job being exported.
--
-- 'outputConfig', 'exportEarthObservationJobResponse_outputConfig' - An object containing information about the output file.
newExportEarthObservationJobResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'arn'
  Prelude.Text ->
  -- | 'creationTime'
  Prelude.UTCTime ->
  -- | 'executionRoleArn'
  Prelude.Text ->
  -- | 'exportStatus'
  EarthObservationJobExportStatus ->
  -- | 'outputConfig'
  OutputConfigInput ->
  ExportEarthObservationJobResponse
newExportEarthObservationJobResponse
  pHttpStatus_
  pArn_
  pCreationTime_
  pExecutionRoleArn_
  pExportStatus_
  pOutputConfig_ =
    ExportEarthObservationJobResponse'
      { exportSourceImages =
          Prelude.Nothing,
        httpStatus = pHttpStatus_,
        arn = pArn_,
        creationTime =
          Data._Time Lens.# pCreationTime_,
        executionRoleArn = pExecutionRoleArn_,
        exportStatus = pExportStatus_,
        outputConfig = pOutputConfig_
      }

-- | The source images provided to the Earth Observation job being exported.
exportEarthObservationJobResponse_exportSourceImages :: Lens.Lens' ExportEarthObservationJobResponse (Prelude.Maybe Prelude.Bool)
exportEarthObservationJobResponse_exportSourceImages = Lens.lens (\ExportEarthObservationJobResponse' {exportSourceImages} -> exportSourceImages) (\s@ExportEarthObservationJobResponse' {} a -> s {exportSourceImages = a} :: ExportEarthObservationJobResponse)

-- | The response's http status code.
exportEarthObservationJobResponse_httpStatus :: Lens.Lens' ExportEarthObservationJobResponse Prelude.Int
exportEarthObservationJobResponse_httpStatus = Lens.lens (\ExportEarthObservationJobResponse' {httpStatus} -> httpStatus) (\s@ExportEarthObservationJobResponse' {} a -> s {httpStatus = a} :: ExportEarthObservationJobResponse)

-- | The output Amazon Resource Name (ARN) of the Earth Observation job being
-- exported.
exportEarthObservationJobResponse_arn :: Lens.Lens' ExportEarthObservationJobResponse Prelude.Text
exportEarthObservationJobResponse_arn = Lens.lens (\ExportEarthObservationJobResponse' {arn} -> arn) (\s@ExportEarthObservationJobResponse' {} a -> s {arn = a} :: ExportEarthObservationJobResponse)

-- | The creation time.
exportEarthObservationJobResponse_creationTime :: Lens.Lens' ExportEarthObservationJobResponse Prelude.UTCTime
exportEarthObservationJobResponse_creationTime = Lens.lens (\ExportEarthObservationJobResponse' {creationTime} -> creationTime) (\s@ExportEarthObservationJobResponse' {} a -> s {creationTime = a} :: ExportEarthObservationJobResponse) Prelude.. Data._Time

-- | The Amazon Resource Name (ARN) of the IAM role that you specified for
-- the job.
exportEarthObservationJobResponse_executionRoleArn :: Lens.Lens' ExportEarthObservationJobResponse Prelude.Text
exportEarthObservationJobResponse_executionRoleArn = Lens.lens (\ExportEarthObservationJobResponse' {executionRoleArn} -> executionRoleArn) (\s@ExportEarthObservationJobResponse' {} a -> s {executionRoleArn = a} :: ExportEarthObservationJobResponse)

-- | The status of the results of the Earth Observation job being exported.
exportEarthObservationJobResponse_exportStatus :: Lens.Lens' ExportEarthObservationJobResponse EarthObservationJobExportStatus
exportEarthObservationJobResponse_exportStatus = Lens.lens (\ExportEarthObservationJobResponse' {exportStatus} -> exportStatus) (\s@ExportEarthObservationJobResponse' {} a -> s {exportStatus = a} :: ExportEarthObservationJobResponse)

-- | An object containing information about the output file.
exportEarthObservationJobResponse_outputConfig :: Lens.Lens' ExportEarthObservationJobResponse OutputConfigInput
exportEarthObservationJobResponse_outputConfig = Lens.lens (\ExportEarthObservationJobResponse' {outputConfig} -> outputConfig) (\s@ExportEarthObservationJobResponse' {} a -> s {outputConfig = a} :: ExportEarthObservationJobResponse)

instance
  Prelude.NFData
    ExportEarthObservationJobResponse
  where
  rnf ExportEarthObservationJobResponse' {..} =
    Prelude.rnf exportSourceImages
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf executionRoleArn
      `Prelude.seq` Prelude.rnf exportStatus
      `Prelude.seq` Prelude.rnf outputConfig
