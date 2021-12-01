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
-- Module      : Amazonka.Discovery.StartContinuousExport
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Start the continuous flow of agent\'s discovered data into Amazon
-- Athena.
module Amazonka.Discovery.StartContinuousExport
  ( -- * Creating a Request
    StartContinuousExport (..),
    newStartContinuousExport,

    -- * Destructuring the Response
    StartContinuousExportResponse (..),
    newStartContinuousExportResponse,

    -- * Response Lenses
    startContinuousExportResponse_startTime,
    startContinuousExportResponse_schemaStorageConfig,
    startContinuousExportResponse_dataSource,
    startContinuousExportResponse_s3Bucket,
    startContinuousExportResponse_exportId,
    startContinuousExportResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import Amazonka.Discovery.Types
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStartContinuousExport' smart constructor.
data StartContinuousExport = StartContinuousExport'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartContinuousExport' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newStartContinuousExport ::
  StartContinuousExport
newStartContinuousExport = StartContinuousExport'

instance Core.AWSRequest StartContinuousExport where
  type
    AWSResponse StartContinuousExport =
      StartContinuousExportResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          StartContinuousExportResponse'
            Prelude.<$> (x Core..?> "startTime")
            Prelude.<*> ( x Core..?> "schemaStorageConfig"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Core..?> "dataSource")
            Prelude.<*> (x Core..?> "s3Bucket")
            Prelude.<*> (x Core..?> "exportId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StartContinuousExport where
  hashWithSalt salt' _ =
    salt' `Prelude.hashWithSalt` (0 :: Prelude.Int)

instance Prelude.NFData StartContinuousExport where
  rnf _ = ()

instance Core.ToHeaders StartContinuousExport where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSPoseidonService_V2015_11_01.StartContinuousExport" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON StartContinuousExport where
  toJSON = Prelude.const (Core.Object Prelude.mempty)

instance Core.ToPath StartContinuousExport where
  toPath = Prelude.const "/"

instance Core.ToQuery StartContinuousExport where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartContinuousExportResponse' smart constructor.
data StartContinuousExportResponse = StartContinuousExportResponse'
  { -- | The timestamp representing when the continuous export was started.
    startTime :: Prelude.Maybe Core.POSIX,
    -- | A dictionary which describes how the data is stored.
    --
    -- -   @databaseName@ - the name of the Glue database used to store the
    --     schema.
    schemaStorageConfig :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The type of data collector used to gather this data (currently only
    -- offered for AGENT).
    dataSource :: Prelude.Maybe DataSource,
    -- | The name of the s3 bucket where the export data parquet files are
    -- stored.
    s3Bucket :: Prelude.Maybe Prelude.Text,
    -- | The unique ID assigned to this export.
    exportId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartContinuousExportResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'startTime', 'startContinuousExportResponse_startTime' - The timestamp representing when the continuous export was started.
--
-- 'schemaStorageConfig', 'startContinuousExportResponse_schemaStorageConfig' - A dictionary which describes how the data is stored.
--
-- -   @databaseName@ - the name of the Glue database used to store the
--     schema.
--
-- 'dataSource', 'startContinuousExportResponse_dataSource' - The type of data collector used to gather this data (currently only
-- offered for AGENT).
--
-- 's3Bucket', 'startContinuousExportResponse_s3Bucket' - The name of the s3 bucket where the export data parquet files are
-- stored.
--
-- 'exportId', 'startContinuousExportResponse_exportId' - The unique ID assigned to this export.
--
-- 'httpStatus', 'startContinuousExportResponse_httpStatus' - The response's http status code.
newStartContinuousExportResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StartContinuousExportResponse
newStartContinuousExportResponse pHttpStatus_ =
  StartContinuousExportResponse'
    { startTime =
        Prelude.Nothing,
      schemaStorageConfig = Prelude.Nothing,
      dataSource = Prelude.Nothing,
      s3Bucket = Prelude.Nothing,
      exportId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The timestamp representing when the continuous export was started.
startContinuousExportResponse_startTime :: Lens.Lens' StartContinuousExportResponse (Prelude.Maybe Prelude.UTCTime)
startContinuousExportResponse_startTime = Lens.lens (\StartContinuousExportResponse' {startTime} -> startTime) (\s@StartContinuousExportResponse' {} a -> s {startTime = a} :: StartContinuousExportResponse) Prelude.. Lens.mapping Core._Time

-- | A dictionary which describes how the data is stored.
--
-- -   @databaseName@ - the name of the Glue database used to store the
--     schema.
startContinuousExportResponse_schemaStorageConfig :: Lens.Lens' StartContinuousExportResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
startContinuousExportResponse_schemaStorageConfig = Lens.lens (\StartContinuousExportResponse' {schemaStorageConfig} -> schemaStorageConfig) (\s@StartContinuousExportResponse' {} a -> s {schemaStorageConfig = a} :: StartContinuousExportResponse) Prelude.. Lens.mapping Lens.coerced

-- | The type of data collector used to gather this data (currently only
-- offered for AGENT).
startContinuousExportResponse_dataSource :: Lens.Lens' StartContinuousExportResponse (Prelude.Maybe DataSource)
startContinuousExportResponse_dataSource = Lens.lens (\StartContinuousExportResponse' {dataSource} -> dataSource) (\s@StartContinuousExportResponse' {} a -> s {dataSource = a} :: StartContinuousExportResponse)

-- | The name of the s3 bucket where the export data parquet files are
-- stored.
startContinuousExportResponse_s3Bucket :: Lens.Lens' StartContinuousExportResponse (Prelude.Maybe Prelude.Text)
startContinuousExportResponse_s3Bucket = Lens.lens (\StartContinuousExportResponse' {s3Bucket} -> s3Bucket) (\s@StartContinuousExportResponse' {} a -> s {s3Bucket = a} :: StartContinuousExportResponse)

-- | The unique ID assigned to this export.
startContinuousExportResponse_exportId :: Lens.Lens' StartContinuousExportResponse (Prelude.Maybe Prelude.Text)
startContinuousExportResponse_exportId = Lens.lens (\StartContinuousExportResponse' {exportId} -> exportId) (\s@StartContinuousExportResponse' {} a -> s {exportId = a} :: StartContinuousExportResponse)

-- | The response's http status code.
startContinuousExportResponse_httpStatus :: Lens.Lens' StartContinuousExportResponse Prelude.Int
startContinuousExportResponse_httpStatus = Lens.lens (\StartContinuousExportResponse' {httpStatus} -> httpStatus) (\s@StartContinuousExportResponse' {} a -> s {httpStatus = a} :: StartContinuousExportResponse)

instance Prelude.NFData StartContinuousExportResponse where
  rnf StartContinuousExportResponse' {..} =
    Prelude.rnf startTime
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf exportId
      `Prelude.seq` Prelude.rnf s3Bucket
      `Prelude.seq` Prelude.rnf dataSource
      `Prelude.seq` Prelude.rnf schemaStorageConfig
