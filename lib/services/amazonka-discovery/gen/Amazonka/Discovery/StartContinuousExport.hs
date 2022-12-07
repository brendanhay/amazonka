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
-- Copyright   : (c) 2013-2022 Brendan Hay
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
    startContinuousExportResponse_s3Bucket,
    startContinuousExportResponse_schemaStorageConfig,
    startContinuousExportResponse_dataSource,
    startContinuousExportResponse_exportId,
    startContinuousExportResponse_startTime,
    startContinuousExportResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Discovery.Types
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
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          StartContinuousExportResponse'
            Prelude.<$> (x Data..?> "s3Bucket")
            Prelude.<*> ( x Data..?> "schemaStorageConfig"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "dataSource")
            Prelude.<*> (x Data..?> "exportId")
            Prelude.<*> (x Data..?> "startTime")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StartContinuousExport where
  hashWithSalt _salt _ =
    _salt `Prelude.hashWithSalt` ()

instance Prelude.NFData StartContinuousExport where
  rnf _ = ()

instance Data.ToHeaders StartContinuousExport where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSPoseidonService_V2015_11_01.StartContinuousExport" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON StartContinuousExport where
  toJSON = Prelude.const (Data.Object Prelude.mempty)

instance Data.ToPath StartContinuousExport where
  toPath = Prelude.const "/"

instance Data.ToQuery StartContinuousExport where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartContinuousExportResponse' smart constructor.
data StartContinuousExportResponse = StartContinuousExportResponse'
  { -- | The name of the s3 bucket where the export data parquet files are
    -- stored.
    s3Bucket :: Prelude.Maybe Prelude.Text,
    -- | A dictionary which describes how the data is stored.
    --
    -- -   @databaseName@ - the name of the Glue database used to store the
    --     schema.
    schemaStorageConfig :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The type of data collector used to gather this data (currently only
    -- offered for AGENT).
    dataSource :: Prelude.Maybe DataSource,
    -- | The unique ID assigned to this export.
    exportId :: Prelude.Maybe Prelude.Text,
    -- | The timestamp representing when the continuous export was started.
    startTime :: Prelude.Maybe Data.POSIX,
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
-- 's3Bucket', 'startContinuousExportResponse_s3Bucket' - The name of the s3 bucket where the export data parquet files are
-- stored.
--
-- 'schemaStorageConfig', 'startContinuousExportResponse_schemaStorageConfig' - A dictionary which describes how the data is stored.
--
-- -   @databaseName@ - the name of the Glue database used to store the
--     schema.
--
-- 'dataSource', 'startContinuousExportResponse_dataSource' - The type of data collector used to gather this data (currently only
-- offered for AGENT).
--
-- 'exportId', 'startContinuousExportResponse_exportId' - The unique ID assigned to this export.
--
-- 'startTime', 'startContinuousExportResponse_startTime' - The timestamp representing when the continuous export was started.
--
-- 'httpStatus', 'startContinuousExportResponse_httpStatus' - The response's http status code.
newStartContinuousExportResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StartContinuousExportResponse
newStartContinuousExportResponse pHttpStatus_ =
  StartContinuousExportResponse'
    { s3Bucket =
        Prelude.Nothing,
      schemaStorageConfig = Prelude.Nothing,
      dataSource = Prelude.Nothing,
      exportId = Prelude.Nothing,
      startTime = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The name of the s3 bucket where the export data parquet files are
-- stored.
startContinuousExportResponse_s3Bucket :: Lens.Lens' StartContinuousExportResponse (Prelude.Maybe Prelude.Text)
startContinuousExportResponse_s3Bucket = Lens.lens (\StartContinuousExportResponse' {s3Bucket} -> s3Bucket) (\s@StartContinuousExportResponse' {} a -> s {s3Bucket = a} :: StartContinuousExportResponse)

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

-- | The unique ID assigned to this export.
startContinuousExportResponse_exportId :: Lens.Lens' StartContinuousExportResponse (Prelude.Maybe Prelude.Text)
startContinuousExportResponse_exportId = Lens.lens (\StartContinuousExportResponse' {exportId} -> exportId) (\s@StartContinuousExportResponse' {} a -> s {exportId = a} :: StartContinuousExportResponse)

-- | The timestamp representing when the continuous export was started.
startContinuousExportResponse_startTime :: Lens.Lens' StartContinuousExportResponse (Prelude.Maybe Prelude.UTCTime)
startContinuousExportResponse_startTime = Lens.lens (\StartContinuousExportResponse' {startTime} -> startTime) (\s@StartContinuousExportResponse' {} a -> s {startTime = a} :: StartContinuousExportResponse) Prelude.. Lens.mapping Data._Time

-- | The response's http status code.
startContinuousExportResponse_httpStatus :: Lens.Lens' StartContinuousExportResponse Prelude.Int
startContinuousExportResponse_httpStatus = Lens.lens (\StartContinuousExportResponse' {httpStatus} -> httpStatus) (\s@StartContinuousExportResponse' {} a -> s {httpStatus = a} :: StartContinuousExportResponse)

instance Prelude.NFData StartContinuousExportResponse where
  rnf StartContinuousExportResponse' {..} =
    Prelude.rnf s3Bucket
      `Prelude.seq` Prelude.rnf schemaStorageConfig
      `Prelude.seq` Prelude.rnf dataSource
      `Prelude.seq` Prelude.rnf exportId
      `Prelude.seq` Prelude.rnf startTime
      `Prelude.seq` Prelude.rnf httpStatus
