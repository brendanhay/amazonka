{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.Discovery.StartContinuousExport
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Start the continuous flow of agent\'s discovered data into Amazon
-- Athena.
module Network.AWS.Discovery.StartContinuousExport
  ( -- * Creating a Request
    StartContinuousExport (..),
    newStartContinuousExport,

    -- * Destructuring the Response
    StartContinuousExportResponse (..),
    newStartContinuousExportResponse,

    -- * Response Lenses
    startContinuousExportResponse_s3Bucket,
    startContinuousExportResponse_dataSource,
    startContinuousExportResponse_startTime,
    startContinuousExportResponse_schemaStorageConfig,
    startContinuousExportResponse_exportId,
    startContinuousExportResponse_httpStatus,
  )
where

import Network.AWS.Discovery.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newStartContinuousExport' smart constructor.
data StartContinuousExport = StartContinuousExport'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'StartContinuousExport' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newStartContinuousExport ::
  StartContinuousExport
newStartContinuousExport = StartContinuousExport'

instance Prelude.AWSRequest StartContinuousExport where
  type
    Rs StartContinuousExport =
      StartContinuousExportResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          StartContinuousExportResponse'
            Prelude.<$> (x Prelude..?> "s3Bucket")
            Prelude.<*> (x Prelude..?> "dataSource")
            Prelude.<*> (x Prelude..?> "startTime")
            Prelude.<*> ( x Prelude..?> "schemaStorageConfig"
                            Prelude..!@ Prelude.mempty
                        )
            Prelude.<*> (x Prelude..?> "exportId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StartContinuousExport

instance Prelude.NFData StartContinuousExport

instance Prelude.ToHeaders StartContinuousExport where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AWSPoseidonService_V2015_11_01.StartContinuousExport" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON StartContinuousExport where
  toJSON =
    Prelude.const (Prelude.Object Prelude.mempty)

instance Prelude.ToPath StartContinuousExport where
  toPath = Prelude.const "/"

instance Prelude.ToQuery StartContinuousExport where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartContinuousExportResponse' smart constructor.
data StartContinuousExportResponse = StartContinuousExportResponse'
  { -- | The name of the s3 bucket where the export data parquet files are
    -- stored.
    s3Bucket :: Prelude.Maybe Prelude.Text,
    -- | The type of data collector used to gather this data (currently only
    -- offered for AGENT).
    dataSource :: Prelude.Maybe DataSource,
    -- | The timestamp representing when the continuous export was started.
    startTime :: Prelude.Maybe Prelude.POSIX,
    -- | A dictionary which describes how the data is stored.
    --
    -- -   @databaseName@ - the name of the Glue database used to store the
    --     schema.
    schemaStorageConfig :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The unique ID assigned to this export.
    exportId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
-- 'dataSource', 'startContinuousExportResponse_dataSource' - The type of data collector used to gather this data (currently only
-- offered for AGENT).
--
-- 'startTime', 'startContinuousExportResponse_startTime' - The timestamp representing when the continuous export was started.
--
-- 'schemaStorageConfig', 'startContinuousExportResponse_schemaStorageConfig' - A dictionary which describes how the data is stored.
--
-- -   @databaseName@ - the name of the Glue database used to store the
--     schema.
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
    { s3Bucket =
        Prelude.Nothing,
      dataSource = Prelude.Nothing,
      startTime = Prelude.Nothing,
      schemaStorageConfig = Prelude.Nothing,
      exportId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The name of the s3 bucket where the export data parquet files are
-- stored.
startContinuousExportResponse_s3Bucket :: Lens.Lens' StartContinuousExportResponse (Prelude.Maybe Prelude.Text)
startContinuousExportResponse_s3Bucket = Lens.lens (\StartContinuousExportResponse' {s3Bucket} -> s3Bucket) (\s@StartContinuousExportResponse' {} a -> s {s3Bucket = a} :: StartContinuousExportResponse)

-- | The type of data collector used to gather this data (currently only
-- offered for AGENT).
startContinuousExportResponse_dataSource :: Lens.Lens' StartContinuousExportResponse (Prelude.Maybe DataSource)
startContinuousExportResponse_dataSource = Lens.lens (\StartContinuousExportResponse' {dataSource} -> dataSource) (\s@StartContinuousExportResponse' {} a -> s {dataSource = a} :: StartContinuousExportResponse)

-- | The timestamp representing when the continuous export was started.
startContinuousExportResponse_startTime :: Lens.Lens' StartContinuousExportResponse (Prelude.Maybe Prelude.UTCTime)
startContinuousExportResponse_startTime = Lens.lens (\StartContinuousExportResponse' {startTime} -> startTime) (\s@StartContinuousExportResponse' {} a -> s {startTime = a} :: StartContinuousExportResponse) Prelude.. Lens.mapping Prelude._Time

-- | A dictionary which describes how the data is stored.
--
-- -   @databaseName@ - the name of the Glue database used to store the
--     schema.
startContinuousExportResponse_schemaStorageConfig :: Lens.Lens' StartContinuousExportResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
startContinuousExportResponse_schemaStorageConfig = Lens.lens (\StartContinuousExportResponse' {schemaStorageConfig} -> schemaStorageConfig) (\s@StartContinuousExportResponse' {} a -> s {schemaStorageConfig = a} :: StartContinuousExportResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The unique ID assigned to this export.
startContinuousExportResponse_exportId :: Lens.Lens' StartContinuousExportResponse (Prelude.Maybe Prelude.Text)
startContinuousExportResponse_exportId = Lens.lens (\StartContinuousExportResponse' {exportId} -> exportId) (\s@StartContinuousExportResponse' {} a -> s {exportId = a} :: StartContinuousExportResponse)

-- | The response's http status code.
startContinuousExportResponse_httpStatus :: Lens.Lens' StartContinuousExportResponse Prelude.Int
startContinuousExportResponse_httpStatus = Lens.lens (\StartContinuousExportResponse' {httpStatus} -> httpStatus) (\s@StartContinuousExportResponse' {} a -> s {httpStatus = a} :: StartContinuousExportResponse)

instance Prelude.NFData StartContinuousExportResponse
