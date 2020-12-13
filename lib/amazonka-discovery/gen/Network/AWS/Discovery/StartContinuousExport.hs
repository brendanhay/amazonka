{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Discovery.StartContinuousExport
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Start the continuous flow of agent's discovered data into Amazon Athena.
module Network.AWS.Discovery.StartContinuousExport
  ( -- * Creating a request
    StartContinuousExport (..),
    mkStartContinuousExport,

    -- * Destructuring the response
    StartContinuousExportResponse (..),
    mkStartContinuousExportResponse,

    -- ** Response lenses
    scersStartTime,
    scersSchemaStorageConfig,
    scersDataSource,
    scersS3Bucket,
    scersExportId,
    scersResponseStatus,
  )
where

import Network.AWS.Discovery.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkStartContinuousExport' smart constructor.
data StartContinuousExport = StartContinuousExport'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StartContinuousExport' with the minimum fields required to make a request.
mkStartContinuousExport ::
  StartContinuousExport
mkStartContinuousExport = StartContinuousExport'

instance Lude.AWSRequest StartContinuousExport where
  type Rs StartContinuousExport = StartContinuousExportResponse
  request = Req.postJSON discoveryService
  response =
    Res.receiveJSON
      ( \s h x ->
          StartContinuousExportResponse'
            Lude.<$> (x Lude..?> "startTime")
            Lude.<*> (x Lude..?> "schemaStorageConfig" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "dataSource")
            Lude.<*> (x Lude..?> "s3Bucket")
            Lude.<*> (x Lude..?> "exportId")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders StartContinuousExport where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSPoseidonService_V2015_11_01.StartContinuousExport" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON StartContinuousExport where
  toJSON = Lude.const (Lude.Object Lude.mempty)

instance Lude.ToPath StartContinuousExport where
  toPath = Lude.const "/"

instance Lude.ToQuery StartContinuousExport where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkStartContinuousExportResponse' smart constructor.
data StartContinuousExportResponse = StartContinuousExportResponse'
  { -- | The timestamp representing when the continuous export was started.
    startTime :: Lude.Maybe Lude.Timestamp,
    -- | A dictionary which describes how the data is stored.
    --
    --
    --     * @databaseName@ - the name of the Glue database used to store the schema.
    schemaStorageConfig :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    -- | The type of data collector used to gather this data (currently only offered for AGENT).
    dataSource :: Lude.Maybe DataSource,
    -- | The name of the s3 bucket where the export data parquet files are stored.
    s3Bucket :: Lude.Maybe Lude.Text,
    -- | The unique ID assigned to this export.
    exportId :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StartContinuousExportResponse' with the minimum fields required to make a request.
--
-- * 'startTime' - The timestamp representing when the continuous export was started.
-- * 'schemaStorageConfig' - A dictionary which describes how the data is stored.
--
--
--     * @databaseName@ - the name of the Glue database used to store the schema.
--
--
-- * 'dataSource' - The type of data collector used to gather this data (currently only offered for AGENT).
-- * 's3Bucket' - The name of the s3 bucket where the export data parquet files are stored.
-- * 'exportId' - The unique ID assigned to this export.
-- * 'responseStatus' - The response status code.
mkStartContinuousExportResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  StartContinuousExportResponse
mkStartContinuousExportResponse pResponseStatus_ =
  StartContinuousExportResponse'
    { startTime = Lude.Nothing,
      schemaStorageConfig = Lude.Nothing,
      dataSource = Lude.Nothing,
      s3Bucket = Lude.Nothing,
      exportId = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The timestamp representing when the continuous export was started.
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scersStartTime :: Lens.Lens' StartContinuousExportResponse (Lude.Maybe Lude.Timestamp)
scersStartTime = Lens.lens (startTime :: StartContinuousExportResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {startTime = a} :: StartContinuousExportResponse)
{-# DEPRECATED scersStartTime "Use generic-lens or generic-optics with 'startTime' instead." #-}

-- | A dictionary which describes how the data is stored.
--
--
--     * @databaseName@ - the name of the Glue database used to store the schema.
--
--
--
-- /Note:/ Consider using 'schemaStorageConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scersSchemaStorageConfig :: Lens.Lens' StartContinuousExportResponse (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
scersSchemaStorageConfig = Lens.lens (schemaStorageConfig :: StartContinuousExportResponse -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {schemaStorageConfig = a} :: StartContinuousExportResponse)
{-# DEPRECATED scersSchemaStorageConfig "Use generic-lens or generic-optics with 'schemaStorageConfig' instead." #-}

-- | The type of data collector used to gather this data (currently only offered for AGENT).
--
-- /Note:/ Consider using 'dataSource' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scersDataSource :: Lens.Lens' StartContinuousExportResponse (Lude.Maybe DataSource)
scersDataSource = Lens.lens (dataSource :: StartContinuousExportResponse -> Lude.Maybe DataSource) (\s a -> s {dataSource = a} :: StartContinuousExportResponse)
{-# DEPRECATED scersDataSource "Use generic-lens or generic-optics with 'dataSource' instead." #-}

-- | The name of the s3 bucket where the export data parquet files are stored.
--
-- /Note:/ Consider using 's3Bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scersS3Bucket :: Lens.Lens' StartContinuousExportResponse (Lude.Maybe Lude.Text)
scersS3Bucket = Lens.lens (s3Bucket :: StartContinuousExportResponse -> Lude.Maybe Lude.Text) (\s a -> s {s3Bucket = a} :: StartContinuousExportResponse)
{-# DEPRECATED scersS3Bucket "Use generic-lens or generic-optics with 's3Bucket' instead." #-}

-- | The unique ID assigned to this export.
--
-- /Note:/ Consider using 'exportId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scersExportId :: Lens.Lens' StartContinuousExportResponse (Lude.Maybe Lude.Text)
scersExportId = Lens.lens (exportId :: StartContinuousExportResponse -> Lude.Maybe Lude.Text) (\s a -> s {exportId = a} :: StartContinuousExportResponse)
{-# DEPRECATED scersExportId "Use generic-lens or generic-optics with 'exportId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scersResponseStatus :: Lens.Lens' StartContinuousExportResponse Lude.Int
scersResponseStatus = Lens.lens (responseStatus :: StartContinuousExportResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: StartContinuousExportResponse)
{-# DEPRECATED scersResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
