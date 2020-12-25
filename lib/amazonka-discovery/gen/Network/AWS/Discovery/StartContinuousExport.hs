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
    scerrsDataSource,
    scerrsExportId,
    scerrsS3Bucket,
    scerrsSchemaStorageConfig,
    scerrsStartTime,
    scerrsResponseStatus,
  )
where

import qualified Network.AWS.Discovery.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkStartContinuousExport' smart constructor.
data StartContinuousExport = StartContinuousExport'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StartContinuousExport' value with any optional fields omitted.
mkStartContinuousExport ::
  StartContinuousExport
mkStartContinuousExport = StartContinuousExport'

instance Core.FromJSON StartContinuousExport where
  toJSON _ = Core.Object Core.mempty

instance Core.AWSRequest StartContinuousExport where
  type Rs StartContinuousExport = StartContinuousExportResponse
  request x@_ =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "AWSPoseidonService_V2015_11_01.StartContinuousExport"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          StartContinuousExportResponse'
            Core.<$> (x Core..:? "dataSource")
            Core.<*> (x Core..:? "exportId")
            Core.<*> (x Core..:? "s3Bucket")
            Core.<*> (x Core..:? "schemaStorageConfig")
            Core.<*> (x Core..:? "startTime")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkStartContinuousExportResponse' smart constructor.
data StartContinuousExportResponse = StartContinuousExportResponse'
  { -- | The type of data collector used to gather this data (currently only offered for AGENT).
    dataSource :: Core.Maybe Types.DataSource,
    -- | The unique ID assigned to this export.
    exportId :: Core.Maybe Types.ExportId,
    -- | The name of the s3 bucket where the export data parquet files are stored.
    s3Bucket :: Core.Maybe Types.S3Bucket,
    -- | A dictionary which describes how the data is stored.
    --
    --
    --     * @databaseName@ - the name of the Glue database used to store the schema.
    schemaStorageConfig :: Core.Maybe (Core.HashMap Types.DatabaseName Types.String),
    -- | The timestamp representing when the continuous export was started.
    startTime :: Core.Maybe Core.NominalDiffTime,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'StartContinuousExportResponse' value with any optional fields omitted.
mkStartContinuousExportResponse ::
  -- | 'responseStatus'
  Core.Int ->
  StartContinuousExportResponse
mkStartContinuousExportResponse responseStatus =
  StartContinuousExportResponse'
    { dataSource = Core.Nothing,
      exportId = Core.Nothing,
      s3Bucket = Core.Nothing,
      schemaStorageConfig = Core.Nothing,
      startTime = Core.Nothing,
      responseStatus
    }

-- | The type of data collector used to gather this data (currently only offered for AGENT).
--
-- /Note:/ Consider using 'dataSource' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scerrsDataSource :: Lens.Lens' StartContinuousExportResponse (Core.Maybe Types.DataSource)
scerrsDataSource = Lens.field @"dataSource"
{-# DEPRECATED scerrsDataSource "Use generic-lens or generic-optics with 'dataSource' instead." #-}

-- | The unique ID assigned to this export.
--
-- /Note:/ Consider using 'exportId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scerrsExportId :: Lens.Lens' StartContinuousExportResponse (Core.Maybe Types.ExportId)
scerrsExportId = Lens.field @"exportId"
{-# DEPRECATED scerrsExportId "Use generic-lens or generic-optics with 'exportId' instead." #-}

-- | The name of the s3 bucket where the export data parquet files are stored.
--
-- /Note:/ Consider using 's3Bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scerrsS3Bucket :: Lens.Lens' StartContinuousExportResponse (Core.Maybe Types.S3Bucket)
scerrsS3Bucket = Lens.field @"s3Bucket"
{-# DEPRECATED scerrsS3Bucket "Use generic-lens or generic-optics with 's3Bucket' instead." #-}

-- | A dictionary which describes how the data is stored.
--
--
--     * @databaseName@ - the name of the Glue database used to store the schema.
--
--
--
-- /Note:/ Consider using 'schemaStorageConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scerrsSchemaStorageConfig :: Lens.Lens' StartContinuousExportResponse (Core.Maybe (Core.HashMap Types.DatabaseName Types.String))
scerrsSchemaStorageConfig = Lens.field @"schemaStorageConfig"
{-# DEPRECATED scerrsSchemaStorageConfig "Use generic-lens or generic-optics with 'schemaStorageConfig' instead." #-}

-- | The timestamp representing when the continuous export was started.
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scerrsStartTime :: Lens.Lens' StartContinuousExportResponse (Core.Maybe Core.NominalDiffTime)
scerrsStartTime = Lens.field @"startTime"
{-# DEPRECATED scerrsStartTime "Use generic-lens or generic-optics with 'startTime' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scerrsResponseStatus :: Lens.Lens' StartContinuousExportResponse Core.Int
scerrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED scerrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
