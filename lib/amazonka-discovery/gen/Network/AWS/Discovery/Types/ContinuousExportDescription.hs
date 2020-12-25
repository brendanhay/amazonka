{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Discovery.Types.ContinuousExportDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Discovery.Types.ContinuousExportDescription
  ( ContinuousExportDescription (..),

    -- * Smart constructor
    mkContinuousExportDescription,

    -- * Lenses
    cedDataSource,
    cedExportId,
    cedS3Bucket,
    cedSchemaStorageConfig,
    cedStartTime,
    cedStatus,
    cedStatusDetail,
    cedStopTime,
  )
where

import qualified Network.AWS.Discovery.Types.ContinuousExportStatus as Types
import qualified Network.AWS.Discovery.Types.DataSource as Types
import qualified Network.AWS.Discovery.Types.DatabaseName as Types
import qualified Network.AWS.Discovery.Types.ExportId as Types
import qualified Network.AWS.Discovery.Types.S3Bucket as Types
import qualified Network.AWS.Discovery.Types.String as Types
import qualified Network.AWS.Discovery.Types.StringMax255 as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A list of continuous export descriptions.
--
-- /See:/ 'mkContinuousExportDescription' smart constructor.
data ContinuousExportDescription = ContinuousExportDescription'
  { -- | The type of data collector used to gather this data (currently only offered for AGENT).
    dataSource :: Core.Maybe Types.DataSource,
    -- | The unique ID assigned to this export.
    exportId :: Core.Maybe Types.ExportId,
    -- | The name of the s3 bucket where the export data parquet files are stored.
    s3Bucket :: Core.Maybe Types.S3Bucket,
    -- | An object which describes how the data is stored.
    --
    --
    --     * @databaseName@ - the name of the Glue database used to store the schema.
    schemaStorageConfig :: Core.Maybe (Core.HashMap Types.DatabaseName Types.String),
    -- | The timestamp representing when the continuous export was started.
    startTime :: Core.Maybe Core.NominalDiffTime,
    -- | Describes the status of the export. Can be one of the following values:
    --
    --
    --     * START_IN_PROGRESS - setting up resources to start continuous export.
    --
    --
    --     * START_FAILED - an error occurred setting up continuous export. To recover, call start-continuous-export again.
    --
    --
    --     * ACTIVE - data is being exported to the customer bucket.
    --
    --
    --     * ERROR - an error occurred during export. To fix the issue, call stop-continuous-export and start-continuous-export.
    --
    --
    --     * STOP_IN_PROGRESS - stopping the export.
    --
    --
    --     * STOP_FAILED - an error occurred stopping the export. To recover, call stop-continuous-export again.
    --
    --
    --     * INACTIVE - the continuous export has been stopped. Data is no longer being exported to the customer bucket.
    status :: Core.Maybe Types.ContinuousExportStatus,
    -- | Contains information about any errors that have occurred. This data type can have the following values:
    --
    --
    --     * ACCESS_DENIED - You don’t have permission to start Data Exploration in Amazon Athena. Contact your AWS administrator for help. For more information, see <http://docs.aws.amazon.com/application-discovery/latest/userguide/setting-up.html Setting Up AWS Application Discovery Service> in the Application Discovery Service User Guide.
    --
    --
    --     * DELIVERY_STREAM_LIMIT_FAILURE - You reached the limit for Amazon Kinesis Data Firehose delivery streams. Reduce the number of streams or request a limit increase and try again. For more information, see <http://docs.aws.amazon.com/streams/latest/dev/service-sizes-and-limits.html Kinesis Data Streams Limits> in the Amazon Kinesis Data Streams Developer Guide.
    --
    --
    --     * FIREHOSE_ROLE_MISSING - The Data Exploration feature is in an error state because your IAM User is missing the AWSApplicationDiscoveryServiceFirehose role. Turn on Data Exploration in Amazon Athena and try again. For more information, see <http://docs.aws.amazon.com/application-discovery/latest/userguide/setting-up.html#setting-up-user-policy Step 3: Provide Application Discovery Service Access to Non-Administrator Users by Attaching Policies> in the Application Discovery Service User Guide.
    --
    --
    --     * FIREHOSE_STREAM_DOES_NOT_EXIST - The Data Exploration feature is in an error state because your IAM User is missing one or more of the Kinesis data delivery streams.
    --
    --
    --     * INTERNAL_FAILURE - The Data Exploration feature is in an error state because of an internal failure. Try again later. If this problem persists, contact AWS Support.
    --
    --
    --     * S3_BUCKET_LIMIT_FAILURE - You reached the limit for Amazon S3 buckets. Reduce the number of Amazon S3 buckets or request a limit increase and try again. For more information, see <http://docs.aws.amazon.com/AmazonS3/latest/dev/BucketRestrictions.html Bucket Restrictions and Limitations> in the Amazon Simple Storage Service Developer Guide.
    --
    --
    --     * S3_NOT_SIGNED_UP - Your account is not signed up for the Amazon S3 service. You must sign up before you can use Amazon S3. You can sign up at the following URL: <https://aws.amazon.com/s3 https://aws.amazon.com/s3> .
    statusDetail :: Core.Maybe Types.StringMax255,
    -- | The timestamp that represents when this continuous export was stopped.
    stopTime :: Core.Maybe Core.NominalDiffTime
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ContinuousExportDescription' value with any optional fields omitted.
mkContinuousExportDescription ::
  ContinuousExportDescription
mkContinuousExportDescription =
  ContinuousExportDescription'
    { dataSource = Core.Nothing,
      exportId = Core.Nothing,
      s3Bucket = Core.Nothing,
      schemaStorageConfig = Core.Nothing,
      startTime = Core.Nothing,
      status = Core.Nothing,
      statusDetail = Core.Nothing,
      stopTime = Core.Nothing
    }

-- | The type of data collector used to gather this data (currently only offered for AGENT).
--
-- /Note:/ Consider using 'dataSource' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cedDataSource :: Lens.Lens' ContinuousExportDescription (Core.Maybe Types.DataSource)
cedDataSource = Lens.field @"dataSource"
{-# DEPRECATED cedDataSource "Use generic-lens or generic-optics with 'dataSource' instead." #-}

-- | The unique ID assigned to this export.
--
-- /Note:/ Consider using 'exportId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cedExportId :: Lens.Lens' ContinuousExportDescription (Core.Maybe Types.ExportId)
cedExportId = Lens.field @"exportId"
{-# DEPRECATED cedExportId "Use generic-lens or generic-optics with 'exportId' instead." #-}

-- | The name of the s3 bucket where the export data parquet files are stored.
--
-- /Note:/ Consider using 's3Bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cedS3Bucket :: Lens.Lens' ContinuousExportDescription (Core.Maybe Types.S3Bucket)
cedS3Bucket = Lens.field @"s3Bucket"
{-# DEPRECATED cedS3Bucket "Use generic-lens or generic-optics with 's3Bucket' instead." #-}

-- | An object which describes how the data is stored.
--
--
--     * @databaseName@ - the name of the Glue database used to store the schema.
--
--
--
-- /Note:/ Consider using 'schemaStorageConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cedSchemaStorageConfig :: Lens.Lens' ContinuousExportDescription (Core.Maybe (Core.HashMap Types.DatabaseName Types.String))
cedSchemaStorageConfig = Lens.field @"schemaStorageConfig"
{-# DEPRECATED cedSchemaStorageConfig "Use generic-lens or generic-optics with 'schemaStorageConfig' instead." #-}

-- | The timestamp representing when the continuous export was started.
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cedStartTime :: Lens.Lens' ContinuousExportDescription (Core.Maybe Core.NominalDiffTime)
cedStartTime = Lens.field @"startTime"
{-# DEPRECATED cedStartTime "Use generic-lens or generic-optics with 'startTime' instead." #-}

-- | Describes the status of the export. Can be one of the following values:
--
--
--     * START_IN_PROGRESS - setting up resources to start continuous export.
--
--
--     * START_FAILED - an error occurred setting up continuous export. To recover, call start-continuous-export again.
--
--
--     * ACTIVE - data is being exported to the customer bucket.
--
--
--     * ERROR - an error occurred during export. To fix the issue, call stop-continuous-export and start-continuous-export.
--
--
--     * STOP_IN_PROGRESS - stopping the export.
--
--
--     * STOP_FAILED - an error occurred stopping the export. To recover, call stop-continuous-export again.
--
--
--     * INACTIVE - the continuous export has been stopped. Data is no longer being exported to the customer bucket.
--
--
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cedStatus :: Lens.Lens' ContinuousExportDescription (Core.Maybe Types.ContinuousExportStatus)
cedStatus = Lens.field @"status"
{-# DEPRECATED cedStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | Contains information about any errors that have occurred. This data type can have the following values:
--
--
--     * ACCESS_DENIED - You don’t have permission to start Data Exploration in Amazon Athena. Contact your AWS administrator for help. For more information, see <http://docs.aws.amazon.com/application-discovery/latest/userguide/setting-up.html Setting Up AWS Application Discovery Service> in the Application Discovery Service User Guide.
--
--
--     * DELIVERY_STREAM_LIMIT_FAILURE - You reached the limit for Amazon Kinesis Data Firehose delivery streams. Reduce the number of streams or request a limit increase and try again. For more information, see <http://docs.aws.amazon.com/streams/latest/dev/service-sizes-and-limits.html Kinesis Data Streams Limits> in the Amazon Kinesis Data Streams Developer Guide.
--
--
--     * FIREHOSE_ROLE_MISSING - The Data Exploration feature is in an error state because your IAM User is missing the AWSApplicationDiscoveryServiceFirehose role. Turn on Data Exploration in Amazon Athena and try again. For more information, see <http://docs.aws.amazon.com/application-discovery/latest/userguide/setting-up.html#setting-up-user-policy Step 3: Provide Application Discovery Service Access to Non-Administrator Users by Attaching Policies> in the Application Discovery Service User Guide.
--
--
--     * FIREHOSE_STREAM_DOES_NOT_EXIST - The Data Exploration feature is in an error state because your IAM User is missing one or more of the Kinesis data delivery streams.
--
--
--     * INTERNAL_FAILURE - The Data Exploration feature is in an error state because of an internal failure. Try again later. If this problem persists, contact AWS Support.
--
--
--     * S3_BUCKET_LIMIT_FAILURE - You reached the limit for Amazon S3 buckets. Reduce the number of Amazon S3 buckets or request a limit increase and try again. For more information, see <http://docs.aws.amazon.com/AmazonS3/latest/dev/BucketRestrictions.html Bucket Restrictions and Limitations> in the Amazon Simple Storage Service Developer Guide.
--
--
--     * S3_NOT_SIGNED_UP - Your account is not signed up for the Amazon S3 service. You must sign up before you can use Amazon S3. You can sign up at the following URL: <https://aws.amazon.com/s3 https://aws.amazon.com/s3> .
--
--
--
-- /Note:/ Consider using 'statusDetail' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cedStatusDetail :: Lens.Lens' ContinuousExportDescription (Core.Maybe Types.StringMax255)
cedStatusDetail = Lens.field @"statusDetail"
{-# DEPRECATED cedStatusDetail "Use generic-lens or generic-optics with 'statusDetail' instead." #-}

-- | The timestamp that represents when this continuous export was stopped.
--
-- /Note:/ Consider using 'stopTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cedStopTime :: Lens.Lens' ContinuousExportDescription (Core.Maybe Core.NominalDiffTime)
cedStopTime = Lens.field @"stopTime"
{-# DEPRECATED cedStopTime "Use generic-lens or generic-optics with 'stopTime' instead." #-}

instance Core.FromJSON ContinuousExportDescription where
  parseJSON =
    Core.withObject "ContinuousExportDescription" Core.$
      \x ->
        ContinuousExportDescription'
          Core.<$> (x Core..:? "dataSource")
          Core.<*> (x Core..:? "exportId")
          Core.<*> (x Core..:? "s3Bucket")
          Core.<*> (x Core..:? "schemaStorageConfig")
          Core.<*> (x Core..:? "startTime")
          Core.<*> (x Core..:? "status")
          Core.<*> (x Core..:? "statusDetail")
          Core.<*> (x Core..:? "stopTime")
