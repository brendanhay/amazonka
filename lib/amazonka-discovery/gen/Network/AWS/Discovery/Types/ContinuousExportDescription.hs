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
    cedStatus,
    cedStartTime,
    cedSchemaStorageConfig,
    cedStatusDetail,
    cedStopTime,
    cedDataSource,
    cedS3Bucket,
    cedExportId,
  )
where

import Network.AWS.Discovery.Types.ContinuousExportStatus
import Network.AWS.Discovery.Types.DataSource
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A list of continuous export descriptions.
--
-- /See:/ 'mkContinuousExportDescription' smart constructor.
data ContinuousExportDescription = ContinuousExportDescription'
  { -- | Describes the status of the export. Can be one of the following values:
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
    status :: Lude.Maybe ContinuousExportStatus,
    -- | The timestamp representing when the continuous export was started.
    startTime :: Lude.Maybe Lude.Timestamp,
    -- | An object which describes how the data is stored.
    --
    --
    --     * @databaseName@ - the name of the Glue database used to store the schema.
    schemaStorageConfig :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
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
    statusDetail :: Lude.Maybe Lude.Text,
    -- | The timestamp that represents when this continuous export was stopped.
    stopTime :: Lude.Maybe Lude.Timestamp,
    -- | The type of data collector used to gather this data (currently only offered for AGENT).
    dataSource :: Lude.Maybe DataSource,
    -- | The name of the s3 bucket where the export data parquet files are stored.
    s3Bucket :: Lude.Maybe Lude.Text,
    -- | The unique ID assigned to this export.
    exportId :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ContinuousExportDescription' with the minimum fields required to make a request.
--
-- * 'status' - Describes the status of the export. Can be one of the following values:
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
-- * 'startTime' - The timestamp representing when the continuous export was started.
-- * 'schemaStorageConfig' - An object which describes how the data is stored.
--
--
--     * @databaseName@ - the name of the Glue database used to store the schema.
--
--
-- * 'statusDetail' - Contains information about any errors that have occurred. This data type can have the following values:
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
-- * 'stopTime' - The timestamp that represents when this continuous export was stopped.
-- * 'dataSource' - The type of data collector used to gather this data (currently only offered for AGENT).
-- * 's3Bucket' - The name of the s3 bucket where the export data parquet files are stored.
-- * 'exportId' - The unique ID assigned to this export.
mkContinuousExportDescription ::
  ContinuousExportDescription
mkContinuousExportDescription =
  ContinuousExportDescription'
    { status = Lude.Nothing,
      startTime = Lude.Nothing,
      schemaStorageConfig = Lude.Nothing,
      statusDetail = Lude.Nothing,
      stopTime = Lude.Nothing,
      dataSource = Lude.Nothing,
      s3Bucket = Lude.Nothing,
      exportId = Lude.Nothing
    }

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
cedStatus :: Lens.Lens' ContinuousExportDescription (Lude.Maybe ContinuousExportStatus)
cedStatus = Lens.lens (status :: ContinuousExportDescription -> Lude.Maybe ContinuousExportStatus) (\s a -> s {status = a} :: ContinuousExportDescription)
{-# DEPRECATED cedStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The timestamp representing when the continuous export was started.
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cedStartTime :: Lens.Lens' ContinuousExportDescription (Lude.Maybe Lude.Timestamp)
cedStartTime = Lens.lens (startTime :: ContinuousExportDescription -> Lude.Maybe Lude.Timestamp) (\s a -> s {startTime = a} :: ContinuousExportDescription)
{-# DEPRECATED cedStartTime "Use generic-lens or generic-optics with 'startTime' instead." #-}

-- | An object which describes how the data is stored.
--
--
--     * @databaseName@ - the name of the Glue database used to store the schema.
--
--
--
-- /Note:/ Consider using 'schemaStorageConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cedSchemaStorageConfig :: Lens.Lens' ContinuousExportDescription (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
cedSchemaStorageConfig = Lens.lens (schemaStorageConfig :: ContinuousExportDescription -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {schemaStorageConfig = a} :: ContinuousExportDescription)
{-# DEPRECATED cedSchemaStorageConfig "Use generic-lens or generic-optics with 'schemaStorageConfig' instead." #-}

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
cedStatusDetail :: Lens.Lens' ContinuousExportDescription (Lude.Maybe Lude.Text)
cedStatusDetail = Lens.lens (statusDetail :: ContinuousExportDescription -> Lude.Maybe Lude.Text) (\s a -> s {statusDetail = a} :: ContinuousExportDescription)
{-# DEPRECATED cedStatusDetail "Use generic-lens or generic-optics with 'statusDetail' instead." #-}

-- | The timestamp that represents when this continuous export was stopped.
--
-- /Note:/ Consider using 'stopTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cedStopTime :: Lens.Lens' ContinuousExportDescription (Lude.Maybe Lude.Timestamp)
cedStopTime = Lens.lens (stopTime :: ContinuousExportDescription -> Lude.Maybe Lude.Timestamp) (\s a -> s {stopTime = a} :: ContinuousExportDescription)
{-# DEPRECATED cedStopTime "Use generic-lens or generic-optics with 'stopTime' instead." #-}

-- | The type of data collector used to gather this data (currently only offered for AGENT).
--
-- /Note:/ Consider using 'dataSource' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cedDataSource :: Lens.Lens' ContinuousExportDescription (Lude.Maybe DataSource)
cedDataSource = Lens.lens (dataSource :: ContinuousExportDescription -> Lude.Maybe DataSource) (\s a -> s {dataSource = a} :: ContinuousExportDescription)
{-# DEPRECATED cedDataSource "Use generic-lens or generic-optics with 'dataSource' instead." #-}

-- | The name of the s3 bucket where the export data parquet files are stored.
--
-- /Note:/ Consider using 's3Bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cedS3Bucket :: Lens.Lens' ContinuousExportDescription (Lude.Maybe Lude.Text)
cedS3Bucket = Lens.lens (s3Bucket :: ContinuousExportDescription -> Lude.Maybe Lude.Text) (\s a -> s {s3Bucket = a} :: ContinuousExportDescription)
{-# DEPRECATED cedS3Bucket "Use generic-lens or generic-optics with 's3Bucket' instead." #-}

-- | The unique ID assigned to this export.
--
-- /Note:/ Consider using 'exportId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cedExportId :: Lens.Lens' ContinuousExportDescription (Lude.Maybe Lude.Text)
cedExportId = Lens.lens (exportId :: ContinuousExportDescription -> Lude.Maybe Lude.Text) (\s a -> s {exportId = a} :: ContinuousExportDescription)
{-# DEPRECATED cedExportId "Use generic-lens or generic-optics with 'exportId' instead." #-}

instance Lude.FromJSON ContinuousExportDescription where
  parseJSON =
    Lude.withObject
      "ContinuousExportDescription"
      ( \x ->
          ContinuousExportDescription'
            Lude.<$> (x Lude..:? "status")
            Lude.<*> (x Lude..:? "startTime")
            Lude.<*> (x Lude..:? "schemaStorageConfig" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "statusDetail")
            Lude.<*> (x Lude..:? "stopTime")
            Lude.<*> (x Lude..:? "dataSource")
            Lude.<*> (x Lude..:? "s3Bucket")
            Lude.<*> (x Lude..:? "exportId")
      )
