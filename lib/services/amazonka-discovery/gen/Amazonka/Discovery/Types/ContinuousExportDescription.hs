{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Discovery.Types.ContinuousExportDescription
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Discovery.Types.ContinuousExportDescription where

import qualified Amazonka.Core as Core
import Amazonka.Discovery.Types.ContinuousExportStatus
import Amazonka.Discovery.Types.DataSource
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | A list of continuous export descriptions.
--
-- /See:/ 'newContinuousExportDescription' smart constructor.
data ContinuousExportDescription = ContinuousExportDescription'
  { -- | Describes the status of the export. Can be one of the following values:
    --
    -- -   START_IN_PROGRESS - setting up resources to start continuous export.
    --
    -- -   START_FAILED - an error occurred setting up continuous export. To
    --     recover, call start-continuous-export again.
    --
    -- -   ACTIVE - data is being exported to the customer bucket.
    --
    -- -   ERROR - an error occurred during export. To fix the issue, call
    --     stop-continuous-export and start-continuous-export.
    --
    -- -   STOP_IN_PROGRESS - stopping the export.
    --
    -- -   STOP_FAILED - an error occurred stopping the export. To recover,
    --     call stop-continuous-export again.
    --
    -- -   INACTIVE - the continuous export has been stopped. Data is no longer
    --     being exported to the customer bucket.
    status :: Prelude.Maybe ContinuousExportStatus,
    -- | The timestamp representing when the continuous export was started.
    startTime :: Prelude.Maybe Core.POSIX,
    -- | An object which describes how the data is stored.
    --
    -- -   @databaseName@ - the name of the Glue database used to store the
    --     schema.
    schemaStorageConfig :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | Contains information about any errors that have occurred. This data type
    -- can have the following values:
    --
    -- -   ACCESS_DENIED - You don’t have permission to start Data Exploration
    --     in Amazon Athena. Contact your AWS administrator for help. For more
    --     information, see
    --     <http://docs.aws.amazon.com/application-discovery/latest/userguide/setting-up.html Setting Up AWS Application Discovery Service>
    --     in the Application Discovery Service User Guide.
    --
    -- -   DELIVERY_STREAM_LIMIT_FAILURE - You reached the limit for Amazon
    --     Kinesis Data Firehose delivery streams. Reduce the number of streams
    --     or request a limit increase and try again. For more information, see
    --     <http://docs.aws.amazon.com/streams/latest/dev/service-sizes-and-limits.html Kinesis Data Streams Limits>
    --     in the Amazon Kinesis Data Streams Developer Guide.
    --
    -- -   FIREHOSE_ROLE_MISSING - The Data Exploration feature is in an error
    --     state because your IAM User is missing the
    --     AWSApplicationDiscoveryServiceFirehose role. Turn on Data
    --     Exploration in Amazon Athena and try again. For more information,
    --     see
    --     <http://docs.aws.amazon.com/application-discovery/latest/userguide/setting-up.html#setting-up-user-policy Step 3: Provide Application Discovery Service Access to Non-Administrator Users by Attaching Policies>
    --     in the Application Discovery Service User Guide.
    --
    -- -   FIREHOSE_STREAM_DOES_NOT_EXIST - The Data Exploration feature is in
    --     an error state because your IAM User is missing one or more of the
    --     Kinesis data delivery streams.
    --
    -- -   INTERNAL_FAILURE - The Data Exploration feature is in an error state
    --     because of an internal failure. Try again later. If this problem
    --     persists, contact AWS Support.
    --
    -- -   S3_BUCKET_LIMIT_FAILURE - You reached the limit for Amazon S3
    --     buckets. Reduce the number of Amazon S3 buckets or request a limit
    --     increase and try again. For more information, see
    --     <http://docs.aws.amazon.com/AmazonS3/latest/dev/BucketRestrictions.html Bucket Restrictions and Limitations>
    --     in the Amazon Simple Storage Service Developer Guide.
    --
    -- -   S3_NOT_SIGNED_UP - Your account is not signed up for the Amazon S3
    --     service. You must sign up before you can use Amazon S3. You can sign
    --     up at the following URL: <https://aws.amazon.com/s3>.
    statusDetail :: Prelude.Maybe Prelude.Text,
    -- | The timestamp that represents when this continuous export was stopped.
    stopTime :: Prelude.Maybe Core.POSIX,
    -- | The type of data collector used to gather this data (currently only
    -- offered for AGENT).
    dataSource :: Prelude.Maybe DataSource,
    -- | The name of the s3 bucket where the export data parquet files are
    -- stored.
    s3Bucket :: Prelude.Maybe Prelude.Text,
    -- | The unique ID assigned to this export.
    exportId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ContinuousExportDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'continuousExportDescription_status' - Describes the status of the export. Can be one of the following values:
--
-- -   START_IN_PROGRESS - setting up resources to start continuous export.
--
-- -   START_FAILED - an error occurred setting up continuous export. To
--     recover, call start-continuous-export again.
--
-- -   ACTIVE - data is being exported to the customer bucket.
--
-- -   ERROR - an error occurred during export. To fix the issue, call
--     stop-continuous-export and start-continuous-export.
--
-- -   STOP_IN_PROGRESS - stopping the export.
--
-- -   STOP_FAILED - an error occurred stopping the export. To recover,
--     call stop-continuous-export again.
--
-- -   INACTIVE - the continuous export has been stopped. Data is no longer
--     being exported to the customer bucket.
--
-- 'startTime', 'continuousExportDescription_startTime' - The timestamp representing when the continuous export was started.
--
-- 'schemaStorageConfig', 'continuousExportDescription_schemaStorageConfig' - An object which describes how the data is stored.
--
-- -   @databaseName@ - the name of the Glue database used to store the
--     schema.
--
-- 'statusDetail', 'continuousExportDescription_statusDetail' - Contains information about any errors that have occurred. This data type
-- can have the following values:
--
-- -   ACCESS_DENIED - You don’t have permission to start Data Exploration
--     in Amazon Athena. Contact your AWS administrator for help. For more
--     information, see
--     <http://docs.aws.amazon.com/application-discovery/latest/userguide/setting-up.html Setting Up AWS Application Discovery Service>
--     in the Application Discovery Service User Guide.
--
-- -   DELIVERY_STREAM_LIMIT_FAILURE - You reached the limit for Amazon
--     Kinesis Data Firehose delivery streams. Reduce the number of streams
--     or request a limit increase and try again. For more information, see
--     <http://docs.aws.amazon.com/streams/latest/dev/service-sizes-and-limits.html Kinesis Data Streams Limits>
--     in the Amazon Kinesis Data Streams Developer Guide.
--
-- -   FIREHOSE_ROLE_MISSING - The Data Exploration feature is in an error
--     state because your IAM User is missing the
--     AWSApplicationDiscoveryServiceFirehose role. Turn on Data
--     Exploration in Amazon Athena and try again. For more information,
--     see
--     <http://docs.aws.amazon.com/application-discovery/latest/userguide/setting-up.html#setting-up-user-policy Step 3: Provide Application Discovery Service Access to Non-Administrator Users by Attaching Policies>
--     in the Application Discovery Service User Guide.
--
-- -   FIREHOSE_STREAM_DOES_NOT_EXIST - The Data Exploration feature is in
--     an error state because your IAM User is missing one or more of the
--     Kinesis data delivery streams.
--
-- -   INTERNAL_FAILURE - The Data Exploration feature is in an error state
--     because of an internal failure. Try again later. If this problem
--     persists, contact AWS Support.
--
-- -   S3_BUCKET_LIMIT_FAILURE - You reached the limit for Amazon S3
--     buckets. Reduce the number of Amazon S3 buckets or request a limit
--     increase and try again. For more information, see
--     <http://docs.aws.amazon.com/AmazonS3/latest/dev/BucketRestrictions.html Bucket Restrictions and Limitations>
--     in the Amazon Simple Storage Service Developer Guide.
--
-- -   S3_NOT_SIGNED_UP - Your account is not signed up for the Amazon S3
--     service. You must sign up before you can use Amazon S3. You can sign
--     up at the following URL: <https://aws.amazon.com/s3>.
--
-- 'stopTime', 'continuousExportDescription_stopTime' - The timestamp that represents when this continuous export was stopped.
--
-- 'dataSource', 'continuousExportDescription_dataSource' - The type of data collector used to gather this data (currently only
-- offered for AGENT).
--
-- 's3Bucket', 'continuousExportDescription_s3Bucket' - The name of the s3 bucket where the export data parquet files are
-- stored.
--
-- 'exportId', 'continuousExportDescription_exportId' - The unique ID assigned to this export.
newContinuousExportDescription ::
  ContinuousExportDescription
newContinuousExportDescription =
  ContinuousExportDescription'
    { status =
        Prelude.Nothing,
      startTime = Prelude.Nothing,
      schemaStorageConfig = Prelude.Nothing,
      statusDetail = Prelude.Nothing,
      stopTime = Prelude.Nothing,
      dataSource = Prelude.Nothing,
      s3Bucket = Prelude.Nothing,
      exportId = Prelude.Nothing
    }

-- | Describes the status of the export. Can be one of the following values:
--
-- -   START_IN_PROGRESS - setting up resources to start continuous export.
--
-- -   START_FAILED - an error occurred setting up continuous export. To
--     recover, call start-continuous-export again.
--
-- -   ACTIVE - data is being exported to the customer bucket.
--
-- -   ERROR - an error occurred during export. To fix the issue, call
--     stop-continuous-export and start-continuous-export.
--
-- -   STOP_IN_PROGRESS - stopping the export.
--
-- -   STOP_FAILED - an error occurred stopping the export. To recover,
--     call stop-continuous-export again.
--
-- -   INACTIVE - the continuous export has been stopped. Data is no longer
--     being exported to the customer bucket.
continuousExportDescription_status :: Lens.Lens' ContinuousExportDescription (Prelude.Maybe ContinuousExportStatus)
continuousExportDescription_status = Lens.lens (\ContinuousExportDescription' {status} -> status) (\s@ContinuousExportDescription' {} a -> s {status = a} :: ContinuousExportDescription)

-- | The timestamp representing when the continuous export was started.
continuousExportDescription_startTime :: Lens.Lens' ContinuousExportDescription (Prelude.Maybe Prelude.UTCTime)
continuousExportDescription_startTime = Lens.lens (\ContinuousExportDescription' {startTime} -> startTime) (\s@ContinuousExportDescription' {} a -> s {startTime = a} :: ContinuousExportDescription) Prelude.. Lens.mapping Core._Time

-- | An object which describes how the data is stored.
--
-- -   @databaseName@ - the name of the Glue database used to store the
--     schema.
continuousExportDescription_schemaStorageConfig :: Lens.Lens' ContinuousExportDescription (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
continuousExportDescription_schemaStorageConfig = Lens.lens (\ContinuousExportDescription' {schemaStorageConfig} -> schemaStorageConfig) (\s@ContinuousExportDescription' {} a -> s {schemaStorageConfig = a} :: ContinuousExportDescription) Prelude.. Lens.mapping Lens.coerced

-- | Contains information about any errors that have occurred. This data type
-- can have the following values:
--
-- -   ACCESS_DENIED - You don’t have permission to start Data Exploration
--     in Amazon Athena. Contact your AWS administrator for help. For more
--     information, see
--     <http://docs.aws.amazon.com/application-discovery/latest/userguide/setting-up.html Setting Up AWS Application Discovery Service>
--     in the Application Discovery Service User Guide.
--
-- -   DELIVERY_STREAM_LIMIT_FAILURE - You reached the limit for Amazon
--     Kinesis Data Firehose delivery streams. Reduce the number of streams
--     or request a limit increase and try again. For more information, see
--     <http://docs.aws.amazon.com/streams/latest/dev/service-sizes-and-limits.html Kinesis Data Streams Limits>
--     in the Amazon Kinesis Data Streams Developer Guide.
--
-- -   FIREHOSE_ROLE_MISSING - The Data Exploration feature is in an error
--     state because your IAM User is missing the
--     AWSApplicationDiscoveryServiceFirehose role. Turn on Data
--     Exploration in Amazon Athena and try again. For more information,
--     see
--     <http://docs.aws.amazon.com/application-discovery/latest/userguide/setting-up.html#setting-up-user-policy Step 3: Provide Application Discovery Service Access to Non-Administrator Users by Attaching Policies>
--     in the Application Discovery Service User Guide.
--
-- -   FIREHOSE_STREAM_DOES_NOT_EXIST - The Data Exploration feature is in
--     an error state because your IAM User is missing one or more of the
--     Kinesis data delivery streams.
--
-- -   INTERNAL_FAILURE - The Data Exploration feature is in an error state
--     because of an internal failure. Try again later. If this problem
--     persists, contact AWS Support.
--
-- -   S3_BUCKET_LIMIT_FAILURE - You reached the limit for Amazon S3
--     buckets. Reduce the number of Amazon S3 buckets or request a limit
--     increase and try again. For more information, see
--     <http://docs.aws.amazon.com/AmazonS3/latest/dev/BucketRestrictions.html Bucket Restrictions and Limitations>
--     in the Amazon Simple Storage Service Developer Guide.
--
-- -   S3_NOT_SIGNED_UP - Your account is not signed up for the Amazon S3
--     service. You must sign up before you can use Amazon S3. You can sign
--     up at the following URL: <https://aws.amazon.com/s3>.
continuousExportDescription_statusDetail :: Lens.Lens' ContinuousExportDescription (Prelude.Maybe Prelude.Text)
continuousExportDescription_statusDetail = Lens.lens (\ContinuousExportDescription' {statusDetail} -> statusDetail) (\s@ContinuousExportDescription' {} a -> s {statusDetail = a} :: ContinuousExportDescription)

-- | The timestamp that represents when this continuous export was stopped.
continuousExportDescription_stopTime :: Lens.Lens' ContinuousExportDescription (Prelude.Maybe Prelude.UTCTime)
continuousExportDescription_stopTime = Lens.lens (\ContinuousExportDescription' {stopTime} -> stopTime) (\s@ContinuousExportDescription' {} a -> s {stopTime = a} :: ContinuousExportDescription) Prelude.. Lens.mapping Core._Time

-- | The type of data collector used to gather this data (currently only
-- offered for AGENT).
continuousExportDescription_dataSource :: Lens.Lens' ContinuousExportDescription (Prelude.Maybe DataSource)
continuousExportDescription_dataSource = Lens.lens (\ContinuousExportDescription' {dataSource} -> dataSource) (\s@ContinuousExportDescription' {} a -> s {dataSource = a} :: ContinuousExportDescription)

-- | The name of the s3 bucket where the export data parquet files are
-- stored.
continuousExportDescription_s3Bucket :: Lens.Lens' ContinuousExportDescription (Prelude.Maybe Prelude.Text)
continuousExportDescription_s3Bucket = Lens.lens (\ContinuousExportDescription' {s3Bucket} -> s3Bucket) (\s@ContinuousExportDescription' {} a -> s {s3Bucket = a} :: ContinuousExportDescription)

-- | The unique ID assigned to this export.
continuousExportDescription_exportId :: Lens.Lens' ContinuousExportDescription (Prelude.Maybe Prelude.Text)
continuousExportDescription_exportId = Lens.lens (\ContinuousExportDescription' {exportId} -> exportId) (\s@ContinuousExportDescription' {} a -> s {exportId = a} :: ContinuousExportDescription)

instance Core.FromJSON ContinuousExportDescription where
  parseJSON =
    Core.withObject
      "ContinuousExportDescription"
      ( \x ->
          ContinuousExportDescription'
            Prelude.<$> (x Core..:? "status")
            Prelude.<*> (x Core..:? "startTime")
            Prelude.<*> ( x Core..:? "schemaStorageConfig"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "statusDetail")
            Prelude.<*> (x Core..:? "stopTime")
            Prelude.<*> (x Core..:? "dataSource")
            Prelude.<*> (x Core..:? "s3Bucket")
            Prelude.<*> (x Core..:? "exportId")
      )

instance Prelude.Hashable ContinuousExportDescription where
  hashWithSalt salt' ContinuousExportDescription' {..} =
    salt' `Prelude.hashWithSalt` exportId
      `Prelude.hashWithSalt` s3Bucket
      `Prelude.hashWithSalt` dataSource
      `Prelude.hashWithSalt` stopTime
      `Prelude.hashWithSalt` statusDetail
      `Prelude.hashWithSalt` schemaStorageConfig
      `Prelude.hashWithSalt` startTime
      `Prelude.hashWithSalt` status

instance Prelude.NFData ContinuousExportDescription where
  rnf ContinuousExportDescription' {..} =
    Prelude.rnf status
      `Prelude.seq` Prelude.rnf exportId
      `Prelude.seq` Prelude.rnf s3Bucket
      `Prelude.seq` Prelude.rnf dataSource
      `Prelude.seq` Prelude.rnf stopTime
      `Prelude.seq` Prelude.rnf statusDetail
      `Prelude.seq` Prelude.rnf schemaStorageConfig
      `Prelude.seq` Prelude.rnf startTime
