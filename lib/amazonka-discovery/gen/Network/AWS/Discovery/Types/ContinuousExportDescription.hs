{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Discovery.Types.ContinuousExportDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Discovery.Types.ContinuousExportDescription where

import Network.AWS.Discovery.Types.ContinuousExportStatus
import Network.AWS.Discovery.Types.DataSource
import Network.AWS.Lens
import Network.AWS.Prelude

-- | A list of continuous export descriptions.
--
--
--
-- /See:/ 'continuousExportDescription' smart constructor.
data ContinuousExportDescription = ContinuousExportDescription'
  { _cedStatus ::
      !(Maybe ContinuousExportStatus),
    _cedStartTime :: !(Maybe POSIX),
    _cedSchemaStorageConfig ::
      !(Maybe (Map Text (Text))),
    _cedStatusDetail :: !(Maybe Text),
    _cedStopTime :: !(Maybe POSIX),
    _cedDataSource ::
      !(Maybe DataSource),
    _cedS3Bucket :: !(Maybe Text),
    _cedExportId :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ContinuousExportDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cedStatus' - Describes the status of the export. Can be one of the following values:     * START_IN_PROGRESS - setting up resources to start continuous export.     * START_FAILED - an error occurred setting up continuous export. To recover, call start-continuous-export again.     * ACTIVE - data is being exported to the customer bucket.     * ERROR - an error occurred during export. To fix the issue, call stop-continuous-export and start-continuous-export.     * STOP_IN_PROGRESS - stopping the export.     * STOP_FAILED - an error occurred stopping the export. To recover, call stop-continuous-export again.     * INACTIVE - the continuous export has been stopped. Data is no longer being exported to the customer bucket.
--
-- * 'cedStartTime' - The timestamp representing when the continuous export was started.
--
-- * 'cedSchemaStorageConfig' - An object which describes how the data is stored.     * @databaseName@ - the name of the Glue database used to store the schema.
--
-- * 'cedStatusDetail' - Contains information about any errors that have occurred. This data type can have the following values:     * ACCESS_DENIED - You don’t have permission to start Data Exploration in Amazon Athena. Contact your AWS administrator for help. For more information, see <http://docs.aws.amazon.com/application-discovery/latest/userguide/setting-up.html Setting Up AWS Application Discovery Service> in the Application Discovery Service User Guide.     * DELIVERY_STREAM_LIMIT_FAILURE - You reached the limit for Amazon Kinesis Data Firehose delivery streams. Reduce the number of streams or request a limit increase and try again. For more information, see <http://docs.aws.amazon.com/streams/latest/dev/service-sizes-and-limits.html Kinesis Data Streams Limits> in the Amazon Kinesis Data Streams Developer Guide.     * FIREHOSE_ROLE_MISSING - The Data Exploration feature is in an error state because your IAM User is missing the AWSApplicationDiscoveryServiceFirehose role. Turn on Data Exploration in Amazon Athena and try again. For more information, see <http://docs.aws.amazon.com/application-discovery/latest/userguide/setting-up.html#setting-up-user-policy Step 3: Provide Application Discovery Service Access to Non-Administrator Users by Attaching Policies> in the Application Discovery Service User Guide.     * FIREHOSE_STREAM_DOES_NOT_EXIST - The Data Exploration feature is in an error state because your IAM User is missing one or more of the Kinesis data delivery streams.     * INTERNAL_FAILURE - The Data Exploration feature is in an error state because of an internal failure. Try again later. If this problem persists, contact AWS Support.     * S3_BUCKET_LIMIT_FAILURE - You reached the limit for Amazon S3 buckets. Reduce the number of Amazon S3 buckets or request a limit increase and try again. For more information, see <http://docs.aws.amazon.com/AmazonS3/latest/dev/BucketRestrictions.html Bucket Restrictions and Limitations> in the Amazon Simple Storage Service Developer Guide.     * S3_NOT_SIGNED_UP - Your account is not signed up for the Amazon S3 service. You must sign up before you can use Amazon S3. You can sign up at the following URL: <https://aws.amazon.com/s3 https://aws.amazon.com/s3> .
--
-- * 'cedStopTime' - The timestamp that represents when this continuous export was stopped.
--
-- * 'cedDataSource' - The type of data collector used to gather this data (currently only offered for AGENT).
--
-- * 'cedS3Bucket' - The name of the s3 bucket where the export data parquet files are stored.
--
-- * 'cedExportId' - The unique ID assigned to this export.
continuousExportDescription ::
  ContinuousExportDescription
continuousExportDescription =
  ContinuousExportDescription'
    { _cedStatus = Nothing,
      _cedStartTime = Nothing,
      _cedSchemaStorageConfig = Nothing,
      _cedStatusDetail = Nothing,
      _cedStopTime = Nothing,
      _cedDataSource = Nothing,
      _cedS3Bucket = Nothing,
      _cedExportId = Nothing
    }

-- | Describes the status of the export. Can be one of the following values:     * START_IN_PROGRESS - setting up resources to start continuous export.     * START_FAILED - an error occurred setting up continuous export. To recover, call start-continuous-export again.     * ACTIVE - data is being exported to the customer bucket.     * ERROR - an error occurred during export. To fix the issue, call stop-continuous-export and start-continuous-export.     * STOP_IN_PROGRESS - stopping the export.     * STOP_FAILED - an error occurred stopping the export. To recover, call stop-continuous-export again.     * INACTIVE - the continuous export has been stopped. Data is no longer being exported to the customer bucket.
cedStatus :: Lens' ContinuousExportDescription (Maybe ContinuousExportStatus)
cedStatus = lens _cedStatus (\s a -> s {_cedStatus = a})

-- | The timestamp representing when the continuous export was started.
cedStartTime :: Lens' ContinuousExportDescription (Maybe UTCTime)
cedStartTime = lens _cedStartTime (\s a -> s {_cedStartTime = a}) . mapping _Time

-- | An object which describes how the data is stored.     * @databaseName@ - the name of the Glue database used to store the schema.
cedSchemaStorageConfig :: Lens' ContinuousExportDescription (HashMap Text (Text))
cedSchemaStorageConfig = lens _cedSchemaStorageConfig (\s a -> s {_cedSchemaStorageConfig = a}) . _Default . _Map

-- | Contains information about any errors that have occurred. This data type can have the following values:     * ACCESS_DENIED - You don’t have permission to start Data Exploration in Amazon Athena. Contact your AWS administrator for help. For more information, see <http://docs.aws.amazon.com/application-discovery/latest/userguide/setting-up.html Setting Up AWS Application Discovery Service> in the Application Discovery Service User Guide.     * DELIVERY_STREAM_LIMIT_FAILURE - You reached the limit for Amazon Kinesis Data Firehose delivery streams. Reduce the number of streams or request a limit increase and try again. For more information, see <http://docs.aws.amazon.com/streams/latest/dev/service-sizes-and-limits.html Kinesis Data Streams Limits> in the Amazon Kinesis Data Streams Developer Guide.     * FIREHOSE_ROLE_MISSING - The Data Exploration feature is in an error state because your IAM User is missing the AWSApplicationDiscoveryServiceFirehose role. Turn on Data Exploration in Amazon Athena and try again. For more information, see <http://docs.aws.amazon.com/application-discovery/latest/userguide/setting-up.html#setting-up-user-policy Step 3: Provide Application Discovery Service Access to Non-Administrator Users by Attaching Policies> in the Application Discovery Service User Guide.     * FIREHOSE_STREAM_DOES_NOT_EXIST - The Data Exploration feature is in an error state because your IAM User is missing one or more of the Kinesis data delivery streams.     * INTERNAL_FAILURE - The Data Exploration feature is in an error state because of an internal failure. Try again later. If this problem persists, contact AWS Support.     * S3_BUCKET_LIMIT_FAILURE - You reached the limit for Amazon S3 buckets. Reduce the number of Amazon S3 buckets or request a limit increase and try again. For more information, see <http://docs.aws.amazon.com/AmazonS3/latest/dev/BucketRestrictions.html Bucket Restrictions and Limitations> in the Amazon Simple Storage Service Developer Guide.     * S3_NOT_SIGNED_UP - Your account is not signed up for the Amazon S3 service. You must sign up before you can use Amazon S3. You can sign up at the following URL: <https://aws.amazon.com/s3 https://aws.amazon.com/s3> .
cedStatusDetail :: Lens' ContinuousExportDescription (Maybe Text)
cedStatusDetail = lens _cedStatusDetail (\s a -> s {_cedStatusDetail = a})

-- | The timestamp that represents when this continuous export was stopped.
cedStopTime :: Lens' ContinuousExportDescription (Maybe UTCTime)
cedStopTime = lens _cedStopTime (\s a -> s {_cedStopTime = a}) . mapping _Time

-- | The type of data collector used to gather this data (currently only offered for AGENT).
cedDataSource :: Lens' ContinuousExportDescription (Maybe DataSource)
cedDataSource = lens _cedDataSource (\s a -> s {_cedDataSource = a})

-- | The name of the s3 bucket where the export data parquet files are stored.
cedS3Bucket :: Lens' ContinuousExportDescription (Maybe Text)
cedS3Bucket = lens _cedS3Bucket (\s a -> s {_cedS3Bucket = a})

-- | The unique ID assigned to this export.
cedExportId :: Lens' ContinuousExportDescription (Maybe Text)
cedExportId = lens _cedExportId (\s a -> s {_cedExportId = a})

instance FromJSON ContinuousExportDescription where
  parseJSON =
    withObject
      "ContinuousExportDescription"
      ( \x ->
          ContinuousExportDescription'
            <$> (x .:? "status")
            <*> (x .:? "startTime")
            <*> (x .:? "schemaStorageConfig" .!= mempty)
            <*> (x .:? "statusDetail")
            <*> (x .:? "stopTime")
            <*> (x .:? "dataSource")
            <*> (x .:? "s3Bucket")
            <*> (x .:? "exportId")
      )

instance Hashable ContinuousExportDescription

instance NFData ContinuousExportDescription
