{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.ExportDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.ExportDescription where

import Network.AWS.DynamoDB.Types.ExportFormat
import Network.AWS.DynamoDB.Types.ExportStatus
import Network.AWS.DynamoDB.Types.S3SseAlgorithm
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents the properties of the exported table.
--
--
--
-- /See:/ 'exportDescription' smart constructor.
data ExportDescription = ExportDescription'
  { _edS3BucketOwner ::
      !(Maybe Text),
    _edExportFormat :: !(Maybe ExportFormat),
    _edS3SseKMSKeyId :: !(Maybe Text),
    _edClientToken :: !(Maybe Text),
    _edStartTime :: !(Maybe POSIX),
    _edFailureCode :: !(Maybe Text),
    _edExportStatus :: !(Maybe ExportStatus),
    _edFailureMessage :: !(Maybe Text),
    _edTableARN :: !(Maybe Text),
    _edBilledSizeBytes :: !(Maybe Nat),
    _edExportARN :: !(Maybe Text),
    _edExportTime :: !(Maybe POSIX),
    _edS3SseAlgorithm :: !(Maybe S3SseAlgorithm),
    _edEndTime :: !(Maybe POSIX),
    _edS3Prefix :: !(Maybe Text),
    _edExportManifest :: !(Maybe Text),
    _edTableId :: !(Maybe Text),
    _edItemCount :: !(Maybe Nat),
    _edS3Bucket :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ExportDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'edS3BucketOwner' - The ID of the AWS account that owns the bucket containing the export.
--
-- * 'edExportFormat' - The format of the exported data. Valid values for @ExportFormat@ are @DYNAMODB_JSON@ or @ION@ .
--
-- * 'edS3SseKMSKeyId' - The ID of the AWS KMS managed key used to encrypt the S3 bucket where export data is stored (if applicable).
--
-- * 'edClientToken' - The client token that was provided for the export task. A client token makes calls to @ExportTableToPointInTimeInput@ idempotent, meaning that multiple identical calls have the same effect as one single call.
--
-- * 'edStartTime' - The time at which the export task began.
--
-- * 'edFailureCode' - Status code for the result of the failed export.
--
-- * 'edExportStatus' - Export can be in one of the following states: IN_PROGRESS, COMPLETED, or FAILED.
--
-- * 'edFailureMessage' - Export failure reason description.
--
-- * 'edTableARN' - The Amazon Resource Name (ARN) of the table that was exported.
--
-- * 'edBilledSizeBytes' - The billable size of the table export.
--
-- * 'edExportARN' - The Amazon Resource Name (ARN) of the table export.
--
-- * 'edExportTime' - Point in time from which table data was exported.
--
-- * 'edS3SseAlgorithm' - Type of encryption used on the bucket where export data is stored. Valid values for @S3SseAlgorithm@ are:     * @AES256@ - server-side encryption with Amazon S3 managed keys     * @KMS@ - server-side encryption with AWS KMS managed keys
--
-- * 'edEndTime' - The time at which the export task completed.
--
-- * 'edS3Prefix' - The Amazon S3 bucket prefix used as the file name and path of the exported snapshot.
--
-- * 'edExportManifest' - The name of the manifest file for the export task.
--
-- * 'edTableId' - Unique ID of the table that was exported.
--
-- * 'edItemCount' - The number of items exported.
--
-- * 'edS3Bucket' - The name of the Amazon S3 bucket containing the export.
exportDescription ::
  ExportDescription
exportDescription =
  ExportDescription'
    { _edS3BucketOwner = Nothing,
      _edExportFormat = Nothing,
      _edS3SseKMSKeyId = Nothing,
      _edClientToken = Nothing,
      _edStartTime = Nothing,
      _edFailureCode = Nothing,
      _edExportStatus = Nothing,
      _edFailureMessage = Nothing,
      _edTableARN = Nothing,
      _edBilledSizeBytes = Nothing,
      _edExportARN = Nothing,
      _edExportTime = Nothing,
      _edS3SseAlgorithm = Nothing,
      _edEndTime = Nothing,
      _edS3Prefix = Nothing,
      _edExportManifest = Nothing,
      _edTableId = Nothing,
      _edItemCount = Nothing,
      _edS3Bucket = Nothing
    }

-- | The ID of the AWS account that owns the bucket containing the export.
edS3BucketOwner :: Lens' ExportDescription (Maybe Text)
edS3BucketOwner = lens _edS3BucketOwner (\s a -> s {_edS3BucketOwner = a})

-- | The format of the exported data. Valid values for @ExportFormat@ are @DYNAMODB_JSON@ or @ION@ .
edExportFormat :: Lens' ExportDescription (Maybe ExportFormat)
edExportFormat = lens _edExportFormat (\s a -> s {_edExportFormat = a})

-- | The ID of the AWS KMS managed key used to encrypt the S3 bucket where export data is stored (if applicable).
edS3SseKMSKeyId :: Lens' ExportDescription (Maybe Text)
edS3SseKMSKeyId = lens _edS3SseKMSKeyId (\s a -> s {_edS3SseKMSKeyId = a})

-- | The client token that was provided for the export task. A client token makes calls to @ExportTableToPointInTimeInput@ idempotent, meaning that multiple identical calls have the same effect as one single call.
edClientToken :: Lens' ExportDescription (Maybe Text)
edClientToken = lens _edClientToken (\s a -> s {_edClientToken = a})

-- | The time at which the export task began.
edStartTime :: Lens' ExportDescription (Maybe UTCTime)
edStartTime = lens _edStartTime (\s a -> s {_edStartTime = a}) . mapping _Time

-- | Status code for the result of the failed export.
edFailureCode :: Lens' ExportDescription (Maybe Text)
edFailureCode = lens _edFailureCode (\s a -> s {_edFailureCode = a})

-- | Export can be in one of the following states: IN_PROGRESS, COMPLETED, or FAILED.
edExportStatus :: Lens' ExportDescription (Maybe ExportStatus)
edExportStatus = lens _edExportStatus (\s a -> s {_edExportStatus = a})

-- | Export failure reason description.
edFailureMessage :: Lens' ExportDescription (Maybe Text)
edFailureMessage = lens _edFailureMessage (\s a -> s {_edFailureMessage = a})

-- | The Amazon Resource Name (ARN) of the table that was exported.
edTableARN :: Lens' ExportDescription (Maybe Text)
edTableARN = lens _edTableARN (\s a -> s {_edTableARN = a})

-- | The billable size of the table export.
edBilledSizeBytes :: Lens' ExportDescription (Maybe Natural)
edBilledSizeBytes = lens _edBilledSizeBytes (\s a -> s {_edBilledSizeBytes = a}) . mapping _Nat

-- | The Amazon Resource Name (ARN) of the table export.
edExportARN :: Lens' ExportDescription (Maybe Text)
edExportARN = lens _edExportARN (\s a -> s {_edExportARN = a})

-- | Point in time from which table data was exported.
edExportTime :: Lens' ExportDescription (Maybe UTCTime)
edExportTime = lens _edExportTime (\s a -> s {_edExportTime = a}) . mapping _Time

-- | Type of encryption used on the bucket where export data is stored. Valid values for @S3SseAlgorithm@ are:     * @AES256@ - server-side encryption with Amazon S3 managed keys     * @KMS@ - server-side encryption with AWS KMS managed keys
edS3SseAlgorithm :: Lens' ExportDescription (Maybe S3SseAlgorithm)
edS3SseAlgorithm = lens _edS3SseAlgorithm (\s a -> s {_edS3SseAlgorithm = a})

-- | The time at which the export task completed.
edEndTime :: Lens' ExportDescription (Maybe UTCTime)
edEndTime = lens _edEndTime (\s a -> s {_edEndTime = a}) . mapping _Time

-- | The Amazon S3 bucket prefix used as the file name and path of the exported snapshot.
edS3Prefix :: Lens' ExportDescription (Maybe Text)
edS3Prefix = lens _edS3Prefix (\s a -> s {_edS3Prefix = a})

-- | The name of the manifest file for the export task.
edExportManifest :: Lens' ExportDescription (Maybe Text)
edExportManifest = lens _edExportManifest (\s a -> s {_edExportManifest = a})

-- | Unique ID of the table that was exported.
edTableId :: Lens' ExportDescription (Maybe Text)
edTableId = lens _edTableId (\s a -> s {_edTableId = a})

-- | The number of items exported.
edItemCount :: Lens' ExportDescription (Maybe Natural)
edItemCount = lens _edItemCount (\s a -> s {_edItemCount = a}) . mapping _Nat

-- | The name of the Amazon S3 bucket containing the export.
edS3Bucket :: Lens' ExportDescription (Maybe Text)
edS3Bucket = lens _edS3Bucket (\s a -> s {_edS3Bucket = a})

instance FromJSON ExportDescription where
  parseJSON =
    withObject
      "ExportDescription"
      ( \x ->
          ExportDescription'
            <$> (x .:? "S3BucketOwner")
            <*> (x .:? "ExportFormat")
            <*> (x .:? "S3SseKmsKeyId")
            <*> (x .:? "ClientToken")
            <*> (x .:? "StartTime")
            <*> (x .:? "FailureCode")
            <*> (x .:? "ExportStatus")
            <*> (x .:? "FailureMessage")
            <*> (x .:? "TableArn")
            <*> (x .:? "BilledSizeBytes")
            <*> (x .:? "ExportArn")
            <*> (x .:? "ExportTime")
            <*> (x .:? "S3SseAlgorithm")
            <*> (x .:? "EndTime")
            <*> (x .:? "S3Prefix")
            <*> (x .:? "ExportManifest")
            <*> (x .:? "TableId")
            <*> (x .:? "ItemCount")
            <*> (x .:? "S3Bucket")
      )

instance Hashable ExportDescription

instance NFData ExportDescription
