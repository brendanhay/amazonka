{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.ExportTask
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.ExportTask where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains the details of a snapshot export to Amazon S3.
--
--
-- This data type is used as a response element in the @DescribeExportTasks@ action.
--
--
-- /See:/ 'exportTask' smart constructor.
data ExportTask = ExportTask'
  { _etTotalExtractedDataInGB ::
      !(Maybe Int),
    _etStatus :: !(Maybe Text),
    _etIAMRoleARN :: !(Maybe Text),
    _etSourceARN :: !(Maybe Text),
    _etExportOnly :: !(Maybe [Text]),
    _etTaskStartTime :: !(Maybe ISO8601),
    _etWarningMessage :: !(Maybe Text),
    _etSnapshotTime :: !(Maybe ISO8601),
    _etKMSKeyId :: !(Maybe Text),
    _etTaskEndTime :: !(Maybe ISO8601),
    _etExportTaskIdentifier :: !(Maybe Text),
    _etS3Prefix :: !(Maybe Text),
    _etPercentProgress :: !(Maybe Int),
    _etS3Bucket :: !(Maybe Text),
    _etFailureCause :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ExportTask' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'etTotalExtractedDataInGB' - The total amount of data exported, in gigabytes.
--
-- * 'etStatus' - The progress status of the export task.
--
-- * 'etIAMRoleARN' - The name of the IAM role that is used to write to Amazon S3 when exporting a snapshot.
--
-- * 'etSourceARN' - The Amazon Resource Name (ARN) of the snapshot exported to Amazon S3.
--
-- * 'etExportOnly' - The data exported from the snapshot. Valid values are the following:     * @database@ - Export all the data from a specified database.     * @database.table@ /table-name/ - Export a table of the snapshot. This format is valid only for RDS for MySQL, RDS for MariaDB, and Aurora MySQL.     * @database.schema@ /schema-name/ - Export a database schema of the snapshot. This format is valid only for RDS for PostgreSQL and Aurora PostgreSQL.     * @database.schema.table@ /table-name/ - Export a table of the database schema. This format is valid only for RDS for PostgreSQL and Aurora PostgreSQL.
--
-- * 'etTaskStartTime' - The time that the snapshot export task started.
--
-- * 'etWarningMessage' - A warning about the snapshot export task.
--
-- * 'etSnapshotTime' - The time that the snapshot was created.
--
-- * 'etKMSKeyId' - The ID of the AWS KMS key that is used to encrypt the snapshot when it's exported to Amazon S3. The KMS key ID is the Amazon Resource Name (ARN), the KMS key identifier, or the KMS key alias for the KMS encryption key. The IAM role used for the snapshot export must have encryption and decryption permissions to use this KMS key.
--
-- * 'etTaskEndTime' - The time that the snapshot export task completed.
--
-- * 'etExportTaskIdentifier' - A unique identifier for the snapshot export task. This ID isn't an identifier for the Amazon S3 bucket where the snapshot is exported to.
--
-- * 'etS3Prefix' - The Amazon S3 bucket prefix that is the file name and path of the exported snapshot.
--
-- * 'etPercentProgress' - The progress of the snapshot export task as a percentage.
--
-- * 'etS3Bucket' - The Amazon S3 bucket that the snapshot is exported to.
--
-- * 'etFailureCause' - The reason the export failed, if it failed.
exportTask ::
  ExportTask
exportTask =
  ExportTask'
    { _etTotalExtractedDataInGB = Nothing,
      _etStatus = Nothing,
      _etIAMRoleARN = Nothing,
      _etSourceARN = Nothing,
      _etExportOnly = Nothing,
      _etTaskStartTime = Nothing,
      _etWarningMessage = Nothing,
      _etSnapshotTime = Nothing,
      _etKMSKeyId = Nothing,
      _etTaskEndTime = Nothing,
      _etExportTaskIdentifier = Nothing,
      _etS3Prefix = Nothing,
      _etPercentProgress = Nothing,
      _etS3Bucket = Nothing,
      _etFailureCause = Nothing
    }

-- | The total amount of data exported, in gigabytes.
etTotalExtractedDataInGB :: Lens' ExportTask (Maybe Int)
etTotalExtractedDataInGB = lens _etTotalExtractedDataInGB (\s a -> s {_etTotalExtractedDataInGB = a})

-- | The progress status of the export task.
etStatus :: Lens' ExportTask (Maybe Text)
etStatus = lens _etStatus (\s a -> s {_etStatus = a})

-- | The name of the IAM role that is used to write to Amazon S3 when exporting a snapshot.
etIAMRoleARN :: Lens' ExportTask (Maybe Text)
etIAMRoleARN = lens _etIAMRoleARN (\s a -> s {_etIAMRoleARN = a})

-- | The Amazon Resource Name (ARN) of the snapshot exported to Amazon S3.
etSourceARN :: Lens' ExportTask (Maybe Text)
etSourceARN = lens _etSourceARN (\s a -> s {_etSourceARN = a})

-- | The data exported from the snapshot. Valid values are the following:     * @database@ - Export all the data from a specified database.     * @database.table@ /table-name/ - Export a table of the snapshot. This format is valid only for RDS for MySQL, RDS for MariaDB, and Aurora MySQL.     * @database.schema@ /schema-name/ - Export a database schema of the snapshot. This format is valid only for RDS for PostgreSQL and Aurora PostgreSQL.     * @database.schema.table@ /table-name/ - Export a table of the database schema. This format is valid only for RDS for PostgreSQL and Aurora PostgreSQL.
etExportOnly :: Lens' ExportTask [Text]
etExportOnly = lens _etExportOnly (\s a -> s {_etExportOnly = a}) . _Default . _Coerce

-- | The time that the snapshot export task started.
etTaskStartTime :: Lens' ExportTask (Maybe UTCTime)
etTaskStartTime = lens _etTaskStartTime (\s a -> s {_etTaskStartTime = a}) . mapping _Time

-- | A warning about the snapshot export task.
etWarningMessage :: Lens' ExportTask (Maybe Text)
etWarningMessage = lens _etWarningMessage (\s a -> s {_etWarningMessage = a})

-- | The time that the snapshot was created.
etSnapshotTime :: Lens' ExportTask (Maybe UTCTime)
etSnapshotTime = lens _etSnapshotTime (\s a -> s {_etSnapshotTime = a}) . mapping _Time

-- | The ID of the AWS KMS key that is used to encrypt the snapshot when it's exported to Amazon S3. The KMS key ID is the Amazon Resource Name (ARN), the KMS key identifier, or the KMS key alias for the KMS encryption key. The IAM role used for the snapshot export must have encryption and decryption permissions to use this KMS key.
etKMSKeyId :: Lens' ExportTask (Maybe Text)
etKMSKeyId = lens _etKMSKeyId (\s a -> s {_etKMSKeyId = a})

-- | The time that the snapshot export task completed.
etTaskEndTime :: Lens' ExportTask (Maybe UTCTime)
etTaskEndTime = lens _etTaskEndTime (\s a -> s {_etTaskEndTime = a}) . mapping _Time

-- | A unique identifier for the snapshot export task. This ID isn't an identifier for the Amazon S3 bucket where the snapshot is exported to.
etExportTaskIdentifier :: Lens' ExportTask (Maybe Text)
etExportTaskIdentifier = lens _etExportTaskIdentifier (\s a -> s {_etExportTaskIdentifier = a})

-- | The Amazon S3 bucket prefix that is the file name and path of the exported snapshot.
etS3Prefix :: Lens' ExportTask (Maybe Text)
etS3Prefix = lens _etS3Prefix (\s a -> s {_etS3Prefix = a})

-- | The progress of the snapshot export task as a percentage.
etPercentProgress :: Lens' ExportTask (Maybe Int)
etPercentProgress = lens _etPercentProgress (\s a -> s {_etPercentProgress = a})

-- | The Amazon S3 bucket that the snapshot is exported to.
etS3Bucket :: Lens' ExportTask (Maybe Text)
etS3Bucket = lens _etS3Bucket (\s a -> s {_etS3Bucket = a})

-- | The reason the export failed, if it failed.
etFailureCause :: Lens' ExportTask (Maybe Text)
etFailureCause = lens _etFailureCause (\s a -> s {_etFailureCause = a})

instance FromXML ExportTask where
  parseXML x =
    ExportTask'
      <$> (x .@? "TotalExtractedDataInGB")
      <*> (x .@? "Status")
      <*> (x .@? "IamRoleArn")
      <*> (x .@? "SourceArn")
      <*> (x .@? "ExportOnly" .!@ mempty >>= may (parseXMLList "member"))
      <*> (x .@? "TaskStartTime")
      <*> (x .@? "WarningMessage")
      <*> (x .@? "SnapshotTime")
      <*> (x .@? "KmsKeyId")
      <*> (x .@? "TaskEndTime")
      <*> (x .@? "ExportTaskIdentifier")
      <*> (x .@? "S3Prefix")
      <*> (x .@? "PercentProgress")
      <*> (x .@? "S3Bucket")
      <*> (x .@? "FailureCause")

instance Hashable ExportTask

instance NFData ExportTask
