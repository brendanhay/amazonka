{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.StartExportTask
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts an export of a snapshot to Amazon S3. The provided IAM role must have access to the S3 bucket.
module Network.AWS.RDS.StartExportTask
  ( -- * Creating a Request
    startExportTask,
    StartExportTask,

    -- * Request Lenses
    setExportOnly,
    setS3Prefix,
    setExportTaskIdentifier,
    setSourceARN,
    setS3BucketName,
    setIAMRoleARN,
    setKMSKeyId,

    -- * Destructuring the Response
    exportTask,
    ExportTask,

    -- * Response Lenses
    etTotalExtractedDataInGB,
    etStatus,
    etIAMRoleARN,
    etSourceARN,
    etExportOnly,
    etTaskStartTime,
    etWarningMessage,
    etSnapshotTime,
    etKMSKeyId,
    etTaskEndTime,
    etExportTaskIdentifier,
    etS3Prefix,
    etPercentProgress,
    etS3Bucket,
    etFailureCause,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.RDS.Types
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'startExportTask' smart constructor.
data StartExportTask = StartExportTask'
  { _setExportOnly ::
      !(Maybe [Text]),
    _setS3Prefix :: !(Maybe Text),
    _setExportTaskIdentifier :: !Text,
    _setSourceARN :: !Text,
    _setS3BucketName :: !Text,
    _setIAMRoleARN :: !Text,
    _setKMSKeyId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'StartExportTask' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'setExportOnly' - The data to be exported from the snapshot. If this parameter is not provided, all the snapshot data is exported. Valid values are the following:     * @database@ - Export all the data from a specified database.     * @database.table@ /table-name/ - Export a table of the snapshot. This format is valid only for RDS for MySQL, RDS for MariaDB, and Aurora MySQL.     * @database.schema@ /schema-name/ - Export a database schema of the snapshot. This format is valid only for RDS for PostgreSQL and Aurora PostgreSQL.     * @database.schema.table@ /table-name/ - Export a table of the database schema. This format is valid only for RDS for PostgreSQL and Aurora PostgreSQL.
--
-- * 'setS3Prefix' - The Amazon S3 bucket prefix to use as the file name and path of the exported snapshot.
--
-- * 'setExportTaskIdentifier' - A unique identifier for the snapshot export task. This ID isn't an identifier for the Amazon S3 bucket where the snapshot is to be exported to.
--
-- * 'setSourceARN' - The Amazon Resource Name (ARN) of the snapshot to export to Amazon S3.
--
-- * 'setS3BucketName' - The name of the Amazon S3 bucket to export the snapshot to.
--
-- * 'setIAMRoleARN' - The name of the IAM role to use for writing to the Amazon S3 bucket when exporting a snapshot.
--
-- * 'setKMSKeyId' - The ID of the AWS KMS key to use to encrypt the snapshot exported to Amazon S3. The KMS key ID is the Amazon Resource Name (ARN), the KMS key identifier, or the KMS key alias for the KMS encryption key. The caller of this operation must be authorized to execute the following operations. These can be set in the KMS key policy:      * GrantOperation.Encrypt     * GrantOperation.Decrypt     * GrantOperation.GenerateDataKey     * GrantOperation.GenerateDataKeyWithoutPlaintext     * GrantOperation.ReEncryptFrom     * GrantOperation.ReEncryptTo     * GrantOperation.CreateGrant     * GrantOperation.DescribeKey     * GrantOperation.RetireGrant
startExportTask ::
  -- | 'setExportTaskIdentifier'
  Text ->
  -- | 'setSourceARN'
  Text ->
  -- | 'setS3BucketName'
  Text ->
  -- | 'setIAMRoleARN'
  Text ->
  -- | 'setKMSKeyId'
  Text ->
  StartExportTask
startExportTask
  pExportTaskIdentifier_
  pSourceARN_
  pS3BucketName_
  pIAMRoleARN_
  pKMSKeyId_ =
    StartExportTask'
      { _setExportOnly = Nothing,
        _setS3Prefix = Nothing,
        _setExportTaskIdentifier = pExportTaskIdentifier_,
        _setSourceARN = pSourceARN_,
        _setS3BucketName = pS3BucketName_,
        _setIAMRoleARN = pIAMRoleARN_,
        _setKMSKeyId = pKMSKeyId_
      }

-- | The data to be exported from the snapshot. If this parameter is not provided, all the snapshot data is exported. Valid values are the following:     * @database@ - Export all the data from a specified database.     * @database.table@ /table-name/ - Export a table of the snapshot. This format is valid only for RDS for MySQL, RDS for MariaDB, and Aurora MySQL.     * @database.schema@ /schema-name/ - Export a database schema of the snapshot. This format is valid only for RDS for PostgreSQL and Aurora PostgreSQL.     * @database.schema.table@ /table-name/ - Export a table of the database schema. This format is valid only for RDS for PostgreSQL and Aurora PostgreSQL.
setExportOnly :: Lens' StartExportTask [Text]
setExportOnly = lens _setExportOnly (\s a -> s {_setExportOnly = a}) . _Default . _Coerce

-- | The Amazon S3 bucket prefix to use as the file name and path of the exported snapshot.
setS3Prefix :: Lens' StartExportTask (Maybe Text)
setS3Prefix = lens _setS3Prefix (\s a -> s {_setS3Prefix = a})

-- | A unique identifier for the snapshot export task. This ID isn't an identifier for the Amazon S3 bucket where the snapshot is to be exported to.
setExportTaskIdentifier :: Lens' StartExportTask Text
setExportTaskIdentifier = lens _setExportTaskIdentifier (\s a -> s {_setExportTaskIdentifier = a})

-- | The Amazon Resource Name (ARN) of the snapshot to export to Amazon S3.
setSourceARN :: Lens' StartExportTask Text
setSourceARN = lens _setSourceARN (\s a -> s {_setSourceARN = a})

-- | The name of the Amazon S3 bucket to export the snapshot to.
setS3BucketName :: Lens' StartExportTask Text
setS3BucketName = lens _setS3BucketName (\s a -> s {_setS3BucketName = a})

-- | The name of the IAM role to use for writing to the Amazon S3 bucket when exporting a snapshot.
setIAMRoleARN :: Lens' StartExportTask Text
setIAMRoleARN = lens _setIAMRoleARN (\s a -> s {_setIAMRoleARN = a})

-- | The ID of the AWS KMS key to use to encrypt the snapshot exported to Amazon S3. The KMS key ID is the Amazon Resource Name (ARN), the KMS key identifier, or the KMS key alias for the KMS encryption key. The caller of this operation must be authorized to execute the following operations. These can be set in the KMS key policy:      * GrantOperation.Encrypt     * GrantOperation.Decrypt     * GrantOperation.GenerateDataKey     * GrantOperation.GenerateDataKeyWithoutPlaintext     * GrantOperation.ReEncryptFrom     * GrantOperation.ReEncryptTo     * GrantOperation.CreateGrant     * GrantOperation.DescribeKey     * GrantOperation.RetireGrant
setKMSKeyId :: Lens' StartExportTask Text
setKMSKeyId = lens _setKMSKeyId (\s a -> s {_setKMSKeyId = a})

instance AWSRequest StartExportTask where
  type Rs StartExportTask = ExportTask
  request = postQuery rds
  response =
    receiveXMLWrapper "StartExportTaskResult" (\s h x -> parseXML x)

instance Hashable StartExportTask

instance NFData StartExportTask

instance ToHeaders StartExportTask where
  toHeaders = const mempty

instance ToPath StartExportTask where
  toPath = const "/"

instance ToQuery StartExportTask where
  toQuery StartExportTask' {..} =
    mconcat
      [ "Action" =: ("StartExportTask" :: ByteString),
        "Version" =: ("2014-10-31" :: ByteString),
        "ExportOnly" =: toQuery (toQueryList "member" <$> _setExportOnly),
        "S3Prefix" =: _setS3Prefix,
        "ExportTaskIdentifier" =: _setExportTaskIdentifier,
        "SourceArn" =: _setSourceARN,
        "S3BucketName" =: _setS3BucketName,
        "IamRoleArn" =: _setIAMRoleARN,
        "KmsKeyId" =: _setKMSKeyId
      ]
