{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.Types.KinesisSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DMS.Types.KinesisSettings where

import Network.AWS.DMS.Types.MessageFormatValue
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Provides information that describes an Amazon Kinesis Data Stream endpoint. This information includes the output format of records applied to the endpoint and details of transaction and control table data information.
--
--
--
-- /See:/ 'kinesisSettings' smart constructor.
data KinesisSettings = KinesisSettings'
  { _kIncludeTransactionDetails ::
      !(Maybe Bool),
    _kIncludeTableAlterOperations :: !(Maybe Bool),
    _kServiceAccessRoleARN :: !(Maybe Text),
    _kPartitionIncludeSchemaTable :: !(Maybe Bool),
    _kStreamARN :: !(Maybe Text),
    _kIncludeControlDetails :: !(Maybe Bool),
    _kIncludePartitionValue :: !(Maybe Bool),
    _kMessageFormat :: !(Maybe MessageFormatValue),
    _kIncludeNullAndEmpty :: !(Maybe Bool)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'KinesisSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'kIncludeTransactionDetails' - Provides detailed transaction information from the source database. This information includes a commit timestamp, a log position, and values for @transaction_id@ , previous @transaction_id@ , and @transaction_record_id@ (the record offset within a transaction). The default is @false@ .
--
-- * 'kIncludeTableAlterOperations' - Includes any data definition language (DDL) operations that change the table in the control data, such as @rename-table@ , @drop-table@ , @add-column@ , @drop-column@ , and @rename-column@ . The default is @false@ .
--
-- * 'kServiceAccessRoleARN' - The Amazon Resource Name (ARN) for the AWS Identity and Access Management (IAM) role that AWS DMS uses to write to the Kinesis data stream.
--
-- * 'kPartitionIncludeSchemaTable' - Prefixes schema and table names to partition values, when the partition type is @primary-key-type@ . Doing this increases data distribution among Kinesis shards. For example, suppose that a SysBench schema has thousands of tables and each table has only limited range for a primary key. In this case, the same primary key is sent from thousands of tables to the same shard, which causes throttling. The default is @false@ .
--
-- * 'kStreamARN' - The Amazon Resource Name (ARN) for the Amazon Kinesis Data Streams endpoint.
--
-- * 'kIncludeControlDetails' - Shows detailed control information for table definition, column definition, and table and column changes in the Kinesis message output. The default is @false@ .
--
-- * 'kIncludePartitionValue' - Shows the partition value within the Kinesis message output, unless the partition type is @schema-table-type@ . The default is @false@ .
--
-- * 'kMessageFormat' - The output format for the records created on the endpoint. The message format is @JSON@ (default) or @JSON_UNFORMATTED@ (a single line with no tab).
--
-- * 'kIncludeNullAndEmpty' - Include NULL and empty columns for records migrated to the endpoint. The default is @false@ .
kinesisSettings ::
  KinesisSettings
kinesisSettings =
  KinesisSettings'
    { _kIncludeTransactionDetails = Nothing,
      _kIncludeTableAlterOperations = Nothing,
      _kServiceAccessRoleARN = Nothing,
      _kPartitionIncludeSchemaTable = Nothing,
      _kStreamARN = Nothing,
      _kIncludeControlDetails = Nothing,
      _kIncludePartitionValue = Nothing,
      _kMessageFormat = Nothing,
      _kIncludeNullAndEmpty = Nothing
    }

-- | Provides detailed transaction information from the source database. This information includes a commit timestamp, a log position, and values for @transaction_id@ , previous @transaction_id@ , and @transaction_record_id@ (the record offset within a transaction). The default is @false@ .
kIncludeTransactionDetails :: Lens' KinesisSettings (Maybe Bool)
kIncludeTransactionDetails = lens _kIncludeTransactionDetails (\s a -> s {_kIncludeTransactionDetails = a})

-- | Includes any data definition language (DDL) operations that change the table in the control data, such as @rename-table@ , @drop-table@ , @add-column@ , @drop-column@ , and @rename-column@ . The default is @false@ .
kIncludeTableAlterOperations :: Lens' KinesisSettings (Maybe Bool)
kIncludeTableAlterOperations = lens _kIncludeTableAlterOperations (\s a -> s {_kIncludeTableAlterOperations = a})

-- | The Amazon Resource Name (ARN) for the AWS Identity and Access Management (IAM) role that AWS DMS uses to write to the Kinesis data stream.
kServiceAccessRoleARN :: Lens' KinesisSettings (Maybe Text)
kServiceAccessRoleARN = lens _kServiceAccessRoleARN (\s a -> s {_kServiceAccessRoleARN = a})

-- | Prefixes schema and table names to partition values, when the partition type is @primary-key-type@ . Doing this increases data distribution among Kinesis shards. For example, suppose that a SysBench schema has thousands of tables and each table has only limited range for a primary key. In this case, the same primary key is sent from thousands of tables to the same shard, which causes throttling. The default is @false@ .
kPartitionIncludeSchemaTable :: Lens' KinesisSettings (Maybe Bool)
kPartitionIncludeSchemaTable = lens _kPartitionIncludeSchemaTable (\s a -> s {_kPartitionIncludeSchemaTable = a})

-- | The Amazon Resource Name (ARN) for the Amazon Kinesis Data Streams endpoint.
kStreamARN :: Lens' KinesisSettings (Maybe Text)
kStreamARN = lens _kStreamARN (\s a -> s {_kStreamARN = a})

-- | Shows detailed control information for table definition, column definition, and table and column changes in the Kinesis message output. The default is @false@ .
kIncludeControlDetails :: Lens' KinesisSettings (Maybe Bool)
kIncludeControlDetails = lens _kIncludeControlDetails (\s a -> s {_kIncludeControlDetails = a})

-- | Shows the partition value within the Kinesis message output, unless the partition type is @schema-table-type@ . The default is @false@ .
kIncludePartitionValue :: Lens' KinesisSettings (Maybe Bool)
kIncludePartitionValue = lens _kIncludePartitionValue (\s a -> s {_kIncludePartitionValue = a})

-- | The output format for the records created on the endpoint. The message format is @JSON@ (default) or @JSON_UNFORMATTED@ (a single line with no tab).
kMessageFormat :: Lens' KinesisSettings (Maybe MessageFormatValue)
kMessageFormat = lens _kMessageFormat (\s a -> s {_kMessageFormat = a})

-- | Include NULL and empty columns for records migrated to the endpoint. The default is @false@ .
kIncludeNullAndEmpty :: Lens' KinesisSettings (Maybe Bool)
kIncludeNullAndEmpty = lens _kIncludeNullAndEmpty (\s a -> s {_kIncludeNullAndEmpty = a})

instance FromJSON KinesisSettings where
  parseJSON =
    withObject
      "KinesisSettings"
      ( \x ->
          KinesisSettings'
            <$> (x .:? "IncludeTransactionDetails")
            <*> (x .:? "IncludeTableAlterOperations")
            <*> (x .:? "ServiceAccessRoleArn")
            <*> (x .:? "PartitionIncludeSchemaTable")
            <*> (x .:? "StreamArn")
            <*> (x .:? "IncludeControlDetails")
            <*> (x .:? "IncludePartitionValue")
            <*> (x .:? "MessageFormat")
            <*> (x .:? "IncludeNullAndEmpty")
      )

instance Hashable KinesisSettings

instance NFData KinesisSettings

instance ToJSON KinesisSettings where
  toJSON KinesisSettings' {..} =
    object
      ( catMaybes
          [ ("IncludeTransactionDetails" .=) <$> _kIncludeTransactionDetails,
            ("IncludeTableAlterOperations" .=)
              <$> _kIncludeTableAlterOperations,
            ("ServiceAccessRoleArn" .=) <$> _kServiceAccessRoleARN,
            ("PartitionIncludeSchemaTable" .=)
              <$> _kPartitionIncludeSchemaTable,
            ("StreamArn" .=) <$> _kStreamARN,
            ("IncludeControlDetails" .=) <$> _kIncludeControlDetails,
            ("IncludePartitionValue" .=) <$> _kIncludePartitionValue,
            ("MessageFormat" .=) <$> _kMessageFormat,
            ("IncludeNullAndEmpty" .=) <$> _kIncludeNullAndEmpty
          ]
      )
