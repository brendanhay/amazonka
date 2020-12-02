{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.Types.KafkaSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DMS.Types.KafkaSettings where

import Network.AWS.DMS.Types.MessageFormatValue
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Provides information that describes an Apache Kafka endpoint. This information includes the output format of records applied to the endpoint and details of transaction and control table data information.
--
--
--
-- /See:/ 'kafkaSettings' smart constructor.
data KafkaSettings = KafkaSettings'
  { _ksIncludeTransactionDetails ::
      !(Maybe Bool),
    _ksIncludeTableAlterOperations :: !(Maybe Bool),
    _ksPartitionIncludeSchemaTable :: !(Maybe Bool),
    _ksTopic :: !(Maybe Text),
    _ksIncludeControlDetails :: !(Maybe Bool),
    _ksIncludePartitionValue :: !(Maybe Bool),
    _ksMessageFormat :: !(Maybe MessageFormatValue),
    _ksBroker :: !(Maybe Text),
    _ksMessageMaxBytes :: !(Maybe Int),
    _ksIncludeNullAndEmpty :: !(Maybe Bool)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'KafkaSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ksIncludeTransactionDetails' - Provides detailed transaction information from the source database. This information includes a commit timestamp, a log position, and values for @transaction_id@ , previous @transaction_id@ , and @transaction_record_id@ (the record offset within a transaction). The default is @false@ .
--
-- * 'ksIncludeTableAlterOperations' - Includes any data definition language (DDL) operations that change the table in the control data, such as @rename-table@ , @drop-table@ , @add-column@ , @drop-column@ , and @rename-column@ . The default is @false@ .
--
-- * 'ksPartitionIncludeSchemaTable' - Prefixes schema and table names to partition values, when the partition type is @primary-key-type@ . Doing this increases data distribution among Kafka partitions. For example, suppose that a SysBench schema has thousands of tables and each table has only limited range for a primary key. In this case, the same primary key is sent from thousands of tables to the same partition, which causes throttling. The default is @false@ .
--
-- * 'ksTopic' - The topic to which you migrate the data. If you don't specify a topic, AWS DMS specifies @"kafka-default-topic"@ as the migration topic.
--
-- * 'ksIncludeControlDetails' - Shows detailed control information for table definition, column definition, and table and column changes in the Kafka message output. The default is @false@ .
--
-- * 'ksIncludePartitionValue' - Shows the partition value within the Kafka message output, unless the partition type is @schema-table-type@ . The default is @false@ .
--
-- * 'ksMessageFormat' - The output format for the records created on the endpoint. The message format is @JSON@ (default) or @JSON_UNFORMATTED@ (a single line with no tab).
--
-- * 'ksBroker' - The broker location and port of the Kafka broker that hosts your Kafka instance. Specify the broker in the form @/broker-hostname-or-ip/ :/port/ @ . For example, @"ec2-12-345-678-901.compute-1.amazonaws.com:2345"@ .
--
-- * 'ksMessageMaxBytes' - The maximum size in bytes for records created on the endpoint The default is 1,000,000.
--
-- * 'ksIncludeNullAndEmpty' - Include NULL and empty columns for records migrated to the endpoint. The default is @false@ .
kafkaSettings ::
  KafkaSettings
kafkaSettings =
  KafkaSettings'
    { _ksIncludeTransactionDetails = Nothing,
      _ksIncludeTableAlterOperations = Nothing,
      _ksPartitionIncludeSchemaTable = Nothing,
      _ksTopic = Nothing,
      _ksIncludeControlDetails = Nothing,
      _ksIncludePartitionValue = Nothing,
      _ksMessageFormat = Nothing,
      _ksBroker = Nothing,
      _ksMessageMaxBytes = Nothing,
      _ksIncludeNullAndEmpty = Nothing
    }

-- | Provides detailed transaction information from the source database. This information includes a commit timestamp, a log position, and values for @transaction_id@ , previous @transaction_id@ , and @transaction_record_id@ (the record offset within a transaction). The default is @false@ .
ksIncludeTransactionDetails :: Lens' KafkaSettings (Maybe Bool)
ksIncludeTransactionDetails = lens _ksIncludeTransactionDetails (\s a -> s {_ksIncludeTransactionDetails = a})

-- | Includes any data definition language (DDL) operations that change the table in the control data, such as @rename-table@ , @drop-table@ , @add-column@ , @drop-column@ , and @rename-column@ . The default is @false@ .
ksIncludeTableAlterOperations :: Lens' KafkaSettings (Maybe Bool)
ksIncludeTableAlterOperations = lens _ksIncludeTableAlterOperations (\s a -> s {_ksIncludeTableAlterOperations = a})

-- | Prefixes schema and table names to partition values, when the partition type is @primary-key-type@ . Doing this increases data distribution among Kafka partitions. For example, suppose that a SysBench schema has thousands of tables and each table has only limited range for a primary key. In this case, the same primary key is sent from thousands of tables to the same partition, which causes throttling. The default is @false@ .
ksPartitionIncludeSchemaTable :: Lens' KafkaSettings (Maybe Bool)
ksPartitionIncludeSchemaTable = lens _ksPartitionIncludeSchemaTable (\s a -> s {_ksPartitionIncludeSchemaTable = a})

-- | The topic to which you migrate the data. If you don't specify a topic, AWS DMS specifies @"kafka-default-topic"@ as the migration topic.
ksTopic :: Lens' KafkaSettings (Maybe Text)
ksTopic = lens _ksTopic (\s a -> s {_ksTopic = a})

-- | Shows detailed control information for table definition, column definition, and table and column changes in the Kafka message output. The default is @false@ .
ksIncludeControlDetails :: Lens' KafkaSettings (Maybe Bool)
ksIncludeControlDetails = lens _ksIncludeControlDetails (\s a -> s {_ksIncludeControlDetails = a})

-- | Shows the partition value within the Kafka message output, unless the partition type is @schema-table-type@ . The default is @false@ .
ksIncludePartitionValue :: Lens' KafkaSettings (Maybe Bool)
ksIncludePartitionValue = lens _ksIncludePartitionValue (\s a -> s {_ksIncludePartitionValue = a})

-- | The output format for the records created on the endpoint. The message format is @JSON@ (default) or @JSON_UNFORMATTED@ (a single line with no tab).
ksMessageFormat :: Lens' KafkaSettings (Maybe MessageFormatValue)
ksMessageFormat = lens _ksMessageFormat (\s a -> s {_ksMessageFormat = a})

-- | The broker location and port of the Kafka broker that hosts your Kafka instance. Specify the broker in the form @/broker-hostname-or-ip/ :/port/ @ . For example, @"ec2-12-345-678-901.compute-1.amazonaws.com:2345"@ .
ksBroker :: Lens' KafkaSettings (Maybe Text)
ksBroker = lens _ksBroker (\s a -> s {_ksBroker = a})

-- | The maximum size in bytes for records created on the endpoint The default is 1,000,000.
ksMessageMaxBytes :: Lens' KafkaSettings (Maybe Int)
ksMessageMaxBytes = lens _ksMessageMaxBytes (\s a -> s {_ksMessageMaxBytes = a})

-- | Include NULL and empty columns for records migrated to the endpoint. The default is @false@ .
ksIncludeNullAndEmpty :: Lens' KafkaSettings (Maybe Bool)
ksIncludeNullAndEmpty = lens _ksIncludeNullAndEmpty (\s a -> s {_ksIncludeNullAndEmpty = a})

instance FromJSON KafkaSettings where
  parseJSON =
    withObject
      "KafkaSettings"
      ( \x ->
          KafkaSettings'
            <$> (x .:? "IncludeTransactionDetails")
            <*> (x .:? "IncludeTableAlterOperations")
            <*> (x .:? "PartitionIncludeSchemaTable")
            <*> (x .:? "Topic")
            <*> (x .:? "IncludeControlDetails")
            <*> (x .:? "IncludePartitionValue")
            <*> (x .:? "MessageFormat")
            <*> (x .:? "Broker")
            <*> (x .:? "MessageMaxBytes")
            <*> (x .:? "IncludeNullAndEmpty")
      )

instance Hashable KafkaSettings

instance NFData KafkaSettings

instance ToJSON KafkaSettings where
  toJSON KafkaSettings' {..} =
    object
      ( catMaybes
          [ ("IncludeTransactionDetails" .=) <$> _ksIncludeTransactionDetails,
            ("IncludeTableAlterOperations" .=)
              <$> _ksIncludeTableAlterOperations,
            ("PartitionIncludeSchemaTable" .=)
              <$> _ksPartitionIncludeSchemaTable,
            ("Topic" .=) <$> _ksTopic,
            ("IncludeControlDetails" .=) <$> _ksIncludeControlDetails,
            ("IncludePartitionValue" .=) <$> _ksIncludePartitionValue,
            ("MessageFormat" .=) <$> _ksMessageFormat,
            ("Broker" .=) <$> _ksBroker,
            ("MessageMaxBytes" .=) <$> _ksMessageMaxBytes,
            ("IncludeNullAndEmpty" .=) <$> _ksIncludeNullAndEmpty
          ]
      )
