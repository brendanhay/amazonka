{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.DMS.Types.KafkaSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DMS.Types.KafkaSettings where

import Network.AWS.DMS.Types.MessageFormatValue
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Provides information that describes an Apache Kafka endpoint. This
-- information includes the output format of records applied to the
-- endpoint and details of transaction and control table data information.
--
-- /See:/ 'newKafkaSettings' smart constructor.
data KafkaSettings = KafkaSettings'
  { -- | Include NULL and empty columns for records migrated to the endpoint. The
    -- default is @false@.
    includeNullAndEmpty :: Prelude.Maybe Prelude.Bool,
    -- | The output format for the records created on the endpoint. The message
    -- format is @JSON@ (default) or @JSON_UNFORMATTED@ (a single line with no
    -- tab).
    messageFormat :: Prelude.Maybe MessageFormatValue,
    -- | Prefixes schema and table names to partition values, when the partition
    -- type is @primary-key-type@. Doing this increases data distribution among
    -- Kafka partitions. For example, suppose that a SysBench schema has
    -- thousands of tables and each table has only limited range for a primary
    -- key. In this case, the same primary key is sent from thousands of tables
    -- to the same partition, which causes throttling. The default is @false@.
    partitionIncludeSchemaTable :: Prelude.Maybe Prelude.Bool,
    -- | Shows detailed control information for table definition, column
    -- definition, and table and column changes in the Kafka message output.
    -- The default is @false@.
    includeControlDetails :: Prelude.Maybe Prelude.Bool,
    -- | The topic to which you migrate the data. If you don\'t specify a topic,
    -- AWS DMS specifies @\"kafka-default-topic\"@ as the migration topic.
    topic :: Prelude.Maybe Prelude.Text,
    -- | The maximum size in bytes for records created on the endpoint The
    -- default is 1,000,000.
    messageMaxBytes :: Prelude.Maybe Prelude.Int,
    -- | The broker location and port of the Kafka broker that hosts your Kafka
    -- instance. Specify the broker in the form @ broker-hostname-or-ip:port @.
    -- For example, @\"ec2-12-345-678-901.compute-1.amazonaws.com:2345\"@.
    broker :: Prelude.Maybe Prelude.Text,
    -- | Shows the partition value within the Kafka message output, unless the
    -- partition type is @schema-table-type@. The default is @false@.
    includePartitionValue :: Prelude.Maybe Prelude.Bool,
    -- | Provides detailed transaction information from the source database. This
    -- information includes a commit timestamp, a log position, and values for
    -- @transaction_id@, previous @transaction_id@, and @transaction_record_id@
    -- (the record offset within a transaction). The default is @false@.
    includeTransactionDetails :: Prelude.Maybe Prelude.Bool,
    -- | Includes any data definition language (DDL) operations that change the
    -- table in the control data, such as @rename-table@, @drop-table@,
    -- @add-column@, @drop-column@, and @rename-column@. The default is
    -- @false@.
    includeTableAlterOperations :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'KafkaSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'includeNullAndEmpty', 'kafkaSettings_includeNullAndEmpty' - Include NULL and empty columns for records migrated to the endpoint. The
-- default is @false@.
--
-- 'messageFormat', 'kafkaSettings_messageFormat' - The output format for the records created on the endpoint. The message
-- format is @JSON@ (default) or @JSON_UNFORMATTED@ (a single line with no
-- tab).
--
-- 'partitionIncludeSchemaTable', 'kafkaSettings_partitionIncludeSchemaTable' - Prefixes schema and table names to partition values, when the partition
-- type is @primary-key-type@. Doing this increases data distribution among
-- Kafka partitions. For example, suppose that a SysBench schema has
-- thousands of tables and each table has only limited range for a primary
-- key. In this case, the same primary key is sent from thousands of tables
-- to the same partition, which causes throttling. The default is @false@.
--
-- 'includeControlDetails', 'kafkaSettings_includeControlDetails' - Shows detailed control information for table definition, column
-- definition, and table and column changes in the Kafka message output.
-- The default is @false@.
--
-- 'topic', 'kafkaSettings_topic' - The topic to which you migrate the data. If you don\'t specify a topic,
-- AWS DMS specifies @\"kafka-default-topic\"@ as the migration topic.
--
-- 'messageMaxBytes', 'kafkaSettings_messageMaxBytes' - The maximum size in bytes for records created on the endpoint The
-- default is 1,000,000.
--
-- 'broker', 'kafkaSettings_broker' - The broker location and port of the Kafka broker that hosts your Kafka
-- instance. Specify the broker in the form @ broker-hostname-or-ip:port @.
-- For example, @\"ec2-12-345-678-901.compute-1.amazonaws.com:2345\"@.
--
-- 'includePartitionValue', 'kafkaSettings_includePartitionValue' - Shows the partition value within the Kafka message output, unless the
-- partition type is @schema-table-type@. The default is @false@.
--
-- 'includeTransactionDetails', 'kafkaSettings_includeTransactionDetails' - Provides detailed transaction information from the source database. This
-- information includes a commit timestamp, a log position, and values for
-- @transaction_id@, previous @transaction_id@, and @transaction_record_id@
-- (the record offset within a transaction). The default is @false@.
--
-- 'includeTableAlterOperations', 'kafkaSettings_includeTableAlterOperations' - Includes any data definition language (DDL) operations that change the
-- table in the control data, such as @rename-table@, @drop-table@,
-- @add-column@, @drop-column@, and @rename-column@. The default is
-- @false@.
newKafkaSettings ::
  KafkaSettings
newKafkaSettings =
  KafkaSettings'
    { includeNullAndEmpty =
        Prelude.Nothing,
      messageFormat = Prelude.Nothing,
      partitionIncludeSchemaTable = Prelude.Nothing,
      includeControlDetails = Prelude.Nothing,
      topic = Prelude.Nothing,
      messageMaxBytes = Prelude.Nothing,
      broker = Prelude.Nothing,
      includePartitionValue = Prelude.Nothing,
      includeTransactionDetails = Prelude.Nothing,
      includeTableAlterOperations = Prelude.Nothing
    }

-- | Include NULL and empty columns for records migrated to the endpoint. The
-- default is @false@.
kafkaSettings_includeNullAndEmpty :: Lens.Lens' KafkaSettings (Prelude.Maybe Prelude.Bool)
kafkaSettings_includeNullAndEmpty = Lens.lens (\KafkaSettings' {includeNullAndEmpty} -> includeNullAndEmpty) (\s@KafkaSettings' {} a -> s {includeNullAndEmpty = a} :: KafkaSettings)

-- | The output format for the records created on the endpoint. The message
-- format is @JSON@ (default) or @JSON_UNFORMATTED@ (a single line with no
-- tab).
kafkaSettings_messageFormat :: Lens.Lens' KafkaSettings (Prelude.Maybe MessageFormatValue)
kafkaSettings_messageFormat = Lens.lens (\KafkaSettings' {messageFormat} -> messageFormat) (\s@KafkaSettings' {} a -> s {messageFormat = a} :: KafkaSettings)

-- | Prefixes schema and table names to partition values, when the partition
-- type is @primary-key-type@. Doing this increases data distribution among
-- Kafka partitions. For example, suppose that a SysBench schema has
-- thousands of tables and each table has only limited range for a primary
-- key. In this case, the same primary key is sent from thousands of tables
-- to the same partition, which causes throttling. The default is @false@.
kafkaSettings_partitionIncludeSchemaTable :: Lens.Lens' KafkaSettings (Prelude.Maybe Prelude.Bool)
kafkaSettings_partitionIncludeSchemaTable = Lens.lens (\KafkaSettings' {partitionIncludeSchemaTable} -> partitionIncludeSchemaTable) (\s@KafkaSettings' {} a -> s {partitionIncludeSchemaTable = a} :: KafkaSettings)

-- | Shows detailed control information for table definition, column
-- definition, and table and column changes in the Kafka message output.
-- The default is @false@.
kafkaSettings_includeControlDetails :: Lens.Lens' KafkaSettings (Prelude.Maybe Prelude.Bool)
kafkaSettings_includeControlDetails = Lens.lens (\KafkaSettings' {includeControlDetails} -> includeControlDetails) (\s@KafkaSettings' {} a -> s {includeControlDetails = a} :: KafkaSettings)

-- | The topic to which you migrate the data. If you don\'t specify a topic,
-- AWS DMS specifies @\"kafka-default-topic\"@ as the migration topic.
kafkaSettings_topic :: Lens.Lens' KafkaSettings (Prelude.Maybe Prelude.Text)
kafkaSettings_topic = Lens.lens (\KafkaSettings' {topic} -> topic) (\s@KafkaSettings' {} a -> s {topic = a} :: KafkaSettings)

-- | The maximum size in bytes for records created on the endpoint The
-- default is 1,000,000.
kafkaSettings_messageMaxBytes :: Lens.Lens' KafkaSettings (Prelude.Maybe Prelude.Int)
kafkaSettings_messageMaxBytes = Lens.lens (\KafkaSettings' {messageMaxBytes} -> messageMaxBytes) (\s@KafkaSettings' {} a -> s {messageMaxBytes = a} :: KafkaSettings)

-- | The broker location and port of the Kafka broker that hosts your Kafka
-- instance. Specify the broker in the form @ broker-hostname-or-ip:port @.
-- For example, @\"ec2-12-345-678-901.compute-1.amazonaws.com:2345\"@.
kafkaSettings_broker :: Lens.Lens' KafkaSettings (Prelude.Maybe Prelude.Text)
kafkaSettings_broker = Lens.lens (\KafkaSettings' {broker} -> broker) (\s@KafkaSettings' {} a -> s {broker = a} :: KafkaSettings)

-- | Shows the partition value within the Kafka message output, unless the
-- partition type is @schema-table-type@. The default is @false@.
kafkaSettings_includePartitionValue :: Lens.Lens' KafkaSettings (Prelude.Maybe Prelude.Bool)
kafkaSettings_includePartitionValue = Lens.lens (\KafkaSettings' {includePartitionValue} -> includePartitionValue) (\s@KafkaSettings' {} a -> s {includePartitionValue = a} :: KafkaSettings)

-- | Provides detailed transaction information from the source database. This
-- information includes a commit timestamp, a log position, and values for
-- @transaction_id@, previous @transaction_id@, and @transaction_record_id@
-- (the record offset within a transaction). The default is @false@.
kafkaSettings_includeTransactionDetails :: Lens.Lens' KafkaSettings (Prelude.Maybe Prelude.Bool)
kafkaSettings_includeTransactionDetails = Lens.lens (\KafkaSettings' {includeTransactionDetails} -> includeTransactionDetails) (\s@KafkaSettings' {} a -> s {includeTransactionDetails = a} :: KafkaSettings)

-- | Includes any data definition language (DDL) operations that change the
-- table in the control data, such as @rename-table@, @drop-table@,
-- @add-column@, @drop-column@, and @rename-column@. The default is
-- @false@.
kafkaSettings_includeTableAlterOperations :: Lens.Lens' KafkaSettings (Prelude.Maybe Prelude.Bool)
kafkaSettings_includeTableAlterOperations = Lens.lens (\KafkaSettings' {includeTableAlterOperations} -> includeTableAlterOperations) (\s@KafkaSettings' {} a -> s {includeTableAlterOperations = a} :: KafkaSettings)

instance Prelude.FromJSON KafkaSettings where
  parseJSON =
    Prelude.withObject
      "KafkaSettings"
      ( \x ->
          KafkaSettings'
            Prelude.<$> (x Prelude..:? "IncludeNullAndEmpty")
            Prelude.<*> (x Prelude..:? "MessageFormat")
            Prelude.<*> (x Prelude..:? "PartitionIncludeSchemaTable")
            Prelude.<*> (x Prelude..:? "IncludeControlDetails")
            Prelude.<*> (x Prelude..:? "Topic")
            Prelude.<*> (x Prelude..:? "MessageMaxBytes")
            Prelude.<*> (x Prelude..:? "Broker")
            Prelude.<*> (x Prelude..:? "IncludePartitionValue")
            Prelude.<*> (x Prelude..:? "IncludeTransactionDetails")
            Prelude.<*> (x Prelude..:? "IncludeTableAlterOperations")
      )

instance Prelude.Hashable KafkaSettings

instance Prelude.NFData KafkaSettings

instance Prelude.ToJSON KafkaSettings where
  toJSON KafkaSettings' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("IncludeNullAndEmpty" Prelude..=)
              Prelude.<$> includeNullAndEmpty,
            ("MessageFormat" Prelude..=)
              Prelude.<$> messageFormat,
            ("PartitionIncludeSchemaTable" Prelude..=)
              Prelude.<$> partitionIncludeSchemaTable,
            ("IncludeControlDetails" Prelude..=)
              Prelude.<$> includeControlDetails,
            ("Topic" Prelude..=) Prelude.<$> topic,
            ("MessageMaxBytes" Prelude..=)
              Prelude.<$> messageMaxBytes,
            ("Broker" Prelude..=) Prelude.<$> broker,
            ("IncludePartitionValue" Prelude..=)
              Prelude.<$> includePartitionValue,
            ("IncludeTransactionDetails" Prelude..=)
              Prelude.<$> includeTransactionDetails,
            ("IncludeTableAlterOperations" Prelude..=)
              Prelude.<$> includeTableAlterOperations
          ]
      )
