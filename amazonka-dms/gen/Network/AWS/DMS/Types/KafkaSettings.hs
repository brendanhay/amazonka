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

import qualified Network.AWS.Core as Core
import Network.AWS.DMS.Types.MessageFormatValue
import qualified Network.AWS.Lens as Lens

-- | Provides information that describes an Apache Kafka endpoint. This
-- information includes the output format of records applied to the
-- endpoint and details of transaction and control table data information.
--
-- /See:/ 'newKafkaSettings' smart constructor.
data KafkaSettings = KafkaSettings'
  { -- | Include NULL and empty columns for records migrated to the endpoint. The
    -- default is @false@.
    includeNullAndEmpty :: Core.Maybe Core.Bool,
    -- | The output format for the records created on the endpoint. The message
    -- format is @JSON@ (default) or @JSON_UNFORMATTED@ (a single line with no
    -- tab).
    messageFormat :: Core.Maybe MessageFormatValue,
    -- | Prefixes schema and table names to partition values, when the partition
    -- type is @primary-key-type@. Doing this increases data distribution among
    -- Kafka partitions. For example, suppose that a SysBench schema has
    -- thousands of tables and each table has only limited range for a primary
    -- key. In this case, the same primary key is sent from thousands of tables
    -- to the same partition, which causes throttling. The default is @false@.
    partitionIncludeSchemaTable :: Core.Maybe Core.Bool,
    -- | Shows detailed control information for table definition, column
    -- definition, and table and column changes in the Kafka message output.
    -- The default is @false@.
    includeControlDetails :: Core.Maybe Core.Bool,
    -- | The topic to which you migrate the data. If you don\'t specify a topic,
    -- AWS DMS specifies @\"kafka-default-topic\"@ as the migration topic.
    topic :: Core.Maybe Core.Text,
    -- | The maximum size in bytes for records created on the endpoint The
    -- default is 1,000,000.
    messageMaxBytes :: Core.Maybe Core.Int,
    -- | The broker location and port of the Kafka broker that hosts your Kafka
    -- instance. Specify the broker in the form @ broker-hostname-or-ip:port @.
    -- For example, @\"ec2-12-345-678-901.compute-1.amazonaws.com:2345\"@.
    broker :: Core.Maybe Core.Text,
    -- | Shows the partition value within the Kafka message output, unless the
    -- partition type is @schema-table-type@. The default is @false@.
    includePartitionValue :: Core.Maybe Core.Bool,
    -- | Provides detailed transaction information from the source database. This
    -- information includes a commit timestamp, a log position, and values for
    -- @transaction_id@, previous @transaction_id@, and @transaction_record_id@
    -- (the record offset within a transaction). The default is @false@.
    includeTransactionDetails :: Core.Maybe Core.Bool,
    -- | Includes any data definition language (DDL) operations that change the
    -- table in the control data, such as @rename-table@, @drop-table@,
    -- @add-column@, @drop-column@, and @rename-column@. The default is
    -- @false@.
    includeTableAlterOperations :: Core.Maybe Core.Bool
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
    { includeNullAndEmpty = Core.Nothing,
      messageFormat = Core.Nothing,
      partitionIncludeSchemaTable = Core.Nothing,
      includeControlDetails = Core.Nothing,
      topic = Core.Nothing,
      messageMaxBytes = Core.Nothing,
      broker = Core.Nothing,
      includePartitionValue = Core.Nothing,
      includeTransactionDetails = Core.Nothing,
      includeTableAlterOperations = Core.Nothing
    }

-- | Include NULL and empty columns for records migrated to the endpoint. The
-- default is @false@.
kafkaSettings_includeNullAndEmpty :: Lens.Lens' KafkaSettings (Core.Maybe Core.Bool)
kafkaSettings_includeNullAndEmpty = Lens.lens (\KafkaSettings' {includeNullAndEmpty} -> includeNullAndEmpty) (\s@KafkaSettings' {} a -> s {includeNullAndEmpty = a} :: KafkaSettings)

-- | The output format for the records created on the endpoint. The message
-- format is @JSON@ (default) or @JSON_UNFORMATTED@ (a single line with no
-- tab).
kafkaSettings_messageFormat :: Lens.Lens' KafkaSettings (Core.Maybe MessageFormatValue)
kafkaSettings_messageFormat = Lens.lens (\KafkaSettings' {messageFormat} -> messageFormat) (\s@KafkaSettings' {} a -> s {messageFormat = a} :: KafkaSettings)

-- | Prefixes schema and table names to partition values, when the partition
-- type is @primary-key-type@. Doing this increases data distribution among
-- Kafka partitions. For example, suppose that a SysBench schema has
-- thousands of tables and each table has only limited range for a primary
-- key. In this case, the same primary key is sent from thousands of tables
-- to the same partition, which causes throttling. The default is @false@.
kafkaSettings_partitionIncludeSchemaTable :: Lens.Lens' KafkaSettings (Core.Maybe Core.Bool)
kafkaSettings_partitionIncludeSchemaTable = Lens.lens (\KafkaSettings' {partitionIncludeSchemaTable} -> partitionIncludeSchemaTable) (\s@KafkaSettings' {} a -> s {partitionIncludeSchemaTable = a} :: KafkaSettings)

-- | Shows detailed control information for table definition, column
-- definition, and table and column changes in the Kafka message output.
-- The default is @false@.
kafkaSettings_includeControlDetails :: Lens.Lens' KafkaSettings (Core.Maybe Core.Bool)
kafkaSettings_includeControlDetails = Lens.lens (\KafkaSettings' {includeControlDetails} -> includeControlDetails) (\s@KafkaSettings' {} a -> s {includeControlDetails = a} :: KafkaSettings)

-- | The topic to which you migrate the data. If you don\'t specify a topic,
-- AWS DMS specifies @\"kafka-default-topic\"@ as the migration topic.
kafkaSettings_topic :: Lens.Lens' KafkaSettings (Core.Maybe Core.Text)
kafkaSettings_topic = Lens.lens (\KafkaSettings' {topic} -> topic) (\s@KafkaSettings' {} a -> s {topic = a} :: KafkaSettings)

-- | The maximum size in bytes for records created on the endpoint The
-- default is 1,000,000.
kafkaSettings_messageMaxBytes :: Lens.Lens' KafkaSettings (Core.Maybe Core.Int)
kafkaSettings_messageMaxBytes = Lens.lens (\KafkaSettings' {messageMaxBytes} -> messageMaxBytes) (\s@KafkaSettings' {} a -> s {messageMaxBytes = a} :: KafkaSettings)

-- | The broker location and port of the Kafka broker that hosts your Kafka
-- instance. Specify the broker in the form @ broker-hostname-or-ip:port @.
-- For example, @\"ec2-12-345-678-901.compute-1.amazonaws.com:2345\"@.
kafkaSettings_broker :: Lens.Lens' KafkaSettings (Core.Maybe Core.Text)
kafkaSettings_broker = Lens.lens (\KafkaSettings' {broker} -> broker) (\s@KafkaSettings' {} a -> s {broker = a} :: KafkaSettings)

-- | Shows the partition value within the Kafka message output, unless the
-- partition type is @schema-table-type@. The default is @false@.
kafkaSettings_includePartitionValue :: Lens.Lens' KafkaSettings (Core.Maybe Core.Bool)
kafkaSettings_includePartitionValue = Lens.lens (\KafkaSettings' {includePartitionValue} -> includePartitionValue) (\s@KafkaSettings' {} a -> s {includePartitionValue = a} :: KafkaSettings)

-- | Provides detailed transaction information from the source database. This
-- information includes a commit timestamp, a log position, and values for
-- @transaction_id@, previous @transaction_id@, and @transaction_record_id@
-- (the record offset within a transaction). The default is @false@.
kafkaSettings_includeTransactionDetails :: Lens.Lens' KafkaSettings (Core.Maybe Core.Bool)
kafkaSettings_includeTransactionDetails = Lens.lens (\KafkaSettings' {includeTransactionDetails} -> includeTransactionDetails) (\s@KafkaSettings' {} a -> s {includeTransactionDetails = a} :: KafkaSettings)

-- | Includes any data definition language (DDL) operations that change the
-- table in the control data, such as @rename-table@, @drop-table@,
-- @add-column@, @drop-column@, and @rename-column@. The default is
-- @false@.
kafkaSettings_includeTableAlterOperations :: Lens.Lens' KafkaSettings (Core.Maybe Core.Bool)
kafkaSettings_includeTableAlterOperations = Lens.lens (\KafkaSettings' {includeTableAlterOperations} -> includeTableAlterOperations) (\s@KafkaSettings' {} a -> s {includeTableAlterOperations = a} :: KafkaSettings)

instance Core.FromJSON KafkaSettings where
  parseJSON =
    Core.withObject
      "KafkaSettings"
      ( \x ->
          KafkaSettings'
            Core.<$> (x Core..:? "IncludeNullAndEmpty")
            Core.<*> (x Core..:? "MessageFormat")
            Core.<*> (x Core..:? "PartitionIncludeSchemaTable")
            Core.<*> (x Core..:? "IncludeControlDetails")
            Core.<*> (x Core..:? "Topic")
            Core.<*> (x Core..:? "MessageMaxBytes")
            Core.<*> (x Core..:? "Broker")
            Core.<*> (x Core..:? "IncludePartitionValue")
            Core.<*> (x Core..:? "IncludeTransactionDetails")
            Core.<*> (x Core..:? "IncludeTableAlterOperations")
      )

instance Core.Hashable KafkaSettings

instance Core.NFData KafkaSettings

instance Core.ToJSON KafkaSettings where
  toJSON KafkaSettings' {..} =
    Core.object
      ( Core.catMaybes
          [ ("IncludeNullAndEmpty" Core..=)
              Core.<$> includeNullAndEmpty,
            ("MessageFormat" Core..=) Core.<$> messageFormat,
            ("PartitionIncludeSchemaTable" Core..=)
              Core.<$> partitionIncludeSchemaTable,
            ("IncludeControlDetails" Core..=)
              Core.<$> includeControlDetails,
            ("Topic" Core..=) Core.<$> topic,
            ("MessageMaxBytes" Core..=) Core.<$> messageMaxBytes,
            ("Broker" Core..=) Core.<$> broker,
            ("IncludePartitionValue" Core..=)
              Core.<$> includePartitionValue,
            ("IncludeTransactionDetails" Core..=)
              Core.<$> includeTransactionDetails,
            ("IncludeTableAlterOperations" Core..=)
              Core.<$> includeTableAlterOperations
          ]
      )
