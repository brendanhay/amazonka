-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.Types.KafkaSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DMS.Types.KafkaSettings
  ( KafkaSettings (..),

    -- * Smart constructor
    mkKafkaSettings,

    -- * Lenses
    ksIncludeTransactionDetails,
    ksIncludeTableAlterOperations,
    ksPartitionIncludeSchemaTable,
    ksTopic,
    ksIncludeControlDetails,
    ksIncludePartitionValue,
    ksMessageFormat,
    ksBroker,
    ksMessageMaxBytes,
    ksIncludeNullAndEmpty,
  )
where

import Network.AWS.DMS.Types.MessageFormatValue
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Provides information that describes an Apache Kafka endpoint. This information includes the output format of records applied to the endpoint and details of transaction and control table data information.
--
-- /See:/ 'mkKafkaSettings' smart constructor.
data KafkaSettings = KafkaSettings'
  { includeTransactionDetails ::
      Lude.Maybe Lude.Bool,
    includeTableAlterOperations :: Lude.Maybe Lude.Bool,
    partitionIncludeSchemaTable :: Lude.Maybe Lude.Bool,
    topic :: Lude.Maybe Lude.Text,
    includeControlDetails :: Lude.Maybe Lude.Bool,
    includePartitionValue :: Lude.Maybe Lude.Bool,
    messageFormat :: Lude.Maybe MessageFormatValue,
    broker :: Lude.Maybe Lude.Text,
    messageMaxBytes :: Lude.Maybe Lude.Int,
    includeNullAndEmpty :: Lude.Maybe Lude.Bool
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'KafkaSettings' with the minimum fields required to make a request.
--
-- * 'broker' - The broker location and port of the Kafka broker that hosts your Kafka instance. Specify the broker in the form @/broker-hostname-or-ip/ :/port/ @ . For example, @"ec2-12-345-678-901.compute-1.amazonaws.com:2345"@ .
-- * 'includeControlDetails' - Shows detailed control information for table definition, column definition, and table and column changes in the Kafka message output. The default is @false@ .
-- * 'includeNullAndEmpty' - Include NULL and empty columns for records migrated to the endpoint. The default is @false@ .
-- * 'includePartitionValue' - Shows the partition value within the Kafka message output, unless the partition type is @schema-table-type@ . The default is @false@ .
-- * 'includeTableAlterOperations' - Includes any data definition language (DDL) operations that change the table in the control data, such as @rename-table@ , @drop-table@ , @add-column@ , @drop-column@ , and @rename-column@ . The default is @false@ .
-- * 'includeTransactionDetails' - Provides detailed transaction information from the source database. This information includes a commit timestamp, a log position, and values for @transaction_id@ , previous @transaction_id@ , and @transaction_record_id@ (the record offset within a transaction). The default is @false@ .
-- * 'messageFormat' - The output format for the records created on the endpoint. The message format is @JSON@ (default) or @JSON_UNFORMATTED@ (a single line with no tab).
-- * 'messageMaxBytes' - The maximum size in bytes for records created on the endpoint The default is 1,000,000.
-- * 'partitionIncludeSchemaTable' - Prefixes schema and table names to partition values, when the partition type is @primary-key-type@ . Doing this increases data distribution among Kafka partitions. For example, suppose that a SysBench schema has thousands of tables and each table has only limited range for a primary key. In this case, the same primary key is sent from thousands of tables to the same partition, which causes throttling. The default is @false@ .
-- * 'topic' - The topic to which you migrate the data. If you don't specify a topic, AWS DMS specifies @"kafka-default-topic"@ as the migration topic.
mkKafkaSettings ::
  KafkaSettings
mkKafkaSettings =
  KafkaSettings'
    { includeTransactionDetails = Lude.Nothing,
      includeTableAlterOperations = Lude.Nothing,
      partitionIncludeSchemaTable = Lude.Nothing,
      topic = Lude.Nothing,
      includeControlDetails = Lude.Nothing,
      includePartitionValue = Lude.Nothing,
      messageFormat = Lude.Nothing,
      broker = Lude.Nothing,
      messageMaxBytes = Lude.Nothing,
      includeNullAndEmpty = Lude.Nothing
    }

-- | Provides detailed transaction information from the source database. This information includes a commit timestamp, a log position, and values for @transaction_id@ , previous @transaction_id@ , and @transaction_record_id@ (the record offset within a transaction). The default is @false@ .
--
-- /Note:/ Consider using 'includeTransactionDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ksIncludeTransactionDetails :: Lens.Lens' KafkaSettings (Lude.Maybe Lude.Bool)
ksIncludeTransactionDetails = Lens.lens (includeTransactionDetails :: KafkaSettings -> Lude.Maybe Lude.Bool) (\s a -> s {includeTransactionDetails = a} :: KafkaSettings)
{-# DEPRECATED ksIncludeTransactionDetails "Use generic-lens or generic-optics with 'includeTransactionDetails' instead." #-}

-- | Includes any data definition language (DDL) operations that change the table in the control data, such as @rename-table@ , @drop-table@ , @add-column@ , @drop-column@ , and @rename-column@ . The default is @false@ .
--
-- /Note:/ Consider using 'includeTableAlterOperations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ksIncludeTableAlterOperations :: Lens.Lens' KafkaSettings (Lude.Maybe Lude.Bool)
ksIncludeTableAlterOperations = Lens.lens (includeTableAlterOperations :: KafkaSettings -> Lude.Maybe Lude.Bool) (\s a -> s {includeTableAlterOperations = a} :: KafkaSettings)
{-# DEPRECATED ksIncludeTableAlterOperations "Use generic-lens or generic-optics with 'includeTableAlterOperations' instead." #-}

-- | Prefixes schema and table names to partition values, when the partition type is @primary-key-type@ . Doing this increases data distribution among Kafka partitions. For example, suppose that a SysBench schema has thousands of tables and each table has only limited range for a primary key. In this case, the same primary key is sent from thousands of tables to the same partition, which causes throttling. The default is @false@ .
--
-- /Note:/ Consider using 'partitionIncludeSchemaTable' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ksPartitionIncludeSchemaTable :: Lens.Lens' KafkaSettings (Lude.Maybe Lude.Bool)
ksPartitionIncludeSchemaTable = Lens.lens (partitionIncludeSchemaTable :: KafkaSettings -> Lude.Maybe Lude.Bool) (\s a -> s {partitionIncludeSchemaTable = a} :: KafkaSettings)
{-# DEPRECATED ksPartitionIncludeSchemaTable "Use generic-lens or generic-optics with 'partitionIncludeSchemaTable' instead." #-}

-- | The topic to which you migrate the data. If you don't specify a topic, AWS DMS specifies @"kafka-default-topic"@ as the migration topic.
--
-- /Note:/ Consider using 'topic' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ksTopic :: Lens.Lens' KafkaSettings (Lude.Maybe Lude.Text)
ksTopic = Lens.lens (topic :: KafkaSettings -> Lude.Maybe Lude.Text) (\s a -> s {topic = a} :: KafkaSettings)
{-# DEPRECATED ksTopic "Use generic-lens or generic-optics with 'topic' instead." #-}

-- | Shows detailed control information for table definition, column definition, and table and column changes in the Kafka message output. The default is @false@ .
--
-- /Note:/ Consider using 'includeControlDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ksIncludeControlDetails :: Lens.Lens' KafkaSettings (Lude.Maybe Lude.Bool)
ksIncludeControlDetails = Lens.lens (includeControlDetails :: KafkaSettings -> Lude.Maybe Lude.Bool) (\s a -> s {includeControlDetails = a} :: KafkaSettings)
{-# DEPRECATED ksIncludeControlDetails "Use generic-lens or generic-optics with 'includeControlDetails' instead." #-}

-- | Shows the partition value within the Kafka message output, unless the partition type is @schema-table-type@ . The default is @false@ .
--
-- /Note:/ Consider using 'includePartitionValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ksIncludePartitionValue :: Lens.Lens' KafkaSettings (Lude.Maybe Lude.Bool)
ksIncludePartitionValue = Lens.lens (includePartitionValue :: KafkaSettings -> Lude.Maybe Lude.Bool) (\s a -> s {includePartitionValue = a} :: KafkaSettings)
{-# DEPRECATED ksIncludePartitionValue "Use generic-lens or generic-optics with 'includePartitionValue' instead." #-}

-- | The output format for the records created on the endpoint. The message format is @JSON@ (default) or @JSON_UNFORMATTED@ (a single line with no tab).
--
-- /Note:/ Consider using 'messageFormat' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ksMessageFormat :: Lens.Lens' KafkaSettings (Lude.Maybe MessageFormatValue)
ksMessageFormat = Lens.lens (messageFormat :: KafkaSettings -> Lude.Maybe MessageFormatValue) (\s a -> s {messageFormat = a} :: KafkaSettings)
{-# DEPRECATED ksMessageFormat "Use generic-lens or generic-optics with 'messageFormat' instead." #-}

-- | The broker location and port of the Kafka broker that hosts your Kafka instance. Specify the broker in the form @/broker-hostname-or-ip/ :/port/ @ . For example, @"ec2-12-345-678-901.compute-1.amazonaws.com:2345"@ .
--
-- /Note:/ Consider using 'broker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ksBroker :: Lens.Lens' KafkaSettings (Lude.Maybe Lude.Text)
ksBroker = Lens.lens (broker :: KafkaSettings -> Lude.Maybe Lude.Text) (\s a -> s {broker = a} :: KafkaSettings)
{-# DEPRECATED ksBroker "Use generic-lens or generic-optics with 'broker' instead." #-}

-- | The maximum size in bytes for records created on the endpoint The default is 1,000,000.
--
-- /Note:/ Consider using 'messageMaxBytes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ksMessageMaxBytes :: Lens.Lens' KafkaSettings (Lude.Maybe Lude.Int)
ksMessageMaxBytes = Lens.lens (messageMaxBytes :: KafkaSettings -> Lude.Maybe Lude.Int) (\s a -> s {messageMaxBytes = a} :: KafkaSettings)
{-# DEPRECATED ksMessageMaxBytes "Use generic-lens or generic-optics with 'messageMaxBytes' instead." #-}

-- | Include NULL and empty columns for records migrated to the endpoint. The default is @false@ .
--
-- /Note:/ Consider using 'includeNullAndEmpty' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ksIncludeNullAndEmpty :: Lens.Lens' KafkaSettings (Lude.Maybe Lude.Bool)
ksIncludeNullAndEmpty = Lens.lens (includeNullAndEmpty :: KafkaSettings -> Lude.Maybe Lude.Bool) (\s a -> s {includeNullAndEmpty = a} :: KafkaSettings)
{-# DEPRECATED ksIncludeNullAndEmpty "Use generic-lens or generic-optics with 'includeNullAndEmpty' instead." #-}

instance Lude.FromJSON KafkaSettings where
  parseJSON =
    Lude.withObject
      "KafkaSettings"
      ( \x ->
          KafkaSettings'
            Lude.<$> (x Lude..:? "IncludeTransactionDetails")
            Lude.<*> (x Lude..:? "IncludeTableAlterOperations")
            Lude.<*> (x Lude..:? "PartitionIncludeSchemaTable")
            Lude.<*> (x Lude..:? "Topic")
            Lude.<*> (x Lude..:? "IncludeControlDetails")
            Lude.<*> (x Lude..:? "IncludePartitionValue")
            Lude.<*> (x Lude..:? "MessageFormat")
            Lude.<*> (x Lude..:? "Broker")
            Lude.<*> (x Lude..:? "MessageMaxBytes")
            Lude.<*> (x Lude..:? "IncludeNullAndEmpty")
      )

instance Lude.ToJSON KafkaSettings where
  toJSON KafkaSettings' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("IncludeTransactionDetails" Lude..=)
              Lude.<$> includeTransactionDetails,
            ("IncludeTableAlterOperations" Lude..=)
              Lude.<$> includeTableAlterOperations,
            ("PartitionIncludeSchemaTable" Lude..=)
              Lude.<$> partitionIncludeSchemaTable,
            ("Topic" Lude..=) Lude.<$> topic,
            ("IncludeControlDetails" Lude..=) Lude.<$> includeControlDetails,
            ("IncludePartitionValue" Lude..=) Lude.<$> includePartitionValue,
            ("MessageFormat" Lude..=) Lude.<$> messageFormat,
            ("Broker" Lude..=) Lude.<$> broker,
            ("MessageMaxBytes" Lude..=) Lude.<$> messageMaxBytes,
            ("IncludeNullAndEmpty" Lude..=) Lude.<$> includeNullAndEmpty
          ]
      )
