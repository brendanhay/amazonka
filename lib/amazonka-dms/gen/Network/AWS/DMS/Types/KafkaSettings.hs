{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.Types.KafkaSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DMS.Types.KafkaSettings
  ( KafkaSettings (..)
  -- * Smart constructor
  , mkKafkaSettings
  -- * Lenses
  , ksBroker
  , ksIncludeControlDetails
  , ksIncludeNullAndEmpty
  , ksIncludePartitionValue
  , ksIncludeTableAlterOperations
  , ksIncludeTransactionDetails
  , ksMessageFormat
  , ksMessageMaxBytes
  , ksPartitionIncludeSchemaTable
  , ksTopic
  ) where

import qualified Network.AWS.DMS.Types.MessageFormatValue as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Provides information that describes an Apache Kafka endpoint. This information includes the output format of records applied to the endpoint and details of transaction and control table data information.
--
-- /See:/ 'mkKafkaSettings' smart constructor.
data KafkaSettings = KafkaSettings'
  { broker :: Core.Maybe Core.Text
    -- ^ The broker location and port of the Kafka broker that hosts your Kafka instance. Specify the broker in the form @/broker-hostname-or-ip/ :/port/ @ . For example, @"ec2-12-345-678-901.compute-1.amazonaws.com:2345"@ .
  , includeControlDetails :: Core.Maybe Core.Bool
    -- ^ Shows detailed control information for table definition, column definition, and table and column changes in the Kafka message output. The default is @false@ .
  , includeNullAndEmpty :: Core.Maybe Core.Bool
    -- ^ Include NULL and empty columns for records migrated to the endpoint. The default is @false@ .
  , includePartitionValue :: Core.Maybe Core.Bool
    -- ^ Shows the partition value within the Kafka message output, unless the partition type is @schema-table-type@ . The default is @false@ .
  , includeTableAlterOperations :: Core.Maybe Core.Bool
    -- ^ Includes any data definition language (DDL) operations that change the table in the control data, such as @rename-table@ , @drop-table@ , @add-column@ , @drop-column@ , and @rename-column@ . The default is @false@ .
  , includeTransactionDetails :: Core.Maybe Core.Bool
    -- ^ Provides detailed transaction information from the source database. This information includes a commit timestamp, a log position, and values for @transaction_id@ , previous @transaction_id@ , and @transaction_record_id@ (the record offset within a transaction). The default is @false@ .
  , messageFormat :: Core.Maybe Types.MessageFormatValue
    -- ^ The output format for the records created on the endpoint. The message format is @JSON@ (default) or @JSON_UNFORMATTED@ (a single line with no tab).
  , messageMaxBytes :: Core.Maybe Core.Int
    -- ^ The maximum size in bytes for records created on the endpoint The default is 1,000,000.
  , partitionIncludeSchemaTable :: Core.Maybe Core.Bool
    -- ^ Prefixes schema and table names to partition values, when the partition type is @primary-key-type@ . Doing this increases data distribution among Kafka partitions. For example, suppose that a SysBench schema has thousands of tables and each table has only limited range for a primary key. In this case, the same primary key is sent from thousands of tables to the same partition, which causes throttling. The default is @false@ .
  , topic :: Core.Maybe Core.Text
    -- ^ The topic to which you migrate the data. If you don't specify a topic, AWS DMS specifies @"kafka-default-topic"@ as the migration topic.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'KafkaSettings' value with any optional fields omitted.
mkKafkaSettings
    :: KafkaSettings
mkKafkaSettings
  = KafkaSettings'{broker = Core.Nothing,
                   includeControlDetails = Core.Nothing,
                   includeNullAndEmpty = Core.Nothing,
                   includePartitionValue = Core.Nothing,
                   includeTableAlterOperations = Core.Nothing,
                   includeTransactionDetails = Core.Nothing,
                   messageFormat = Core.Nothing, messageMaxBytes = Core.Nothing,
                   partitionIncludeSchemaTable = Core.Nothing, topic = Core.Nothing}

-- | The broker location and port of the Kafka broker that hosts your Kafka instance. Specify the broker in the form @/broker-hostname-or-ip/ :/port/ @ . For example, @"ec2-12-345-678-901.compute-1.amazonaws.com:2345"@ .
--
-- /Note:/ Consider using 'broker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ksBroker :: Lens.Lens' KafkaSettings (Core.Maybe Core.Text)
ksBroker = Lens.field @"broker"
{-# INLINEABLE ksBroker #-}
{-# DEPRECATED broker "Use generic-lens or generic-optics with 'broker' instead"  #-}

-- | Shows detailed control information for table definition, column definition, and table and column changes in the Kafka message output. The default is @false@ .
--
-- /Note:/ Consider using 'includeControlDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ksIncludeControlDetails :: Lens.Lens' KafkaSettings (Core.Maybe Core.Bool)
ksIncludeControlDetails = Lens.field @"includeControlDetails"
{-# INLINEABLE ksIncludeControlDetails #-}
{-# DEPRECATED includeControlDetails "Use generic-lens or generic-optics with 'includeControlDetails' instead"  #-}

-- | Include NULL and empty columns for records migrated to the endpoint. The default is @false@ .
--
-- /Note:/ Consider using 'includeNullAndEmpty' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ksIncludeNullAndEmpty :: Lens.Lens' KafkaSettings (Core.Maybe Core.Bool)
ksIncludeNullAndEmpty = Lens.field @"includeNullAndEmpty"
{-# INLINEABLE ksIncludeNullAndEmpty #-}
{-# DEPRECATED includeNullAndEmpty "Use generic-lens or generic-optics with 'includeNullAndEmpty' instead"  #-}

-- | Shows the partition value within the Kafka message output, unless the partition type is @schema-table-type@ . The default is @false@ .
--
-- /Note:/ Consider using 'includePartitionValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ksIncludePartitionValue :: Lens.Lens' KafkaSettings (Core.Maybe Core.Bool)
ksIncludePartitionValue = Lens.field @"includePartitionValue"
{-# INLINEABLE ksIncludePartitionValue #-}
{-# DEPRECATED includePartitionValue "Use generic-lens or generic-optics with 'includePartitionValue' instead"  #-}

-- | Includes any data definition language (DDL) operations that change the table in the control data, such as @rename-table@ , @drop-table@ , @add-column@ , @drop-column@ , and @rename-column@ . The default is @false@ .
--
-- /Note:/ Consider using 'includeTableAlterOperations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ksIncludeTableAlterOperations :: Lens.Lens' KafkaSettings (Core.Maybe Core.Bool)
ksIncludeTableAlterOperations = Lens.field @"includeTableAlterOperations"
{-# INLINEABLE ksIncludeTableAlterOperations #-}
{-# DEPRECATED includeTableAlterOperations "Use generic-lens or generic-optics with 'includeTableAlterOperations' instead"  #-}

-- | Provides detailed transaction information from the source database. This information includes a commit timestamp, a log position, and values for @transaction_id@ , previous @transaction_id@ , and @transaction_record_id@ (the record offset within a transaction). The default is @false@ .
--
-- /Note:/ Consider using 'includeTransactionDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ksIncludeTransactionDetails :: Lens.Lens' KafkaSettings (Core.Maybe Core.Bool)
ksIncludeTransactionDetails = Lens.field @"includeTransactionDetails"
{-# INLINEABLE ksIncludeTransactionDetails #-}
{-# DEPRECATED includeTransactionDetails "Use generic-lens or generic-optics with 'includeTransactionDetails' instead"  #-}

-- | The output format for the records created on the endpoint. The message format is @JSON@ (default) or @JSON_UNFORMATTED@ (a single line with no tab).
--
-- /Note:/ Consider using 'messageFormat' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ksMessageFormat :: Lens.Lens' KafkaSettings (Core.Maybe Types.MessageFormatValue)
ksMessageFormat = Lens.field @"messageFormat"
{-# INLINEABLE ksMessageFormat #-}
{-# DEPRECATED messageFormat "Use generic-lens or generic-optics with 'messageFormat' instead"  #-}

-- | The maximum size in bytes for records created on the endpoint The default is 1,000,000.
--
-- /Note:/ Consider using 'messageMaxBytes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ksMessageMaxBytes :: Lens.Lens' KafkaSettings (Core.Maybe Core.Int)
ksMessageMaxBytes = Lens.field @"messageMaxBytes"
{-# INLINEABLE ksMessageMaxBytes #-}
{-# DEPRECATED messageMaxBytes "Use generic-lens or generic-optics with 'messageMaxBytes' instead"  #-}

-- | Prefixes schema and table names to partition values, when the partition type is @primary-key-type@ . Doing this increases data distribution among Kafka partitions. For example, suppose that a SysBench schema has thousands of tables and each table has only limited range for a primary key. In this case, the same primary key is sent from thousands of tables to the same partition, which causes throttling. The default is @false@ .
--
-- /Note:/ Consider using 'partitionIncludeSchemaTable' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ksPartitionIncludeSchemaTable :: Lens.Lens' KafkaSettings (Core.Maybe Core.Bool)
ksPartitionIncludeSchemaTable = Lens.field @"partitionIncludeSchemaTable"
{-# INLINEABLE ksPartitionIncludeSchemaTable #-}
{-# DEPRECATED partitionIncludeSchemaTable "Use generic-lens or generic-optics with 'partitionIncludeSchemaTable' instead"  #-}

-- | The topic to which you migrate the data. If you don't specify a topic, AWS DMS specifies @"kafka-default-topic"@ as the migration topic.
--
-- /Note:/ Consider using 'topic' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ksTopic :: Lens.Lens' KafkaSettings (Core.Maybe Core.Text)
ksTopic = Lens.field @"topic"
{-# INLINEABLE ksTopic #-}
{-# DEPRECATED topic "Use generic-lens or generic-optics with 'topic' instead"  #-}

instance Core.FromJSON KafkaSettings where
        toJSON KafkaSettings{..}
          = Core.object
              (Core.catMaybes
                 [("Broker" Core..=) Core.<$> broker,
                  ("IncludeControlDetails" Core..=) Core.<$> includeControlDetails,
                  ("IncludeNullAndEmpty" Core..=) Core.<$> includeNullAndEmpty,
                  ("IncludePartitionValue" Core..=) Core.<$> includePartitionValue,
                  ("IncludeTableAlterOperations" Core..=) Core.<$>
                    includeTableAlterOperations,
                  ("IncludeTransactionDetails" Core..=) Core.<$>
                    includeTransactionDetails,
                  ("MessageFormat" Core..=) Core.<$> messageFormat,
                  ("MessageMaxBytes" Core..=) Core.<$> messageMaxBytes,
                  ("PartitionIncludeSchemaTable" Core..=) Core.<$>
                    partitionIncludeSchemaTable,
                  ("Topic" Core..=) Core.<$> topic])

instance Core.FromJSON KafkaSettings where
        parseJSON
          = Core.withObject "KafkaSettings" Core.$
              \ x ->
                KafkaSettings' Core.<$>
                  (x Core..:? "Broker") Core.<*> x Core..:? "IncludeControlDetails"
                    Core.<*> x Core..:? "IncludeNullAndEmpty"
                    Core.<*> x Core..:? "IncludePartitionValue"
                    Core.<*> x Core..:? "IncludeTableAlterOperations"
                    Core.<*> x Core..:? "IncludeTransactionDetails"
                    Core.<*> x Core..:? "MessageFormat"
                    Core.<*> x Core..:? "MessageMaxBytes"
                    Core.<*> x Core..:? "PartitionIncludeSchemaTable"
                    Core.<*> x Core..:? "Topic"
