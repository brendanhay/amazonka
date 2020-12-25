{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.Types.KinesisSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DMS.Types.KinesisSettings
  ( KinesisSettings (..),

    -- * Smart constructor
    mkKinesisSettings,

    -- * Lenses
    kIncludeControlDetails,
    kIncludeNullAndEmpty,
    kIncludePartitionValue,
    kIncludeTableAlterOperations,
    kIncludeTransactionDetails,
    kMessageFormat,
    kPartitionIncludeSchemaTable,
    kServiceAccessRoleArn,
    kStreamArn,
  )
where

import qualified Network.AWS.DMS.Types.MessageFormatValue as Types
import qualified Network.AWS.DMS.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Provides information that describes an Amazon Kinesis Data Stream endpoint. This information includes the output format of records applied to the endpoint and details of transaction and control table data information.
--
-- /See:/ 'mkKinesisSettings' smart constructor.
data KinesisSettings = KinesisSettings'
  { -- | Shows detailed control information for table definition, column definition, and table and column changes in the Kinesis message output. The default is @false@ .
    includeControlDetails :: Core.Maybe Core.Bool,
    -- | Include NULL and empty columns for records migrated to the endpoint. The default is @false@ .
    includeNullAndEmpty :: Core.Maybe Core.Bool,
    -- | Shows the partition value within the Kinesis message output, unless the partition type is @schema-table-type@ . The default is @false@ .
    includePartitionValue :: Core.Maybe Core.Bool,
    -- | Includes any data definition language (DDL) operations that change the table in the control data, such as @rename-table@ , @drop-table@ , @add-column@ , @drop-column@ , and @rename-column@ . The default is @false@ .
    includeTableAlterOperations :: Core.Maybe Core.Bool,
    -- | Provides detailed transaction information from the source database. This information includes a commit timestamp, a log position, and values for @transaction_id@ , previous @transaction_id@ , and @transaction_record_id@ (the record offset within a transaction). The default is @false@ .
    includeTransactionDetails :: Core.Maybe Core.Bool,
    -- | The output format for the records created on the endpoint. The message format is @JSON@ (default) or @JSON_UNFORMATTED@ (a single line with no tab).
    messageFormat :: Core.Maybe Types.MessageFormatValue,
    -- | Prefixes schema and table names to partition values, when the partition type is @primary-key-type@ . Doing this increases data distribution among Kinesis shards. For example, suppose that a SysBench schema has thousands of tables and each table has only limited range for a primary key. In this case, the same primary key is sent from thousands of tables to the same shard, which causes throttling. The default is @false@ .
    partitionIncludeSchemaTable :: Core.Maybe Core.Bool,
    -- | The Amazon Resource Name (ARN) for the AWS Identity and Access Management (IAM) role that AWS DMS uses to write to the Kinesis data stream.
    serviceAccessRoleArn :: Core.Maybe Types.String,
    -- | The Amazon Resource Name (ARN) for the Amazon Kinesis Data Streams endpoint.
    streamArn :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'KinesisSettings' value with any optional fields omitted.
mkKinesisSettings ::
  KinesisSettings
mkKinesisSettings =
  KinesisSettings'
    { includeControlDetails = Core.Nothing,
      includeNullAndEmpty = Core.Nothing,
      includePartitionValue = Core.Nothing,
      includeTableAlterOperations = Core.Nothing,
      includeTransactionDetails = Core.Nothing,
      messageFormat = Core.Nothing,
      partitionIncludeSchemaTable = Core.Nothing,
      serviceAccessRoleArn = Core.Nothing,
      streamArn = Core.Nothing
    }

-- | Shows detailed control information for table definition, column definition, and table and column changes in the Kinesis message output. The default is @false@ .
--
-- /Note:/ Consider using 'includeControlDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kIncludeControlDetails :: Lens.Lens' KinesisSettings (Core.Maybe Core.Bool)
kIncludeControlDetails = Lens.field @"includeControlDetails"
{-# DEPRECATED kIncludeControlDetails "Use generic-lens or generic-optics with 'includeControlDetails' instead." #-}

-- | Include NULL and empty columns for records migrated to the endpoint. The default is @false@ .
--
-- /Note:/ Consider using 'includeNullAndEmpty' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kIncludeNullAndEmpty :: Lens.Lens' KinesisSettings (Core.Maybe Core.Bool)
kIncludeNullAndEmpty = Lens.field @"includeNullAndEmpty"
{-# DEPRECATED kIncludeNullAndEmpty "Use generic-lens or generic-optics with 'includeNullAndEmpty' instead." #-}

-- | Shows the partition value within the Kinesis message output, unless the partition type is @schema-table-type@ . The default is @false@ .
--
-- /Note:/ Consider using 'includePartitionValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kIncludePartitionValue :: Lens.Lens' KinesisSettings (Core.Maybe Core.Bool)
kIncludePartitionValue = Lens.field @"includePartitionValue"
{-# DEPRECATED kIncludePartitionValue "Use generic-lens or generic-optics with 'includePartitionValue' instead." #-}

-- | Includes any data definition language (DDL) operations that change the table in the control data, such as @rename-table@ , @drop-table@ , @add-column@ , @drop-column@ , and @rename-column@ . The default is @false@ .
--
-- /Note:/ Consider using 'includeTableAlterOperations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kIncludeTableAlterOperations :: Lens.Lens' KinesisSettings (Core.Maybe Core.Bool)
kIncludeTableAlterOperations = Lens.field @"includeTableAlterOperations"
{-# DEPRECATED kIncludeTableAlterOperations "Use generic-lens or generic-optics with 'includeTableAlterOperations' instead." #-}

-- | Provides detailed transaction information from the source database. This information includes a commit timestamp, a log position, and values for @transaction_id@ , previous @transaction_id@ , and @transaction_record_id@ (the record offset within a transaction). The default is @false@ .
--
-- /Note:/ Consider using 'includeTransactionDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kIncludeTransactionDetails :: Lens.Lens' KinesisSettings (Core.Maybe Core.Bool)
kIncludeTransactionDetails = Lens.field @"includeTransactionDetails"
{-# DEPRECATED kIncludeTransactionDetails "Use generic-lens or generic-optics with 'includeTransactionDetails' instead." #-}

-- | The output format for the records created on the endpoint. The message format is @JSON@ (default) or @JSON_UNFORMATTED@ (a single line with no tab).
--
-- /Note:/ Consider using 'messageFormat' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kMessageFormat :: Lens.Lens' KinesisSettings (Core.Maybe Types.MessageFormatValue)
kMessageFormat = Lens.field @"messageFormat"
{-# DEPRECATED kMessageFormat "Use generic-lens or generic-optics with 'messageFormat' instead." #-}

-- | Prefixes schema and table names to partition values, when the partition type is @primary-key-type@ . Doing this increases data distribution among Kinesis shards. For example, suppose that a SysBench schema has thousands of tables and each table has only limited range for a primary key. In this case, the same primary key is sent from thousands of tables to the same shard, which causes throttling. The default is @false@ .
--
-- /Note:/ Consider using 'partitionIncludeSchemaTable' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kPartitionIncludeSchemaTable :: Lens.Lens' KinesisSettings (Core.Maybe Core.Bool)
kPartitionIncludeSchemaTable = Lens.field @"partitionIncludeSchemaTable"
{-# DEPRECATED kPartitionIncludeSchemaTable "Use generic-lens or generic-optics with 'partitionIncludeSchemaTable' instead." #-}

-- | The Amazon Resource Name (ARN) for the AWS Identity and Access Management (IAM) role that AWS DMS uses to write to the Kinesis data stream.
--
-- /Note:/ Consider using 'serviceAccessRoleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kServiceAccessRoleArn :: Lens.Lens' KinesisSettings (Core.Maybe Types.String)
kServiceAccessRoleArn = Lens.field @"serviceAccessRoleArn"
{-# DEPRECATED kServiceAccessRoleArn "Use generic-lens or generic-optics with 'serviceAccessRoleArn' instead." #-}

-- | The Amazon Resource Name (ARN) for the Amazon Kinesis Data Streams endpoint.
--
-- /Note:/ Consider using 'streamArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kStreamArn :: Lens.Lens' KinesisSettings (Core.Maybe Types.String)
kStreamArn = Lens.field @"streamArn"
{-# DEPRECATED kStreamArn "Use generic-lens or generic-optics with 'streamArn' instead." #-}

instance Core.FromJSON KinesisSettings where
  toJSON KinesisSettings {..} =
    Core.object
      ( Core.catMaybes
          [ ("IncludeControlDetails" Core..=) Core.<$> includeControlDetails,
            ("IncludeNullAndEmpty" Core..=) Core.<$> includeNullAndEmpty,
            ("IncludePartitionValue" Core..=) Core.<$> includePartitionValue,
            ("IncludeTableAlterOperations" Core..=)
              Core.<$> includeTableAlterOperations,
            ("IncludeTransactionDetails" Core..=)
              Core.<$> includeTransactionDetails,
            ("MessageFormat" Core..=) Core.<$> messageFormat,
            ("PartitionIncludeSchemaTable" Core..=)
              Core.<$> partitionIncludeSchemaTable,
            ("ServiceAccessRoleArn" Core..=) Core.<$> serviceAccessRoleArn,
            ("StreamArn" Core..=) Core.<$> streamArn
          ]
      )

instance Core.FromJSON KinesisSettings where
  parseJSON =
    Core.withObject "KinesisSettings" Core.$
      \x ->
        KinesisSettings'
          Core.<$> (x Core..:? "IncludeControlDetails")
          Core.<*> (x Core..:? "IncludeNullAndEmpty")
          Core.<*> (x Core..:? "IncludePartitionValue")
          Core.<*> (x Core..:? "IncludeTableAlterOperations")
          Core.<*> (x Core..:? "IncludeTransactionDetails")
          Core.<*> (x Core..:? "MessageFormat")
          Core.<*> (x Core..:? "PartitionIncludeSchemaTable")
          Core.<*> (x Core..:? "ServiceAccessRoleArn")
          Core.<*> (x Core..:? "StreamArn")
