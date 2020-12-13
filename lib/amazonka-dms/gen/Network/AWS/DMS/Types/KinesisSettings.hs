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
    kIncludeTransactionDetails,
    kIncludeTableAlterOperations,
    kServiceAccessRoleARN,
    kPartitionIncludeSchemaTable,
    kStreamARN,
    kIncludeControlDetails,
    kIncludePartitionValue,
    kMessageFormat,
    kIncludeNullAndEmpty,
  )
where

import Network.AWS.DMS.Types.MessageFormatValue
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Provides information that describes an Amazon Kinesis Data Stream endpoint. This information includes the output format of records applied to the endpoint and details of transaction and control table data information.
--
-- /See:/ 'mkKinesisSettings' smart constructor.
data KinesisSettings = KinesisSettings'
  { -- | Provides detailed transaction information from the source database. This information includes a commit timestamp, a log position, and values for @transaction_id@ , previous @transaction_id@ , and @transaction_record_id@ (the record offset within a transaction). The default is @false@ .
    includeTransactionDetails :: Lude.Maybe Lude.Bool,
    -- | Includes any data definition language (DDL) operations that change the table in the control data, such as @rename-table@ , @drop-table@ , @add-column@ , @drop-column@ , and @rename-column@ . The default is @false@ .
    includeTableAlterOperations :: Lude.Maybe Lude.Bool,
    -- | The Amazon Resource Name (ARN) for the AWS Identity and Access Management (IAM) role that AWS DMS uses to write to the Kinesis data stream.
    serviceAccessRoleARN :: Lude.Maybe Lude.Text,
    -- | Prefixes schema and table names to partition values, when the partition type is @primary-key-type@ . Doing this increases data distribution among Kinesis shards. For example, suppose that a SysBench schema has thousands of tables and each table has only limited range for a primary key. In this case, the same primary key is sent from thousands of tables to the same shard, which causes throttling. The default is @false@ .
    partitionIncludeSchemaTable :: Lude.Maybe Lude.Bool,
    -- | The Amazon Resource Name (ARN) for the Amazon Kinesis Data Streams endpoint.
    streamARN :: Lude.Maybe Lude.Text,
    -- | Shows detailed control information for table definition, column definition, and table and column changes in the Kinesis message output. The default is @false@ .
    includeControlDetails :: Lude.Maybe Lude.Bool,
    -- | Shows the partition value within the Kinesis message output, unless the partition type is @schema-table-type@ . The default is @false@ .
    includePartitionValue :: Lude.Maybe Lude.Bool,
    -- | The output format for the records created on the endpoint. The message format is @JSON@ (default) or @JSON_UNFORMATTED@ (a single line with no tab).
    messageFormat :: Lude.Maybe MessageFormatValue,
    -- | Include NULL and empty columns for records migrated to the endpoint. The default is @false@ .
    includeNullAndEmpty :: Lude.Maybe Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'KinesisSettings' with the minimum fields required to make a request.
--
-- * 'includeTransactionDetails' - Provides detailed transaction information from the source database. This information includes a commit timestamp, a log position, and values for @transaction_id@ , previous @transaction_id@ , and @transaction_record_id@ (the record offset within a transaction). The default is @false@ .
-- * 'includeTableAlterOperations' - Includes any data definition language (DDL) operations that change the table in the control data, such as @rename-table@ , @drop-table@ , @add-column@ , @drop-column@ , and @rename-column@ . The default is @false@ .
-- * 'serviceAccessRoleARN' - The Amazon Resource Name (ARN) for the AWS Identity and Access Management (IAM) role that AWS DMS uses to write to the Kinesis data stream.
-- * 'partitionIncludeSchemaTable' - Prefixes schema and table names to partition values, when the partition type is @primary-key-type@ . Doing this increases data distribution among Kinesis shards. For example, suppose that a SysBench schema has thousands of tables and each table has only limited range for a primary key. In this case, the same primary key is sent from thousands of tables to the same shard, which causes throttling. The default is @false@ .
-- * 'streamARN' - The Amazon Resource Name (ARN) for the Amazon Kinesis Data Streams endpoint.
-- * 'includeControlDetails' - Shows detailed control information for table definition, column definition, and table and column changes in the Kinesis message output. The default is @false@ .
-- * 'includePartitionValue' - Shows the partition value within the Kinesis message output, unless the partition type is @schema-table-type@ . The default is @false@ .
-- * 'messageFormat' - The output format for the records created on the endpoint. The message format is @JSON@ (default) or @JSON_UNFORMATTED@ (a single line with no tab).
-- * 'includeNullAndEmpty' - Include NULL and empty columns for records migrated to the endpoint. The default is @false@ .
mkKinesisSettings ::
  KinesisSettings
mkKinesisSettings =
  KinesisSettings'
    { includeTransactionDetails = Lude.Nothing,
      includeTableAlterOperations = Lude.Nothing,
      serviceAccessRoleARN = Lude.Nothing,
      partitionIncludeSchemaTable = Lude.Nothing,
      streamARN = Lude.Nothing,
      includeControlDetails = Lude.Nothing,
      includePartitionValue = Lude.Nothing,
      messageFormat = Lude.Nothing,
      includeNullAndEmpty = Lude.Nothing
    }

-- | Provides detailed transaction information from the source database. This information includes a commit timestamp, a log position, and values for @transaction_id@ , previous @transaction_id@ , and @transaction_record_id@ (the record offset within a transaction). The default is @false@ .
--
-- /Note:/ Consider using 'includeTransactionDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kIncludeTransactionDetails :: Lens.Lens' KinesisSettings (Lude.Maybe Lude.Bool)
kIncludeTransactionDetails = Lens.lens (includeTransactionDetails :: KinesisSettings -> Lude.Maybe Lude.Bool) (\s a -> s {includeTransactionDetails = a} :: KinesisSettings)
{-# DEPRECATED kIncludeTransactionDetails "Use generic-lens or generic-optics with 'includeTransactionDetails' instead." #-}

-- | Includes any data definition language (DDL) operations that change the table in the control data, such as @rename-table@ , @drop-table@ , @add-column@ , @drop-column@ , and @rename-column@ . The default is @false@ .
--
-- /Note:/ Consider using 'includeTableAlterOperations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kIncludeTableAlterOperations :: Lens.Lens' KinesisSettings (Lude.Maybe Lude.Bool)
kIncludeTableAlterOperations = Lens.lens (includeTableAlterOperations :: KinesisSettings -> Lude.Maybe Lude.Bool) (\s a -> s {includeTableAlterOperations = a} :: KinesisSettings)
{-# DEPRECATED kIncludeTableAlterOperations "Use generic-lens or generic-optics with 'includeTableAlterOperations' instead." #-}

-- | The Amazon Resource Name (ARN) for the AWS Identity and Access Management (IAM) role that AWS DMS uses to write to the Kinesis data stream.
--
-- /Note:/ Consider using 'serviceAccessRoleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kServiceAccessRoleARN :: Lens.Lens' KinesisSettings (Lude.Maybe Lude.Text)
kServiceAccessRoleARN = Lens.lens (serviceAccessRoleARN :: KinesisSettings -> Lude.Maybe Lude.Text) (\s a -> s {serviceAccessRoleARN = a} :: KinesisSettings)
{-# DEPRECATED kServiceAccessRoleARN "Use generic-lens or generic-optics with 'serviceAccessRoleARN' instead." #-}

-- | Prefixes schema and table names to partition values, when the partition type is @primary-key-type@ . Doing this increases data distribution among Kinesis shards. For example, suppose that a SysBench schema has thousands of tables and each table has only limited range for a primary key. In this case, the same primary key is sent from thousands of tables to the same shard, which causes throttling. The default is @false@ .
--
-- /Note:/ Consider using 'partitionIncludeSchemaTable' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kPartitionIncludeSchemaTable :: Lens.Lens' KinesisSettings (Lude.Maybe Lude.Bool)
kPartitionIncludeSchemaTable = Lens.lens (partitionIncludeSchemaTable :: KinesisSettings -> Lude.Maybe Lude.Bool) (\s a -> s {partitionIncludeSchemaTable = a} :: KinesisSettings)
{-# DEPRECATED kPartitionIncludeSchemaTable "Use generic-lens or generic-optics with 'partitionIncludeSchemaTable' instead." #-}

-- | The Amazon Resource Name (ARN) for the Amazon Kinesis Data Streams endpoint.
--
-- /Note:/ Consider using 'streamARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kStreamARN :: Lens.Lens' KinesisSettings (Lude.Maybe Lude.Text)
kStreamARN = Lens.lens (streamARN :: KinesisSettings -> Lude.Maybe Lude.Text) (\s a -> s {streamARN = a} :: KinesisSettings)
{-# DEPRECATED kStreamARN "Use generic-lens or generic-optics with 'streamARN' instead." #-}

-- | Shows detailed control information for table definition, column definition, and table and column changes in the Kinesis message output. The default is @false@ .
--
-- /Note:/ Consider using 'includeControlDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kIncludeControlDetails :: Lens.Lens' KinesisSettings (Lude.Maybe Lude.Bool)
kIncludeControlDetails = Lens.lens (includeControlDetails :: KinesisSettings -> Lude.Maybe Lude.Bool) (\s a -> s {includeControlDetails = a} :: KinesisSettings)
{-# DEPRECATED kIncludeControlDetails "Use generic-lens or generic-optics with 'includeControlDetails' instead." #-}

-- | Shows the partition value within the Kinesis message output, unless the partition type is @schema-table-type@ . The default is @false@ .
--
-- /Note:/ Consider using 'includePartitionValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kIncludePartitionValue :: Lens.Lens' KinesisSettings (Lude.Maybe Lude.Bool)
kIncludePartitionValue = Lens.lens (includePartitionValue :: KinesisSettings -> Lude.Maybe Lude.Bool) (\s a -> s {includePartitionValue = a} :: KinesisSettings)
{-# DEPRECATED kIncludePartitionValue "Use generic-lens or generic-optics with 'includePartitionValue' instead." #-}

-- | The output format for the records created on the endpoint. The message format is @JSON@ (default) or @JSON_UNFORMATTED@ (a single line with no tab).
--
-- /Note:/ Consider using 'messageFormat' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kMessageFormat :: Lens.Lens' KinesisSettings (Lude.Maybe MessageFormatValue)
kMessageFormat = Lens.lens (messageFormat :: KinesisSettings -> Lude.Maybe MessageFormatValue) (\s a -> s {messageFormat = a} :: KinesisSettings)
{-# DEPRECATED kMessageFormat "Use generic-lens or generic-optics with 'messageFormat' instead." #-}

-- | Include NULL and empty columns for records migrated to the endpoint. The default is @false@ .
--
-- /Note:/ Consider using 'includeNullAndEmpty' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kIncludeNullAndEmpty :: Lens.Lens' KinesisSettings (Lude.Maybe Lude.Bool)
kIncludeNullAndEmpty = Lens.lens (includeNullAndEmpty :: KinesisSettings -> Lude.Maybe Lude.Bool) (\s a -> s {includeNullAndEmpty = a} :: KinesisSettings)
{-# DEPRECATED kIncludeNullAndEmpty "Use generic-lens or generic-optics with 'includeNullAndEmpty' instead." #-}

instance Lude.FromJSON KinesisSettings where
  parseJSON =
    Lude.withObject
      "KinesisSettings"
      ( \x ->
          KinesisSettings'
            Lude.<$> (x Lude..:? "IncludeTransactionDetails")
            Lude.<*> (x Lude..:? "IncludeTableAlterOperations")
            Lude.<*> (x Lude..:? "ServiceAccessRoleArn")
            Lude.<*> (x Lude..:? "PartitionIncludeSchemaTable")
            Lude.<*> (x Lude..:? "StreamArn")
            Lude.<*> (x Lude..:? "IncludeControlDetails")
            Lude.<*> (x Lude..:? "IncludePartitionValue")
            Lude.<*> (x Lude..:? "MessageFormat")
            Lude.<*> (x Lude..:? "IncludeNullAndEmpty")
      )

instance Lude.ToJSON KinesisSettings where
  toJSON KinesisSettings' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("IncludeTransactionDetails" Lude..=)
              Lude.<$> includeTransactionDetails,
            ("IncludeTableAlterOperations" Lude..=)
              Lude.<$> includeTableAlterOperations,
            ("ServiceAccessRoleArn" Lude..=) Lude.<$> serviceAccessRoleARN,
            ("PartitionIncludeSchemaTable" Lude..=)
              Lude.<$> partitionIncludeSchemaTable,
            ("StreamArn" Lude..=) Lude.<$> streamARN,
            ("IncludeControlDetails" Lude..=) Lude.<$> includeControlDetails,
            ("IncludePartitionValue" Lude..=) Lude.<$> includePartitionValue,
            ("MessageFormat" Lude..=) Lude.<$> messageFormat,
            ("IncludeNullAndEmpty" Lude..=) Lude.<$> includeNullAndEmpty
          ]
      )
