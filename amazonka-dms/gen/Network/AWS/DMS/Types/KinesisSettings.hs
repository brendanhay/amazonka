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
-- Module      : Network.AWS.DMS.Types.KinesisSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DMS.Types.KinesisSettings where

import qualified Network.AWS.Core as Core
import Network.AWS.DMS.Types.MessageFormatValue
import qualified Network.AWS.Lens as Lens

-- | Provides information that describes an Amazon Kinesis Data Stream
-- endpoint. This information includes the output format of records applied
-- to the endpoint and details of transaction and control table data
-- information.
--
-- /See:/ 'newKinesisSettings' smart constructor.
data KinesisSettings = KinesisSettings'
  { -- | Include NULL and empty columns for records migrated to the endpoint. The
    -- default is @false@.
    includeNullAndEmpty :: Core.Maybe Core.Bool,
    -- | The output format for the records created on the endpoint. The message
    -- format is @JSON@ (default) or @JSON_UNFORMATTED@ (a single line with no
    -- tab).
    messageFormat :: Core.Maybe MessageFormatValue,
    -- | The Amazon Resource Name (ARN) for the AWS Identity and Access
    -- Management (IAM) role that AWS DMS uses to write to the Kinesis data
    -- stream.
    serviceAccessRoleArn :: Core.Maybe Core.Text,
    -- | The Amazon Resource Name (ARN) for the Amazon Kinesis Data Streams
    -- endpoint.
    streamArn :: Core.Maybe Core.Text,
    -- | Prefixes schema and table names to partition values, when the partition
    -- type is @primary-key-type@. Doing this increases data distribution among
    -- Kinesis shards. For example, suppose that a SysBench schema has
    -- thousands of tables and each table has only limited range for a primary
    -- key. In this case, the same primary key is sent from thousands of tables
    -- to the same shard, which causes throttling. The default is @false@.
    partitionIncludeSchemaTable :: Core.Maybe Core.Bool,
    -- | Shows detailed control information for table definition, column
    -- definition, and table and column changes in the Kinesis message output.
    -- The default is @false@.
    includeControlDetails :: Core.Maybe Core.Bool,
    -- | Shows the partition value within the Kinesis message output, unless the
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
-- Create a value of 'KinesisSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'includeNullAndEmpty', 'kinesisSettings_includeNullAndEmpty' - Include NULL and empty columns for records migrated to the endpoint. The
-- default is @false@.
--
-- 'messageFormat', 'kinesisSettings_messageFormat' - The output format for the records created on the endpoint. The message
-- format is @JSON@ (default) or @JSON_UNFORMATTED@ (a single line with no
-- tab).
--
-- 'serviceAccessRoleArn', 'kinesisSettings_serviceAccessRoleArn' - The Amazon Resource Name (ARN) for the AWS Identity and Access
-- Management (IAM) role that AWS DMS uses to write to the Kinesis data
-- stream.
--
-- 'streamArn', 'kinesisSettings_streamArn' - The Amazon Resource Name (ARN) for the Amazon Kinesis Data Streams
-- endpoint.
--
-- 'partitionIncludeSchemaTable', 'kinesisSettings_partitionIncludeSchemaTable' - Prefixes schema and table names to partition values, when the partition
-- type is @primary-key-type@. Doing this increases data distribution among
-- Kinesis shards. For example, suppose that a SysBench schema has
-- thousands of tables and each table has only limited range for a primary
-- key. In this case, the same primary key is sent from thousands of tables
-- to the same shard, which causes throttling. The default is @false@.
--
-- 'includeControlDetails', 'kinesisSettings_includeControlDetails' - Shows detailed control information for table definition, column
-- definition, and table and column changes in the Kinesis message output.
-- The default is @false@.
--
-- 'includePartitionValue', 'kinesisSettings_includePartitionValue' - Shows the partition value within the Kinesis message output, unless the
-- partition type is @schema-table-type@. The default is @false@.
--
-- 'includeTransactionDetails', 'kinesisSettings_includeTransactionDetails' - Provides detailed transaction information from the source database. This
-- information includes a commit timestamp, a log position, and values for
-- @transaction_id@, previous @transaction_id@, and @transaction_record_id@
-- (the record offset within a transaction). The default is @false@.
--
-- 'includeTableAlterOperations', 'kinesisSettings_includeTableAlterOperations' - Includes any data definition language (DDL) operations that change the
-- table in the control data, such as @rename-table@, @drop-table@,
-- @add-column@, @drop-column@, and @rename-column@. The default is
-- @false@.
newKinesisSettings ::
  KinesisSettings
newKinesisSettings =
  KinesisSettings'
    { includeNullAndEmpty =
        Core.Nothing,
      messageFormat = Core.Nothing,
      serviceAccessRoleArn = Core.Nothing,
      streamArn = Core.Nothing,
      partitionIncludeSchemaTable = Core.Nothing,
      includeControlDetails = Core.Nothing,
      includePartitionValue = Core.Nothing,
      includeTransactionDetails = Core.Nothing,
      includeTableAlterOperations = Core.Nothing
    }

-- | Include NULL and empty columns for records migrated to the endpoint. The
-- default is @false@.
kinesisSettings_includeNullAndEmpty :: Lens.Lens' KinesisSettings (Core.Maybe Core.Bool)
kinesisSettings_includeNullAndEmpty = Lens.lens (\KinesisSettings' {includeNullAndEmpty} -> includeNullAndEmpty) (\s@KinesisSettings' {} a -> s {includeNullAndEmpty = a} :: KinesisSettings)

-- | The output format for the records created on the endpoint. The message
-- format is @JSON@ (default) or @JSON_UNFORMATTED@ (a single line with no
-- tab).
kinesisSettings_messageFormat :: Lens.Lens' KinesisSettings (Core.Maybe MessageFormatValue)
kinesisSettings_messageFormat = Lens.lens (\KinesisSettings' {messageFormat} -> messageFormat) (\s@KinesisSettings' {} a -> s {messageFormat = a} :: KinesisSettings)

-- | The Amazon Resource Name (ARN) for the AWS Identity and Access
-- Management (IAM) role that AWS DMS uses to write to the Kinesis data
-- stream.
kinesisSettings_serviceAccessRoleArn :: Lens.Lens' KinesisSettings (Core.Maybe Core.Text)
kinesisSettings_serviceAccessRoleArn = Lens.lens (\KinesisSettings' {serviceAccessRoleArn} -> serviceAccessRoleArn) (\s@KinesisSettings' {} a -> s {serviceAccessRoleArn = a} :: KinesisSettings)

-- | The Amazon Resource Name (ARN) for the Amazon Kinesis Data Streams
-- endpoint.
kinesisSettings_streamArn :: Lens.Lens' KinesisSettings (Core.Maybe Core.Text)
kinesisSettings_streamArn = Lens.lens (\KinesisSettings' {streamArn} -> streamArn) (\s@KinesisSettings' {} a -> s {streamArn = a} :: KinesisSettings)

-- | Prefixes schema and table names to partition values, when the partition
-- type is @primary-key-type@. Doing this increases data distribution among
-- Kinesis shards. For example, suppose that a SysBench schema has
-- thousands of tables and each table has only limited range for a primary
-- key. In this case, the same primary key is sent from thousands of tables
-- to the same shard, which causes throttling. The default is @false@.
kinesisSettings_partitionIncludeSchemaTable :: Lens.Lens' KinesisSettings (Core.Maybe Core.Bool)
kinesisSettings_partitionIncludeSchemaTable = Lens.lens (\KinesisSettings' {partitionIncludeSchemaTable} -> partitionIncludeSchemaTable) (\s@KinesisSettings' {} a -> s {partitionIncludeSchemaTable = a} :: KinesisSettings)

-- | Shows detailed control information for table definition, column
-- definition, and table and column changes in the Kinesis message output.
-- The default is @false@.
kinesisSettings_includeControlDetails :: Lens.Lens' KinesisSettings (Core.Maybe Core.Bool)
kinesisSettings_includeControlDetails = Lens.lens (\KinesisSettings' {includeControlDetails} -> includeControlDetails) (\s@KinesisSettings' {} a -> s {includeControlDetails = a} :: KinesisSettings)

-- | Shows the partition value within the Kinesis message output, unless the
-- partition type is @schema-table-type@. The default is @false@.
kinesisSettings_includePartitionValue :: Lens.Lens' KinesisSettings (Core.Maybe Core.Bool)
kinesisSettings_includePartitionValue = Lens.lens (\KinesisSettings' {includePartitionValue} -> includePartitionValue) (\s@KinesisSettings' {} a -> s {includePartitionValue = a} :: KinesisSettings)

-- | Provides detailed transaction information from the source database. This
-- information includes a commit timestamp, a log position, and values for
-- @transaction_id@, previous @transaction_id@, and @transaction_record_id@
-- (the record offset within a transaction). The default is @false@.
kinesisSettings_includeTransactionDetails :: Lens.Lens' KinesisSettings (Core.Maybe Core.Bool)
kinesisSettings_includeTransactionDetails = Lens.lens (\KinesisSettings' {includeTransactionDetails} -> includeTransactionDetails) (\s@KinesisSettings' {} a -> s {includeTransactionDetails = a} :: KinesisSettings)

-- | Includes any data definition language (DDL) operations that change the
-- table in the control data, such as @rename-table@, @drop-table@,
-- @add-column@, @drop-column@, and @rename-column@. The default is
-- @false@.
kinesisSettings_includeTableAlterOperations :: Lens.Lens' KinesisSettings (Core.Maybe Core.Bool)
kinesisSettings_includeTableAlterOperations = Lens.lens (\KinesisSettings' {includeTableAlterOperations} -> includeTableAlterOperations) (\s@KinesisSettings' {} a -> s {includeTableAlterOperations = a} :: KinesisSettings)

instance Core.FromJSON KinesisSettings where
  parseJSON =
    Core.withObject
      "KinesisSettings"
      ( \x ->
          KinesisSettings'
            Core.<$> (x Core..:? "IncludeNullAndEmpty")
            Core.<*> (x Core..:? "MessageFormat")
            Core.<*> (x Core..:? "ServiceAccessRoleArn")
            Core.<*> (x Core..:? "StreamArn")
            Core.<*> (x Core..:? "PartitionIncludeSchemaTable")
            Core.<*> (x Core..:? "IncludeControlDetails")
            Core.<*> (x Core..:? "IncludePartitionValue")
            Core.<*> (x Core..:? "IncludeTransactionDetails")
            Core.<*> (x Core..:? "IncludeTableAlterOperations")
      )

instance Core.Hashable KinesisSettings

instance Core.NFData KinesisSettings

instance Core.ToJSON KinesisSettings where
  toJSON KinesisSettings' {..} =
    Core.object
      ( Core.catMaybes
          [ ("IncludeNullAndEmpty" Core..=)
              Core.<$> includeNullAndEmpty,
            ("MessageFormat" Core..=) Core.<$> messageFormat,
            ("ServiceAccessRoleArn" Core..=)
              Core.<$> serviceAccessRoleArn,
            ("StreamArn" Core..=) Core.<$> streamArn,
            ("PartitionIncludeSchemaTable" Core..=)
              Core.<$> partitionIncludeSchemaTable,
            ("IncludeControlDetails" Core..=)
              Core.<$> includeControlDetails,
            ("IncludePartitionValue" Core..=)
              Core.<$> includePartitionValue,
            ("IncludeTransactionDetails" Core..=)
              Core.<$> includeTransactionDetails,
            ("IncludeTableAlterOperations" Core..=)
              Core.<$> includeTableAlterOperations
          ]
      )
