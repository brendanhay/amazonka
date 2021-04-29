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
-- Module      : Network.AWS.DMS.Types.KinesisSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DMS.Types.KinesisSettings where

import Network.AWS.DMS.Types.MessageFormatValue
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Provides information that describes an Amazon Kinesis Data Stream
-- endpoint. This information includes the output format of records applied
-- to the endpoint and details of transaction and control table data
-- information.
--
-- /See:/ 'newKinesisSettings' smart constructor.
data KinesisSettings = KinesisSettings'
  { -- | Include NULL and empty columns for records migrated to the endpoint. The
    -- default is @false@.
    includeNullAndEmpty :: Prelude.Maybe Prelude.Bool,
    -- | The output format for the records created on the endpoint. The message
    -- format is @JSON@ (default) or @JSON_UNFORMATTED@ (a single line with no
    -- tab).
    messageFormat :: Prelude.Maybe MessageFormatValue,
    -- | The Amazon Resource Name (ARN) for the AWS Identity and Access
    -- Management (IAM) role that AWS DMS uses to write to the Kinesis data
    -- stream.
    serviceAccessRoleArn :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) for the Amazon Kinesis Data Streams
    -- endpoint.
    streamArn :: Prelude.Maybe Prelude.Text,
    -- | Prefixes schema and table names to partition values, when the partition
    -- type is @primary-key-type@. Doing this increases data distribution among
    -- Kinesis shards. For example, suppose that a SysBench schema has
    -- thousands of tables and each table has only limited range for a primary
    -- key. In this case, the same primary key is sent from thousands of tables
    -- to the same shard, which causes throttling. The default is @false@.
    partitionIncludeSchemaTable :: Prelude.Maybe Prelude.Bool,
    -- | Shows detailed control information for table definition, column
    -- definition, and table and column changes in the Kinesis message output.
    -- The default is @false@.
    includeControlDetails :: Prelude.Maybe Prelude.Bool,
    -- | Shows the partition value within the Kinesis message output, unless the
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
        Prelude.Nothing,
      messageFormat = Prelude.Nothing,
      serviceAccessRoleArn = Prelude.Nothing,
      streamArn = Prelude.Nothing,
      partitionIncludeSchemaTable = Prelude.Nothing,
      includeControlDetails = Prelude.Nothing,
      includePartitionValue = Prelude.Nothing,
      includeTransactionDetails = Prelude.Nothing,
      includeTableAlterOperations = Prelude.Nothing
    }

-- | Include NULL and empty columns for records migrated to the endpoint. The
-- default is @false@.
kinesisSettings_includeNullAndEmpty :: Lens.Lens' KinesisSettings (Prelude.Maybe Prelude.Bool)
kinesisSettings_includeNullAndEmpty = Lens.lens (\KinesisSettings' {includeNullAndEmpty} -> includeNullAndEmpty) (\s@KinesisSettings' {} a -> s {includeNullAndEmpty = a} :: KinesisSettings)

-- | The output format for the records created on the endpoint. The message
-- format is @JSON@ (default) or @JSON_UNFORMATTED@ (a single line with no
-- tab).
kinesisSettings_messageFormat :: Lens.Lens' KinesisSettings (Prelude.Maybe MessageFormatValue)
kinesisSettings_messageFormat = Lens.lens (\KinesisSettings' {messageFormat} -> messageFormat) (\s@KinesisSettings' {} a -> s {messageFormat = a} :: KinesisSettings)

-- | The Amazon Resource Name (ARN) for the AWS Identity and Access
-- Management (IAM) role that AWS DMS uses to write to the Kinesis data
-- stream.
kinesisSettings_serviceAccessRoleArn :: Lens.Lens' KinesisSettings (Prelude.Maybe Prelude.Text)
kinesisSettings_serviceAccessRoleArn = Lens.lens (\KinesisSettings' {serviceAccessRoleArn} -> serviceAccessRoleArn) (\s@KinesisSettings' {} a -> s {serviceAccessRoleArn = a} :: KinesisSettings)

-- | The Amazon Resource Name (ARN) for the Amazon Kinesis Data Streams
-- endpoint.
kinesisSettings_streamArn :: Lens.Lens' KinesisSettings (Prelude.Maybe Prelude.Text)
kinesisSettings_streamArn = Lens.lens (\KinesisSettings' {streamArn} -> streamArn) (\s@KinesisSettings' {} a -> s {streamArn = a} :: KinesisSettings)

-- | Prefixes schema and table names to partition values, when the partition
-- type is @primary-key-type@. Doing this increases data distribution among
-- Kinesis shards. For example, suppose that a SysBench schema has
-- thousands of tables and each table has only limited range for a primary
-- key. In this case, the same primary key is sent from thousands of tables
-- to the same shard, which causes throttling. The default is @false@.
kinesisSettings_partitionIncludeSchemaTable :: Lens.Lens' KinesisSettings (Prelude.Maybe Prelude.Bool)
kinesisSettings_partitionIncludeSchemaTable = Lens.lens (\KinesisSettings' {partitionIncludeSchemaTable} -> partitionIncludeSchemaTable) (\s@KinesisSettings' {} a -> s {partitionIncludeSchemaTable = a} :: KinesisSettings)

-- | Shows detailed control information for table definition, column
-- definition, and table and column changes in the Kinesis message output.
-- The default is @false@.
kinesisSettings_includeControlDetails :: Lens.Lens' KinesisSettings (Prelude.Maybe Prelude.Bool)
kinesisSettings_includeControlDetails = Lens.lens (\KinesisSettings' {includeControlDetails} -> includeControlDetails) (\s@KinesisSettings' {} a -> s {includeControlDetails = a} :: KinesisSettings)

-- | Shows the partition value within the Kinesis message output, unless the
-- partition type is @schema-table-type@. The default is @false@.
kinesisSettings_includePartitionValue :: Lens.Lens' KinesisSettings (Prelude.Maybe Prelude.Bool)
kinesisSettings_includePartitionValue = Lens.lens (\KinesisSettings' {includePartitionValue} -> includePartitionValue) (\s@KinesisSettings' {} a -> s {includePartitionValue = a} :: KinesisSettings)

-- | Provides detailed transaction information from the source database. This
-- information includes a commit timestamp, a log position, and values for
-- @transaction_id@, previous @transaction_id@, and @transaction_record_id@
-- (the record offset within a transaction). The default is @false@.
kinesisSettings_includeTransactionDetails :: Lens.Lens' KinesisSettings (Prelude.Maybe Prelude.Bool)
kinesisSettings_includeTransactionDetails = Lens.lens (\KinesisSettings' {includeTransactionDetails} -> includeTransactionDetails) (\s@KinesisSettings' {} a -> s {includeTransactionDetails = a} :: KinesisSettings)

-- | Includes any data definition language (DDL) operations that change the
-- table in the control data, such as @rename-table@, @drop-table@,
-- @add-column@, @drop-column@, and @rename-column@. The default is
-- @false@.
kinesisSettings_includeTableAlterOperations :: Lens.Lens' KinesisSettings (Prelude.Maybe Prelude.Bool)
kinesisSettings_includeTableAlterOperations = Lens.lens (\KinesisSettings' {includeTableAlterOperations} -> includeTableAlterOperations) (\s@KinesisSettings' {} a -> s {includeTableAlterOperations = a} :: KinesisSettings)

instance Prelude.FromJSON KinesisSettings where
  parseJSON =
    Prelude.withObject
      "KinesisSettings"
      ( \x ->
          KinesisSettings'
            Prelude.<$> (x Prelude..:? "IncludeNullAndEmpty")
            Prelude.<*> (x Prelude..:? "MessageFormat")
            Prelude.<*> (x Prelude..:? "ServiceAccessRoleArn")
            Prelude.<*> (x Prelude..:? "StreamArn")
            Prelude.<*> (x Prelude..:? "PartitionIncludeSchemaTable")
            Prelude.<*> (x Prelude..:? "IncludeControlDetails")
            Prelude.<*> (x Prelude..:? "IncludePartitionValue")
            Prelude.<*> (x Prelude..:? "IncludeTransactionDetails")
            Prelude.<*> (x Prelude..:? "IncludeTableAlterOperations")
      )

instance Prelude.Hashable KinesisSettings

instance Prelude.NFData KinesisSettings

instance Prelude.ToJSON KinesisSettings where
  toJSON KinesisSettings' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("IncludeNullAndEmpty" Prelude..=)
              Prelude.<$> includeNullAndEmpty,
            ("MessageFormat" Prelude..=)
              Prelude.<$> messageFormat,
            ("ServiceAccessRoleArn" Prelude..=)
              Prelude.<$> serviceAccessRoleArn,
            ("StreamArn" Prelude..=) Prelude.<$> streamArn,
            ("PartitionIncludeSchemaTable" Prelude..=)
              Prelude.<$> partitionIncludeSchemaTable,
            ("IncludeControlDetails" Prelude..=)
              Prelude.<$> includeControlDetails,
            ("IncludePartitionValue" Prelude..=)
              Prelude.<$> includePartitionValue,
            ("IncludeTransactionDetails" Prelude..=)
              Prelude.<$> includeTransactionDetails,
            ("IncludeTableAlterOperations" Prelude..=)
              Prelude.<$> includeTableAlterOperations
          ]
      )
