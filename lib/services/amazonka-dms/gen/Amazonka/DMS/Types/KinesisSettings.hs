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
-- Module      : Amazonka.DMS.Types.KinesisSettings
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DMS.Types.KinesisSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.DMS.Types.MessageFormatValue
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Provides information that describes an Amazon Kinesis Data Stream
-- endpoint. This information includes the output format of records applied
-- to the endpoint and details of transaction and control table data
-- information.
--
-- /See:/ 'newKinesisSettings' smart constructor.
data KinesisSettings = KinesisSettings'
  { -- | Shows detailed control information for table definition, column
    -- definition, and table and column changes in the Kinesis message output.
    -- The default is @false@.
    includeControlDetails :: Prelude.Maybe Prelude.Bool,
    -- | Include NULL and empty columns for records migrated to the endpoint. The
    -- default is @false@.
    includeNullAndEmpty :: Prelude.Maybe Prelude.Bool,
    -- | Shows the partition value within the Kinesis message output, unless the
    -- partition type is @schema-table-type@. The default is @false@.
    includePartitionValue :: Prelude.Maybe Prelude.Bool,
    -- | Includes any data definition language (DDL) operations that change the
    -- table in the control data, such as @rename-table@, @drop-table@,
    -- @add-column@, @drop-column@, and @rename-column@. The default is
    -- @false@.
    includeTableAlterOperations :: Prelude.Maybe Prelude.Bool,
    -- | Provides detailed transaction information from the source database. This
    -- information includes a commit timestamp, a log position, and values for
    -- @transaction_id@, previous @transaction_id@, and @transaction_record_id@
    -- (the record offset within a transaction). The default is @false@.
    includeTransactionDetails :: Prelude.Maybe Prelude.Bool,
    -- | The output format for the records created on the endpoint. The message
    -- format is @JSON@ (default) or @JSON_UNFORMATTED@ (a single line with no
    -- tab).
    messageFormat :: Prelude.Maybe MessageFormatValue,
    -- | Set this optional parameter to @true@ to avoid adding a \'0x\' prefix to
    -- raw data in hexadecimal format. For example, by default, DMS adds a
    -- \'0x\' prefix to the LOB column type in hexadecimal format moving from
    -- an Oracle source to an Amazon Kinesis target. Use the @NoHexPrefix@
    -- endpoint setting to enable migration of RAW data type columns without
    -- adding the \'0x\' prefix.
    noHexPrefix :: Prelude.Maybe Prelude.Bool,
    -- | Prefixes schema and table names to partition values, when the partition
    -- type is @primary-key-type@. Doing this increases data distribution among
    -- Kinesis shards. For example, suppose that a SysBench schema has
    -- thousands of tables and each table has only limited range for a primary
    -- key. In this case, the same primary key is sent from thousands of tables
    -- to the same shard, which causes throttling. The default is @false@.
    partitionIncludeSchemaTable :: Prelude.Maybe Prelude.Bool,
    -- | The Amazon Resource Name (ARN) for the IAM role that DMS uses to write
    -- to the Kinesis data stream. The role must allow the @iam:PassRole@
    -- action.
    serviceAccessRoleArn :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) for the Amazon Kinesis Data Streams
    -- endpoint.
    streamArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'KinesisSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'includeControlDetails', 'kinesisSettings_includeControlDetails' - Shows detailed control information for table definition, column
-- definition, and table and column changes in the Kinesis message output.
-- The default is @false@.
--
-- 'includeNullAndEmpty', 'kinesisSettings_includeNullAndEmpty' - Include NULL and empty columns for records migrated to the endpoint. The
-- default is @false@.
--
-- 'includePartitionValue', 'kinesisSettings_includePartitionValue' - Shows the partition value within the Kinesis message output, unless the
-- partition type is @schema-table-type@. The default is @false@.
--
-- 'includeTableAlterOperations', 'kinesisSettings_includeTableAlterOperations' - Includes any data definition language (DDL) operations that change the
-- table in the control data, such as @rename-table@, @drop-table@,
-- @add-column@, @drop-column@, and @rename-column@. The default is
-- @false@.
--
-- 'includeTransactionDetails', 'kinesisSettings_includeTransactionDetails' - Provides detailed transaction information from the source database. This
-- information includes a commit timestamp, a log position, and values for
-- @transaction_id@, previous @transaction_id@, and @transaction_record_id@
-- (the record offset within a transaction). The default is @false@.
--
-- 'messageFormat', 'kinesisSettings_messageFormat' - The output format for the records created on the endpoint. The message
-- format is @JSON@ (default) or @JSON_UNFORMATTED@ (a single line with no
-- tab).
--
-- 'noHexPrefix', 'kinesisSettings_noHexPrefix' - Set this optional parameter to @true@ to avoid adding a \'0x\' prefix to
-- raw data in hexadecimal format. For example, by default, DMS adds a
-- \'0x\' prefix to the LOB column type in hexadecimal format moving from
-- an Oracle source to an Amazon Kinesis target. Use the @NoHexPrefix@
-- endpoint setting to enable migration of RAW data type columns without
-- adding the \'0x\' prefix.
--
-- 'partitionIncludeSchemaTable', 'kinesisSettings_partitionIncludeSchemaTable' - Prefixes schema and table names to partition values, when the partition
-- type is @primary-key-type@. Doing this increases data distribution among
-- Kinesis shards. For example, suppose that a SysBench schema has
-- thousands of tables and each table has only limited range for a primary
-- key. In this case, the same primary key is sent from thousands of tables
-- to the same shard, which causes throttling. The default is @false@.
--
-- 'serviceAccessRoleArn', 'kinesisSettings_serviceAccessRoleArn' - The Amazon Resource Name (ARN) for the IAM role that DMS uses to write
-- to the Kinesis data stream. The role must allow the @iam:PassRole@
-- action.
--
-- 'streamArn', 'kinesisSettings_streamArn' - The Amazon Resource Name (ARN) for the Amazon Kinesis Data Streams
-- endpoint.
newKinesisSettings ::
  KinesisSettings
newKinesisSettings =
  KinesisSettings'
    { includeControlDetails =
        Prelude.Nothing,
      includeNullAndEmpty = Prelude.Nothing,
      includePartitionValue = Prelude.Nothing,
      includeTableAlterOperations = Prelude.Nothing,
      includeTransactionDetails = Prelude.Nothing,
      messageFormat = Prelude.Nothing,
      noHexPrefix = Prelude.Nothing,
      partitionIncludeSchemaTable = Prelude.Nothing,
      serviceAccessRoleArn = Prelude.Nothing,
      streamArn = Prelude.Nothing
    }

-- | Shows detailed control information for table definition, column
-- definition, and table and column changes in the Kinesis message output.
-- The default is @false@.
kinesisSettings_includeControlDetails :: Lens.Lens' KinesisSettings (Prelude.Maybe Prelude.Bool)
kinesisSettings_includeControlDetails = Lens.lens (\KinesisSettings' {includeControlDetails} -> includeControlDetails) (\s@KinesisSettings' {} a -> s {includeControlDetails = a} :: KinesisSettings)

-- | Include NULL and empty columns for records migrated to the endpoint. The
-- default is @false@.
kinesisSettings_includeNullAndEmpty :: Lens.Lens' KinesisSettings (Prelude.Maybe Prelude.Bool)
kinesisSettings_includeNullAndEmpty = Lens.lens (\KinesisSettings' {includeNullAndEmpty} -> includeNullAndEmpty) (\s@KinesisSettings' {} a -> s {includeNullAndEmpty = a} :: KinesisSettings)

-- | Shows the partition value within the Kinesis message output, unless the
-- partition type is @schema-table-type@. The default is @false@.
kinesisSettings_includePartitionValue :: Lens.Lens' KinesisSettings (Prelude.Maybe Prelude.Bool)
kinesisSettings_includePartitionValue = Lens.lens (\KinesisSettings' {includePartitionValue} -> includePartitionValue) (\s@KinesisSettings' {} a -> s {includePartitionValue = a} :: KinesisSettings)

-- | Includes any data definition language (DDL) operations that change the
-- table in the control data, such as @rename-table@, @drop-table@,
-- @add-column@, @drop-column@, and @rename-column@. The default is
-- @false@.
kinesisSettings_includeTableAlterOperations :: Lens.Lens' KinesisSettings (Prelude.Maybe Prelude.Bool)
kinesisSettings_includeTableAlterOperations = Lens.lens (\KinesisSettings' {includeTableAlterOperations} -> includeTableAlterOperations) (\s@KinesisSettings' {} a -> s {includeTableAlterOperations = a} :: KinesisSettings)

-- | Provides detailed transaction information from the source database. This
-- information includes a commit timestamp, a log position, and values for
-- @transaction_id@, previous @transaction_id@, and @transaction_record_id@
-- (the record offset within a transaction). The default is @false@.
kinesisSettings_includeTransactionDetails :: Lens.Lens' KinesisSettings (Prelude.Maybe Prelude.Bool)
kinesisSettings_includeTransactionDetails = Lens.lens (\KinesisSettings' {includeTransactionDetails} -> includeTransactionDetails) (\s@KinesisSettings' {} a -> s {includeTransactionDetails = a} :: KinesisSettings)

-- | The output format for the records created on the endpoint. The message
-- format is @JSON@ (default) or @JSON_UNFORMATTED@ (a single line with no
-- tab).
kinesisSettings_messageFormat :: Lens.Lens' KinesisSettings (Prelude.Maybe MessageFormatValue)
kinesisSettings_messageFormat = Lens.lens (\KinesisSettings' {messageFormat} -> messageFormat) (\s@KinesisSettings' {} a -> s {messageFormat = a} :: KinesisSettings)

-- | Set this optional parameter to @true@ to avoid adding a \'0x\' prefix to
-- raw data in hexadecimal format. For example, by default, DMS adds a
-- \'0x\' prefix to the LOB column type in hexadecimal format moving from
-- an Oracle source to an Amazon Kinesis target. Use the @NoHexPrefix@
-- endpoint setting to enable migration of RAW data type columns without
-- adding the \'0x\' prefix.
kinesisSettings_noHexPrefix :: Lens.Lens' KinesisSettings (Prelude.Maybe Prelude.Bool)
kinesisSettings_noHexPrefix = Lens.lens (\KinesisSettings' {noHexPrefix} -> noHexPrefix) (\s@KinesisSettings' {} a -> s {noHexPrefix = a} :: KinesisSettings)

-- | Prefixes schema and table names to partition values, when the partition
-- type is @primary-key-type@. Doing this increases data distribution among
-- Kinesis shards. For example, suppose that a SysBench schema has
-- thousands of tables and each table has only limited range for a primary
-- key. In this case, the same primary key is sent from thousands of tables
-- to the same shard, which causes throttling. The default is @false@.
kinesisSettings_partitionIncludeSchemaTable :: Lens.Lens' KinesisSettings (Prelude.Maybe Prelude.Bool)
kinesisSettings_partitionIncludeSchemaTable = Lens.lens (\KinesisSettings' {partitionIncludeSchemaTable} -> partitionIncludeSchemaTable) (\s@KinesisSettings' {} a -> s {partitionIncludeSchemaTable = a} :: KinesisSettings)

-- | The Amazon Resource Name (ARN) for the IAM role that DMS uses to write
-- to the Kinesis data stream. The role must allow the @iam:PassRole@
-- action.
kinesisSettings_serviceAccessRoleArn :: Lens.Lens' KinesisSettings (Prelude.Maybe Prelude.Text)
kinesisSettings_serviceAccessRoleArn = Lens.lens (\KinesisSettings' {serviceAccessRoleArn} -> serviceAccessRoleArn) (\s@KinesisSettings' {} a -> s {serviceAccessRoleArn = a} :: KinesisSettings)

-- | The Amazon Resource Name (ARN) for the Amazon Kinesis Data Streams
-- endpoint.
kinesisSettings_streamArn :: Lens.Lens' KinesisSettings (Prelude.Maybe Prelude.Text)
kinesisSettings_streamArn = Lens.lens (\KinesisSettings' {streamArn} -> streamArn) (\s@KinesisSettings' {} a -> s {streamArn = a} :: KinesisSettings)

instance Data.FromJSON KinesisSettings where
  parseJSON =
    Data.withObject
      "KinesisSettings"
      ( \x ->
          KinesisSettings'
            Prelude.<$> (x Data..:? "IncludeControlDetails")
            Prelude.<*> (x Data..:? "IncludeNullAndEmpty")
            Prelude.<*> (x Data..:? "IncludePartitionValue")
            Prelude.<*> (x Data..:? "IncludeTableAlterOperations")
            Prelude.<*> (x Data..:? "IncludeTransactionDetails")
            Prelude.<*> (x Data..:? "MessageFormat")
            Prelude.<*> (x Data..:? "NoHexPrefix")
            Prelude.<*> (x Data..:? "PartitionIncludeSchemaTable")
            Prelude.<*> (x Data..:? "ServiceAccessRoleArn")
            Prelude.<*> (x Data..:? "StreamArn")
      )

instance Prelude.Hashable KinesisSettings where
  hashWithSalt _salt KinesisSettings' {..} =
    _salt
      `Prelude.hashWithSalt` includeControlDetails
      `Prelude.hashWithSalt` includeNullAndEmpty
      `Prelude.hashWithSalt` includePartitionValue
      `Prelude.hashWithSalt` includeTableAlterOperations
      `Prelude.hashWithSalt` includeTransactionDetails
      `Prelude.hashWithSalt` messageFormat
      `Prelude.hashWithSalt` noHexPrefix
      `Prelude.hashWithSalt` partitionIncludeSchemaTable
      `Prelude.hashWithSalt` serviceAccessRoleArn
      `Prelude.hashWithSalt` streamArn

instance Prelude.NFData KinesisSettings where
  rnf KinesisSettings' {..} =
    Prelude.rnf includeControlDetails
      `Prelude.seq` Prelude.rnf includeNullAndEmpty
      `Prelude.seq` Prelude.rnf includePartitionValue
      `Prelude.seq` Prelude.rnf includeTableAlterOperations
      `Prelude.seq` Prelude.rnf includeTransactionDetails
      `Prelude.seq` Prelude.rnf messageFormat
      `Prelude.seq` Prelude.rnf noHexPrefix
      `Prelude.seq` Prelude.rnf partitionIncludeSchemaTable
      `Prelude.seq` Prelude.rnf serviceAccessRoleArn
      `Prelude.seq` Prelude.rnf streamArn

instance Data.ToJSON KinesisSettings where
  toJSON KinesisSettings' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("IncludeControlDetails" Data..=)
              Prelude.<$> includeControlDetails,
            ("IncludeNullAndEmpty" Data..=)
              Prelude.<$> includeNullAndEmpty,
            ("IncludePartitionValue" Data..=)
              Prelude.<$> includePartitionValue,
            ("IncludeTableAlterOperations" Data..=)
              Prelude.<$> includeTableAlterOperations,
            ("IncludeTransactionDetails" Data..=)
              Prelude.<$> includeTransactionDetails,
            ("MessageFormat" Data..=) Prelude.<$> messageFormat,
            ("NoHexPrefix" Data..=) Prelude.<$> noHexPrefix,
            ("PartitionIncludeSchemaTable" Data..=)
              Prelude.<$> partitionIncludeSchemaTable,
            ("ServiceAccessRoleArn" Data..=)
              Prelude.<$> serviceAccessRoleArn,
            ("StreamArn" Data..=) Prelude.<$> streamArn
          ]
      )
