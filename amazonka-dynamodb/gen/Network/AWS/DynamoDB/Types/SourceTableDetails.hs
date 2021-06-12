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
-- Module      : Network.AWS.DynamoDB.Types.SourceTableDetails
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.SourceTableDetails where

import qualified Network.AWS.Core as Core
import Network.AWS.DynamoDB.Types.BillingMode
import Network.AWS.DynamoDB.Types.KeySchemaElement
import Network.AWS.DynamoDB.Types.ProvisionedThroughput
import qualified Network.AWS.Lens as Lens

-- | Contains the details of the table when the backup was created.
--
-- /See:/ 'newSourceTableDetails' smart constructor.
data SourceTableDetails = SourceTableDetails'
  { -- | ARN of the table for which backup was created.
    tableArn :: Core.Maybe Core.Text,
    -- | Controls how you are charged for read and write throughput and how you
    -- manage capacity. This setting can be changed later.
    --
    -- -   @PROVISIONED@ - Sets the read\/write capacity mode to @PROVISIONED@.
    --     We recommend using @PROVISIONED@ for predictable workloads.
    --
    -- -   @PAY_PER_REQUEST@ - Sets the read\/write capacity mode to
    --     @PAY_PER_REQUEST@. We recommend using @PAY_PER_REQUEST@ for
    --     unpredictable workloads.
    billingMode :: Core.Maybe BillingMode,
    -- | Size of the table in bytes. Note that this is an approximate value.
    tableSizeBytes :: Core.Maybe Core.Integer,
    -- | Number of items in the table. Note that this is an approximate value.
    itemCount :: Core.Maybe Core.Natural,
    -- | The name of the table for which the backup was created.
    tableName :: Core.Text,
    -- | Unique identifier for the table for which the backup was created.
    tableId :: Core.Text,
    -- | Schema of the table.
    keySchema :: Core.NonEmpty KeySchemaElement,
    -- | Time when the source table was created.
    tableCreationDateTime :: Core.POSIX,
    -- | Read IOPs and Write IOPS on the table when the backup was created.
    provisionedThroughput :: ProvisionedThroughput
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'SourceTableDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tableArn', 'sourceTableDetails_tableArn' - ARN of the table for which backup was created.
--
-- 'billingMode', 'sourceTableDetails_billingMode' - Controls how you are charged for read and write throughput and how you
-- manage capacity. This setting can be changed later.
--
-- -   @PROVISIONED@ - Sets the read\/write capacity mode to @PROVISIONED@.
--     We recommend using @PROVISIONED@ for predictable workloads.
--
-- -   @PAY_PER_REQUEST@ - Sets the read\/write capacity mode to
--     @PAY_PER_REQUEST@. We recommend using @PAY_PER_REQUEST@ for
--     unpredictable workloads.
--
-- 'tableSizeBytes', 'sourceTableDetails_tableSizeBytes' - Size of the table in bytes. Note that this is an approximate value.
--
-- 'itemCount', 'sourceTableDetails_itemCount' - Number of items in the table. Note that this is an approximate value.
--
-- 'tableName', 'sourceTableDetails_tableName' - The name of the table for which the backup was created.
--
-- 'tableId', 'sourceTableDetails_tableId' - Unique identifier for the table for which the backup was created.
--
-- 'keySchema', 'sourceTableDetails_keySchema' - Schema of the table.
--
-- 'tableCreationDateTime', 'sourceTableDetails_tableCreationDateTime' - Time when the source table was created.
--
-- 'provisionedThroughput', 'sourceTableDetails_provisionedThroughput' - Read IOPs and Write IOPS on the table when the backup was created.
newSourceTableDetails ::
  -- | 'tableName'
  Core.Text ->
  -- | 'tableId'
  Core.Text ->
  -- | 'keySchema'
  Core.NonEmpty KeySchemaElement ->
  -- | 'tableCreationDateTime'
  Core.UTCTime ->
  -- | 'provisionedThroughput'
  ProvisionedThroughput ->
  SourceTableDetails
newSourceTableDetails
  pTableName_
  pTableId_
  pKeySchema_
  pTableCreationDateTime_
  pProvisionedThroughput_ =
    SourceTableDetails'
      { tableArn = Core.Nothing,
        billingMode = Core.Nothing,
        tableSizeBytes = Core.Nothing,
        itemCount = Core.Nothing,
        tableName = pTableName_,
        tableId = pTableId_,
        keySchema = Lens._Coerce Lens.# pKeySchema_,
        tableCreationDateTime =
          Core._Time Lens.# pTableCreationDateTime_,
        provisionedThroughput = pProvisionedThroughput_
      }

-- | ARN of the table for which backup was created.
sourceTableDetails_tableArn :: Lens.Lens' SourceTableDetails (Core.Maybe Core.Text)
sourceTableDetails_tableArn = Lens.lens (\SourceTableDetails' {tableArn} -> tableArn) (\s@SourceTableDetails' {} a -> s {tableArn = a} :: SourceTableDetails)

-- | Controls how you are charged for read and write throughput and how you
-- manage capacity. This setting can be changed later.
--
-- -   @PROVISIONED@ - Sets the read\/write capacity mode to @PROVISIONED@.
--     We recommend using @PROVISIONED@ for predictable workloads.
--
-- -   @PAY_PER_REQUEST@ - Sets the read\/write capacity mode to
--     @PAY_PER_REQUEST@. We recommend using @PAY_PER_REQUEST@ for
--     unpredictable workloads.
sourceTableDetails_billingMode :: Lens.Lens' SourceTableDetails (Core.Maybe BillingMode)
sourceTableDetails_billingMode = Lens.lens (\SourceTableDetails' {billingMode} -> billingMode) (\s@SourceTableDetails' {} a -> s {billingMode = a} :: SourceTableDetails)

-- | Size of the table in bytes. Note that this is an approximate value.
sourceTableDetails_tableSizeBytes :: Lens.Lens' SourceTableDetails (Core.Maybe Core.Integer)
sourceTableDetails_tableSizeBytes = Lens.lens (\SourceTableDetails' {tableSizeBytes} -> tableSizeBytes) (\s@SourceTableDetails' {} a -> s {tableSizeBytes = a} :: SourceTableDetails)

-- | Number of items in the table. Note that this is an approximate value.
sourceTableDetails_itemCount :: Lens.Lens' SourceTableDetails (Core.Maybe Core.Natural)
sourceTableDetails_itemCount = Lens.lens (\SourceTableDetails' {itemCount} -> itemCount) (\s@SourceTableDetails' {} a -> s {itemCount = a} :: SourceTableDetails)

-- | The name of the table for which the backup was created.
sourceTableDetails_tableName :: Lens.Lens' SourceTableDetails Core.Text
sourceTableDetails_tableName = Lens.lens (\SourceTableDetails' {tableName} -> tableName) (\s@SourceTableDetails' {} a -> s {tableName = a} :: SourceTableDetails)

-- | Unique identifier for the table for which the backup was created.
sourceTableDetails_tableId :: Lens.Lens' SourceTableDetails Core.Text
sourceTableDetails_tableId = Lens.lens (\SourceTableDetails' {tableId} -> tableId) (\s@SourceTableDetails' {} a -> s {tableId = a} :: SourceTableDetails)

-- | Schema of the table.
sourceTableDetails_keySchema :: Lens.Lens' SourceTableDetails (Core.NonEmpty KeySchemaElement)
sourceTableDetails_keySchema = Lens.lens (\SourceTableDetails' {keySchema} -> keySchema) (\s@SourceTableDetails' {} a -> s {keySchema = a} :: SourceTableDetails) Core.. Lens._Coerce

-- | Time when the source table was created.
sourceTableDetails_tableCreationDateTime :: Lens.Lens' SourceTableDetails Core.UTCTime
sourceTableDetails_tableCreationDateTime = Lens.lens (\SourceTableDetails' {tableCreationDateTime} -> tableCreationDateTime) (\s@SourceTableDetails' {} a -> s {tableCreationDateTime = a} :: SourceTableDetails) Core.. Core._Time

-- | Read IOPs and Write IOPS on the table when the backup was created.
sourceTableDetails_provisionedThroughput :: Lens.Lens' SourceTableDetails ProvisionedThroughput
sourceTableDetails_provisionedThroughput = Lens.lens (\SourceTableDetails' {provisionedThroughput} -> provisionedThroughput) (\s@SourceTableDetails' {} a -> s {provisionedThroughput = a} :: SourceTableDetails)

instance Core.FromJSON SourceTableDetails where
  parseJSON =
    Core.withObject
      "SourceTableDetails"
      ( \x ->
          SourceTableDetails'
            Core.<$> (x Core..:? "TableArn")
            Core.<*> (x Core..:? "BillingMode")
            Core.<*> (x Core..:? "TableSizeBytes")
            Core.<*> (x Core..:? "ItemCount")
            Core.<*> (x Core..: "TableName")
            Core.<*> (x Core..: "TableId")
            Core.<*> (x Core..: "KeySchema")
            Core.<*> (x Core..: "TableCreationDateTime")
            Core.<*> (x Core..: "ProvisionedThroughput")
      )

instance Core.Hashable SourceTableDetails

instance Core.NFData SourceTableDetails
