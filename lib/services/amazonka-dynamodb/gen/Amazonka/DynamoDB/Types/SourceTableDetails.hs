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
-- Module      : Amazonka.DynamoDB.Types.SourceTableDetails
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DynamoDB.Types.SourceTableDetails where

import qualified Amazonka.Core as Core
import Amazonka.DynamoDB.Types.BillingMode
import Amazonka.DynamoDB.Types.KeySchemaElement
import Amazonka.DynamoDB.Types.ProvisionedThroughput
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Contains the details of the table when the backup was created.
--
-- /See:/ 'newSourceTableDetails' smart constructor.
data SourceTableDetails = SourceTableDetails'
  { -- | Size of the table in bytes. Note that this is an approximate value.
    tableSizeBytes :: Prelude.Maybe Prelude.Integer,
    -- | ARN of the table for which backup was created.
    tableArn :: Prelude.Maybe Prelude.Text,
    -- | Controls how you are charged for read and write throughput and how you
    -- manage capacity. This setting can be changed later.
    --
    -- -   @PROVISIONED@ - Sets the read\/write capacity mode to @PROVISIONED@.
    --     We recommend using @PROVISIONED@ for predictable workloads.
    --
    -- -   @PAY_PER_REQUEST@ - Sets the read\/write capacity mode to
    --     @PAY_PER_REQUEST@. We recommend using @PAY_PER_REQUEST@ for
    --     unpredictable workloads.
    billingMode :: Prelude.Maybe BillingMode,
    -- | Number of items in the table. Note that this is an approximate value.
    itemCount :: Prelude.Maybe Prelude.Natural,
    -- | The name of the table for which the backup was created.
    tableName :: Prelude.Text,
    -- | Unique identifier for the table for which the backup was created.
    tableId :: Prelude.Text,
    -- | Schema of the table.
    keySchema :: Prelude.NonEmpty KeySchemaElement,
    -- | Time when the source table was created.
    tableCreationDateTime :: Core.POSIX,
    -- | Read IOPs and Write IOPS on the table when the backup was created.
    provisionedThroughput :: ProvisionedThroughput
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SourceTableDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tableSizeBytes', 'sourceTableDetails_tableSizeBytes' - Size of the table in bytes. Note that this is an approximate value.
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
  Prelude.Text ->
  -- | 'tableId'
  Prelude.Text ->
  -- | 'keySchema'
  Prelude.NonEmpty KeySchemaElement ->
  -- | 'tableCreationDateTime'
  Prelude.UTCTime ->
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
      { tableSizeBytes =
          Prelude.Nothing,
        tableArn = Prelude.Nothing,
        billingMode = Prelude.Nothing,
        itemCount = Prelude.Nothing,
        tableName = pTableName_,
        tableId = pTableId_,
        keySchema = Lens.coerced Lens.# pKeySchema_,
        tableCreationDateTime =
          Core._Time Lens.# pTableCreationDateTime_,
        provisionedThroughput = pProvisionedThroughput_
      }

-- | Size of the table in bytes. Note that this is an approximate value.
sourceTableDetails_tableSizeBytes :: Lens.Lens' SourceTableDetails (Prelude.Maybe Prelude.Integer)
sourceTableDetails_tableSizeBytes = Lens.lens (\SourceTableDetails' {tableSizeBytes} -> tableSizeBytes) (\s@SourceTableDetails' {} a -> s {tableSizeBytes = a} :: SourceTableDetails)

-- | ARN of the table for which backup was created.
sourceTableDetails_tableArn :: Lens.Lens' SourceTableDetails (Prelude.Maybe Prelude.Text)
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
sourceTableDetails_billingMode :: Lens.Lens' SourceTableDetails (Prelude.Maybe BillingMode)
sourceTableDetails_billingMode = Lens.lens (\SourceTableDetails' {billingMode} -> billingMode) (\s@SourceTableDetails' {} a -> s {billingMode = a} :: SourceTableDetails)

-- | Number of items in the table. Note that this is an approximate value.
sourceTableDetails_itemCount :: Lens.Lens' SourceTableDetails (Prelude.Maybe Prelude.Natural)
sourceTableDetails_itemCount = Lens.lens (\SourceTableDetails' {itemCount} -> itemCount) (\s@SourceTableDetails' {} a -> s {itemCount = a} :: SourceTableDetails)

-- | The name of the table for which the backup was created.
sourceTableDetails_tableName :: Lens.Lens' SourceTableDetails Prelude.Text
sourceTableDetails_tableName = Lens.lens (\SourceTableDetails' {tableName} -> tableName) (\s@SourceTableDetails' {} a -> s {tableName = a} :: SourceTableDetails)

-- | Unique identifier for the table for which the backup was created.
sourceTableDetails_tableId :: Lens.Lens' SourceTableDetails Prelude.Text
sourceTableDetails_tableId = Lens.lens (\SourceTableDetails' {tableId} -> tableId) (\s@SourceTableDetails' {} a -> s {tableId = a} :: SourceTableDetails)

-- | Schema of the table.
sourceTableDetails_keySchema :: Lens.Lens' SourceTableDetails (Prelude.NonEmpty KeySchemaElement)
sourceTableDetails_keySchema = Lens.lens (\SourceTableDetails' {keySchema} -> keySchema) (\s@SourceTableDetails' {} a -> s {keySchema = a} :: SourceTableDetails) Prelude.. Lens.coerced

-- | Time when the source table was created.
sourceTableDetails_tableCreationDateTime :: Lens.Lens' SourceTableDetails Prelude.UTCTime
sourceTableDetails_tableCreationDateTime = Lens.lens (\SourceTableDetails' {tableCreationDateTime} -> tableCreationDateTime) (\s@SourceTableDetails' {} a -> s {tableCreationDateTime = a} :: SourceTableDetails) Prelude.. Core._Time

-- | Read IOPs and Write IOPS on the table when the backup was created.
sourceTableDetails_provisionedThroughput :: Lens.Lens' SourceTableDetails ProvisionedThroughput
sourceTableDetails_provisionedThroughput = Lens.lens (\SourceTableDetails' {provisionedThroughput} -> provisionedThroughput) (\s@SourceTableDetails' {} a -> s {provisionedThroughput = a} :: SourceTableDetails)

instance Core.FromJSON SourceTableDetails where
  parseJSON =
    Core.withObject
      "SourceTableDetails"
      ( \x ->
          SourceTableDetails'
            Prelude.<$> (x Core..:? "TableSizeBytes")
            Prelude.<*> (x Core..:? "TableArn")
            Prelude.<*> (x Core..:? "BillingMode")
            Prelude.<*> (x Core..:? "ItemCount")
            Prelude.<*> (x Core..: "TableName")
            Prelude.<*> (x Core..: "TableId")
            Prelude.<*> (x Core..: "KeySchema")
            Prelude.<*> (x Core..: "TableCreationDateTime")
            Prelude.<*> (x Core..: "ProvisionedThroughput")
      )

instance Prelude.Hashable SourceTableDetails where
  hashWithSalt salt' SourceTableDetails' {..} =
    salt' `Prelude.hashWithSalt` provisionedThroughput
      `Prelude.hashWithSalt` tableCreationDateTime
      `Prelude.hashWithSalt` keySchema
      `Prelude.hashWithSalt` tableId
      `Prelude.hashWithSalt` tableName
      `Prelude.hashWithSalt` itemCount
      `Prelude.hashWithSalt` billingMode
      `Prelude.hashWithSalt` tableArn
      `Prelude.hashWithSalt` tableSizeBytes

instance Prelude.NFData SourceTableDetails where
  rnf SourceTableDetails' {..} =
    Prelude.rnf tableSizeBytes
      `Prelude.seq` Prelude.rnf provisionedThroughput
      `Prelude.seq` Prelude.rnf tableCreationDateTime
      `Prelude.seq` Prelude.rnf keySchema
      `Prelude.seq` Prelude.rnf tableId
      `Prelude.seq` Prelude.rnf tableName
      `Prelude.seq` Prelude.rnf itemCount
      `Prelude.seq` Prelude.rnf billingMode
      `Prelude.seq` Prelude.rnf tableArn
