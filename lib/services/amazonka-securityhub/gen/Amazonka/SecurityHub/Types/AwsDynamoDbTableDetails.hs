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
-- Module      : Amazonka.SecurityHub.Types.AwsDynamoDbTableDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsDynamoDbTableDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.AwsDynamoDbTableAttributeDefinition
import Amazonka.SecurityHub.Types.AwsDynamoDbTableBillingModeSummary
import Amazonka.SecurityHub.Types.AwsDynamoDbTableGlobalSecondaryIndex
import Amazonka.SecurityHub.Types.AwsDynamoDbTableKeySchema
import Amazonka.SecurityHub.Types.AwsDynamoDbTableLocalSecondaryIndex
import Amazonka.SecurityHub.Types.AwsDynamoDbTableProvisionedThroughput
import Amazonka.SecurityHub.Types.AwsDynamoDbTableReplica
import Amazonka.SecurityHub.Types.AwsDynamoDbTableRestoreSummary
import Amazonka.SecurityHub.Types.AwsDynamoDbTableSseDescription
import Amazonka.SecurityHub.Types.AwsDynamoDbTableStreamSpecification

-- | Provides details about a DynamoDB table.
--
-- /See:/ 'newAwsDynamoDbTableDetails' smart constructor.
data AwsDynamoDbTableDetails = AwsDynamoDbTableDetails'
  { -- | A list of attribute definitions for the table.
    attributeDefinitions :: Prelude.Maybe [AwsDynamoDbTableAttributeDefinition],
    -- | Information about the billing for read\/write capacity on the table.
    billingModeSummary :: Prelude.Maybe AwsDynamoDbTableBillingModeSummary,
    -- | Indicates when the table was created.
    --
    -- Uses the @date-time@ format specified in
    -- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
    -- The value cannot contain spaces. For example,
    -- @2020-03-22T13:22:13.933Z@.
    creationDateTime :: Prelude.Maybe Prelude.Text,
    -- | List of global secondary indexes for the table.
    globalSecondaryIndexes :: Prelude.Maybe [AwsDynamoDbTableGlobalSecondaryIndex],
    -- | The version of global tables being used.
    globalTableVersion :: Prelude.Maybe Prelude.Text,
    -- | The number of items in the table.
    itemCount :: Prelude.Maybe Prelude.Int,
    -- | The primary key structure for the table.
    keySchema :: Prelude.Maybe [AwsDynamoDbTableKeySchema],
    -- | The ARN of the latest stream for the table.
    latestStreamArn :: Prelude.Maybe Prelude.Text,
    -- | The label of the latest stream. The label is not a unique identifier.
    latestStreamLabel :: Prelude.Maybe Prelude.Text,
    -- | The list of local secondary indexes for the table.
    localSecondaryIndexes :: Prelude.Maybe [AwsDynamoDbTableLocalSecondaryIndex],
    -- | Information about the provisioned throughput for the table.
    provisionedThroughput :: Prelude.Maybe AwsDynamoDbTableProvisionedThroughput,
    -- | The list of replicas of this table.
    replicas :: Prelude.Maybe [AwsDynamoDbTableReplica],
    -- | Information about the restore for the table.
    restoreSummary :: Prelude.Maybe AwsDynamoDbTableRestoreSummary,
    -- | Information about the server-side encryption for the table.
    sseDescription :: Prelude.Maybe AwsDynamoDbTableSseDescription,
    -- | The current DynamoDB Streams configuration for the table.
    streamSpecification :: Prelude.Maybe AwsDynamoDbTableStreamSpecification,
    -- | The identifier of the table.
    tableId :: Prelude.Maybe Prelude.Text,
    -- | The name of the table.
    tableName :: Prelude.Maybe Prelude.Text,
    -- | The total size of the table in bytes.
    tableSizeBytes :: Prelude.Maybe Prelude.Integer,
    -- | The current status of the table. Valid values are as follows:
    --
    -- -   @ACTIVE@
    --
    -- -   @ARCHIVED@
    --
    -- -   @ARCHIVING@
    --
    -- -   @CREATING@
    --
    -- -   @DELETING@
    --
    -- -   @INACCESSIBLE_ENCRYPTION_CREDENTIALS@
    --
    -- -   @UPDATING@
    tableStatus :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsDynamoDbTableDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attributeDefinitions', 'awsDynamoDbTableDetails_attributeDefinitions' - A list of attribute definitions for the table.
--
-- 'billingModeSummary', 'awsDynamoDbTableDetails_billingModeSummary' - Information about the billing for read\/write capacity on the table.
--
-- 'creationDateTime', 'awsDynamoDbTableDetails_creationDateTime' - Indicates when the table was created.
--
-- Uses the @date-time@ format specified in
-- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
-- The value cannot contain spaces. For example,
-- @2020-03-22T13:22:13.933Z@.
--
-- 'globalSecondaryIndexes', 'awsDynamoDbTableDetails_globalSecondaryIndexes' - List of global secondary indexes for the table.
--
-- 'globalTableVersion', 'awsDynamoDbTableDetails_globalTableVersion' - The version of global tables being used.
--
-- 'itemCount', 'awsDynamoDbTableDetails_itemCount' - The number of items in the table.
--
-- 'keySchema', 'awsDynamoDbTableDetails_keySchema' - The primary key structure for the table.
--
-- 'latestStreamArn', 'awsDynamoDbTableDetails_latestStreamArn' - The ARN of the latest stream for the table.
--
-- 'latestStreamLabel', 'awsDynamoDbTableDetails_latestStreamLabel' - The label of the latest stream. The label is not a unique identifier.
--
-- 'localSecondaryIndexes', 'awsDynamoDbTableDetails_localSecondaryIndexes' - The list of local secondary indexes for the table.
--
-- 'provisionedThroughput', 'awsDynamoDbTableDetails_provisionedThroughput' - Information about the provisioned throughput for the table.
--
-- 'replicas', 'awsDynamoDbTableDetails_replicas' - The list of replicas of this table.
--
-- 'restoreSummary', 'awsDynamoDbTableDetails_restoreSummary' - Information about the restore for the table.
--
-- 'sseDescription', 'awsDynamoDbTableDetails_sseDescription' - Information about the server-side encryption for the table.
--
-- 'streamSpecification', 'awsDynamoDbTableDetails_streamSpecification' - The current DynamoDB Streams configuration for the table.
--
-- 'tableId', 'awsDynamoDbTableDetails_tableId' - The identifier of the table.
--
-- 'tableName', 'awsDynamoDbTableDetails_tableName' - The name of the table.
--
-- 'tableSizeBytes', 'awsDynamoDbTableDetails_tableSizeBytes' - The total size of the table in bytes.
--
-- 'tableStatus', 'awsDynamoDbTableDetails_tableStatus' - The current status of the table. Valid values are as follows:
--
-- -   @ACTIVE@
--
-- -   @ARCHIVED@
--
-- -   @ARCHIVING@
--
-- -   @CREATING@
--
-- -   @DELETING@
--
-- -   @INACCESSIBLE_ENCRYPTION_CREDENTIALS@
--
-- -   @UPDATING@
newAwsDynamoDbTableDetails ::
  AwsDynamoDbTableDetails
newAwsDynamoDbTableDetails =
  AwsDynamoDbTableDetails'
    { attributeDefinitions =
        Prelude.Nothing,
      billingModeSummary = Prelude.Nothing,
      creationDateTime = Prelude.Nothing,
      globalSecondaryIndexes = Prelude.Nothing,
      globalTableVersion = Prelude.Nothing,
      itemCount = Prelude.Nothing,
      keySchema = Prelude.Nothing,
      latestStreamArn = Prelude.Nothing,
      latestStreamLabel = Prelude.Nothing,
      localSecondaryIndexes = Prelude.Nothing,
      provisionedThroughput = Prelude.Nothing,
      replicas = Prelude.Nothing,
      restoreSummary = Prelude.Nothing,
      sseDescription = Prelude.Nothing,
      streamSpecification = Prelude.Nothing,
      tableId = Prelude.Nothing,
      tableName = Prelude.Nothing,
      tableSizeBytes = Prelude.Nothing,
      tableStatus = Prelude.Nothing
    }

-- | A list of attribute definitions for the table.
awsDynamoDbTableDetails_attributeDefinitions :: Lens.Lens' AwsDynamoDbTableDetails (Prelude.Maybe [AwsDynamoDbTableAttributeDefinition])
awsDynamoDbTableDetails_attributeDefinitions = Lens.lens (\AwsDynamoDbTableDetails' {attributeDefinitions} -> attributeDefinitions) (\s@AwsDynamoDbTableDetails' {} a -> s {attributeDefinitions = a} :: AwsDynamoDbTableDetails) Prelude.. Lens.mapping Lens.coerced

-- | Information about the billing for read\/write capacity on the table.
awsDynamoDbTableDetails_billingModeSummary :: Lens.Lens' AwsDynamoDbTableDetails (Prelude.Maybe AwsDynamoDbTableBillingModeSummary)
awsDynamoDbTableDetails_billingModeSummary = Lens.lens (\AwsDynamoDbTableDetails' {billingModeSummary} -> billingModeSummary) (\s@AwsDynamoDbTableDetails' {} a -> s {billingModeSummary = a} :: AwsDynamoDbTableDetails)

-- | Indicates when the table was created.
--
-- Uses the @date-time@ format specified in
-- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
-- The value cannot contain spaces. For example,
-- @2020-03-22T13:22:13.933Z@.
awsDynamoDbTableDetails_creationDateTime :: Lens.Lens' AwsDynamoDbTableDetails (Prelude.Maybe Prelude.Text)
awsDynamoDbTableDetails_creationDateTime = Lens.lens (\AwsDynamoDbTableDetails' {creationDateTime} -> creationDateTime) (\s@AwsDynamoDbTableDetails' {} a -> s {creationDateTime = a} :: AwsDynamoDbTableDetails)

-- | List of global secondary indexes for the table.
awsDynamoDbTableDetails_globalSecondaryIndexes :: Lens.Lens' AwsDynamoDbTableDetails (Prelude.Maybe [AwsDynamoDbTableGlobalSecondaryIndex])
awsDynamoDbTableDetails_globalSecondaryIndexes = Lens.lens (\AwsDynamoDbTableDetails' {globalSecondaryIndexes} -> globalSecondaryIndexes) (\s@AwsDynamoDbTableDetails' {} a -> s {globalSecondaryIndexes = a} :: AwsDynamoDbTableDetails) Prelude.. Lens.mapping Lens.coerced

-- | The version of global tables being used.
awsDynamoDbTableDetails_globalTableVersion :: Lens.Lens' AwsDynamoDbTableDetails (Prelude.Maybe Prelude.Text)
awsDynamoDbTableDetails_globalTableVersion = Lens.lens (\AwsDynamoDbTableDetails' {globalTableVersion} -> globalTableVersion) (\s@AwsDynamoDbTableDetails' {} a -> s {globalTableVersion = a} :: AwsDynamoDbTableDetails)

-- | The number of items in the table.
awsDynamoDbTableDetails_itemCount :: Lens.Lens' AwsDynamoDbTableDetails (Prelude.Maybe Prelude.Int)
awsDynamoDbTableDetails_itemCount = Lens.lens (\AwsDynamoDbTableDetails' {itemCount} -> itemCount) (\s@AwsDynamoDbTableDetails' {} a -> s {itemCount = a} :: AwsDynamoDbTableDetails)

-- | The primary key structure for the table.
awsDynamoDbTableDetails_keySchema :: Lens.Lens' AwsDynamoDbTableDetails (Prelude.Maybe [AwsDynamoDbTableKeySchema])
awsDynamoDbTableDetails_keySchema = Lens.lens (\AwsDynamoDbTableDetails' {keySchema} -> keySchema) (\s@AwsDynamoDbTableDetails' {} a -> s {keySchema = a} :: AwsDynamoDbTableDetails) Prelude.. Lens.mapping Lens.coerced

-- | The ARN of the latest stream for the table.
awsDynamoDbTableDetails_latestStreamArn :: Lens.Lens' AwsDynamoDbTableDetails (Prelude.Maybe Prelude.Text)
awsDynamoDbTableDetails_latestStreamArn = Lens.lens (\AwsDynamoDbTableDetails' {latestStreamArn} -> latestStreamArn) (\s@AwsDynamoDbTableDetails' {} a -> s {latestStreamArn = a} :: AwsDynamoDbTableDetails)

-- | The label of the latest stream. The label is not a unique identifier.
awsDynamoDbTableDetails_latestStreamLabel :: Lens.Lens' AwsDynamoDbTableDetails (Prelude.Maybe Prelude.Text)
awsDynamoDbTableDetails_latestStreamLabel = Lens.lens (\AwsDynamoDbTableDetails' {latestStreamLabel} -> latestStreamLabel) (\s@AwsDynamoDbTableDetails' {} a -> s {latestStreamLabel = a} :: AwsDynamoDbTableDetails)

-- | The list of local secondary indexes for the table.
awsDynamoDbTableDetails_localSecondaryIndexes :: Lens.Lens' AwsDynamoDbTableDetails (Prelude.Maybe [AwsDynamoDbTableLocalSecondaryIndex])
awsDynamoDbTableDetails_localSecondaryIndexes = Lens.lens (\AwsDynamoDbTableDetails' {localSecondaryIndexes} -> localSecondaryIndexes) (\s@AwsDynamoDbTableDetails' {} a -> s {localSecondaryIndexes = a} :: AwsDynamoDbTableDetails) Prelude.. Lens.mapping Lens.coerced

-- | Information about the provisioned throughput for the table.
awsDynamoDbTableDetails_provisionedThroughput :: Lens.Lens' AwsDynamoDbTableDetails (Prelude.Maybe AwsDynamoDbTableProvisionedThroughput)
awsDynamoDbTableDetails_provisionedThroughput = Lens.lens (\AwsDynamoDbTableDetails' {provisionedThroughput} -> provisionedThroughput) (\s@AwsDynamoDbTableDetails' {} a -> s {provisionedThroughput = a} :: AwsDynamoDbTableDetails)

-- | The list of replicas of this table.
awsDynamoDbTableDetails_replicas :: Lens.Lens' AwsDynamoDbTableDetails (Prelude.Maybe [AwsDynamoDbTableReplica])
awsDynamoDbTableDetails_replicas = Lens.lens (\AwsDynamoDbTableDetails' {replicas} -> replicas) (\s@AwsDynamoDbTableDetails' {} a -> s {replicas = a} :: AwsDynamoDbTableDetails) Prelude.. Lens.mapping Lens.coerced

-- | Information about the restore for the table.
awsDynamoDbTableDetails_restoreSummary :: Lens.Lens' AwsDynamoDbTableDetails (Prelude.Maybe AwsDynamoDbTableRestoreSummary)
awsDynamoDbTableDetails_restoreSummary = Lens.lens (\AwsDynamoDbTableDetails' {restoreSummary} -> restoreSummary) (\s@AwsDynamoDbTableDetails' {} a -> s {restoreSummary = a} :: AwsDynamoDbTableDetails)

-- | Information about the server-side encryption for the table.
awsDynamoDbTableDetails_sseDescription :: Lens.Lens' AwsDynamoDbTableDetails (Prelude.Maybe AwsDynamoDbTableSseDescription)
awsDynamoDbTableDetails_sseDescription = Lens.lens (\AwsDynamoDbTableDetails' {sseDescription} -> sseDescription) (\s@AwsDynamoDbTableDetails' {} a -> s {sseDescription = a} :: AwsDynamoDbTableDetails)

-- | The current DynamoDB Streams configuration for the table.
awsDynamoDbTableDetails_streamSpecification :: Lens.Lens' AwsDynamoDbTableDetails (Prelude.Maybe AwsDynamoDbTableStreamSpecification)
awsDynamoDbTableDetails_streamSpecification = Lens.lens (\AwsDynamoDbTableDetails' {streamSpecification} -> streamSpecification) (\s@AwsDynamoDbTableDetails' {} a -> s {streamSpecification = a} :: AwsDynamoDbTableDetails)

-- | The identifier of the table.
awsDynamoDbTableDetails_tableId :: Lens.Lens' AwsDynamoDbTableDetails (Prelude.Maybe Prelude.Text)
awsDynamoDbTableDetails_tableId = Lens.lens (\AwsDynamoDbTableDetails' {tableId} -> tableId) (\s@AwsDynamoDbTableDetails' {} a -> s {tableId = a} :: AwsDynamoDbTableDetails)

-- | The name of the table.
awsDynamoDbTableDetails_tableName :: Lens.Lens' AwsDynamoDbTableDetails (Prelude.Maybe Prelude.Text)
awsDynamoDbTableDetails_tableName = Lens.lens (\AwsDynamoDbTableDetails' {tableName} -> tableName) (\s@AwsDynamoDbTableDetails' {} a -> s {tableName = a} :: AwsDynamoDbTableDetails)

-- | The total size of the table in bytes.
awsDynamoDbTableDetails_tableSizeBytes :: Lens.Lens' AwsDynamoDbTableDetails (Prelude.Maybe Prelude.Integer)
awsDynamoDbTableDetails_tableSizeBytes = Lens.lens (\AwsDynamoDbTableDetails' {tableSizeBytes} -> tableSizeBytes) (\s@AwsDynamoDbTableDetails' {} a -> s {tableSizeBytes = a} :: AwsDynamoDbTableDetails)

-- | The current status of the table. Valid values are as follows:
--
-- -   @ACTIVE@
--
-- -   @ARCHIVED@
--
-- -   @ARCHIVING@
--
-- -   @CREATING@
--
-- -   @DELETING@
--
-- -   @INACCESSIBLE_ENCRYPTION_CREDENTIALS@
--
-- -   @UPDATING@
awsDynamoDbTableDetails_tableStatus :: Lens.Lens' AwsDynamoDbTableDetails (Prelude.Maybe Prelude.Text)
awsDynamoDbTableDetails_tableStatus = Lens.lens (\AwsDynamoDbTableDetails' {tableStatus} -> tableStatus) (\s@AwsDynamoDbTableDetails' {} a -> s {tableStatus = a} :: AwsDynamoDbTableDetails)

instance Data.FromJSON AwsDynamoDbTableDetails where
  parseJSON =
    Data.withObject
      "AwsDynamoDbTableDetails"
      ( \x ->
          AwsDynamoDbTableDetails'
            Prelude.<$> ( x
                            Data..:? "AttributeDefinitions"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "BillingModeSummary")
            Prelude.<*> (x Data..:? "CreationDateTime")
            Prelude.<*> ( x
                            Data..:? "GlobalSecondaryIndexes"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "GlobalTableVersion")
            Prelude.<*> (x Data..:? "ItemCount")
            Prelude.<*> (x Data..:? "KeySchema" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "LatestStreamArn")
            Prelude.<*> (x Data..:? "LatestStreamLabel")
            Prelude.<*> ( x
                            Data..:? "LocalSecondaryIndexes"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "ProvisionedThroughput")
            Prelude.<*> (x Data..:? "Replicas" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "RestoreSummary")
            Prelude.<*> (x Data..:? "SseDescription")
            Prelude.<*> (x Data..:? "StreamSpecification")
            Prelude.<*> (x Data..:? "TableId")
            Prelude.<*> (x Data..:? "TableName")
            Prelude.<*> (x Data..:? "TableSizeBytes")
            Prelude.<*> (x Data..:? "TableStatus")
      )

instance Prelude.Hashable AwsDynamoDbTableDetails where
  hashWithSalt _salt AwsDynamoDbTableDetails' {..} =
    _salt
      `Prelude.hashWithSalt` attributeDefinitions
      `Prelude.hashWithSalt` billingModeSummary
      `Prelude.hashWithSalt` creationDateTime
      `Prelude.hashWithSalt` globalSecondaryIndexes
      `Prelude.hashWithSalt` globalTableVersion
      `Prelude.hashWithSalt` itemCount
      `Prelude.hashWithSalt` keySchema
      `Prelude.hashWithSalt` latestStreamArn
      `Prelude.hashWithSalt` latestStreamLabel
      `Prelude.hashWithSalt` localSecondaryIndexes
      `Prelude.hashWithSalt` provisionedThroughput
      `Prelude.hashWithSalt` replicas
      `Prelude.hashWithSalt` restoreSummary
      `Prelude.hashWithSalt` sseDescription
      `Prelude.hashWithSalt` streamSpecification
      `Prelude.hashWithSalt` tableId
      `Prelude.hashWithSalt` tableName
      `Prelude.hashWithSalt` tableSizeBytes
      `Prelude.hashWithSalt` tableStatus

instance Prelude.NFData AwsDynamoDbTableDetails where
  rnf AwsDynamoDbTableDetails' {..} =
    Prelude.rnf attributeDefinitions
      `Prelude.seq` Prelude.rnf billingModeSummary
      `Prelude.seq` Prelude.rnf creationDateTime
      `Prelude.seq` Prelude.rnf globalSecondaryIndexes
      `Prelude.seq` Prelude.rnf globalTableVersion
      `Prelude.seq` Prelude.rnf itemCount
      `Prelude.seq` Prelude.rnf keySchema
      `Prelude.seq` Prelude.rnf latestStreamArn
      `Prelude.seq` Prelude.rnf latestStreamLabel
      `Prelude.seq` Prelude.rnf localSecondaryIndexes
      `Prelude.seq` Prelude.rnf provisionedThroughput
      `Prelude.seq` Prelude.rnf replicas
      `Prelude.seq` Prelude.rnf restoreSummary
      `Prelude.seq` Prelude.rnf sseDescription
      `Prelude.seq` Prelude.rnf streamSpecification
      `Prelude.seq` Prelude.rnf tableId
      `Prelude.seq` Prelude.rnf tableName
      `Prelude.seq` Prelude.rnf tableSizeBytes
      `Prelude.seq` Prelude.rnf tableStatus

instance Data.ToJSON AwsDynamoDbTableDetails where
  toJSON AwsDynamoDbTableDetails' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AttributeDefinitions" Data..=)
              Prelude.<$> attributeDefinitions,
            ("BillingModeSummary" Data..=)
              Prelude.<$> billingModeSummary,
            ("CreationDateTime" Data..=)
              Prelude.<$> creationDateTime,
            ("GlobalSecondaryIndexes" Data..=)
              Prelude.<$> globalSecondaryIndexes,
            ("GlobalTableVersion" Data..=)
              Prelude.<$> globalTableVersion,
            ("ItemCount" Data..=) Prelude.<$> itemCount,
            ("KeySchema" Data..=) Prelude.<$> keySchema,
            ("LatestStreamArn" Data..=)
              Prelude.<$> latestStreamArn,
            ("LatestStreamLabel" Data..=)
              Prelude.<$> latestStreamLabel,
            ("LocalSecondaryIndexes" Data..=)
              Prelude.<$> localSecondaryIndexes,
            ("ProvisionedThroughput" Data..=)
              Prelude.<$> provisionedThroughput,
            ("Replicas" Data..=) Prelude.<$> replicas,
            ("RestoreSummary" Data..=)
              Prelude.<$> restoreSummary,
            ("SseDescription" Data..=)
              Prelude.<$> sseDescription,
            ("StreamSpecification" Data..=)
              Prelude.<$> streamSpecification,
            ("TableId" Data..=) Prelude.<$> tableId,
            ("TableName" Data..=) Prelude.<$> tableName,
            ("TableSizeBytes" Data..=)
              Prelude.<$> tableSizeBytes,
            ("TableStatus" Data..=) Prelude.<$> tableStatus
          ]
      )
