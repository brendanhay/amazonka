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
-- Module      : Amazonka.SecurityHub.Types.AwsDynamoDbTableGlobalSecondaryIndex
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsDynamoDbTableGlobalSecondaryIndex where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.AwsDynamoDbTableKeySchema
import Amazonka.SecurityHub.Types.AwsDynamoDbTableProjection
import Amazonka.SecurityHub.Types.AwsDynamoDbTableProvisionedThroughput

-- | Information abut a global secondary index for the table.
--
-- /See:/ 'newAwsDynamoDbTableGlobalSecondaryIndex' smart constructor.
data AwsDynamoDbTableGlobalSecondaryIndex = AwsDynamoDbTableGlobalSecondaryIndex'
  { -- | Whether the index is currently backfilling.
    backfilling :: Prelude.Maybe Prelude.Bool,
    -- | The ARN of the index.
    indexArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the index.
    indexName :: Prelude.Maybe Prelude.Text,
    -- | The total size in bytes of the index.
    indexSizeBytes :: Prelude.Maybe Prelude.Integer,
    -- | The current status of the index.
    --
    -- -   @ACTIVE@
    --
    -- -   @CREATING@
    --
    -- -   @DELETING@
    --
    -- -   @UPDATING@
    indexStatus :: Prelude.Maybe Prelude.Text,
    -- | The number of items in the index.
    itemCount :: Prelude.Maybe Prelude.Int,
    -- | The key schema for the index.
    keySchema :: Prelude.Maybe [AwsDynamoDbTableKeySchema],
    -- | Attributes that are copied from the table into an index.
    projection :: Prelude.Maybe AwsDynamoDbTableProjection,
    -- | Information about the provisioned throughput settings for the indexes.
    provisionedThroughput :: Prelude.Maybe AwsDynamoDbTableProvisionedThroughput
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsDynamoDbTableGlobalSecondaryIndex' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'backfilling', 'awsDynamoDbTableGlobalSecondaryIndex_backfilling' - Whether the index is currently backfilling.
--
-- 'indexArn', 'awsDynamoDbTableGlobalSecondaryIndex_indexArn' - The ARN of the index.
--
-- 'indexName', 'awsDynamoDbTableGlobalSecondaryIndex_indexName' - The name of the index.
--
-- 'indexSizeBytes', 'awsDynamoDbTableGlobalSecondaryIndex_indexSizeBytes' - The total size in bytes of the index.
--
-- 'indexStatus', 'awsDynamoDbTableGlobalSecondaryIndex_indexStatus' - The current status of the index.
--
-- -   @ACTIVE@
--
-- -   @CREATING@
--
-- -   @DELETING@
--
-- -   @UPDATING@
--
-- 'itemCount', 'awsDynamoDbTableGlobalSecondaryIndex_itemCount' - The number of items in the index.
--
-- 'keySchema', 'awsDynamoDbTableGlobalSecondaryIndex_keySchema' - The key schema for the index.
--
-- 'projection', 'awsDynamoDbTableGlobalSecondaryIndex_projection' - Attributes that are copied from the table into an index.
--
-- 'provisionedThroughput', 'awsDynamoDbTableGlobalSecondaryIndex_provisionedThroughput' - Information about the provisioned throughput settings for the indexes.
newAwsDynamoDbTableGlobalSecondaryIndex ::
  AwsDynamoDbTableGlobalSecondaryIndex
newAwsDynamoDbTableGlobalSecondaryIndex =
  AwsDynamoDbTableGlobalSecondaryIndex'
    { backfilling =
        Prelude.Nothing,
      indexArn = Prelude.Nothing,
      indexName = Prelude.Nothing,
      indexSizeBytes = Prelude.Nothing,
      indexStatus = Prelude.Nothing,
      itemCount = Prelude.Nothing,
      keySchema = Prelude.Nothing,
      projection = Prelude.Nothing,
      provisionedThroughput =
        Prelude.Nothing
    }

-- | Whether the index is currently backfilling.
awsDynamoDbTableGlobalSecondaryIndex_backfilling :: Lens.Lens' AwsDynamoDbTableGlobalSecondaryIndex (Prelude.Maybe Prelude.Bool)
awsDynamoDbTableGlobalSecondaryIndex_backfilling = Lens.lens (\AwsDynamoDbTableGlobalSecondaryIndex' {backfilling} -> backfilling) (\s@AwsDynamoDbTableGlobalSecondaryIndex' {} a -> s {backfilling = a} :: AwsDynamoDbTableGlobalSecondaryIndex)

-- | The ARN of the index.
awsDynamoDbTableGlobalSecondaryIndex_indexArn :: Lens.Lens' AwsDynamoDbTableGlobalSecondaryIndex (Prelude.Maybe Prelude.Text)
awsDynamoDbTableGlobalSecondaryIndex_indexArn = Lens.lens (\AwsDynamoDbTableGlobalSecondaryIndex' {indexArn} -> indexArn) (\s@AwsDynamoDbTableGlobalSecondaryIndex' {} a -> s {indexArn = a} :: AwsDynamoDbTableGlobalSecondaryIndex)

-- | The name of the index.
awsDynamoDbTableGlobalSecondaryIndex_indexName :: Lens.Lens' AwsDynamoDbTableGlobalSecondaryIndex (Prelude.Maybe Prelude.Text)
awsDynamoDbTableGlobalSecondaryIndex_indexName = Lens.lens (\AwsDynamoDbTableGlobalSecondaryIndex' {indexName} -> indexName) (\s@AwsDynamoDbTableGlobalSecondaryIndex' {} a -> s {indexName = a} :: AwsDynamoDbTableGlobalSecondaryIndex)

-- | The total size in bytes of the index.
awsDynamoDbTableGlobalSecondaryIndex_indexSizeBytes :: Lens.Lens' AwsDynamoDbTableGlobalSecondaryIndex (Prelude.Maybe Prelude.Integer)
awsDynamoDbTableGlobalSecondaryIndex_indexSizeBytes = Lens.lens (\AwsDynamoDbTableGlobalSecondaryIndex' {indexSizeBytes} -> indexSizeBytes) (\s@AwsDynamoDbTableGlobalSecondaryIndex' {} a -> s {indexSizeBytes = a} :: AwsDynamoDbTableGlobalSecondaryIndex)

-- | The current status of the index.
--
-- -   @ACTIVE@
--
-- -   @CREATING@
--
-- -   @DELETING@
--
-- -   @UPDATING@
awsDynamoDbTableGlobalSecondaryIndex_indexStatus :: Lens.Lens' AwsDynamoDbTableGlobalSecondaryIndex (Prelude.Maybe Prelude.Text)
awsDynamoDbTableGlobalSecondaryIndex_indexStatus = Lens.lens (\AwsDynamoDbTableGlobalSecondaryIndex' {indexStatus} -> indexStatus) (\s@AwsDynamoDbTableGlobalSecondaryIndex' {} a -> s {indexStatus = a} :: AwsDynamoDbTableGlobalSecondaryIndex)

-- | The number of items in the index.
awsDynamoDbTableGlobalSecondaryIndex_itemCount :: Lens.Lens' AwsDynamoDbTableGlobalSecondaryIndex (Prelude.Maybe Prelude.Int)
awsDynamoDbTableGlobalSecondaryIndex_itemCount = Lens.lens (\AwsDynamoDbTableGlobalSecondaryIndex' {itemCount} -> itemCount) (\s@AwsDynamoDbTableGlobalSecondaryIndex' {} a -> s {itemCount = a} :: AwsDynamoDbTableGlobalSecondaryIndex)

-- | The key schema for the index.
awsDynamoDbTableGlobalSecondaryIndex_keySchema :: Lens.Lens' AwsDynamoDbTableGlobalSecondaryIndex (Prelude.Maybe [AwsDynamoDbTableKeySchema])
awsDynamoDbTableGlobalSecondaryIndex_keySchema = Lens.lens (\AwsDynamoDbTableGlobalSecondaryIndex' {keySchema} -> keySchema) (\s@AwsDynamoDbTableGlobalSecondaryIndex' {} a -> s {keySchema = a} :: AwsDynamoDbTableGlobalSecondaryIndex) Prelude.. Lens.mapping Lens.coerced

-- | Attributes that are copied from the table into an index.
awsDynamoDbTableGlobalSecondaryIndex_projection :: Lens.Lens' AwsDynamoDbTableGlobalSecondaryIndex (Prelude.Maybe AwsDynamoDbTableProjection)
awsDynamoDbTableGlobalSecondaryIndex_projection = Lens.lens (\AwsDynamoDbTableGlobalSecondaryIndex' {projection} -> projection) (\s@AwsDynamoDbTableGlobalSecondaryIndex' {} a -> s {projection = a} :: AwsDynamoDbTableGlobalSecondaryIndex)

-- | Information about the provisioned throughput settings for the indexes.
awsDynamoDbTableGlobalSecondaryIndex_provisionedThroughput :: Lens.Lens' AwsDynamoDbTableGlobalSecondaryIndex (Prelude.Maybe AwsDynamoDbTableProvisionedThroughput)
awsDynamoDbTableGlobalSecondaryIndex_provisionedThroughput = Lens.lens (\AwsDynamoDbTableGlobalSecondaryIndex' {provisionedThroughput} -> provisionedThroughput) (\s@AwsDynamoDbTableGlobalSecondaryIndex' {} a -> s {provisionedThroughput = a} :: AwsDynamoDbTableGlobalSecondaryIndex)

instance
  Data.FromJSON
    AwsDynamoDbTableGlobalSecondaryIndex
  where
  parseJSON =
    Data.withObject
      "AwsDynamoDbTableGlobalSecondaryIndex"
      ( \x ->
          AwsDynamoDbTableGlobalSecondaryIndex'
            Prelude.<$> (x Data..:? "Backfilling")
            Prelude.<*> (x Data..:? "IndexArn")
            Prelude.<*> (x Data..:? "IndexName")
            Prelude.<*> (x Data..:? "IndexSizeBytes")
            Prelude.<*> (x Data..:? "IndexStatus")
            Prelude.<*> (x Data..:? "ItemCount")
            Prelude.<*> (x Data..:? "KeySchema" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "Projection")
            Prelude.<*> (x Data..:? "ProvisionedThroughput")
      )

instance
  Prelude.Hashable
    AwsDynamoDbTableGlobalSecondaryIndex
  where
  hashWithSalt
    _salt
    AwsDynamoDbTableGlobalSecondaryIndex' {..} =
      _salt
        `Prelude.hashWithSalt` backfilling
        `Prelude.hashWithSalt` indexArn
        `Prelude.hashWithSalt` indexName
        `Prelude.hashWithSalt` indexSizeBytes
        `Prelude.hashWithSalt` indexStatus
        `Prelude.hashWithSalt` itemCount
        `Prelude.hashWithSalt` keySchema
        `Prelude.hashWithSalt` projection
        `Prelude.hashWithSalt` provisionedThroughput

instance
  Prelude.NFData
    AwsDynamoDbTableGlobalSecondaryIndex
  where
  rnf AwsDynamoDbTableGlobalSecondaryIndex' {..} =
    Prelude.rnf backfilling
      `Prelude.seq` Prelude.rnf indexArn
      `Prelude.seq` Prelude.rnf indexName
      `Prelude.seq` Prelude.rnf indexSizeBytes
      `Prelude.seq` Prelude.rnf indexStatus
      `Prelude.seq` Prelude.rnf itemCount
      `Prelude.seq` Prelude.rnf keySchema
      `Prelude.seq` Prelude.rnf projection
      `Prelude.seq` Prelude.rnf provisionedThroughput

instance
  Data.ToJSON
    AwsDynamoDbTableGlobalSecondaryIndex
  where
  toJSON AwsDynamoDbTableGlobalSecondaryIndex' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Backfilling" Data..=) Prelude.<$> backfilling,
            ("IndexArn" Data..=) Prelude.<$> indexArn,
            ("IndexName" Data..=) Prelude.<$> indexName,
            ("IndexSizeBytes" Data..=)
              Prelude.<$> indexSizeBytes,
            ("IndexStatus" Data..=) Prelude.<$> indexStatus,
            ("ItemCount" Data..=) Prelude.<$> itemCount,
            ("KeySchema" Data..=) Prelude.<$> keySchema,
            ("Projection" Data..=) Prelude.<$> projection,
            ("ProvisionedThroughput" Data..=)
              Prelude.<$> provisionedThroughput
          ]
      )
