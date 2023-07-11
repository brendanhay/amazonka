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
-- Module      : Amazonka.SecurityHub.Types.AwsDynamoDbTableLocalSecondaryIndex
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsDynamoDbTableLocalSecondaryIndex where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.AwsDynamoDbTableKeySchema
import Amazonka.SecurityHub.Types.AwsDynamoDbTableProjection

-- | Information about a local secondary index for a DynamoDB table.
--
-- /See:/ 'newAwsDynamoDbTableLocalSecondaryIndex' smart constructor.
data AwsDynamoDbTableLocalSecondaryIndex = AwsDynamoDbTableLocalSecondaryIndex'
  { -- | The ARN of the index.
    indexArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the index.
    indexName :: Prelude.Maybe Prelude.Text,
    -- | The complete key schema for the index.
    keySchema :: Prelude.Maybe [AwsDynamoDbTableKeySchema],
    -- | Attributes that are copied from the table into the index. These are in
    -- addition to the primary key attributes and index key attributes, which
    -- are automatically projected.
    projection :: Prelude.Maybe AwsDynamoDbTableProjection
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsDynamoDbTableLocalSecondaryIndex' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'indexArn', 'awsDynamoDbTableLocalSecondaryIndex_indexArn' - The ARN of the index.
--
-- 'indexName', 'awsDynamoDbTableLocalSecondaryIndex_indexName' - The name of the index.
--
-- 'keySchema', 'awsDynamoDbTableLocalSecondaryIndex_keySchema' - The complete key schema for the index.
--
-- 'projection', 'awsDynamoDbTableLocalSecondaryIndex_projection' - Attributes that are copied from the table into the index. These are in
-- addition to the primary key attributes and index key attributes, which
-- are automatically projected.
newAwsDynamoDbTableLocalSecondaryIndex ::
  AwsDynamoDbTableLocalSecondaryIndex
newAwsDynamoDbTableLocalSecondaryIndex =
  AwsDynamoDbTableLocalSecondaryIndex'
    { indexArn =
        Prelude.Nothing,
      indexName = Prelude.Nothing,
      keySchema = Prelude.Nothing,
      projection = Prelude.Nothing
    }

-- | The ARN of the index.
awsDynamoDbTableLocalSecondaryIndex_indexArn :: Lens.Lens' AwsDynamoDbTableLocalSecondaryIndex (Prelude.Maybe Prelude.Text)
awsDynamoDbTableLocalSecondaryIndex_indexArn = Lens.lens (\AwsDynamoDbTableLocalSecondaryIndex' {indexArn} -> indexArn) (\s@AwsDynamoDbTableLocalSecondaryIndex' {} a -> s {indexArn = a} :: AwsDynamoDbTableLocalSecondaryIndex)

-- | The name of the index.
awsDynamoDbTableLocalSecondaryIndex_indexName :: Lens.Lens' AwsDynamoDbTableLocalSecondaryIndex (Prelude.Maybe Prelude.Text)
awsDynamoDbTableLocalSecondaryIndex_indexName = Lens.lens (\AwsDynamoDbTableLocalSecondaryIndex' {indexName} -> indexName) (\s@AwsDynamoDbTableLocalSecondaryIndex' {} a -> s {indexName = a} :: AwsDynamoDbTableLocalSecondaryIndex)

-- | The complete key schema for the index.
awsDynamoDbTableLocalSecondaryIndex_keySchema :: Lens.Lens' AwsDynamoDbTableLocalSecondaryIndex (Prelude.Maybe [AwsDynamoDbTableKeySchema])
awsDynamoDbTableLocalSecondaryIndex_keySchema = Lens.lens (\AwsDynamoDbTableLocalSecondaryIndex' {keySchema} -> keySchema) (\s@AwsDynamoDbTableLocalSecondaryIndex' {} a -> s {keySchema = a} :: AwsDynamoDbTableLocalSecondaryIndex) Prelude.. Lens.mapping Lens.coerced

-- | Attributes that are copied from the table into the index. These are in
-- addition to the primary key attributes and index key attributes, which
-- are automatically projected.
awsDynamoDbTableLocalSecondaryIndex_projection :: Lens.Lens' AwsDynamoDbTableLocalSecondaryIndex (Prelude.Maybe AwsDynamoDbTableProjection)
awsDynamoDbTableLocalSecondaryIndex_projection = Lens.lens (\AwsDynamoDbTableLocalSecondaryIndex' {projection} -> projection) (\s@AwsDynamoDbTableLocalSecondaryIndex' {} a -> s {projection = a} :: AwsDynamoDbTableLocalSecondaryIndex)

instance
  Data.FromJSON
    AwsDynamoDbTableLocalSecondaryIndex
  where
  parseJSON =
    Data.withObject
      "AwsDynamoDbTableLocalSecondaryIndex"
      ( \x ->
          AwsDynamoDbTableLocalSecondaryIndex'
            Prelude.<$> (x Data..:? "IndexArn")
            Prelude.<*> (x Data..:? "IndexName")
            Prelude.<*> (x Data..:? "KeySchema" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "Projection")
      )

instance
  Prelude.Hashable
    AwsDynamoDbTableLocalSecondaryIndex
  where
  hashWithSalt
    _salt
    AwsDynamoDbTableLocalSecondaryIndex' {..} =
      _salt
        `Prelude.hashWithSalt` indexArn
        `Prelude.hashWithSalt` indexName
        `Prelude.hashWithSalt` keySchema
        `Prelude.hashWithSalt` projection

instance
  Prelude.NFData
    AwsDynamoDbTableLocalSecondaryIndex
  where
  rnf AwsDynamoDbTableLocalSecondaryIndex' {..} =
    Prelude.rnf indexArn
      `Prelude.seq` Prelude.rnf indexName
      `Prelude.seq` Prelude.rnf keySchema
      `Prelude.seq` Prelude.rnf projection

instance
  Data.ToJSON
    AwsDynamoDbTableLocalSecondaryIndex
  where
  toJSON AwsDynamoDbTableLocalSecondaryIndex' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("IndexArn" Data..=) Prelude.<$> indexArn,
            ("IndexName" Data..=) Prelude.<$> indexName,
            ("KeySchema" Data..=) Prelude.<$> keySchema,
            ("Projection" Data..=) Prelude.<$> projection
          ]
      )
