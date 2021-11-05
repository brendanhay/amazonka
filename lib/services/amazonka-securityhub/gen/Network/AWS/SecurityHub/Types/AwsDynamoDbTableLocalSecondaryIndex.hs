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
-- Module      : Network.AWS.SecurityHub.Types.AwsDynamoDbTableLocalSecondaryIndex
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SecurityHub.Types.AwsDynamoDbTableLocalSecondaryIndex where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SecurityHub.Types.AwsDynamoDbTableKeySchema
import Network.AWS.SecurityHub.Types.AwsDynamoDbTableProjection

-- | Information about a local secondary index for a DynamoDB table.
--
-- /See:/ 'newAwsDynamoDbTableLocalSecondaryIndex' smart constructor.
data AwsDynamoDbTableLocalSecondaryIndex = AwsDynamoDbTableLocalSecondaryIndex'
  { -- | The ARN of the index.
    indexArn :: Prelude.Maybe Prelude.Text,
    -- | The complete key schema for the index.
    keySchema :: Prelude.Maybe [AwsDynamoDbTableKeySchema],
    -- | Attributes that are copied from the table into the index. These are in
    -- addition to the primary key attributes and index key attributes, which
    -- are automatically projected.
    projection :: Prelude.Maybe AwsDynamoDbTableProjection,
    -- | The name of the index.
    indexName :: Prelude.Maybe Prelude.Text
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
-- 'keySchema', 'awsDynamoDbTableLocalSecondaryIndex_keySchema' - The complete key schema for the index.
--
-- 'projection', 'awsDynamoDbTableLocalSecondaryIndex_projection' - Attributes that are copied from the table into the index. These are in
-- addition to the primary key attributes and index key attributes, which
-- are automatically projected.
--
-- 'indexName', 'awsDynamoDbTableLocalSecondaryIndex_indexName' - The name of the index.
newAwsDynamoDbTableLocalSecondaryIndex ::
  AwsDynamoDbTableLocalSecondaryIndex
newAwsDynamoDbTableLocalSecondaryIndex =
  AwsDynamoDbTableLocalSecondaryIndex'
    { indexArn =
        Prelude.Nothing,
      keySchema = Prelude.Nothing,
      projection = Prelude.Nothing,
      indexName = Prelude.Nothing
    }

-- | The ARN of the index.
awsDynamoDbTableLocalSecondaryIndex_indexArn :: Lens.Lens' AwsDynamoDbTableLocalSecondaryIndex (Prelude.Maybe Prelude.Text)
awsDynamoDbTableLocalSecondaryIndex_indexArn = Lens.lens (\AwsDynamoDbTableLocalSecondaryIndex' {indexArn} -> indexArn) (\s@AwsDynamoDbTableLocalSecondaryIndex' {} a -> s {indexArn = a} :: AwsDynamoDbTableLocalSecondaryIndex)

-- | The complete key schema for the index.
awsDynamoDbTableLocalSecondaryIndex_keySchema :: Lens.Lens' AwsDynamoDbTableLocalSecondaryIndex (Prelude.Maybe [AwsDynamoDbTableKeySchema])
awsDynamoDbTableLocalSecondaryIndex_keySchema = Lens.lens (\AwsDynamoDbTableLocalSecondaryIndex' {keySchema} -> keySchema) (\s@AwsDynamoDbTableLocalSecondaryIndex' {} a -> s {keySchema = a} :: AwsDynamoDbTableLocalSecondaryIndex) Prelude.. Lens.mapping Lens.coerced

-- | Attributes that are copied from the table into the index. These are in
-- addition to the primary key attributes and index key attributes, which
-- are automatically projected.
awsDynamoDbTableLocalSecondaryIndex_projection :: Lens.Lens' AwsDynamoDbTableLocalSecondaryIndex (Prelude.Maybe AwsDynamoDbTableProjection)
awsDynamoDbTableLocalSecondaryIndex_projection = Lens.lens (\AwsDynamoDbTableLocalSecondaryIndex' {projection} -> projection) (\s@AwsDynamoDbTableLocalSecondaryIndex' {} a -> s {projection = a} :: AwsDynamoDbTableLocalSecondaryIndex)

-- | The name of the index.
awsDynamoDbTableLocalSecondaryIndex_indexName :: Lens.Lens' AwsDynamoDbTableLocalSecondaryIndex (Prelude.Maybe Prelude.Text)
awsDynamoDbTableLocalSecondaryIndex_indexName = Lens.lens (\AwsDynamoDbTableLocalSecondaryIndex' {indexName} -> indexName) (\s@AwsDynamoDbTableLocalSecondaryIndex' {} a -> s {indexName = a} :: AwsDynamoDbTableLocalSecondaryIndex)

instance
  Core.FromJSON
    AwsDynamoDbTableLocalSecondaryIndex
  where
  parseJSON =
    Core.withObject
      "AwsDynamoDbTableLocalSecondaryIndex"
      ( \x ->
          AwsDynamoDbTableLocalSecondaryIndex'
            Prelude.<$> (x Core..:? "IndexArn")
            Prelude.<*> (x Core..:? "KeySchema" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "Projection")
            Prelude.<*> (x Core..:? "IndexName")
      )

instance
  Prelude.Hashable
    AwsDynamoDbTableLocalSecondaryIndex

instance
  Prelude.NFData
    AwsDynamoDbTableLocalSecondaryIndex

instance
  Core.ToJSON
    AwsDynamoDbTableLocalSecondaryIndex
  where
  toJSON AwsDynamoDbTableLocalSecondaryIndex' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("IndexArn" Core..=) Prelude.<$> indexArn,
            ("KeySchema" Core..=) Prelude.<$> keySchema,
            ("Projection" Core..=) Prelude.<$> projection,
            ("IndexName" Core..=) Prelude.<$> indexName
          ]
      )
