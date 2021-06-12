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
-- Module      : Network.AWS.DynamoDB.Types.CreateGlobalSecondaryIndexAction
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.CreateGlobalSecondaryIndexAction where

import qualified Network.AWS.Core as Core
import Network.AWS.DynamoDB.Types.KeySchemaElement
import Network.AWS.DynamoDB.Types.Projection
import Network.AWS.DynamoDB.Types.ProvisionedThroughput
import qualified Network.AWS.Lens as Lens

-- | Represents a new global secondary index to be added to an existing
-- table.
--
-- /See:/ 'newCreateGlobalSecondaryIndexAction' smart constructor.
data CreateGlobalSecondaryIndexAction = CreateGlobalSecondaryIndexAction'
  { -- | Represents the provisioned throughput settings for the specified global
    -- secondary index.
    --
    -- For current minimum and maximum provisioned throughput values, see
    -- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Limits.html Service, Account, and Table Quotas>
    -- in the /Amazon DynamoDB Developer Guide/.
    provisionedThroughput :: Core.Maybe ProvisionedThroughput,
    -- | The name of the global secondary index to be created.
    indexName :: Core.Text,
    -- | The key schema for the global secondary index.
    keySchema :: Core.NonEmpty KeySchemaElement,
    -- | Represents attributes that are copied (projected) from the table into an
    -- index. These are in addition to the primary key attributes and index key
    -- attributes, which are automatically projected.
    projection :: Projection
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateGlobalSecondaryIndexAction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'provisionedThroughput', 'createGlobalSecondaryIndexAction_provisionedThroughput' - Represents the provisioned throughput settings for the specified global
-- secondary index.
--
-- For current minimum and maximum provisioned throughput values, see
-- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Limits.html Service, Account, and Table Quotas>
-- in the /Amazon DynamoDB Developer Guide/.
--
-- 'indexName', 'createGlobalSecondaryIndexAction_indexName' - The name of the global secondary index to be created.
--
-- 'keySchema', 'createGlobalSecondaryIndexAction_keySchema' - The key schema for the global secondary index.
--
-- 'projection', 'createGlobalSecondaryIndexAction_projection' - Represents attributes that are copied (projected) from the table into an
-- index. These are in addition to the primary key attributes and index key
-- attributes, which are automatically projected.
newCreateGlobalSecondaryIndexAction ::
  -- | 'indexName'
  Core.Text ->
  -- | 'keySchema'
  Core.NonEmpty KeySchemaElement ->
  -- | 'projection'
  Projection ->
  CreateGlobalSecondaryIndexAction
newCreateGlobalSecondaryIndexAction
  pIndexName_
  pKeySchema_
  pProjection_ =
    CreateGlobalSecondaryIndexAction'
      { provisionedThroughput =
          Core.Nothing,
        indexName = pIndexName_,
        keySchema =
          Lens._Coerce Lens.# pKeySchema_,
        projection = pProjection_
      }

-- | Represents the provisioned throughput settings for the specified global
-- secondary index.
--
-- For current minimum and maximum provisioned throughput values, see
-- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Limits.html Service, Account, and Table Quotas>
-- in the /Amazon DynamoDB Developer Guide/.
createGlobalSecondaryIndexAction_provisionedThroughput :: Lens.Lens' CreateGlobalSecondaryIndexAction (Core.Maybe ProvisionedThroughput)
createGlobalSecondaryIndexAction_provisionedThroughput = Lens.lens (\CreateGlobalSecondaryIndexAction' {provisionedThroughput} -> provisionedThroughput) (\s@CreateGlobalSecondaryIndexAction' {} a -> s {provisionedThroughput = a} :: CreateGlobalSecondaryIndexAction)

-- | The name of the global secondary index to be created.
createGlobalSecondaryIndexAction_indexName :: Lens.Lens' CreateGlobalSecondaryIndexAction Core.Text
createGlobalSecondaryIndexAction_indexName = Lens.lens (\CreateGlobalSecondaryIndexAction' {indexName} -> indexName) (\s@CreateGlobalSecondaryIndexAction' {} a -> s {indexName = a} :: CreateGlobalSecondaryIndexAction)

-- | The key schema for the global secondary index.
createGlobalSecondaryIndexAction_keySchema :: Lens.Lens' CreateGlobalSecondaryIndexAction (Core.NonEmpty KeySchemaElement)
createGlobalSecondaryIndexAction_keySchema = Lens.lens (\CreateGlobalSecondaryIndexAction' {keySchema} -> keySchema) (\s@CreateGlobalSecondaryIndexAction' {} a -> s {keySchema = a} :: CreateGlobalSecondaryIndexAction) Core.. Lens._Coerce

-- | Represents attributes that are copied (projected) from the table into an
-- index. These are in addition to the primary key attributes and index key
-- attributes, which are automatically projected.
createGlobalSecondaryIndexAction_projection :: Lens.Lens' CreateGlobalSecondaryIndexAction Projection
createGlobalSecondaryIndexAction_projection = Lens.lens (\CreateGlobalSecondaryIndexAction' {projection} -> projection) (\s@CreateGlobalSecondaryIndexAction' {} a -> s {projection = a} :: CreateGlobalSecondaryIndexAction)

instance
  Core.Hashable
    CreateGlobalSecondaryIndexAction

instance Core.NFData CreateGlobalSecondaryIndexAction

instance Core.ToJSON CreateGlobalSecondaryIndexAction where
  toJSON CreateGlobalSecondaryIndexAction' {..} =
    Core.object
      ( Core.catMaybes
          [ ("ProvisionedThroughput" Core..=)
              Core.<$> provisionedThroughput,
            Core.Just ("IndexName" Core..= indexName),
            Core.Just ("KeySchema" Core..= keySchema),
            Core.Just ("Projection" Core..= projection)
          ]
      )
