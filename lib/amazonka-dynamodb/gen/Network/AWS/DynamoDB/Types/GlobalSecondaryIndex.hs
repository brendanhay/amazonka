{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.GlobalSecondaryIndex
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.GlobalSecondaryIndex
  ( GlobalSecondaryIndex (..),

    -- * Smart constructor
    mkGlobalSecondaryIndex,

    -- * Lenses
    gsiIndexName,
    gsiKeySchema,
    gsiProjection,
    gsiProvisionedThroughput,
  )
where

import qualified Network.AWS.DynamoDB.Types.IndexName as Types
import qualified Network.AWS.DynamoDB.Types.KeySchemaElement as Types
import qualified Network.AWS.DynamoDB.Types.Projection as Types
import qualified Network.AWS.DynamoDB.Types.ProvisionedThroughput as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents the properties of a global secondary index.
--
-- /See:/ 'mkGlobalSecondaryIndex' smart constructor.
data GlobalSecondaryIndex = GlobalSecondaryIndex'
  { -- | The name of the global secondary index. The name must be unique among all other indexes on this table.
    indexName :: Types.IndexName,
    -- | The complete key schema for a global secondary index, which consists of one or more pairs of attribute names and key types:
    --
    --
    --     * @HASH@ - partition key
    --
    --
    --     * @RANGE@ - sort key
    keySchema :: Core.NonEmpty Types.KeySchemaElement,
    -- | Represents attributes that are copied (projected) from the table into the global secondary index. These are in addition to the primary key attributes and index key attributes, which are automatically projected.
    projection :: Types.Projection,
    -- | Represents the provisioned throughput settings for the specified global secondary index.
    --
    -- For current minimum and maximum provisioned throughput values, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Limits.html Service, Account, and Table Quotas> in the /Amazon DynamoDB Developer Guide/ .
    provisionedThroughput :: Core.Maybe Types.ProvisionedThroughput
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GlobalSecondaryIndex' value with any optional fields omitted.
mkGlobalSecondaryIndex ::
  -- | 'indexName'
  Types.IndexName ->
  -- | 'keySchema'
  Core.NonEmpty Types.KeySchemaElement ->
  -- | 'projection'
  Types.Projection ->
  GlobalSecondaryIndex
mkGlobalSecondaryIndex indexName keySchema projection =
  GlobalSecondaryIndex'
    { indexName,
      keySchema,
      projection,
      provisionedThroughput = Core.Nothing
    }

-- | The name of the global secondary index. The name must be unique among all other indexes on this table.
--
-- /Note:/ Consider using 'indexName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsiIndexName :: Lens.Lens' GlobalSecondaryIndex Types.IndexName
gsiIndexName = Lens.field @"indexName"
{-# DEPRECATED gsiIndexName "Use generic-lens or generic-optics with 'indexName' instead." #-}

-- | The complete key schema for a global secondary index, which consists of one or more pairs of attribute names and key types:
--
--
--     * @HASH@ - partition key
--
--
--     * @RANGE@ - sort key
--
--
--
-- /Note:/ Consider using 'keySchema' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsiKeySchema :: Lens.Lens' GlobalSecondaryIndex (Core.NonEmpty Types.KeySchemaElement)
gsiKeySchema = Lens.field @"keySchema"
{-# DEPRECATED gsiKeySchema "Use generic-lens or generic-optics with 'keySchema' instead." #-}

-- | Represents attributes that are copied (projected) from the table into the global secondary index. These are in addition to the primary key attributes and index key attributes, which are automatically projected.
--
-- /Note:/ Consider using 'projection' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsiProjection :: Lens.Lens' GlobalSecondaryIndex Types.Projection
gsiProjection = Lens.field @"projection"
{-# DEPRECATED gsiProjection "Use generic-lens or generic-optics with 'projection' instead." #-}

-- | Represents the provisioned throughput settings for the specified global secondary index.
--
-- For current minimum and maximum provisioned throughput values, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Limits.html Service, Account, and Table Quotas> in the /Amazon DynamoDB Developer Guide/ .
--
-- /Note:/ Consider using 'provisionedThroughput' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsiProvisionedThroughput :: Lens.Lens' GlobalSecondaryIndex (Core.Maybe Types.ProvisionedThroughput)
gsiProvisionedThroughput = Lens.field @"provisionedThroughput"
{-# DEPRECATED gsiProvisionedThroughput "Use generic-lens or generic-optics with 'provisionedThroughput' instead." #-}

instance Core.FromJSON GlobalSecondaryIndex where
  toJSON GlobalSecondaryIndex {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("IndexName" Core..= indexName),
            Core.Just ("KeySchema" Core..= keySchema),
            Core.Just ("Projection" Core..= projection),
            ("ProvisionedThroughput" Core..=) Core.<$> provisionedThroughput
          ]
      )
