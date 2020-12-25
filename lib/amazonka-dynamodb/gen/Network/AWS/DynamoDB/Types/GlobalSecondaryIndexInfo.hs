{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.GlobalSecondaryIndexInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.GlobalSecondaryIndexInfo
  ( GlobalSecondaryIndexInfo (..),

    -- * Smart constructor
    mkGlobalSecondaryIndexInfo,

    -- * Lenses
    gsiiIndexName,
    gsiiKeySchema,
    gsiiProjection,
    gsiiProvisionedThroughput,
  )
where

import qualified Network.AWS.DynamoDB.Types.IndexName as Types
import qualified Network.AWS.DynamoDB.Types.KeySchemaElement as Types
import qualified Network.AWS.DynamoDB.Types.Projection as Types
import qualified Network.AWS.DynamoDB.Types.ProvisionedThroughput as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents the properties of a global secondary index for the table when the backup was created.
--
-- /See:/ 'mkGlobalSecondaryIndexInfo' smart constructor.
data GlobalSecondaryIndexInfo = GlobalSecondaryIndexInfo'
  { -- | The name of the global secondary index.
    indexName :: Core.Maybe Types.IndexName,
    -- | The complete key schema for a global secondary index, which consists of one or more pairs of attribute names and key types:
    --
    --
    --     * @HASH@ - partition key
    --
    --
    --     * @RANGE@ - sort key
    keySchema :: Core.Maybe (Core.NonEmpty Types.KeySchemaElement),
    -- | Represents attributes that are copied (projected) from the table into the global secondary index. These are in addition to the primary key attributes and index key attributes, which are automatically projected.
    projection :: Core.Maybe Types.Projection,
    -- | Represents the provisioned throughput settings for the specified global secondary index.
    provisionedThroughput :: Core.Maybe Types.ProvisionedThroughput
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GlobalSecondaryIndexInfo' value with any optional fields omitted.
mkGlobalSecondaryIndexInfo ::
  GlobalSecondaryIndexInfo
mkGlobalSecondaryIndexInfo =
  GlobalSecondaryIndexInfo'
    { indexName = Core.Nothing,
      keySchema = Core.Nothing,
      projection = Core.Nothing,
      provisionedThroughput = Core.Nothing
    }

-- | The name of the global secondary index.
--
-- /Note:/ Consider using 'indexName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsiiIndexName :: Lens.Lens' GlobalSecondaryIndexInfo (Core.Maybe Types.IndexName)
gsiiIndexName = Lens.field @"indexName"
{-# DEPRECATED gsiiIndexName "Use generic-lens or generic-optics with 'indexName' instead." #-}

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
gsiiKeySchema :: Lens.Lens' GlobalSecondaryIndexInfo (Core.Maybe (Core.NonEmpty Types.KeySchemaElement))
gsiiKeySchema = Lens.field @"keySchema"
{-# DEPRECATED gsiiKeySchema "Use generic-lens or generic-optics with 'keySchema' instead." #-}

-- | Represents attributes that are copied (projected) from the table into the global secondary index. These are in addition to the primary key attributes and index key attributes, which are automatically projected.
--
-- /Note:/ Consider using 'projection' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsiiProjection :: Lens.Lens' GlobalSecondaryIndexInfo (Core.Maybe Types.Projection)
gsiiProjection = Lens.field @"projection"
{-# DEPRECATED gsiiProjection "Use generic-lens or generic-optics with 'projection' instead." #-}

-- | Represents the provisioned throughput settings for the specified global secondary index.
--
-- /Note:/ Consider using 'provisionedThroughput' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsiiProvisionedThroughput :: Lens.Lens' GlobalSecondaryIndexInfo (Core.Maybe Types.ProvisionedThroughput)
gsiiProvisionedThroughput = Lens.field @"provisionedThroughput"
{-# DEPRECATED gsiiProvisionedThroughput "Use generic-lens or generic-optics with 'provisionedThroughput' instead." #-}

instance Core.FromJSON GlobalSecondaryIndexInfo where
  parseJSON =
    Core.withObject "GlobalSecondaryIndexInfo" Core.$
      \x ->
        GlobalSecondaryIndexInfo'
          Core.<$> (x Core..:? "IndexName")
          Core.<*> (x Core..:? "KeySchema")
          Core.<*> (x Core..:? "Projection")
          Core.<*> (x Core..:? "ProvisionedThroughput")
