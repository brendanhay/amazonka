{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.LocalSecondaryIndexInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DynamoDB.Types.LocalSecondaryIndexInfo
  ( LocalSecondaryIndexInfo (..)
  -- * Smart constructor
  , mkLocalSecondaryIndexInfo
  -- * Lenses
  , lsiiIndexName
  , lsiiKeySchema
  , lsiiProjection
  ) where

import qualified Network.AWS.DynamoDB.Types.IndexName as Types
import qualified Network.AWS.DynamoDB.Types.KeySchemaElement as Types
import qualified Network.AWS.DynamoDB.Types.Projection as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents the properties of a local secondary index for the table when the backup was created.
--
-- /See:/ 'mkLocalSecondaryIndexInfo' smart constructor.
data LocalSecondaryIndexInfo = LocalSecondaryIndexInfo'
  { indexName :: Core.Maybe Types.IndexName
    -- ^ Represents the name of the local secondary index.
  , keySchema :: Core.Maybe (Core.NonEmpty Types.KeySchemaElement)
    -- ^ The complete key schema for a local secondary index, which consists of one or more pairs of attribute names and key types:
--
--
--     * @HASH@ - partition key
--
--
--     * @RANGE@ - sort key
--
--
  , projection :: Core.Maybe Types.Projection
    -- ^ Represents attributes that are copied (projected) from the table into the global secondary index. These are in addition to the primary key attributes and index key attributes, which are automatically projected. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'LocalSecondaryIndexInfo' value with any optional fields omitted.
mkLocalSecondaryIndexInfo
    :: LocalSecondaryIndexInfo
mkLocalSecondaryIndexInfo
  = LocalSecondaryIndexInfo'{indexName = Core.Nothing,
                             keySchema = Core.Nothing, projection = Core.Nothing}

-- | Represents the name of the local secondary index.
--
-- /Note:/ Consider using 'indexName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsiiIndexName :: Lens.Lens' LocalSecondaryIndexInfo (Core.Maybe Types.IndexName)
lsiiIndexName = Lens.field @"indexName"
{-# INLINEABLE lsiiIndexName #-}
{-# DEPRECATED indexName "Use generic-lens or generic-optics with 'indexName' instead"  #-}

-- | The complete key schema for a local secondary index, which consists of one or more pairs of attribute names and key types:
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
lsiiKeySchema :: Lens.Lens' LocalSecondaryIndexInfo (Core.Maybe (Core.NonEmpty Types.KeySchemaElement))
lsiiKeySchema = Lens.field @"keySchema"
{-# INLINEABLE lsiiKeySchema #-}
{-# DEPRECATED keySchema "Use generic-lens or generic-optics with 'keySchema' instead"  #-}

-- | Represents attributes that are copied (projected) from the table into the global secondary index. These are in addition to the primary key attributes and index key attributes, which are automatically projected. 
--
-- /Note:/ Consider using 'projection' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsiiProjection :: Lens.Lens' LocalSecondaryIndexInfo (Core.Maybe Types.Projection)
lsiiProjection = Lens.field @"projection"
{-# INLINEABLE lsiiProjection #-}
{-# DEPRECATED projection "Use generic-lens or generic-optics with 'projection' instead"  #-}

instance Core.FromJSON LocalSecondaryIndexInfo where
        parseJSON
          = Core.withObject "LocalSecondaryIndexInfo" Core.$
              \ x ->
                LocalSecondaryIndexInfo' Core.<$>
                  (x Core..:? "IndexName") Core.<*> x Core..:? "KeySchema" Core.<*>
                    x Core..:? "Projection"
