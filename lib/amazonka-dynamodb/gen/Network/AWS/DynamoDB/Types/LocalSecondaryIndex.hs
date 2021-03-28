{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.LocalSecondaryIndex
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DynamoDB.Types.LocalSecondaryIndex
  ( LocalSecondaryIndex (..)
  -- * Smart constructor
  , mkLocalSecondaryIndex
  -- * Lenses
  , lsiIndexName
  , lsiKeySchema
  , lsiProjection
  ) where

import qualified Network.AWS.DynamoDB.Types.IndexName as Types
import qualified Network.AWS.DynamoDB.Types.KeySchemaElement as Types
import qualified Network.AWS.DynamoDB.Types.Projection as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents the properties of a local secondary index.
--
-- /See:/ 'mkLocalSecondaryIndex' smart constructor.
data LocalSecondaryIndex = LocalSecondaryIndex'
  { indexName :: Types.IndexName
    -- ^ The name of the local secondary index. The name must be unique among all other indexes on this table.
  , keySchema :: Core.NonEmpty Types.KeySchemaElement
    -- ^ The complete key schema for the local secondary index, consisting of one or more pairs of attribute names and key types:
--
--
--     * @HASH@ - partition key
--
--
--     * @RANGE@ - sort key
--
--
  , projection :: Types.Projection
    -- ^ Represents attributes that are copied (projected) from the table into the local secondary index. These are in addition to the primary key attributes and index key attributes, which are automatically projected. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'LocalSecondaryIndex' value with any optional fields omitted.
mkLocalSecondaryIndex
    :: Types.IndexName -- ^ 'indexName'
    -> Core.NonEmpty Types.KeySchemaElement -- ^ 'keySchema'
    -> Types.Projection -- ^ 'projection'
    -> LocalSecondaryIndex
mkLocalSecondaryIndex indexName keySchema projection
  = LocalSecondaryIndex'{indexName, keySchema, projection}

-- | The name of the local secondary index. The name must be unique among all other indexes on this table.
--
-- /Note:/ Consider using 'indexName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsiIndexName :: Lens.Lens' LocalSecondaryIndex Types.IndexName
lsiIndexName = Lens.field @"indexName"
{-# INLINEABLE lsiIndexName #-}
{-# DEPRECATED indexName "Use generic-lens or generic-optics with 'indexName' instead"  #-}

-- | The complete key schema for the local secondary index, consisting of one or more pairs of attribute names and key types:
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
lsiKeySchema :: Lens.Lens' LocalSecondaryIndex (Core.NonEmpty Types.KeySchemaElement)
lsiKeySchema = Lens.field @"keySchema"
{-# INLINEABLE lsiKeySchema #-}
{-# DEPRECATED keySchema "Use generic-lens or generic-optics with 'keySchema' instead"  #-}

-- | Represents attributes that are copied (projected) from the table into the local secondary index. These are in addition to the primary key attributes and index key attributes, which are automatically projected. 
--
-- /Note:/ Consider using 'projection' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsiProjection :: Lens.Lens' LocalSecondaryIndex Types.Projection
lsiProjection = Lens.field @"projection"
{-# INLINEABLE lsiProjection #-}
{-# DEPRECATED projection "Use generic-lens or generic-optics with 'projection' instead"  #-}

instance Core.FromJSON LocalSecondaryIndex where
        toJSON LocalSecondaryIndex{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("IndexName" Core..= indexName),
                  Core.Just ("KeySchema" Core..= keySchema),
                  Core.Just ("Projection" Core..= projection)])
