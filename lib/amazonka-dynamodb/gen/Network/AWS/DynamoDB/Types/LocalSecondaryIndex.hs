{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.LocalSecondaryIndex
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.LocalSecondaryIndex
  ( LocalSecondaryIndex (..),

    -- * Smart constructor
    mkLocalSecondaryIndex,

    -- * Lenses
    lsiKeySchema,
    lsiProjection,
    lsiIndexName,
  )
where

import Network.AWS.DynamoDB.Types.KeySchemaElement
import Network.AWS.DynamoDB.Types.Projection
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents the properties of a local secondary index.
--
-- /See:/ 'mkLocalSecondaryIndex' smart constructor.
data LocalSecondaryIndex = LocalSecondaryIndex'
  { -- | The complete key schema for the local secondary index, consisting of one or more pairs of attribute names and key types:
    --
    --
    --     * @HASH@ - partition key
    --
    --
    --     * @RANGE@ - sort key
    keySchema :: Lude.NonEmpty KeySchemaElement,
    -- | Represents attributes that are copied (projected) from the table into the local secondary index. These are in addition to the primary key attributes and index key attributes, which are automatically projected.
    projection :: Projection,
    -- | The name of the local secondary index. The name must be unique among all other indexes on this table.
    indexName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'LocalSecondaryIndex' with the minimum fields required to make a request.
--
-- * 'keySchema' - The complete key schema for the local secondary index, consisting of one or more pairs of attribute names and key types:
--
--
--     * @HASH@ - partition key
--
--
--     * @RANGE@ - sort key
--
--
-- * 'projection' - Represents attributes that are copied (projected) from the table into the local secondary index. These are in addition to the primary key attributes and index key attributes, which are automatically projected.
-- * 'indexName' - The name of the local secondary index. The name must be unique among all other indexes on this table.
mkLocalSecondaryIndex ::
  -- | 'keySchema'
  Lude.NonEmpty KeySchemaElement ->
  -- | 'projection'
  Projection ->
  -- | 'indexName'
  Lude.Text ->
  LocalSecondaryIndex
mkLocalSecondaryIndex pKeySchema_ pProjection_ pIndexName_ =
  LocalSecondaryIndex'
    { keySchema = pKeySchema_,
      projection = pProjection_,
      indexName = pIndexName_
    }

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
lsiKeySchema :: Lens.Lens' LocalSecondaryIndex (Lude.NonEmpty KeySchemaElement)
lsiKeySchema = Lens.lens (keySchema :: LocalSecondaryIndex -> Lude.NonEmpty KeySchemaElement) (\s a -> s {keySchema = a} :: LocalSecondaryIndex)
{-# DEPRECATED lsiKeySchema "Use generic-lens or generic-optics with 'keySchema' instead." #-}

-- | Represents attributes that are copied (projected) from the table into the local secondary index. These are in addition to the primary key attributes and index key attributes, which are automatically projected.
--
-- /Note:/ Consider using 'projection' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsiProjection :: Lens.Lens' LocalSecondaryIndex Projection
lsiProjection = Lens.lens (projection :: LocalSecondaryIndex -> Projection) (\s a -> s {projection = a} :: LocalSecondaryIndex)
{-# DEPRECATED lsiProjection "Use generic-lens or generic-optics with 'projection' instead." #-}

-- | The name of the local secondary index. The name must be unique among all other indexes on this table.
--
-- /Note:/ Consider using 'indexName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsiIndexName :: Lens.Lens' LocalSecondaryIndex Lude.Text
lsiIndexName = Lens.lens (indexName :: LocalSecondaryIndex -> Lude.Text) (\s a -> s {indexName = a} :: LocalSecondaryIndex)
{-# DEPRECATED lsiIndexName "Use generic-lens or generic-optics with 'indexName' instead." #-}

instance Lude.ToJSON LocalSecondaryIndex where
  toJSON LocalSecondaryIndex' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("KeySchema" Lude..= keySchema),
            Lude.Just ("Projection" Lude..= projection),
            Lude.Just ("IndexName" Lude..= indexName)
          ]
      )
