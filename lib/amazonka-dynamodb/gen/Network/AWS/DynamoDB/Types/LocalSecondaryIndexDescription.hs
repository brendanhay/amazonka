{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.LocalSecondaryIndexDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.LocalSecondaryIndexDescription
  ( LocalSecondaryIndexDescription (..),

    -- * Smart constructor
    mkLocalSecondaryIndexDescription,

    -- * Lenses
    lsidIndexSizeBytes,
    lsidIndexARN,
    lsidKeySchema,
    lsidProjection,
    lsidItemCount,
    lsidIndexName,
  )
where

import Network.AWS.DynamoDB.Types.KeySchemaElement
import Network.AWS.DynamoDB.Types.Projection
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents the properties of a local secondary index.
--
-- /See:/ 'mkLocalSecondaryIndexDescription' smart constructor.
data LocalSecondaryIndexDescription = LocalSecondaryIndexDescription'
  { indexSizeBytes ::
      Lude.Maybe Lude.Integer,
    indexARN ::
      Lude.Maybe Lude.Text,
    keySchema ::
      Lude.Maybe
        ( Lude.NonEmpty
            KeySchemaElement
        ),
    projection ::
      Lude.Maybe Projection,
    itemCount ::
      Lude.Maybe Lude.Integer,
    indexName ::
      Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'LocalSecondaryIndexDescription' with the minimum fields required to make a request.
--
-- * 'indexARN' - The Amazon Resource Name (ARN) that uniquely identifies the index.
-- * 'indexName' - Represents the name of the local secondary index.
-- * 'indexSizeBytes' - The total size of the specified index, in bytes. DynamoDB updates this value approximately every six hours. Recent changes might not be reflected in this value.
-- * 'itemCount' - The number of items in the specified index. DynamoDB updates this value approximately every six hours. Recent changes might not be reflected in this value.
-- * 'keySchema' - The complete key schema for the local secondary index, consisting of one or more pairs of attribute names and key types:
--
--
--     * @HASH@ - partition key
--
--
--     * @RANGE@ - sort key
--
--
-- * 'projection' - Represents attributes that are copied (projected) from the table into the global secondary index. These are in addition to the primary key attributes and index key attributes, which are automatically projected.
mkLocalSecondaryIndexDescription ::
  LocalSecondaryIndexDescription
mkLocalSecondaryIndexDescription =
  LocalSecondaryIndexDescription'
    { indexSizeBytes = Lude.Nothing,
      indexARN = Lude.Nothing,
      keySchema = Lude.Nothing,
      projection = Lude.Nothing,
      itemCount = Lude.Nothing,
      indexName = Lude.Nothing
    }

-- | The total size of the specified index, in bytes. DynamoDB updates this value approximately every six hours. Recent changes might not be reflected in this value.
--
-- /Note:/ Consider using 'indexSizeBytes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsidIndexSizeBytes :: Lens.Lens' LocalSecondaryIndexDescription (Lude.Maybe Lude.Integer)
lsidIndexSizeBytes = Lens.lens (indexSizeBytes :: LocalSecondaryIndexDescription -> Lude.Maybe Lude.Integer) (\s a -> s {indexSizeBytes = a} :: LocalSecondaryIndexDescription)
{-# DEPRECATED lsidIndexSizeBytes "Use generic-lens or generic-optics with 'indexSizeBytes' instead." #-}

-- | The Amazon Resource Name (ARN) that uniquely identifies the index.
--
-- /Note:/ Consider using 'indexARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsidIndexARN :: Lens.Lens' LocalSecondaryIndexDescription (Lude.Maybe Lude.Text)
lsidIndexARN = Lens.lens (indexARN :: LocalSecondaryIndexDescription -> Lude.Maybe Lude.Text) (\s a -> s {indexARN = a} :: LocalSecondaryIndexDescription)
{-# DEPRECATED lsidIndexARN "Use generic-lens or generic-optics with 'indexARN' instead." #-}

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
lsidKeySchema :: Lens.Lens' LocalSecondaryIndexDescription (Lude.Maybe (Lude.NonEmpty KeySchemaElement))
lsidKeySchema = Lens.lens (keySchema :: LocalSecondaryIndexDescription -> Lude.Maybe (Lude.NonEmpty KeySchemaElement)) (\s a -> s {keySchema = a} :: LocalSecondaryIndexDescription)
{-# DEPRECATED lsidKeySchema "Use generic-lens or generic-optics with 'keySchema' instead." #-}

-- | Represents attributes that are copied (projected) from the table into the global secondary index. These are in addition to the primary key attributes and index key attributes, which are automatically projected.
--
-- /Note:/ Consider using 'projection' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsidProjection :: Lens.Lens' LocalSecondaryIndexDescription (Lude.Maybe Projection)
lsidProjection = Lens.lens (projection :: LocalSecondaryIndexDescription -> Lude.Maybe Projection) (\s a -> s {projection = a} :: LocalSecondaryIndexDescription)
{-# DEPRECATED lsidProjection "Use generic-lens or generic-optics with 'projection' instead." #-}

-- | The number of items in the specified index. DynamoDB updates this value approximately every six hours. Recent changes might not be reflected in this value.
--
-- /Note:/ Consider using 'itemCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsidItemCount :: Lens.Lens' LocalSecondaryIndexDescription (Lude.Maybe Lude.Integer)
lsidItemCount = Lens.lens (itemCount :: LocalSecondaryIndexDescription -> Lude.Maybe Lude.Integer) (\s a -> s {itemCount = a} :: LocalSecondaryIndexDescription)
{-# DEPRECATED lsidItemCount "Use generic-lens or generic-optics with 'itemCount' instead." #-}

-- | Represents the name of the local secondary index.
--
-- /Note:/ Consider using 'indexName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsidIndexName :: Lens.Lens' LocalSecondaryIndexDescription (Lude.Maybe Lude.Text)
lsidIndexName = Lens.lens (indexName :: LocalSecondaryIndexDescription -> Lude.Maybe Lude.Text) (\s a -> s {indexName = a} :: LocalSecondaryIndexDescription)
{-# DEPRECATED lsidIndexName "Use generic-lens or generic-optics with 'indexName' instead." #-}

instance Lude.FromJSON LocalSecondaryIndexDescription where
  parseJSON =
    Lude.withObject
      "LocalSecondaryIndexDescription"
      ( \x ->
          LocalSecondaryIndexDescription'
            Lude.<$> (x Lude..:? "IndexSizeBytes")
            Lude.<*> (x Lude..:? "IndexArn")
            Lude.<*> (x Lude..:? "KeySchema")
            Lude.<*> (x Lude..:? "Projection")
            Lude.<*> (x Lude..:? "ItemCount")
            Lude.<*> (x Lude..:? "IndexName")
      )
