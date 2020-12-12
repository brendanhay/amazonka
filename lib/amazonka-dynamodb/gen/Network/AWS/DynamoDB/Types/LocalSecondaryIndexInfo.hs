{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.LocalSecondaryIndexInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.LocalSecondaryIndexInfo
  ( LocalSecondaryIndexInfo (..),

    -- * Smart constructor
    mkLocalSecondaryIndexInfo,

    -- * Lenses
    lsiiKeySchema,
    lsiiProjection,
    lsiiIndexName,
  )
where

import Network.AWS.DynamoDB.Types.KeySchemaElement
import Network.AWS.DynamoDB.Types.Projection
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents the properties of a local secondary index for the table when the backup was created.
--
-- /See:/ 'mkLocalSecondaryIndexInfo' smart constructor.
data LocalSecondaryIndexInfo = LocalSecondaryIndexInfo'
  { keySchema ::
      Lude.Maybe (Lude.NonEmpty KeySchemaElement),
    projection :: Lude.Maybe Projection,
    indexName :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'LocalSecondaryIndexInfo' with the minimum fields required to make a request.
--
-- * 'indexName' - Represents the name of the local secondary index.
-- * 'keySchema' - The complete key schema for a local secondary index, which consists of one or more pairs of attribute names and key types:
--
--
--     * @HASH@ - partition key
--
--
--     * @RANGE@ - sort key
--
--
-- * 'projection' - Represents attributes that are copied (projected) from the table into the global secondary index. These are in addition to the primary key attributes and index key attributes, which are automatically projected.
mkLocalSecondaryIndexInfo ::
  LocalSecondaryIndexInfo
mkLocalSecondaryIndexInfo =
  LocalSecondaryIndexInfo'
    { keySchema = Lude.Nothing,
      projection = Lude.Nothing,
      indexName = Lude.Nothing
    }

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
lsiiKeySchema :: Lens.Lens' LocalSecondaryIndexInfo (Lude.Maybe (Lude.NonEmpty KeySchemaElement))
lsiiKeySchema = Lens.lens (keySchema :: LocalSecondaryIndexInfo -> Lude.Maybe (Lude.NonEmpty KeySchemaElement)) (\s a -> s {keySchema = a} :: LocalSecondaryIndexInfo)
{-# DEPRECATED lsiiKeySchema "Use generic-lens or generic-optics with 'keySchema' instead." #-}

-- | Represents attributes that are copied (projected) from the table into the global secondary index. These are in addition to the primary key attributes and index key attributes, which are automatically projected.
--
-- /Note:/ Consider using 'projection' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsiiProjection :: Lens.Lens' LocalSecondaryIndexInfo (Lude.Maybe Projection)
lsiiProjection = Lens.lens (projection :: LocalSecondaryIndexInfo -> Lude.Maybe Projection) (\s a -> s {projection = a} :: LocalSecondaryIndexInfo)
{-# DEPRECATED lsiiProjection "Use generic-lens or generic-optics with 'projection' instead." #-}

-- | Represents the name of the local secondary index.
--
-- /Note:/ Consider using 'indexName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsiiIndexName :: Lens.Lens' LocalSecondaryIndexInfo (Lude.Maybe Lude.Text)
lsiiIndexName = Lens.lens (indexName :: LocalSecondaryIndexInfo -> Lude.Maybe Lude.Text) (\s a -> s {indexName = a} :: LocalSecondaryIndexInfo)
{-# DEPRECATED lsiiIndexName "Use generic-lens or generic-optics with 'indexName' instead." #-}

instance Lude.FromJSON LocalSecondaryIndexInfo where
  parseJSON =
    Lude.withObject
      "LocalSecondaryIndexInfo"
      ( \x ->
          LocalSecondaryIndexInfo'
            Lude.<$> (x Lude..:? "KeySchema")
            Lude.<*> (x Lude..:? "Projection")
            Lude.<*> (x Lude..:? "IndexName")
      )
