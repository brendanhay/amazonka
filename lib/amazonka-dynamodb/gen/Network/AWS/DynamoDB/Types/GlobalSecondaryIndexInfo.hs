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
    gsiiProvisionedThroughput,
    gsiiKeySchema,
    gsiiProjection,
    gsiiIndexName,
  )
where

import Network.AWS.DynamoDB.Types.KeySchemaElement
import Network.AWS.DynamoDB.Types.Projection
import Network.AWS.DynamoDB.Types.ProvisionedThroughput
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents the properties of a global secondary index for the table when the backup was created.
--
-- /See:/ 'mkGlobalSecondaryIndexInfo' smart constructor.
data GlobalSecondaryIndexInfo = GlobalSecondaryIndexInfo'
  { provisionedThroughput ::
      Lude.Maybe ProvisionedThroughput,
    keySchema ::
      Lude.Maybe
        (Lude.NonEmpty KeySchemaElement),
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

-- | Creates a value of 'GlobalSecondaryIndexInfo' with the minimum fields required to make a request.
--
-- * 'indexName' - The name of the global secondary index.
-- * 'keySchema' - The complete key schema for a global secondary index, which consists of one or more pairs of attribute names and key types:
--
--
--     * @HASH@ - partition key
--
--
--     * @RANGE@ - sort key
--
--
-- * 'projection' - Represents attributes that are copied (projected) from the table into the global secondary index. These are in addition to the primary key attributes and index key attributes, which are automatically projected.
-- * 'provisionedThroughput' - Represents the provisioned throughput settings for the specified global secondary index.
mkGlobalSecondaryIndexInfo ::
  GlobalSecondaryIndexInfo
mkGlobalSecondaryIndexInfo =
  GlobalSecondaryIndexInfo'
    { provisionedThroughput = Lude.Nothing,
      keySchema = Lude.Nothing,
      projection = Lude.Nothing,
      indexName = Lude.Nothing
    }

-- | Represents the provisioned throughput settings for the specified global secondary index.
--
-- /Note:/ Consider using 'provisionedThroughput' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsiiProvisionedThroughput :: Lens.Lens' GlobalSecondaryIndexInfo (Lude.Maybe ProvisionedThroughput)
gsiiProvisionedThroughput = Lens.lens (provisionedThroughput :: GlobalSecondaryIndexInfo -> Lude.Maybe ProvisionedThroughput) (\s a -> s {provisionedThroughput = a} :: GlobalSecondaryIndexInfo)
{-# DEPRECATED gsiiProvisionedThroughput "Use generic-lens or generic-optics with 'provisionedThroughput' instead." #-}

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
gsiiKeySchema :: Lens.Lens' GlobalSecondaryIndexInfo (Lude.Maybe (Lude.NonEmpty KeySchemaElement))
gsiiKeySchema = Lens.lens (keySchema :: GlobalSecondaryIndexInfo -> Lude.Maybe (Lude.NonEmpty KeySchemaElement)) (\s a -> s {keySchema = a} :: GlobalSecondaryIndexInfo)
{-# DEPRECATED gsiiKeySchema "Use generic-lens or generic-optics with 'keySchema' instead." #-}

-- | Represents attributes that are copied (projected) from the table into the global secondary index. These are in addition to the primary key attributes and index key attributes, which are automatically projected.
--
-- /Note:/ Consider using 'projection' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsiiProjection :: Lens.Lens' GlobalSecondaryIndexInfo (Lude.Maybe Projection)
gsiiProjection = Lens.lens (projection :: GlobalSecondaryIndexInfo -> Lude.Maybe Projection) (\s a -> s {projection = a} :: GlobalSecondaryIndexInfo)
{-# DEPRECATED gsiiProjection "Use generic-lens or generic-optics with 'projection' instead." #-}

-- | The name of the global secondary index.
--
-- /Note:/ Consider using 'indexName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsiiIndexName :: Lens.Lens' GlobalSecondaryIndexInfo (Lude.Maybe Lude.Text)
gsiiIndexName = Lens.lens (indexName :: GlobalSecondaryIndexInfo -> Lude.Maybe Lude.Text) (\s a -> s {indexName = a} :: GlobalSecondaryIndexInfo)
{-# DEPRECATED gsiiIndexName "Use generic-lens or generic-optics with 'indexName' instead." #-}

instance Lude.FromJSON GlobalSecondaryIndexInfo where
  parseJSON =
    Lude.withObject
      "GlobalSecondaryIndexInfo"
      ( \x ->
          GlobalSecondaryIndexInfo'
            Lude.<$> (x Lude..:? "ProvisionedThroughput")
            Lude.<*> (x Lude..:? "KeySchema")
            Lude.<*> (x Lude..:? "Projection")
            Lude.<*> (x Lude..:? "IndexName")
      )
