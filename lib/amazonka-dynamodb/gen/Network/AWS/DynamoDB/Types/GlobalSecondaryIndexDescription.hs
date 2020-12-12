{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.GlobalSecondaryIndexDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.GlobalSecondaryIndexDescription
  ( GlobalSecondaryIndexDescription (..),

    -- * Smart constructor
    mkGlobalSecondaryIndexDescription,

    -- * Lenses
    gsidBackfilling,
    gsidIndexSizeBytes,
    gsidIndexStatus,
    gsidProvisionedThroughput,
    gsidIndexARN,
    gsidKeySchema,
    gsidProjection,
    gsidItemCount,
    gsidIndexName,
  )
where

import Network.AWS.DynamoDB.Types.IndexStatus
import Network.AWS.DynamoDB.Types.KeySchemaElement
import Network.AWS.DynamoDB.Types.Projection
import Network.AWS.DynamoDB.Types.ProvisionedThroughputDescription
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents the properties of a global secondary index.
--
-- /See:/ 'mkGlobalSecondaryIndexDescription' smart constructor.
data GlobalSecondaryIndexDescription = GlobalSecondaryIndexDescription'
  { backfilling ::
      Lude.Maybe Lude.Bool,
    indexSizeBytes ::
      Lude.Maybe Lude.Integer,
    indexStatus ::
      Lude.Maybe IndexStatus,
    provisionedThroughput ::
      Lude.Maybe
        ProvisionedThroughputDescription,
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

-- | Creates a value of 'GlobalSecondaryIndexDescription' with the minimum fields required to make a request.
--
-- * 'backfilling' - Indicates whether the index is currently backfilling. /Backfilling/ is the process of reading items from the table and determining whether they can be added to the index. (Not all items will qualify: For example, a partition key cannot have any duplicate values.) If an item can be added to the index, DynamoDB will do so. After all items have been processed, the backfilling operation is complete and @Backfilling@ is false.
--
-- You can delete an index that is being created during the @Backfilling@ phase when @IndexStatus@ is set to CREATING and @Backfilling@ is true. You can't delete the index that is being created when @IndexStatus@ is set to CREATING and @Backfilling@ is false.
-- * 'indexARN' - The Amazon Resource Name (ARN) that uniquely identifies the index.
-- * 'indexName' - The name of the global secondary index.
-- * 'indexSizeBytes' - The total size of the specified index, in bytes. DynamoDB updates this value approximately every six hours. Recent changes might not be reflected in this value.
-- * 'indexStatus' - The current state of the global secondary index:
--
--
--     * @CREATING@ - The index is being created.
--
--
--     * @UPDATING@ - The index is being updated.
--
--
--     * @DELETING@ - The index is being deleted.
--
--
--     * @ACTIVE@ - The index is ready for use.
--
--
-- * 'itemCount' - The number of items in the specified index. DynamoDB updates this value approximately every six hours. Recent changes might not be reflected in this value.
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
--
-- For current minimum and maximum provisioned throughput values, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Limits.html Service, Account, and Table Quotas> in the /Amazon DynamoDB Developer Guide/ .
mkGlobalSecondaryIndexDescription ::
  GlobalSecondaryIndexDescription
mkGlobalSecondaryIndexDescription =
  GlobalSecondaryIndexDescription'
    { backfilling = Lude.Nothing,
      indexSizeBytes = Lude.Nothing,
      indexStatus = Lude.Nothing,
      provisionedThroughput = Lude.Nothing,
      indexARN = Lude.Nothing,
      keySchema = Lude.Nothing,
      projection = Lude.Nothing,
      itemCount = Lude.Nothing,
      indexName = Lude.Nothing
    }

-- | Indicates whether the index is currently backfilling. /Backfilling/ is the process of reading items from the table and determining whether they can be added to the index. (Not all items will qualify: For example, a partition key cannot have any duplicate values.) If an item can be added to the index, DynamoDB will do so. After all items have been processed, the backfilling operation is complete and @Backfilling@ is false.
--
-- You can delete an index that is being created during the @Backfilling@ phase when @IndexStatus@ is set to CREATING and @Backfilling@ is true. You can't delete the index that is being created when @IndexStatus@ is set to CREATING and @Backfilling@ is false.
--
-- /Note:/ Consider using 'backfilling' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsidBackfilling :: Lens.Lens' GlobalSecondaryIndexDescription (Lude.Maybe Lude.Bool)
gsidBackfilling = Lens.lens (backfilling :: GlobalSecondaryIndexDescription -> Lude.Maybe Lude.Bool) (\s a -> s {backfilling = a} :: GlobalSecondaryIndexDescription)
{-# DEPRECATED gsidBackfilling "Use generic-lens or generic-optics with 'backfilling' instead." #-}

-- | The total size of the specified index, in bytes. DynamoDB updates this value approximately every six hours. Recent changes might not be reflected in this value.
--
-- /Note:/ Consider using 'indexSizeBytes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsidIndexSizeBytes :: Lens.Lens' GlobalSecondaryIndexDescription (Lude.Maybe Lude.Integer)
gsidIndexSizeBytes = Lens.lens (indexSizeBytes :: GlobalSecondaryIndexDescription -> Lude.Maybe Lude.Integer) (\s a -> s {indexSizeBytes = a} :: GlobalSecondaryIndexDescription)
{-# DEPRECATED gsidIndexSizeBytes "Use generic-lens or generic-optics with 'indexSizeBytes' instead." #-}

-- | The current state of the global secondary index:
--
--
--     * @CREATING@ - The index is being created.
--
--
--     * @UPDATING@ - The index is being updated.
--
--
--     * @DELETING@ - The index is being deleted.
--
--
--     * @ACTIVE@ - The index is ready for use.
--
--
--
-- /Note:/ Consider using 'indexStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsidIndexStatus :: Lens.Lens' GlobalSecondaryIndexDescription (Lude.Maybe IndexStatus)
gsidIndexStatus = Lens.lens (indexStatus :: GlobalSecondaryIndexDescription -> Lude.Maybe IndexStatus) (\s a -> s {indexStatus = a} :: GlobalSecondaryIndexDescription)
{-# DEPRECATED gsidIndexStatus "Use generic-lens or generic-optics with 'indexStatus' instead." #-}

-- | Represents the provisioned throughput settings for the specified global secondary index.
--
-- For current minimum and maximum provisioned throughput values, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Limits.html Service, Account, and Table Quotas> in the /Amazon DynamoDB Developer Guide/ .
--
-- /Note:/ Consider using 'provisionedThroughput' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsidProvisionedThroughput :: Lens.Lens' GlobalSecondaryIndexDescription (Lude.Maybe ProvisionedThroughputDescription)
gsidProvisionedThroughput = Lens.lens (provisionedThroughput :: GlobalSecondaryIndexDescription -> Lude.Maybe ProvisionedThroughputDescription) (\s a -> s {provisionedThroughput = a} :: GlobalSecondaryIndexDescription)
{-# DEPRECATED gsidProvisionedThroughput "Use generic-lens or generic-optics with 'provisionedThroughput' instead." #-}

-- | The Amazon Resource Name (ARN) that uniquely identifies the index.
--
-- /Note:/ Consider using 'indexARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsidIndexARN :: Lens.Lens' GlobalSecondaryIndexDescription (Lude.Maybe Lude.Text)
gsidIndexARN = Lens.lens (indexARN :: GlobalSecondaryIndexDescription -> Lude.Maybe Lude.Text) (\s a -> s {indexARN = a} :: GlobalSecondaryIndexDescription)
{-# DEPRECATED gsidIndexARN "Use generic-lens or generic-optics with 'indexARN' instead." #-}

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
gsidKeySchema :: Lens.Lens' GlobalSecondaryIndexDescription (Lude.Maybe (Lude.NonEmpty KeySchemaElement))
gsidKeySchema = Lens.lens (keySchema :: GlobalSecondaryIndexDescription -> Lude.Maybe (Lude.NonEmpty KeySchemaElement)) (\s a -> s {keySchema = a} :: GlobalSecondaryIndexDescription)
{-# DEPRECATED gsidKeySchema "Use generic-lens or generic-optics with 'keySchema' instead." #-}

-- | Represents attributes that are copied (projected) from the table into the global secondary index. These are in addition to the primary key attributes and index key attributes, which are automatically projected.
--
-- /Note:/ Consider using 'projection' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsidProjection :: Lens.Lens' GlobalSecondaryIndexDescription (Lude.Maybe Projection)
gsidProjection = Lens.lens (projection :: GlobalSecondaryIndexDescription -> Lude.Maybe Projection) (\s a -> s {projection = a} :: GlobalSecondaryIndexDescription)
{-# DEPRECATED gsidProjection "Use generic-lens or generic-optics with 'projection' instead." #-}

-- | The number of items in the specified index. DynamoDB updates this value approximately every six hours. Recent changes might not be reflected in this value.
--
-- /Note:/ Consider using 'itemCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsidItemCount :: Lens.Lens' GlobalSecondaryIndexDescription (Lude.Maybe Lude.Integer)
gsidItemCount = Lens.lens (itemCount :: GlobalSecondaryIndexDescription -> Lude.Maybe Lude.Integer) (\s a -> s {itemCount = a} :: GlobalSecondaryIndexDescription)
{-# DEPRECATED gsidItemCount "Use generic-lens or generic-optics with 'itemCount' instead." #-}

-- | The name of the global secondary index.
--
-- /Note:/ Consider using 'indexName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsidIndexName :: Lens.Lens' GlobalSecondaryIndexDescription (Lude.Maybe Lude.Text)
gsidIndexName = Lens.lens (indexName :: GlobalSecondaryIndexDescription -> Lude.Maybe Lude.Text) (\s a -> s {indexName = a} :: GlobalSecondaryIndexDescription)
{-# DEPRECATED gsidIndexName "Use generic-lens or generic-optics with 'indexName' instead." #-}

instance Lude.FromJSON GlobalSecondaryIndexDescription where
  parseJSON =
    Lude.withObject
      "GlobalSecondaryIndexDescription"
      ( \x ->
          GlobalSecondaryIndexDescription'
            Lude.<$> (x Lude..:? "Backfilling")
            Lude.<*> (x Lude..:? "IndexSizeBytes")
            Lude.<*> (x Lude..:? "IndexStatus")
            Lude.<*> (x Lude..:? "ProvisionedThroughput")
            Lude.<*> (x Lude..:? "IndexArn")
            Lude.<*> (x Lude..:? "KeySchema")
            Lude.<*> (x Lude..:? "Projection")
            Lude.<*> (x Lude..:? "ItemCount")
            Lude.<*> (x Lude..:? "IndexName")
      )
