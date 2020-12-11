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
    gsiProvisionedThroughput,
    gsiIndexName,
    gsiKeySchema,
    gsiProjection,
  )
where

import Network.AWS.DynamoDB.Types.KeySchemaElement
import Network.AWS.DynamoDB.Types.Projection
import Network.AWS.DynamoDB.Types.ProvisionedThroughput
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents the properties of a global secondary index.
--
-- /See:/ 'mkGlobalSecondaryIndex' smart constructor.
data GlobalSecondaryIndex = GlobalSecondaryIndex'
  { provisionedThroughput ::
      Lude.Maybe ProvisionedThroughput,
    indexName :: Lude.Text,
    keySchema :: Lude.NonEmpty KeySchemaElement,
    projection :: Projection
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GlobalSecondaryIndex' with the minimum fields required to make a request.
--
-- * 'indexName' - The name of the global secondary index. The name must be unique among all other indexes on this table.
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
mkGlobalSecondaryIndex ::
  -- | 'indexName'
  Lude.Text ->
  -- | 'keySchema'
  Lude.NonEmpty KeySchemaElement ->
  -- | 'projection'
  Projection ->
  GlobalSecondaryIndex
mkGlobalSecondaryIndex pIndexName_ pKeySchema_ pProjection_ =
  GlobalSecondaryIndex'
    { provisionedThroughput = Lude.Nothing,
      indexName = pIndexName_,
      keySchema = pKeySchema_,
      projection = pProjection_
    }

-- | Represents the provisioned throughput settings for the specified global secondary index.
--
-- For current minimum and maximum provisioned throughput values, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Limits.html Service, Account, and Table Quotas> in the /Amazon DynamoDB Developer Guide/ .
--
-- /Note:/ Consider using 'provisionedThroughput' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsiProvisionedThroughput :: Lens.Lens' GlobalSecondaryIndex (Lude.Maybe ProvisionedThroughput)
gsiProvisionedThroughput = Lens.lens (provisionedThroughput :: GlobalSecondaryIndex -> Lude.Maybe ProvisionedThroughput) (\s a -> s {provisionedThroughput = a} :: GlobalSecondaryIndex)
{-# DEPRECATED gsiProvisionedThroughput "Use generic-lens or generic-optics with 'provisionedThroughput' instead." #-}

-- | The name of the global secondary index. The name must be unique among all other indexes on this table.
--
-- /Note:/ Consider using 'indexName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsiIndexName :: Lens.Lens' GlobalSecondaryIndex Lude.Text
gsiIndexName = Lens.lens (indexName :: GlobalSecondaryIndex -> Lude.Text) (\s a -> s {indexName = a} :: GlobalSecondaryIndex)
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
gsiKeySchema :: Lens.Lens' GlobalSecondaryIndex (Lude.NonEmpty KeySchemaElement)
gsiKeySchema = Lens.lens (keySchema :: GlobalSecondaryIndex -> Lude.NonEmpty KeySchemaElement) (\s a -> s {keySchema = a} :: GlobalSecondaryIndex)
{-# DEPRECATED gsiKeySchema "Use generic-lens or generic-optics with 'keySchema' instead." #-}

-- | Represents attributes that are copied (projected) from the table into the global secondary index. These are in addition to the primary key attributes and index key attributes, which are automatically projected.
--
-- /Note:/ Consider using 'projection' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsiProjection :: Lens.Lens' GlobalSecondaryIndex Projection
gsiProjection = Lens.lens (projection :: GlobalSecondaryIndex -> Projection) (\s a -> s {projection = a} :: GlobalSecondaryIndex)
{-# DEPRECATED gsiProjection "Use generic-lens or generic-optics with 'projection' instead." #-}

instance Lude.ToJSON GlobalSecondaryIndex where
  toJSON GlobalSecondaryIndex' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("ProvisionedThroughput" Lude..=) Lude.<$> provisionedThroughput,
            Lude.Just ("IndexName" Lude..= indexName),
            Lude.Just ("KeySchema" Lude..= keySchema),
            Lude.Just ("Projection" Lude..= projection)
          ]
      )
