{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.CreateGlobalSecondaryIndexAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.CreateGlobalSecondaryIndexAction
  ( CreateGlobalSecondaryIndexAction (..),

    -- * Smart constructor
    mkCreateGlobalSecondaryIndexAction,

    -- * Lenses
    cgsiaProvisionedThroughput,
    cgsiaIndexName,
    cgsiaKeySchema,
    cgsiaProjection,
  )
where

import Network.AWS.DynamoDB.Types.KeySchemaElement
import Network.AWS.DynamoDB.Types.Projection
import Network.AWS.DynamoDB.Types.ProvisionedThroughput
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents a new global secondary index to be added to an existing table.
--
-- /See:/ 'mkCreateGlobalSecondaryIndexAction' smart constructor.
data CreateGlobalSecondaryIndexAction = CreateGlobalSecondaryIndexAction'
  { provisionedThroughput ::
      Lude.Maybe
        ProvisionedThroughput,
    indexName :: Lude.Text,
    keySchema ::
      Lude.NonEmpty
        KeySchemaElement,
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

-- | Creates a value of 'CreateGlobalSecondaryIndexAction' with the minimum fields required to make a request.
--
-- * 'indexName' - The name of the global secondary index to be created.
-- * 'keySchema' - The key schema for the global secondary index.
-- * 'projection' - Represents attributes that are copied (projected) from the table into an index. These are in addition to the primary key attributes and index key attributes, which are automatically projected.
-- * 'provisionedThroughput' - Represents the provisioned throughput settings for the specified global secondary index.
--
-- For current minimum and maximum provisioned throughput values, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Limits.html Service, Account, and Table Quotas> in the /Amazon DynamoDB Developer Guide/ .
mkCreateGlobalSecondaryIndexAction ::
  -- | 'indexName'
  Lude.Text ->
  -- | 'keySchema'
  Lude.NonEmpty KeySchemaElement ->
  -- | 'projection'
  Projection ->
  CreateGlobalSecondaryIndexAction
mkCreateGlobalSecondaryIndexAction
  pIndexName_
  pKeySchema_
  pProjection_ =
    CreateGlobalSecondaryIndexAction'
      { provisionedThroughput =
          Lude.Nothing,
        indexName = pIndexName_,
        keySchema = pKeySchema_,
        projection = pProjection_
      }

-- | Represents the provisioned throughput settings for the specified global secondary index.
--
-- For current minimum and maximum provisioned throughput values, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Limits.html Service, Account, and Table Quotas> in the /Amazon DynamoDB Developer Guide/ .
--
-- /Note:/ Consider using 'provisionedThroughput' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgsiaProvisionedThroughput :: Lens.Lens' CreateGlobalSecondaryIndexAction (Lude.Maybe ProvisionedThroughput)
cgsiaProvisionedThroughput = Lens.lens (provisionedThroughput :: CreateGlobalSecondaryIndexAction -> Lude.Maybe ProvisionedThroughput) (\s a -> s {provisionedThroughput = a} :: CreateGlobalSecondaryIndexAction)
{-# DEPRECATED cgsiaProvisionedThroughput "Use generic-lens or generic-optics with 'provisionedThroughput' instead." #-}

-- | The name of the global secondary index to be created.
--
-- /Note:/ Consider using 'indexName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgsiaIndexName :: Lens.Lens' CreateGlobalSecondaryIndexAction Lude.Text
cgsiaIndexName = Lens.lens (indexName :: CreateGlobalSecondaryIndexAction -> Lude.Text) (\s a -> s {indexName = a} :: CreateGlobalSecondaryIndexAction)
{-# DEPRECATED cgsiaIndexName "Use generic-lens or generic-optics with 'indexName' instead." #-}

-- | The key schema for the global secondary index.
--
-- /Note:/ Consider using 'keySchema' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgsiaKeySchema :: Lens.Lens' CreateGlobalSecondaryIndexAction (Lude.NonEmpty KeySchemaElement)
cgsiaKeySchema = Lens.lens (keySchema :: CreateGlobalSecondaryIndexAction -> Lude.NonEmpty KeySchemaElement) (\s a -> s {keySchema = a} :: CreateGlobalSecondaryIndexAction)
{-# DEPRECATED cgsiaKeySchema "Use generic-lens or generic-optics with 'keySchema' instead." #-}

-- | Represents attributes that are copied (projected) from the table into an index. These are in addition to the primary key attributes and index key attributes, which are automatically projected.
--
-- /Note:/ Consider using 'projection' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgsiaProjection :: Lens.Lens' CreateGlobalSecondaryIndexAction Projection
cgsiaProjection = Lens.lens (projection :: CreateGlobalSecondaryIndexAction -> Projection) (\s a -> s {projection = a} :: CreateGlobalSecondaryIndexAction)
{-# DEPRECATED cgsiaProjection "Use generic-lens or generic-optics with 'projection' instead." #-}

instance Lude.ToJSON CreateGlobalSecondaryIndexAction where
  toJSON CreateGlobalSecondaryIndexAction' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("ProvisionedThroughput" Lude..=) Lude.<$> provisionedThroughput,
            Lude.Just ("IndexName" Lude..= indexName),
            Lude.Just ("KeySchema" Lude..= keySchema),
            Lude.Just ("Projection" Lude..= projection)
          ]
      )
