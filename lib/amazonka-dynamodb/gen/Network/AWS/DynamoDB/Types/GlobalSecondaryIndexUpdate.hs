-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.GlobalSecondaryIndexUpdate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.GlobalSecondaryIndexUpdate
  ( GlobalSecondaryIndexUpdate (..),

    -- * Smart constructor
    mkGlobalSecondaryIndexUpdate,

    -- * Lenses
    gsiuCreate,
    gsiuDelete,
    gsiuUpdate,
  )
where

import Network.AWS.DynamoDB.Types.CreateGlobalSecondaryIndexAction
import Network.AWS.DynamoDB.Types.DeleteGlobalSecondaryIndexAction
import Network.AWS.DynamoDB.Types.UpdateGlobalSecondaryIndexAction
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents one of the following:
--
--
--     * A new global secondary index to be added to an existing table.
--
--
--     * New provisioned throughput parameters for an existing global secondary index.
--
--
--     * An existing global secondary index to be removed from an existing table.
--
--
--
-- /See:/ 'mkGlobalSecondaryIndexUpdate' smart constructor.
data GlobalSecondaryIndexUpdate = GlobalSecondaryIndexUpdate'
  { create ::
      Lude.Maybe
        CreateGlobalSecondaryIndexAction,
    delete ::
      Lude.Maybe
        DeleteGlobalSecondaryIndexAction,
    update ::
      Lude.Maybe
        UpdateGlobalSecondaryIndexAction
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GlobalSecondaryIndexUpdate' with the minimum fields required to make a request.
--
-- * 'create' - The parameters required for creating a global secondary index on an existing table:
--
--
--     * @IndexName @
--
--
--     * @KeySchema @
--
--
--     * @AttributeDefinitions @
--
--
--     * @Projection @
--
--
--     * @ProvisionedThroughput @
--
--
-- * 'delete' - The name of an existing global secondary index to be removed.
-- * 'update' - The name of an existing global secondary index, along with new provisioned throughput settings to be applied to that index.
mkGlobalSecondaryIndexUpdate ::
  GlobalSecondaryIndexUpdate
mkGlobalSecondaryIndexUpdate =
  GlobalSecondaryIndexUpdate'
    { create = Lude.Nothing,
      delete = Lude.Nothing,
      update = Lude.Nothing
    }

-- | The parameters required for creating a global secondary index on an existing table:
--
--
--     * @IndexName @
--
--
--     * @KeySchema @
--
--
--     * @AttributeDefinitions @
--
--
--     * @Projection @
--
--
--     * @ProvisionedThroughput @
--
--
--
-- /Note:/ Consider using 'create' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsiuCreate :: Lens.Lens' GlobalSecondaryIndexUpdate (Lude.Maybe CreateGlobalSecondaryIndexAction)
gsiuCreate = Lens.lens (create :: GlobalSecondaryIndexUpdate -> Lude.Maybe CreateGlobalSecondaryIndexAction) (\s a -> s {create = a} :: GlobalSecondaryIndexUpdate)
{-# DEPRECATED gsiuCreate "Use generic-lens or generic-optics with 'create' instead." #-}

-- | The name of an existing global secondary index to be removed.
--
-- /Note:/ Consider using 'delete' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsiuDelete :: Lens.Lens' GlobalSecondaryIndexUpdate (Lude.Maybe DeleteGlobalSecondaryIndexAction)
gsiuDelete = Lens.lens (delete :: GlobalSecondaryIndexUpdate -> Lude.Maybe DeleteGlobalSecondaryIndexAction) (\s a -> s {delete = a} :: GlobalSecondaryIndexUpdate)
{-# DEPRECATED gsiuDelete "Use generic-lens or generic-optics with 'delete' instead." #-}

-- | The name of an existing global secondary index, along with new provisioned throughput settings to be applied to that index.
--
-- /Note:/ Consider using 'update' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsiuUpdate :: Lens.Lens' GlobalSecondaryIndexUpdate (Lude.Maybe UpdateGlobalSecondaryIndexAction)
gsiuUpdate = Lens.lens (update :: GlobalSecondaryIndexUpdate -> Lude.Maybe UpdateGlobalSecondaryIndexAction) (\s a -> s {update = a} :: GlobalSecondaryIndexUpdate)
{-# DEPRECATED gsiuUpdate "Use generic-lens or generic-optics with 'update' instead." #-}

instance Lude.ToJSON GlobalSecondaryIndexUpdate where
  toJSON GlobalSecondaryIndexUpdate' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Create" Lude..=) Lude.<$> create,
            ("Delete" Lude..=) Lude.<$> delete,
            ("Update" Lude..=) Lude.<$> update
          ]
      )
