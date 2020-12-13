{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.SchemaChangePolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.SchemaChangePolicy
  ( SchemaChangePolicy (..),

    -- * Smart constructor
    mkSchemaChangePolicy,

    -- * Lenses
    scpDeleteBehavior,
    scpUpdateBehavior,
  )
where

import Network.AWS.Glue.Types.DeleteBehavior
import Network.AWS.Glue.Types.UpdateBehavior
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A policy that specifies update and deletion behaviors for the crawler.
--
-- /See:/ 'mkSchemaChangePolicy' smart constructor.
data SchemaChangePolicy = SchemaChangePolicy'
  { -- | The deletion behavior when the crawler finds a deleted object.
    deleteBehavior :: Lude.Maybe DeleteBehavior,
    -- | The update behavior when the crawler finds a changed schema.
    updateBehavior :: Lude.Maybe UpdateBehavior
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SchemaChangePolicy' with the minimum fields required to make a request.
--
-- * 'deleteBehavior' - The deletion behavior when the crawler finds a deleted object.
-- * 'updateBehavior' - The update behavior when the crawler finds a changed schema.
mkSchemaChangePolicy ::
  SchemaChangePolicy
mkSchemaChangePolicy =
  SchemaChangePolicy'
    { deleteBehavior = Lude.Nothing,
      updateBehavior = Lude.Nothing
    }

-- | The deletion behavior when the crawler finds a deleted object.
--
-- /Note:/ Consider using 'deleteBehavior' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scpDeleteBehavior :: Lens.Lens' SchemaChangePolicy (Lude.Maybe DeleteBehavior)
scpDeleteBehavior = Lens.lens (deleteBehavior :: SchemaChangePolicy -> Lude.Maybe DeleteBehavior) (\s a -> s {deleteBehavior = a} :: SchemaChangePolicy)
{-# DEPRECATED scpDeleteBehavior "Use generic-lens or generic-optics with 'deleteBehavior' instead." #-}

-- | The update behavior when the crawler finds a changed schema.
--
-- /Note:/ Consider using 'updateBehavior' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scpUpdateBehavior :: Lens.Lens' SchemaChangePolicy (Lude.Maybe UpdateBehavior)
scpUpdateBehavior = Lens.lens (updateBehavior :: SchemaChangePolicy -> Lude.Maybe UpdateBehavior) (\s a -> s {updateBehavior = a} :: SchemaChangePolicy)
{-# DEPRECATED scpUpdateBehavior "Use generic-lens or generic-optics with 'updateBehavior' instead." #-}

instance Lude.FromJSON SchemaChangePolicy where
  parseJSON =
    Lude.withObject
      "SchemaChangePolicy"
      ( \x ->
          SchemaChangePolicy'
            Lude.<$> (x Lude..:? "DeleteBehavior")
            Lude.<*> (x Lude..:? "UpdateBehavior")
      )

instance Lude.ToJSON SchemaChangePolicy where
  toJSON SchemaChangePolicy' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("DeleteBehavior" Lude..=) Lude.<$> deleteBehavior,
            ("UpdateBehavior" Lude..=) Lude.<$> updateBehavior
          ]
      )
