-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkDocs.Types.GroupMetadata
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkDocs.Types.GroupMetadata
  ( GroupMetadata (..),

    -- * Smart constructor
    mkGroupMetadata,

    -- * Lenses
    gmName,
    gmId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the metadata of a user group.
--
-- /See:/ 'mkGroupMetadata' smart constructor.
data GroupMetadata = GroupMetadata'
  { name :: Lude.Maybe Lude.Text,
    id :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GroupMetadata' with the minimum fields required to make a request.
--
-- * 'id' - The ID of the user group.
-- * 'name' - The name of the group.
mkGroupMetadata ::
  GroupMetadata
mkGroupMetadata =
  GroupMetadata' {name = Lude.Nothing, id = Lude.Nothing}

-- | The name of the group.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmName :: Lens.Lens' GroupMetadata (Lude.Maybe Lude.Text)
gmName = Lens.lens (name :: GroupMetadata -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: GroupMetadata)
{-# DEPRECATED gmName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The ID of the user group.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmId :: Lens.Lens' GroupMetadata (Lude.Maybe Lude.Text)
gmId = Lens.lens (id :: GroupMetadata -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: GroupMetadata)
{-# DEPRECATED gmId "Use generic-lens or generic-optics with 'id' instead." #-}

instance Lude.FromJSON GroupMetadata where
  parseJSON =
    Lude.withObject
      "GroupMetadata"
      ( \x ->
          GroupMetadata'
            Lude.<$> (x Lude..:? "Name") Lude.<*> (x Lude..:? "Id")
      )
