-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.Types.UserStorage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkSpaces.Types.UserStorage
  ( UserStorage (..),

    -- * Smart constructor
    mkUserStorage,

    -- * Lenses
    usCapacity,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the user storage for a WorkSpace bundle.
--
-- /See:/ 'mkUserStorage' smart constructor.
newtype UserStorage = UserStorage'
  { capacity ::
      Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UserStorage' with the minimum fields required to make a request.
--
-- * 'capacity' - The size of the user storage.
mkUserStorage ::
  UserStorage
mkUserStorage = UserStorage' {capacity = Lude.Nothing}

-- | The size of the user storage.
--
-- /Note:/ Consider using 'capacity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usCapacity :: Lens.Lens' UserStorage (Lude.Maybe Lude.Text)
usCapacity = Lens.lens (capacity :: UserStorage -> Lude.Maybe Lude.Text) (\s a -> s {capacity = a} :: UserStorage)
{-# DEPRECATED usCapacity "Use generic-lens or generic-optics with 'capacity' instead." #-}

instance Lude.FromJSON UserStorage where
  parseJSON =
    Lude.withObject
      "UserStorage"
      (\x -> UserStorage' Lude.<$> (x Lude..:? "Capacity"))
