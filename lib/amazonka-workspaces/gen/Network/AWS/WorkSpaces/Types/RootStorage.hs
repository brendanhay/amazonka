-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.Types.RootStorage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkSpaces.Types.RootStorage
  ( RootStorage (..),

    -- * Smart constructor
    mkRootStorage,

    -- * Lenses
    rsCapacity,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the root volume for a WorkSpace bundle.
--
-- /See:/ 'mkRootStorage' smart constructor.
newtype RootStorage = RootStorage'
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

-- | Creates a value of 'RootStorage' with the minimum fields required to make a request.
--
-- * 'capacity' - The size of the root volume.
mkRootStorage ::
  RootStorage
mkRootStorage = RootStorage' {capacity = Lude.Nothing}

-- | The size of the root volume.
--
-- /Note:/ Consider using 'capacity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsCapacity :: Lens.Lens' RootStorage (Lude.Maybe Lude.Text)
rsCapacity = Lens.lens (capacity :: RootStorage -> Lude.Maybe Lude.Text) (\s a -> s {capacity = a} :: RootStorage)
{-# DEPRECATED rsCapacity "Use generic-lens or generic-optics with 'capacity' instead." #-}

instance Lude.FromJSON RootStorage where
  parseJSON =
    Lude.withObject
      "RootStorage"
      (\x -> RootStorage' Lude.<$> (x Lude..:? "Capacity"))
