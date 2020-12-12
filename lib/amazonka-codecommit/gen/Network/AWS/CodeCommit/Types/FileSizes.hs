{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.Types.FileSizes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeCommit.Types.FileSizes
  ( FileSizes (..),

    -- * Smart constructor
    mkFileSizes,

    -- * Lenses
    fsDestination,
    fsBase,
    fsSource,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about the size of files in a merge or pull request.
--
-- /See:/ 'mkFileSizes' smart constructor.
data FileSizes = FileSizes'
  { destination :: Lude.Maybe Lude.Integer,
    base :: Lude.Maybe Lude.Integer,
    source :: Lude.Maybe Lude.Integer
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'FileSizes' with the minimum fields required to make a request.
--
-- * 'base' - The size of a file in the base of a merge or pull request.
-- * 'destination' - The size of a file in the destination of a merge or pull request.
-- * 'source' - The size of a file in the source of a merge or pull request.
mkFileSizes ::
  FileSizes
mkFileSizes =
  FileSizes'
    { destination = Lude.Nothing,
      base = Lude.Nothing,
      source = Lude.Nothing
    }

-- | The size of a file in the destination of a merge or pull request.
--
-- /Note:/ Consider using 'destination' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fsDestination :: Lens.Lens' FileSizes (Lude.Maybe Lude.Integer)
fsDestination = Lens.lens (destination :: FileSizes -> Lude.Maybe Lude.Integer) (\s a -> s {destination = a} :: FileSizes)
{-# DEPRECATED fsDestination "Use generic-lens or generic-optics with 'destination' instead." #-}

-- | The size of a file in the base of a merge or pull request.
--
-- /Note:/ Consider using 'base' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fsBase :: Lens.Lens' FileSizes (Lude.Maybe Lude.Integer)
fsBase = Lens.lens (base :: FileSizes -> Lude.Maybe Lude.Integer) (\s a -> s {base = a} :: FileSizes)
{-# DEPRECATED fsBase "Use generic-lens or generic-optics with 'base' instead." #-}

-- | The size of a file in the source of a merge or pull request.
--
-- /Note:/ Consider using 'source' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fsSource :: Lens.Lens' FileSizes (Lude.Maybe Lude.Integer)
fsSource = Lens.lens (source :: FileSizes -> Lude.Maybe Lude.Integer) (\s a -> s {source = a} :: FileSizes)
{-# DEPRECATED fsSource "Use generic-lens or generic-optics with 'source' instead." #-}

instance Lude.FromJSON FileSizes where
  parseJSON =
    Lude.withObject
      "FileSizes"
      ( \x ->
          FileSizes'
            Lude.<$> (x Lude..:? "destination")
            Lude.<*> (x Lude..:? "base")
            Lude.<*> (x Lude..:? "source")
      )
