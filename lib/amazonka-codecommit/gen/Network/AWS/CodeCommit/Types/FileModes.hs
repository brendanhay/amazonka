{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.Types.FileModes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeCommit.Types.FileModes
  ( FileModes (..),

    -- * Smart constructor
    mkFileModes,

    -- * Lenses
    fmDestination,
    fmBase,
    fmSource,
  )
where

import Network.AWS.CodeCommit.Types.FileModeTypeEnum
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about file modes in a merge or pull request.
--
-- /See:/ 'mkFileModes' smart constructor.
data FileModes = FileModes'
  { destination ::
      Lude.Maybe FileModeTypeEnum,
    base :: Lude.Maybe FileModeTypeEnum,
    source :: Lude.Maybe FileModeTypeEnum
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'FileModes' with the minimum fields required to make a request.
--
-- * 'base' - The file mode of a file in the base of a merge or pull request.
-- * 'destination' - The file mode of a file in the destination of a merge or pull request.
-- * 'source' - The file mode of a file in the source of a merge or pull request.
mkFileModes ::
  FileModes
mkFileModes =
  FileModes'
    { destination = Lude.Nothing,
      base = Lude.Nothing,
      source = Lude.Nothing
    }

-- | The file mode of a file in the destination of a merge or pull request.
--
-- /Note:/ Consider using 'destination' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fmDestination :: Lens.Lens' FileModes (Lude.Maybe FileModeTypeEnum)
fmDestination = Lens.lens (destination :: FileModes -> Lude.Maybe FileModeTypeEnum) (\s a -> s {destination = a} :: FileModes)
{-# DEPRECATED fmDestination "Use generic-lens or generic-optics with 'destination' instead." #-}

-- | The file mode of a file in the base of a merge or pull request.
--
-- /Note:/ Consider using 'base' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fmBase :: Lens.Lens' FileModes (Lude.Maybe FileModeTypeEnum)
fmBase = Lens.lens (base :: FileModes -> Lude.Maybe FileModeTypeEnum) (\s a -> s {base = a} :: FileModes)
{-# DEPRECATED fmBase "Use generic-lens or generic-optics with 'base' instead." #-}

-- | The file mode of a file in the source of a merge or pull request.
--
-- /Note:/ Consider using 'source' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fmSource :: Lens.Lens' FileModes (Lude.Maybe FileModeTypeEnum)
fmSource = Lens.lens (source :: FileModes -> Lude.Maybe FileModeTypeEnum) (\s a -> s {source = a} :: FileModes)
{-# DEPRECATED fmSource "Use generic-lens or generic-optics with 'source' instead." #-}

instance Lude.FromJSON FileModes where
  parseJSON =
    Lude.withObject
      "FileModes"
      ( \x ->
          FileModes'
            Lude.<$> (x Lude..:? "destination")
            Lude.<*> (x Lude..:? "base")
            Lude.<*> (x Lude..:? "source")
      )
