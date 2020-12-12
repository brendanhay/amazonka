{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.Types.IsBinaryFile
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeCommit.Types.IsBinaryFile
  ( IsBinaryFile (..),

    -- * Smart constructor
    mkIsBinaryFile,

    -- * Lenses
    ibfDestination,
    ibfBase,
    ibfSource,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about whether a file is binary or textual in a merge or pull request operation.
--
-- /See:/ 'mkIsBinaryFile' smart constructor.
data IsBinaryFile = IsBinaryFile'
  { destination ::
      Lude.Maybe Lude.Bool,
    base :: Lude.Maybe Lude.Bool,
    source :: Lude.Maybe Lude.Bool
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'IsBinaryFile' with the minimum fields required to make a request.
--
-- * 'base' - The binary or non-binary status of a file in the base of a merge or pull request.
-- * 'destination' - The binary or non-binary status of a file in the destination of a merge or pull request.
-- * 'source' - The binary or non-binary status of file in the source of a merge or pull request.
mkIsBinaryFile ::
  IsBinaryFile
mkIsBinaryFile =
  IsBinaryFile'
    { destination = Lude.Nothing,
      base = Lude.Nothing,
      source = Lude.Nothing
    }

-- | The binary or non-binary status of a file in the destination of a merge or pull request.
--
-- /Note:/ Consider using 'destination' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ibfDestination :: Lens.Lens' IsBinaryFile (Lude.Maybe Lude.Bool)
ibfDestination = Lens.lens (destination :: IsBinaryFile -> Lude.Maybe Lude.Bool) (\s a -> s {destination = a} :: IsBinaryFile)
{-# DEPRECATED ibfDestination "Use generic-lens or generic-optics with 'destination' instead." #-}

-- | The binary or non-binary status of a file in the base of a merge or pull request.
--
-- /Note:/ Consider using 'base' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ibfBase :: Lens.Lens' IsBinaryFile (Lude.Maybe Lude.Bool)
ibfBase = Lens.lens (base :: IsBinaryFile -> Lude.Maybe Lude.Bool) (\s a -> s {base = a} :: IsBinaryFile)
{-# DEPRECATED ibfBase "Use generic-lens or generic-optics with 'base' instead." #-}

-- | The binary or non-binary status of file in the source of a merge or pull request.
--
-- /Note:/ Consider using 'source' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ibfSource :: Lens.Lens' IsBinaryFile (Lude.Maybe Lude.Bool)
ibfSource = Lens.lens (source :: IsBinaryFile -> Lude.Maybe Lude.Bool) (\s a -> s {source = a} :: IsBinaryFile)
{-# DEPRECATED ibfSource "Use generic-lens or generic-optics with 'source' instead." #-}

instance Lude.FromJSON IsBinaryFile where
  parseJSON =
    Lude.withObject
      "IsBinaryFile"
      ( \x ->
          IsBinaryFile'
            Lude.<$> (x Lude..:? "destination")
            Lude.<*> (x Lude..:? "base")
            Lude.<*> (x Lude..:? "source")
      )
