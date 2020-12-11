-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.Types.Position
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IAM.Types.Position
  ( Position (..),

    -- * Smart constructor
    mkPosition,

    -- * Lenses
    pLine,
    pColumn,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains the row and column of a location of a @Statement@ element in a policy document.
--
-- This data type is used as a member of the @'Statement' @ type.
--
-- /See:/ 'mkPosition' smart constructor.
data Position = Position'
  { line :: Lude.Maybe Lude.Int,
    column :: Lude.Maybe Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Position' with the minimum fields required to make a request.
--
-- * 'column' - The column in the line containing the specified position in the document.
-- * 'line' - The line containing the specified position in the document.
mkPosition ::
  Position
mkPosition = Position' {line = Lude.Nothing, column = Lude.Nothing}

-- | The line containing the specified position in the document.
--
-- /Note:/ Consider using 'line' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pLine :: Lens.Lens' Position (Lude.Maybe Lude.Int)
pLine = Lens.lens (line :: Position -> Lude.Maybe Lude.Int) (\s a -> s {line = a} :: Position)
{-# DEPRECATED pLine "Use generic-lens or generic-optics with 'line' instead." #-}

-- | The column in the line containing the specified position in the document.
--
-- /Note:/ Consider using 'column' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pColumn :: Lens.Lens' Position (Lude.Maybe Lude.Int)
pColumn = Lens.lens (column :: Position -> Lude.Maybe Lude.Int) (\s a -> s {column = a} :: Position)
{-# DEPRECATED pColumn "Use generic-lens or generic-optics with 'column' instead." #-}

instance Lude.FromXML Position where
  parseXML x =
    Position'
      Lude.<$> (x Lude..@? "Line") Lude.<*> (x Lude..@? "Column")
