{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
    pColumn,
    pLine,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains the row and column of a location of a @Statement@ element in a policy document.
--
-- This data type is used as a member of the @'Statement' @ type.
--
-- /See:/ 'mkPosition' smart constructor.
data Position = Position'
  { -- | The column in the line containing the specified position in the document.
    column :: Core.Maybe Core.Int,
    -- | The line containing the specified position in the document.
    line :: Core.Maybe Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Position' value with any optional fields omitted.
mkPosition ::
  Position
mkPosition = Position' {column = Core.Nothing, line = Core.Nothing}

-- | The column in the line containing the specified position in the document.
--
-- /Note:/ Consider using 'column' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pColumn :: Lens.Lens' Position (Core.Maybe Core.Int)
pColumn = Lens.field @"column"
{-# DEPRECATED pColumn "Use generic-lens or generic-optics with 'column' instead." #-}

-- | The line containing the specified position in the document.
--
-- /Note:/ Consider using 'line' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pLine :: Lens.Lens' Position (Core.Maybe Core.Int)
pLine = Lens.field @"line"
{-# DEPRECATED pLine "Use generic-lens or generic-optics with 'line' instead." #-}

instance Core.FromXML Position where
  parseXML x =
    Position'
      Core.<$> (x Core..@? "Column") Core.<*> (x Core..@? "Line")
