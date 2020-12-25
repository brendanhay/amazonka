{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Athena.Types.Row
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Athena.Types.Row
  ( Row (..),

    -- * Smart constructor
    mkRow,

    -- * Lenses
    rData,
  )
where

import qualified Network.AWS.Athena.Types.Datum as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The rows that comprise a query result table.
--
-- /See:/ 'mkRow' smart constructor.
newtype Row = Row'
  { -- | The data that populates a row in a query result table.
    data' :: Core.Maybe [Types.Datum]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'Row' value with any optional fields omitted.
mkRow ::
  Row
mkRow = Row' {data' = Core.Nothing}

-- | The data that populates a row in a query result table.
--
-- /Note:/ Consider using 'data'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rData :: Lens.Lens' Row (Core.Maybe [Types.Datum])
rData = Lens.field @"data'"
{-# DEPRECATED rData "Use generic-lens or generic-optics with 'data'' instead." #-}

instance Core.FromJSON Row where
  parseJSON =
    Core.withObject "Row" Core.$
      \x -> Row' Core.<$> (x Core..:? "Data")
