{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Athena.Types.ResultSet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Athena.Types.ResultSet
  ( ResultSet (..),

    -- * Smart constructor
    mkResultSet,

    -- * Lenses
    rsResultSetMetadata,
    rsRows,
  )
where

import qualified Network.AWS.Athena.Types.ResultSetMetadata as Types
import qualified Network.AWS.Athena.Types.Row as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The metadata and rows that comprise a query result set. The metadata describes the column structure and data types. To return a @ResultSet@ object, use 'GetQueryResults' .
--
-- /See:/ 'mkResultSet' smart constructor.
data ResultSet = ResultSet'
  { -- | The metadata that describes the column structure and data types of a table of query results.
    resultSetMetadata :: Core.Maybe Types.ResultSetMetadata,
    -- | The rows in the table.
    rows :: Core.Maybe [Types.Row]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ResultSet' value with any optional fields omitted.
mkResultSet ::
  ResultSet
mkResultSet =
  ResultSet' {resultSetMetadata = Core.Nothing, rows = Core.Nothing}

-- | The metadata that describes the column structure and data types of a table of query results.
--
-- /Note:/ Consider using 'resultSetMetadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsResultSetMetadata :: Lens.Lens' ResultSet (Core.Maybe Types.ResultSetMetadata)
rsResultSetMetadata = Lens.field @"resultSetMetadata"
{-# DEPRECATED rsResultSetMetadata "Use generic-lens or generic-optics with 'resultSetMetadata' instead." #-}

-- | The rows in the table.
--
-- /Note:/ Consider using 'rows' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsRows :: Lens.Lens' ResultSet (Core.Maybe [Types.Row])
rsRows = Lens.field @"rows"
{-# DEPRECATED rsRows "Use generic-lens or generic-optics with 'rows' instead." #-}

instance Core.FromJSON ResultSet where
  parseJSON =
    Core.withObject "ResultSet" Core.$
      \x ->
        ResultSet'
          Core.<$> (x Core..:? "ResultSetMetadata") Core.<*> (x Core..:? "Rows")
