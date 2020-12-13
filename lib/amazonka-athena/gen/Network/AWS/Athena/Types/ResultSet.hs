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
    rsRows,
    rsResultSetMetadata,
  )
where

import Network.AWS.Athena.Types.ResultSetMetadata
import Network.AWS.Athena.Types.Row
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The metadata and rows that comprise a query result set. The metadata describes the column structure and data types. To return a @ResultSet@ object, use 'GetQueryResults' .
--
-- /See:/ 'mkResultSet' smart constructor.
data ResultSet = ResultSet'
  { -- | The rows in the table.
    rows :: Lude.Maybe [Row],
    -- | The metadata that describes the column structure and data types of a table of query results.
    resultSetMetadata :: Lude.Maybe ResultSetMetadata
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ResultSet' with the minimum fields required to make a request.
--
-- * 'rows' - The rows in the table.
-- * 'resultSetMetadata' - The metadata that describes the column structure and data types of a table of query results.
mkResultSet ::
  ResultSet
mkResultSet =
  ResultSet' {rows = Lude.Nothing, resultSetMetadata = Lude.Nothing}

-- | The rows in the table.
--
-- /Note:/ Consider using 'rows' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsRows :: Lens.Lens' ResultSet (Lude.Maybe [Row])
rsRows = Lens.lens (rows :: ResultSet -> Lude.Maybe [Row]) (\s a -> s {rows = a} :: ResultSet)
{-# DEPRECATED rsRows "Use generic-lens or generic-optics with 'rows' instead." #-}

-- | The metadata that describes the column structure and data types of a table of query results.
--
-- /Note:/ Consider using 'resultSetMetadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsResultSetMetadata :: Lens.Lens' ResultSet (Lude.Maybe ResultSetMetadata)
rsResultSetMetadata = Lens.lens (resultSetMetadata :: ResultSet -> Lude.Maybe ResultSetMetadata) (\s a -> s {resultSetMetadata = a} :: ResultSet)
{-# DEPRECATED rsResultSetMetadata "Use generic-lens or generic-optics with 'resultSetMetadata' instead." #-}

instance Lude.FromJSON ResultSet where
  parseJSON =
    Lude.withObject
      "ResultSet"
      ( \x ->
          ResultSet'
            Lude.<$> (x Lude..:? "Rows" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "ResultSetMetadata")
      )
