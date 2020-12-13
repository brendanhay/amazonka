{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Athena.Types.ResultSetMetadata
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Athena.Types.ResultSetMetadata
  ( ResultSetMetadata (..),

    -- * Smart constructor
    mkResultSetMetadata,

    -- * Lenses
    rsmColumnInfo,
  )
where

import Network.AWS.Athena.Types.ColumnInfo
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The metadata that describes the column structure and data types of a table of query results. To return a @ResultSetMetadata@ object, use 'GetQueryResults' .
--
-- /See:/ 'mkResultSetMetadata' smart constructor.
newtype ResultSetMetadata = ResultSetMetadata'
  { -- | Information about the columns returned in a query result metadata.
    columnInfo :: Lude.Maybe [ColumnInfo]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ResultSetMetadata' with the minimum fields required to make a request.
--
-- * 'columnInfo' - Information about the columns returned in a query result metadata.
mkResultSetMetadata ::
  ResultSetMetadata
mkResultSetMetadata = ResultSetMetadata' {columnInfo = Lude.Nothing}

-- | Information about the columns returned in a query result metadata.
--
-- /Note:/ Consider using 'columnInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsmColumnInfo :: Lens.Lens' ResultSetMetadata (Lude.Maybe [ColumnInfo])
rsmColumnInfo = Lens.lens (columnInfo :: ResultSetMetadata -> Lude.Maybe [ColumnInfo]) (\s a -> s {columnInfo = a} :: ResultSetMetadata)
{-# DEPRECATED rsmColumnInfo "Use generic-lens or generic-optics with 'columnInfo' instead." #-}

instance Lude.FromJSON ResultSetMetadata where
  parseJSON =
    Lude.withObject
      "ResultSetMetadata"
      ( \x ->
          ResultSetMetadata'
            Lude.<$> (x Lude..:? "ColumnInfo" Lude..!= Lude.mempty)
      )
