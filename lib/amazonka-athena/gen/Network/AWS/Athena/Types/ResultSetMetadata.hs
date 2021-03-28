{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Athena.Types.ResultSetMetadata
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Athena.Types.ResultSetMetadata
  ( ResultSetMetadata (..)
  -- * Smart constructor
  , mkResultSetMetadata
  -- * Lenses
  , rsmColumnInfo
  ) where

import qualified Network.AWS.Athena.Types.ColumnInfo as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The metadata that describes the column structure and data types of a table of query results. To return a @ResultSetMetadata@ object, use 'GetQueryResults' .
--
-- /See:/ 'mkResultSetMetadata' smart constructor.
newtype ResultSetMetadata = ResultSetMetadata'
  { columnInfo :: Core.Maybe [Types.ColumnInfo]
    -- ^ Information about the columns returned in a query result metadata.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'ResultSetMetadata' value with any optional fields omitted.
mkResultSetMetadata
    :: ResultSetMetadata
mkResultSetMetadata = ResultSetMetadata'{columnInfo = Core.Nothing}

-- | Information about the columns returned in a query result metadata.
--
-- /Note:/ Consider using 'columnInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsmColumnInfo :: Lens.Lens' ResultSetMetadata (Core.Maybe [Types.ColumnInfo])
rsmColumnInfo = Lens.field @"columnInfo"
{-# INLINEABLE rsmColumnInfo #-}
{-# DEPRECATED columnInfo "Use generic-lens or generic-optics with 'columnInfo' instead"  #-}

instance Core.FromJSON ResultSetMetadata where
        parseJSON
          = Core.withObject "ResultSetMetadata" Core.$
              \ x -> ResultSetMetadata' Core.<$> (x Core..:? "ColumnInfo")
