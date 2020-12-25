{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.Types.SqlQueryDatasetAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTAnalytics.Types.SqlQueryDatasetAction
  ( SqlQueryDatasetAction (..),

    -- * Smart constructor
    mkSqlQueryDatasetAction,

    -- * Lenses
    sqdaSqlQuery,
    sqdaFilters,
  )
where

import qualified Network.AWS.IoTAnalytics.Types.QueryFilter as Types
import qualified Network.AWS.IoTAnalytics.Types.SqlQuery as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The SQL query to modify the message.
--
-- /See:/ 'mkSqlQueryDatasetAction' smart constructor.
data SqlQueryDatasetAction = SqlQueryDatasetAction'
  { -- | A SQL query string.
    sqlQuery :: Types.SqlQuery,
    -- | Prefilters applied to message data.
    filters :: Core.Maybe [Types.QueryFilter]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SqlQueryDatasetAction' value with any optional fields omitted.
mkSqlQueryDatasetAction ::
  -- | 'sqlQuery'
  Types.SqlQuery ->
  SqlQueryDatasetAction
mkSqlQueryDatasetAction sqlQuery =
  SqlQueryDatasetAction' {sqlQuery, filters = Core.Nothing}

-- | A SQL query string.
--
-- /Note:/ Consider using 'sqlQuery' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sqdaSqlQuery :: Lens.Lens' SqlQueryDatasetAction Types.SqlQuery
sqdaSqlQuery = Lens.field @"sqlQuery"
{-# DEPRECATED sqdaSqlQuery "Use generic-lens or generic-optics with 'sqlQuery' instead." #-}

-- | Prefilters applied to message data.
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sqdaFilters :: Lens.Lens' SqlQueryDatasetAction (Core.Maybe [Types.QueryFilter])
sqdaFilters = Lens.field @"filters"
{-# DEPRECATED sqdaFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

instance Core.FromJSON SqlQueryDatasetAction where
  toJSON SqlQueryDatasetAction {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("sqlQuery" Core..= sqlQuery),
            ("filters" Core..=) Core.<$> filters
          ]
      )

instance Core.FromJSON SqlQueryDatasetAction where
  parseJSON =
    Core.withObject "SqlQueryDatasetAction" Core.$
      \x ->
        SqlQueryDatasetAction'
          Core.<$> (x Core..: "sqlQuery") Core.<*> (x Core..:? "filters")
