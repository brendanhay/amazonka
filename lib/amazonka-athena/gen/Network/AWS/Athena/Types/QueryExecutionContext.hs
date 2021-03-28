{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Athena.Types.QueryExecutionContext
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Athena.Types.QueryExecutionContext
  ( QueryExecutionContext (..)
  -- * Smart constructor
  , mkQueryExecutionContext
  -- * Lenses
  , qecCatalog
  , qecDatabase
  ) where

import qualified Network.AWS.Athena.Types.Catalog as Types
import qualified Network.AWS.Athena.Types.DatabaseString as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The database and data catalog context in which the query execution occurs.
--
-- /See:/ 'mkQueryExecutionContext' smart constructor.
data QueryExecutionContext = QueryExecutionContext'
  { catalog :: Core.Maybe Types.Catalog
    -- ^ The name of the data catalog used in the query execution.
  , database :: Core.Maybe Types.DatabaseString
    -- ^ The name of the database used in the query execution.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'QueryExecutionContext' value with any optional fields omitted.
mkQueryExecutionContext
    :: QueryExecutionContext
mkQueryExecutionContext
  = QueryExecutionContext'{catalog = Core.Nothing,
                           database = Core.Nothing}

-- | The name of the data catalog used in the query execution.
--
-- /Note:/ Consider using 'catalog' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qecCatalog :: Lens.Lens' QueryExecutionContext (Core.Maybe Types.Catalog)
qecCatalog = Lens.field @"catalog"
{-# INLINEABLE qecCatalog #-}
{-# DEPRECATED catalog "Use generic-lens or generic-optics with 'catalog' instead"  #-}

-- | The name of the database used in the query execution.
--
-- /Note:/ Consider using 'database' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qecDatabase :: Lens.Lens' QueryExecutionContext (Core.Maybe Types.DatabaseString)
qecDatabase = Lens.field @"database"
{-# INLINEABLE qecDatabase #-}
{-# DEPRECATED database "Use generic-lens or generic-optics with 'database' instead"  #-}

instance Core.FromJSON QueryExecutionContext where
        toJSON QueryExecutionContext{..}
          = Core.object
              (Core.catMaybes
                 [("Catalog" Core..=) Core.<$> catalog,
                  ("Database" Core..=) Core.<$> database])

instance Core.FromJSON QueryExecutionContext where
        parseJSON
          = Core.withObject "QueryExecutionContext" Core.$
              \ x ->
                QueryExecutionContext' Core.<$>
                  (x Core..:? "Catalog") Core.<*> x Core..:? "Database"
