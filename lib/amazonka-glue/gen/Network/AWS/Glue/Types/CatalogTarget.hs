{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.CatalogTarget
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Glue.Types.CatalogTarget
  ( CatalogTarget (..)
  -- * Smart constructor
  , mkCatalogTarget
  -- * Lenses
  , ctDatabaseName
  , ctTables
  ) where

import qualified Network.AWS.Glue.Types.NameString as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Specifies an AWS Glue Data Catalog target.
--
-- /See:/ 'mkCatalogTarget' smart constructor.
data CatalogTarget = CatalogTarget'
  { databaseName :: Types.NameString
    -- ^ The name of the database to be synchronized.
  , tables :: Core.NonEmpty Types.NameString
    -- ^ A list of the tables to be synchronized.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CatalogTarget' value with any optional fields omitted.
mkCatalogTarget
    :: Types.NameString -- ^ 'databaseName'
    -> Core.NonEmpty Types.NameString -- ^ 'tables'
    -> CatalogTarget
mkCatalogTarget databaseName tables
  = CatalogTarget'{databaseName, tables}

-- | The name of the database to be synchronized.
--
-- /Note:/ Consider using 'databaseName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctDatabaseName :: Lens.Lens' CatalogTarget Types.NameString
ctDatabaseName = Lens.field @"databaseName"
{-# INLINEABLE ctDatabaseName #-}
{-# DEPRECATED databaseName "Use generic-lens or generic-optics with 'databaseName' instead"  #-}

-- | A list of the tables to be synchronized.
--
-- /Note:/ Consider using 'tables' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctTables :: Lens.Lens' CatalogTarget (Core.NonEmpty Types.NameString)
ctTables = Lens.field @"tables"
{-# INLINEABLE ctTables #-}
{-# DEPRECATED tables "Use generic-lens or generic-optics with 'tables' instead"  #-}

instance Core.FromJSON CatalogTarget where
        toJSON CatalogTarget{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("DatabaseName" Core..= databaseName),
                  Core.Just ("Tables" Core..= tables)])

instance Core.FromJSON CatalogTarget where
        parseJSON
          = Core.withObject "CatalogTarget" Core.$
              \ x ->
                CatalogTarget' Core.<$>
                  (x Core..: "DatabaseName") Core.<*> x Core..: "Tables"
