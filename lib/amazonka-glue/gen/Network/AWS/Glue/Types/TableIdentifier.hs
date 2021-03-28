{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.TableIdentifier
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Glue.Types.TableIdentifier
  ( TableIdentifier (..)
  -- * Smart constructor
  , mkTableIdentifier
  -- * Lenses
  , tiCatalogId
  , tiDatabaseName
  , tiName
  ) where

import qualified Network.AWS.Glue.Types.CatalogId as Types
import qualified Network.AWS.Glue.Types.DatabaseName as Types
import qualified Network.AWS.Glue.Types.Name as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A structure that describes a target table for resource linking.
--
-- /See:/ 'mkTableIdentifier' smart constructor.
data TableIdentifier = TableIdentifier'
  { catalogId :: Core.Maybe Types.CatalogId
    -- ^ The ID of the Data Catalog in which the table resides.
  , databaseName :: Core.Maybe Types.DatabaseName
    -- ^ The name of the catalog database that contains the target table.
  , name :: Core.Maybe Types.Name
    -- ^ The name of the target table.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TableIdentifier' value with any optional fields omitted.
mkTableIdentifier
    :: TableIdentifier
mkTableIdentifier
  = TableIdentifier'{catalogId = Core.Nothing,
                     databaseName = Core.Nothing, name = Core.Nothing}

-- | The ID of the Data Catalog in which the table resides.
--
-- /Note:/ Consider using 'catalogId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tiCatalogId :: Lens.Lens' TableIdentifier (Core.Maybe Types.CatalogId)
tiCatalogId = Lens.field @"catalogId"
{-# INLINEABLE tiCatalogId #-}
{-# DEPRECATED catalogId "Use generic-lens or generic-optics with 'catalogId' instead"  #-}

-- | The name of the catalog database that contains the target table.
--
-- /Note:/ Consider using 'databaseName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tiDatabaseName :: Lens.Lens' TableIdentifier (Core.Maybe Types.DatabaseName)
tiDatabaseName = Lens.field @"databaseName"
{-# INLINEABLE tiDatabaseName #-}
{-# DEPRECATED databaseName "Use generic-lens or generic-optics with 'databaseName' instead"  #-}

-- | The name of the target table.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tiName :: Lens.Lens' TableIdentifier (Core.Maybe Types.Name)
tiName = Lens.field @"name"
{-# INLINEABLE tiName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

instance Core.FromJSON TableIdentifier where
        toJSON TableIdentifier{..}
          = Core.object
              (Core.catMaybes
                 [("CatalogId" Core..=) Core.<$> catalogId,
                  ("DatabaseName" Core..=) Core.<$> databaseName,
                  ("Name" Core..=) Core.<$> name])

instance Core.FromJSON TableIdentifier where
        parseJSON
          = Core.withObject "TableIdentifier" Core.$
              \ x ->
                TableIdentifier' Core.<$>
                  (x Core..:? "CatalogId") Core.<*> x Core..:? "DatabaseName"
                    Core.<*> x Core..:? "Name"
