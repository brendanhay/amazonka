{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.DatabaseIdentifier
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.DatabaseIdentifier
  ( DatabaseIdentifier (..),

    -- * Smart constructor
    mkDatabaseIdentifier,

    -- * Lenses
    diCatalogId,
    diDatabaseName,
  )
where

import qualified Network.AWS.Glue.Types.CatalogIdString as Types
import qualified Network.AWS.Glue.Types.NameString as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A structure that describes a target database for resource linking.
--
-- /See:/ 'mkDatabaseIdentifier' smart constructor.
data DatabaseIdentifier = DatabaseIdentifier'
  { -- | The ID of the Data Catalog in which the database resides.
    catalogId :: Core.Maybe Types.CatalogIdString,
    -- | The name of the catalog database.
    databaseName :: Core.Maybe Types.NameString
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DatabaseIdentifier' value with any optional fields omitted.
mkDatabaseIdentifier ::
  DatabaseIdentifier
mkDatabaseIdentifier =
  DatabaseIdentifier'
    { catalogId = Core.Nothing,
      databaseName = Core.Nothing
    }

-- | The ID of the Data Catalog in which the database resides.
--
-- /Note:/ Consider using 'catalogId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diCatalogId :: Lens.Lens' DatabaseIdentifier (Core.Maybe Types.CatalogIdString)
diCatalogId = Lens.field @"catalogId"
{-# DEPRECATED diCatalogId "Use generic-lens or generic-optics with 'catalogId' instead." #-}

-- | The name of the catalog database.
--
-- /Note:/ Consider using 'databaseName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diDatabaseName :: Lens.Lens' DatabaseIdentifier (Core.Maybe Types.NameString)
diDatabaseName = Lens.field @"databaseName"
{-# DEPRECATED diDatabaseName "Use generic-lens or generic-optics with 'databaseName' instead." #-}

instance Core.FromJSON DatabaseIdentifier where
  toJSON DatabaseIdentifier {..} =
    Core.object
      ( Core.catMaybes
          [ ("CatalogId" Core..=) Core.<$> catalogId,
            ("DatabaseName" Core..=) Core.<$> databaseName
          ]
      )

instance Core.FromJSON DatabaseIdentifier where
  parseJSON =
    Core.withObject "DatabaseIdentifier" Core.$
      \x ->
        DatabaseIdentifier'
          Core.<$> (x Core..:? "CatalogId") Core.<*> (x Core..:? "DatabaseName")
