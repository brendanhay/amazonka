{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.CatalogEntry
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.CatalogEntry
  ( CatalogEntry (..),

    -- * Smart constructor
    mkCatalogEntry,

    -- * Lenses
    ceDatabaseName,
    ceTableName,
  )
where

import qualified Network.AWS.Glue.Types.DatabaseName as Types
import qualified Network.AWS.Glue.Types.TableName as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Specifies a table definition in the AWS Glue Data Catalog.
--
-- /See:/ 'mkCatalogEntry' smart constructor.
data CatalogEntry = CatalogEntry'
  { -- | The database in which the table metadata resides.
    databaseName :: Types.DatabaseName,
    -- | The name of the table in question.
    tableName :: Types.TableName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CatalogEntry' value with any optional fields omitted.
mkCatalogEntry ::
  -- | 'databaseName'
  Types.DatabaseName ->
  -- | 'tableName'
  Types.TableName ->
  CatalogEntry
mkCatalogEntry databaseName tableName =
  CatalogEntry' {databaseName, tableName}

-- | The database in which the table metadata resides.
--
-- /Note:/ Consider using 'databaseName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ceDatabaseName :: Lens.Lens' CatalogEntry Types.DatabaseName
ceDatabaseName = Lens.field @"databaseName"
{-# DEPRECATED ceDatabaseName "Use generic-lens or generic-optics with 'databaseName' instead." #-}

-- | The name of the table in question.
--
-- /Note:/ Consider using 'tableName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ceTableName :: Lens.Lens' CatalogEntry Types.TableName
ceTableName = Lens.field @"tableName"
{-# DEPRECATED ceTableName "Use generic-lens or generic-optics with 'tableName' instead." #-}

instance Core.FromJSON CatalogEntry where
  toJSON CatalogEntry {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("DatabaseName" Core..= databaseName),
            Core.Just ("TableName" Core..= tableName)
          ]
      )
