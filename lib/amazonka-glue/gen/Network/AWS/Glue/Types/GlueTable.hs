{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.GlueTable
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Glue.Types.GlueTable
  ( GlueTable (..)
  -- * Smart constructor
  , mkGlueTable
  -- * Lenses
  , gtDatabaseName
  , gtTableName
  , gtCatalogId
  , gtConnectionName
  ) where

import qualified Network.AWS.Glue.Types.NameString as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The database and table in the AWS Glue Data Catalog that is used for input or output data.
--
-- /See:/ 'mkGlueTable' smart constructor.
data GlueTable = GlueTable'
  { databaseName :: Types.NameString
    -- ^ A database name in the AWS Glue Data Catalog.
  , tableName :: Types.NameString
    -- ^ A table name in the AWS Glue Data Catalog.
  , catalogId :: Core.Maybe Types.NameString
    -- ^ A unique identifier for the AWS Glue Data Catalog.
  , connectionName :: Core.Maybe Types.NameString
    -- ^ The name of the connection to the AWS Glue Data Catalog.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GlueTable' value with any optional fields omitted.
mkGlueTable
    :: Types.NameString -- ^ 'databaseName'
    -> Types.NameString -- ^ 'tableName'
    -> GlueTable
mkGlueTable databaseName tableName
  = GlueTable'{databaseName, tableName, catalogId = Core.Nothing,
               connectionName = Core.Nothing}

-- | A database name in the AWS Glue Data Catalog.
--
-- /Note:/ Consider using 'databaseName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtDatabaseName :: Lens.Lens' GlueTable Types.NameString
gtDatabaseName = Lens.field @"databaseName"
{-# INLINEABLE gtDatabaseName #-}
{-# DEPRECATED databaseName "Use generic-lens or generic-optics with 'databaseName' instead"  #-}

-- | A table name in the AWS Glue Data Catalog.
--
-- /Note:/ Consider using 'tableName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtTableName :: Lens.Lens' GlueTable Types.NameString
gtTableName = Lens.field @"tableName"
{-# INLINEABLE gtTableName #-}
{-# DEPRECATED tableName "Use generic-lens or generic-optics with 'tableName' instead"  #-}

-- | A unique identifier for the AWS Glue Data Catalog.
--
-- /Note:/ Consider using 'catalogId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtCatalogId :: Lens.Lens' GlueTable (Core.Maybe Types.NameString)
gtCatalogId = Lens.field @"catalogId"
{-# INLINEABLE gtCatalogId #-}
{-# DEPRECATED catalogId "Use generic-lens or generic-optics with 'catalogId' instead"  #-}

-- | The name of the connection to the AWS Glue Data Catalog.
--
-- /Note:/ Consider using 'connectionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtConnectionName :: Lens.Lens' GlueTable (Core.Maybe Types.NameString)
gtConnectionName = Lens.field @"connectionName"
{-# INLINEABLE gtConnectionName #-}
{-# DEPRECATED connectionName "Use generic-lens or generic-optics with 'connectionName' instead"  #-}

instance Core.FromJSON GlueTable where
        toJSON GlueTable{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("DatabaseName" Core..= databaseName),
                  Core.Just ("TableName" Core..= tableName),
                  ("CatalogId" Core..=) Core.<$> catalogId,
                  ("ConnectionName" Core..=) Core.<$> connectionName])

instance Core.FromJSON GlueTable where
        parseJSON
          = Core.withObject "GlueTable" Core.$
              \ x ->
                GlueTable' Core.<$>
                  (x Core..: "DatabaseName") Core.<*> x Core..: "TableName" Core.<*>
                    x Core..:? "CatalogId"
                    Core.<*> x Core..:? "ConnectionName"
