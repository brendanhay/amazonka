{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.Types.GlueConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IoTAnalytics.Types.GlueConfiguration
  ( GlueConfiguration (..)
  -- * Smart constructor
  , mkGlueConfiguration
  -- * Lenses
  , gcTableName
  , gcDatabaseName
  ) where

import qualified Network.AWS.IoTAnalytics.Types.DatabaseName as Types
import qualified Network.AWS.IoTAnalytics.Types.GlueTableName as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Configuration information for coordination with AWS Glue, a fully managed extract, transform and load (ETL) service.
--
-- /See:/ 'mkGlueConfiguration' smart constructor.
data GlueConfiguration = GlueConfiguration'
  { tableName :: Types.GlueTableName
    -- ^ The name of the table in your AWS Glue Data Catalog that is used to perform the ETL operations. An AWS Glue Data Catalog table contains partitioned data and descriptions of data sources and targets.
  , databaseName :: Types.DatabaseName
    -- ^ The name of the database in your AWS Glue Data Catalog in which the table is located. An AWS Glue Data Catalog database contains metadata tables.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GlueConfiguration' value with any optional fields omitted.
mkGlueConfiguration
    :: Types.GlueTableName -- ^ 'tableName'
    -> Types.DatabaseName -- ^ 'databaseName'
    -> GlueConfiguration
mkGlueConfiguration tableName databaseName
  = GlueConfiguration'{tableName, databaseName}

-- | The name of the table in your AWS Glue Data Catalog that is used to perform the ETL operations. An AWS Glue Data Catalog table contains partitioned data and descriptions of data sources and targets.
--
-- /Note:/ Consider using 'tableName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcTableName :: Lens.Lens' GlueConfiguration Types.GlueTableName
gcTableName = Lens.field @"tableName"
{-# INLINEABLE gcTableName #-}
{-# DEPRECATED tableName "Use generic-lens or generic-optics with 'tableName' instead"  #-}

-- | The name of the database in your AWS Glue Data Catalog in which the table is located. An AWS Glue Data Catalog database contains metadata tables.
--
-- /Note:/ Consider using 'databaseName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcDatabaseName :: Lens.Lens' GlueConfiguration Types.DatabaseName
gcDatabaseName = Lens.field @"databaseName"
{-# INLINEABLE gcDatabaseName #-}
{-# DEPRECATED databaseName "Use generic-lens or generic-optics with 'databaseName' instead"  #-}

instance Core.FromJSON GlueConfiguration where
        toJSON GlueConfiguration{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("tableName" Core..= tableName),
                  Core.Just ("databaseName" Core..= databaseName)])

instance Core.FromJSON GlueConfiguration where
        parseJSON
          = Core.withObject "GlueConfiguration" Core.$
              \ x ->
                GlueConfiguration' Core.<$>
                  (x Core..: "tableName") Core.<*> x Core..: "databaseName"
