{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MachineLearning.Types.RedshiftMetadata
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MachineLearning.Types.RedshiftMetadata
  ( RedshiftMetadata (..)
  -- * Smart constructor
  , mkRedshiftMetadata
  -- * Lenses
  , rmDatabaseUserName
  , rmRedshiftDatabase
  , rmSelectSqlQuery
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MachineLearning.Types.RedshiftDatabase as Types
import qualified Network.AWS.MachineLearning.Types.RedshiftDatabaseUsername as Types
import qualified Network.AWS.MachineLearning.Types.RedshiftSelectSqlQuery as Types
import qualified Network.AWS.Prelude as Core

-- | Describes the @DataSource@ details specific to Amazon Redshift.
--
-- /See:/ 'mkRedshiftMetadata' smart constructor.
data RedshiftMetadata = RedshiftMetadata'
  { databaseUserName :: Core.Maybe Types.RedshiftDatabaseUsername
  , redshiftDatabase :: Core.Maybe Types.RedshiftDatabase
  , selectSqlQuery :: Core.Maybe Types.RedshiftSelectSqlQuery
    -- ^ The SQL query that is specified during 'CreateDataSourceFromRedshift' . Returns only if @Verbose@ is true in GetDataSourceInput. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RedshiftMetadata' value with any optional fields omitted.
mkRedshiftMetadata
    :: RedshiftMetadata
mkRedshiftMetadata
  = RedshiftMetadata'{databaseUserName = Core.Nothing,
                      redshiftDatabase = Core.Nothing, selectSqlQuery = Core.Nothing}

-- | Undocumented field.
--
-- /Note:/ Consider using 'databaseUserName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rmDatabaseUserName :: Lens.Lens' RedshiftMetadata (Core.Maybe Types.RedshiftDatabaseUsername)
rmDatabaseUserName = Lens.field @"databaseUserName"
{-# INLINEABLE rmDatabaseUserName #-}
{-# DEPRECATED databaseUserName "Use generic-lens or generic-optics with 'databaseUserName' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'redshiftDatabase' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rmRedshiftDatabase :: Lens.Lens' RedshiftMetadata (Core.Maybe Types.RedshiftDatabase)
rmRedshiftDatabase = Lens.field @"redshiftDatabase"
{-# INLINEABLE rmRedshiftDatabase #-}
{-# DEPRECATED redshiftDatabase "Use generic-lens or generic-optics with 'redshiftDatabase' instead"  #-}

-- | The SQL query that is specified during 'CreateDataSourceFromRedshift' . Returns only if @Verbose@ is true in GetDataSourceInput. 
--
-- /Note:/ Consider using 'selectSqlQuery' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rmSelectSqlQuery :: Lens.Lens' RedshiftMetadata (Core.Maybe Types.RedshiftSelectSqlQuery)
rmSelectSqlQuery = Lens.field @"selectSqlQuery"
{-# INLINEABLE rmSelectSqlQuery #-}
{-# DEPRECATED selectSqlQuery "Use generic-lens or generic-optics with 'selectSqlQuery' instead"  #-}

instance Core.FromJSON RedshiftMetadata where
        parseJSON
          = Core.withObject "RedshiftMetadata" Core.$
              \ x ->
                RedshiftMetadata' Core.<$>
                  (x Core..:? "DatabaseUserName") Core.<*>
                    x Core..:? "RedshiftDatabase"
                    Core.<*> x Core..:? "SelectSqlQuery"
