{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MachineLearning.Types.RDSMetadata
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MachineLearning.Types.RDSMetadata
  ( RDSMetadata (..)
  -- * Smart constructor
  , mkRDSMetadata
  -- * Lenses
  , rdsmDataPipelineId
  , rdsmDatabase
  , rdsmDatabaseUserName
  , rdsmResourceRole
  , rdsmSelectSqlQuery
  , rdsmServiceRole
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MachineLearning.Types.DataPipelineId as Types
import qualified Network.AWS.MachineLearning.Types.DatabaseUserName as Types
import qualified Network.AWS.MachineLearning.Types.EDPResourceRole as Types
import qualified Network.AWS.MachineLearning.Types.EDPServiceRole as Types
import qualified Network.AWS.MachineLearning.Types.RDSDatabase as Types
import qualified Network.AWS.MachineLearning.Types.SelectSqlQuery as Types
import qualified Network.AWS.Prelude as Core

-- | The datasource details that are specific to Amazon RDS.
--
-- /See:/ 'mkRDSMetadata' smart constructor.
data RDSMetadata = RDSMetadata'
  { dataPipelineId :: Core.Maybe Types.DataPipelineId
    -- ^ The ID of the Data Pipeline instance that is used to carry to copy data from Amazon RDS to Amazon S3. You can use the ID to find details about the instance in the Data Pipeline console.
  , database :: Core.Maybe Types.RDSDatabase
    -- ^ The database details required to connect to an Amazon RDS.
  , databaseUserName :: Core.Maybe Types.DatabaseUserName
  , resourceRole :: Core.Maybe Types.EDPResourceRole
    -- ^ The role (DataPipelineDefaultResourceRole) assumed by an Amazon EC2 instance to carry out the copy task from Amazon RDS to Amazon S3. For more information, see <http://docs.aws.amazon.com/datapipeline/latest/DeveloperGuide/dp-iam-roles.html Role templates> for data pipelines.
  , selectSqlQuery :: Core.Maybe Types.SelectSqlQuery
    -- ^ The SQL query that is supplied during 'CreateDataSourceFromRDS' . Returns only if @Verbose@ is true in @GetDataSourceInput@ . 
  , serviceRole :: Core.Maybe Types.EDPServiceRole
    -- ^ The role (DataPipelineDefaultRole) assumed by the Data Pipeline service to monitor the progress of the copy task from Amazon RDS to Amazon S3. For more information, see <http://docs.aws.amazon.com/datapipeline/latest/DeveloperGuide/dp-iam-roles.html Role templates> for data pipelines.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RDSMetadata' value with any optional fields omitted.
mkRDSMetadata
    :: RDSMetadata
mkRDSMetadata
  = RDSMetadata'{dataPipelineId = Core.Nothing,
                 database = Core.Nothing, databaseUserName = Core.Nothing,
                 resourceRole = Core.Nothing, selectSqlQuery = Core.Nothing,
                 serviceRole = Core.Nothing}

-- | The ID of the Data Pipeline instance that is used to carry to copy data from Amazon RDS to Amazon S3. You can use the ID to find details about the instance in the Data Pipeline console.
--
-- /Note:/ Consider using 'dataPipelineId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdsmDataPipelineId :: Lens.Lens' RDSMetadata (Core.Maybe Types.DataPipelineId)
rdsmDataPipelineId = Lens.field @"dataPipelineId"
{-# INLINEABLE rdsmDataPipelineId #-}
{-# DEPRECATED dataPipelineId "Use generic-lens or generic-optics with 'dataPipelineId' instead"  #-}

-- | The database details required to connect to an Amazon RDS.
--
-- /Note:/ Consider using 'database' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdsmDatabase :: Lens.Lens' RDSMetadata (Core.Maybe Types.RDSDatabase)
rdsmDatabase = Lens.field @"database"
{-# INLINEABLE rdsmDatabase #-}
{-# DEPRECATED database "Use generic-lens or generic-optics with 'database' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'databaseUserName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdsmDatabaseUserName :: Lens.Lens' RDSMetadata (Core.Maybe Types.DatabaseUserName)
rdsmDatabaseUserName = Lens.field @"databaseUserName"
{-# INLINEABLE rdsmDatabaseUserName #-}
{-# DEPRECATED databaseUserName "Use generic-lens or generic-optics with 'databaseUserName' instead"  #-}

-- | The role (DataPipelineDefaultResourceRole) assumed by an Amazon EC2 instance to carry out the copy task from Amazon RDS to Amazon S3. For more information, see <http://docs.aws.amazon.com/datapipeline/latest/DeveloperGuide/dp-iam-roles.html Role templates> for data pipelines.
--
-- /Note:/ Consider using 'resourceRole' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdsmResourceRole :: Lens.Lens' RDSMetadata (Core.Maybe Types.EDPResourceRole)
rdsmResourceRole = Lens.field @"resourceRole"
{-# INLINEABLE rdsmResourceRole #-}
{-# DEPRECATED resourceRole "Use generic-lens or generic-optics with 'resourceRole' instead"  #-}

-- | The SQL query that is supplied during 'CreateDataSourceFromRDS' . Returns only if @Verbose@ is true in @GetDataSourceInput@ . 
--
-- /Note:/ Consider using 'selectSqlQuery' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdsmSelectSqlQuery :: Lens.Lens' RDSMetadata (Core.Maybe Types.SelectSqlQuery)
rdsmSelectSqlQuery = Lens.field @"selectSqlQuery"
{-# INLINEABLE rdsmSelectSqlQuery #-}
{-# DEPRECATED selectSqlQuery "Use generic-lens or generic-optics with 'selectSqlQuery' instead"  #-}

-- | The role (DataPipelineDefaultRole) assumed by the Data Pipeline service to monitor the progress of the copy task from Amazon RDS to Amazon S3. For more information, see <http://docs.aws.amazon.com/datapipeline/latest/DeveloperGuide/dp-iam-roles.html Role templates> for data pipelines.
--
-- /Note:/ Consider using 'serviceRole' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdsmServiceRole :: Lens.Lens' RDSMetadata (Core.Maybe Types.EDPServiceRole)
rdsmServiceRole = Lens.field @"serviceRole"
{-# INLINEABLE rdsmServiceRole #-}
{-# DEPRECATED serviceRole "Use generic-lens or generic-optics with 'serviceRole' instead"  #-}

instance Core.FromJSON RDSMetadata where
        parseJSON
          = Core.withObject "RDSMetadata" Core.$
              \ x ->
                RDSMetadata' Core.<$>
                  (x Core..:? "DataPipelineId") Core.<*> x Core..:? "Database"
                    Core.<*> x Core..:? "DatabaseUserName"
                    Core.<*> x Core..:? "ResourceRole"
                    Core.<*> x Core..:? "SelectSqlQuery"
                    Core.<*> x Core..:? "ServiceRole"
