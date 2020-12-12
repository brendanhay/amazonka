{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MachineLearning.Types.RDSMetadata
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MachineLearning.Types.RDSMetadata
  ( RDSMetadata (..),

    -- * Smart constructor
    mkRDSMetadata,

    -- * Lenses
    rmSelectSqlQuery,
    rmDataPipelineId,
    rmDatabase,
    rmDatabaseUserName,
    rmResourceRole,
    rmServiceRole,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MachineLearning.Types.RDSDatabase
import qualified Network.AWS.Prelude as Lude

-- | The datasource details that are specific to Amazon RDS.
--
-- /See:/ 'mkRDSMetadata' smart constructor.
data RDSMetadata = RDSMetadata'
  { selectSqlQuery ::
      Lude.Maybe Lude.Text,
    dataPipelineId :: Lude.Maybe Lude.Text,
    database :: Lude.Maybe RDSDatabase,
    databaseUserName :: Lude.Maybe Lude.Text,
    resourceRole :: Lude.Maybe Lude.Text,
    serviceRole :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RDSMetadata' with the minimum fields required to make a request.
--
-- * 'dataPipelineId' - The ID of the Data Pipeline instance that is used to carry to copy data from Amazon RDS to Amazon S3. You can use the ID to find details about the instance in the Data Pipeline console.
-- * 'database' - The database details required to connect to an Amazon RDS.
-- * 'databaseUserName' - Undocumented field.
-- * 'resourceRole' - The role (DataPipelineDefaultResourceRole) assumed by an Amazon EC2 instance to carry out the copy task from Amazon RDS to Amazon S3. For more information, see <http://docs.aws.amazon.com/datapipeline/latest/DeveloperGuide/dp-iam-roles.html Role templates> for data pipelines.
-- * 'selectSqlQuery' - The SQL query that is supplied during 'CreateDataSourceFromRDS' . Returns only if @Verbose@ is true in @GetDataSourceInput@ .
-- * 'serviceRole' - The role (DataPipelineDefaultRole) assumed by the Data Pipeline service to monitor the progress of the copy task from Amazon RDS to Amazon S3. For more information, see <http://docs.aws.amazon.com/datapipeline/latest/DeveloperGuide/dp-iam-roles.html Role templates> for data pipelines.
mkRDSMetadata ::
  RDSMetadata
mkRDSMetadata =
  RDSMetadata'
    { selectSqlQuery = Lude.Nothing,
      dataPipelineId = Lude.Nothing,
      database = Lude.Nothing,
      databaseUserName = Lude.Nothing,
      resourceRole = Lude.Nothing,
      serviceRole = Lude.Nothing
    }

-- | The SQL query that is supplied during 'CreateDataSourceFromRDS' . Returns only if @Verbose@ is true in @GetDataSourceInput@ .
--
-- /Note:/ Consider using 'selectSqlQuery' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rmSelectSqlQuery :: Lens.Lens' RDSMetadata (Lude.Maybe Lude.Text)
rmSelectSqlQuery = Lens.lens (selectSqlQuery :: RDSMetadata -> Lude.Maybe Lude.Text) (\s a -> s {selectSqlQuery = a} :: RDSMetadata)
{-# DEPRECATED rmSelectSqlQuery "Use generic-lens or generic-optics with 'selectSqlQuery' instead." #-}

-- | The ID of the Data Pipeline instance that is used to carry to copy data from Amazon RDS to Amazon S3. You can use the ID to find details about the instance in the Data Pipeline console.
--
-- /Note:/ Consider using 'dataPipelineId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rmDataPipelineId :: Lens.Lens' RDSMetadata (Lude.Maybe Lude.Text)
rmDataPipelineId = Lens.lens (dataPipelineId :: RDSMetadata -> Lude.Maybe Lude.Text) (\s a -> s {dataPipelineId = a} :: RDSMetadata)
{-# DEPRECATED rmDataPipelineId "Use generic-lens or generic-optics with 'dataPipelineId' instead." #-}

-- | The database details required to connect to an Amazon RDS.
--
-- /Note:/ Consider using 'database' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rmDatabase :: Lens.Lens' RDSMetadata (Lude.Maybe RDSDatabase)
rmDatabase = Lens.lens (database :: RDSMetadata -> Lude.Maybe RDSDatabase) (\s a -> s {database = a} :: RDSMetadata)
{-# DEPRECATED rmDatabase "Use generic-lens or generic-optics with 'database' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'databaseUserName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rmDatabaseUserName :: Lens.Lens' RDSMetadata (Lude.Maybe Lude.Text)
rmDatabaseUserName = Lens.lens (databaseUserName :: RDSMetadata -> Lude.Maybe Lude.Text) (\s a -> s {databaseUserName = a} :: RDSMetadata)
{-# DEPRECATED rmDatabaseUserName "Use generic-lens or generic-optics with 'databaseUserName' instead." #-}

-- | The role (DataPipelineDefaultResourceRole) assumed by an Amazon EC2 instance to carry out the copy task from Amazon RDS to Amazon S3. For more information, see <http://docs.aws.amazon.com/datapipeline/latest/DeveloperGuide/dp-iam-roles.html Role templates> for data pipelines.
--
-- /Note:/ Consider using 'resourceRole' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rmResourceRole :: Lens.Lens' RDSMetadata (Lude.Maybe Lude.Text)
rmResourceRole = Lens.lens (resourceRole :: RDSMetadata -> Lude.Maybe Lude.Text) (\s a -> s {resourceRole = a} :: RDSMetadata)
{-# DEPRECATED rmResourceRole "Use generic-lens or generic-optics with 'resourceRole' instead." #-}

-- | The role (DataPipelineDefaultRole) assumed by the Data Pipeline service to monitor the progress of the copy task from Amazon RDS to Amazon S3. For more information, see <http://docs.aws.amazon.com/datapipeline/latest/DeveloperGuide/dp-iam-roles.html Role templates> for data pipelines.
--
-- /Note:/ Consider using 'serviceRole' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rmServiceRole :: Lens.Lens' RDSMetadata (Lude.Maybe Lude.Text)
rmServiceRole = Lens.lens (serviceRole :: RDSMetadata -> Lude.Maybe Lude.Text) (\s a -> s {serviceRole = a} :: RDSMetadata)
{-# DEPRECATED rmServiceRole "Use generic-lens or generic-optics with 'serviceRole' instead." #-}

instance Lude.FromJSON RDSMetadata where
  parseJSON =
    Lude.withObject
      "RDSMetadata"
      ( \x ->
          RDSMetadata'
            Lude.<$> (x Lude..:? "SelectSqlQuery")
            Lude.<*> (x Lude..:? "DataPipelineId")
            Lude.<*> (x Lude..:? "Database")
            Lude.<*> (x Lude..:? "DatabaseUserName")
            Lude.<*> (x Lude..:? "ResourceRole")
            Lude.<*> (x Lude..:? "ServiceRole")
      )
