{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.MachineLearning.Types.RDSMetadata
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MachineLearning.Types.RDSMetadata where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MachineLearning.Types.RDSDatabase
import qualified Amazonka.Prelude as Prelude

-- | The datasource details that are specific to Amazon RDS.
--
-- /See:/ 'newRDSMetadata' smart constructor.
data RDSMetadata = RDSMetadata'
  { -- | The ID of the Data Pipeline instance that is used to carry to copy data
    -- from Amazon RDS to Amazon S3. You can use the ID to find details about
    -- the instance in the Data Pipeline console.
    dataPipelineId :: Prelude.Maybe Prelude.Text,
    -- | The database details required to connect to an Amazon RDS.
    database :: Prelude.Maybe RDSDatabase,
    databaseUserName :: Prelude.Maybe Prelude.Text,
    -- | The role (DataPipelineDefaultResourceRole) assumed by an Amazon EC2
    -- instance to carry out the copy task from Amazon RDS to Amazon S3. For
    -- more information, see
    -- <https://docs.aws.amazon.com/datapipeline/latest/DeveloperGuide/dp-iam-roles.html Role templates>
    -- for data pipelines.
    resourceRole :: Prelude.Maybe Prelude.Text,
    -- | The SQL query that is supplied during CreateDataSourceFromRDS. Returns
    -- only if @Verbose@ is true in @GetDataSourceInput@.
    selectSqlQuery :: Prelude.Maybe Prelude.Text,
    -- | The role (DataPipelineDefaultRole) assumed by the Data Pipeline service
    -- to monitor the progress of the copy task from Amazon RDS to Amazon S3.
    -- For more information, see
    -- <https://docs.aws.amazon.com/datapipeline/latest/DeveloperGuide/dp-iam-roles.html Role templates>
    -- for data pipelines.
    serviceRole :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RDSMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dataPipelineId', 'rDSMetadata_dataPipelineId' - The ID of the Data Pipeline instance that is used to carry to copy data
-- from Amazon RDS to Amazon S3. You can use the ID to find details about
-- the instance in the Data Pipeline console.
--
-- 'database', 'rDSMetadata_database' - The database details required to connect to an Amazon RDS.
--
-- 'databaseUserName', 'rDSMetadata_databaseUserName' - Undocumented member.
--
-- 'resourceRole', 'rDSMetadata_resourceRole' - The role (DataPipelineDefaultResourceRole) assumed by an Amazon EC2
-- instance to carry out the copy task from Amazon RDS to Amazon S3. For
-- more information, see
-- <https://docs.aws.amazon.com/datapipeline/latest/DeveloperGuide/dp-iam-roles.html Role templates>
-- for data pipelines.
--
-- 'selectSqlQuery', 'rDSMetadata_selectSqlQuery' - The SQL query that is supplied during CreateDataSourceFromRDS. Returns
-- only if @Verbose@ is true in @GetDataSourceInput@.
--
-- 'serviceRole', 'rDSMetadata_serviceRole' - The role (DataPipelineDefaultRole) assumed by the Data Pipeline service
-- to monitor the progress of the copy task from Amazon RDS to Amazon S3.
-- For more information, see
-- <https://docs.aws.amazon.com/datapipeline/latest/DeveloperGuide/dp-iam-roles.html Role templates>
-- for data pipelines.
newRDSMetadata ::
  RDSMetadata
newRDSMetadata =
  RDSMetadata'
    { dataPipelineId = Prelude.Nothing,
      database = Prelude.Nothing,
      databaseUserName = Prelude.Nothing,
      resourceRole = Prelude.Nothing,
      selectSqlQuery = Prelude.Nothing,
      serviceRole = Prelude.Nothing
    }

-- | The ID of the Data Pipeline instance that is used to carry to copy data
-- from Amazon RDS to Amazon S3. You can use the ID to find details about
-- the instance in the Data Pipeline console.
rDSMetadata_dataPipelineId :: Lens.Lens' RDSMetadata (Prelude.Maybe Prelude.Text)
rDSMetadata_dataPipelineId = Lens.lens (\RDSMetadata' {dataPipelineId} -> dataPipelineId) (\s@RDSMetadata' {} a -> s {dataPipelineId = a} :: RDSMetadata)

-- | The database details required to connect to an Amazon RDS.
rDSMetadata_database :: Lens.Lens' RDSMetadata (Prelude.Maybe RDSDatabase)
rDSMetadata_database = Lens.lens (\RDSMetadata' {database} -> database) (\s@RDSMetadata' {} a -> s {database = a} :: RDSMetadata)

-- | Undocumented member.
rDSMetadata_databaseUserName :: Lens.Lens' RDSMetadata (Prelude.Maybe Prelude.Text)
rDSMetadata_databaseUserName = Lens.lens (\RDSMetadata' {databaseUserName} -> databaseUserName) (\s@RDSMetadata' {} a -> s {databaseUserName = a} :: RDSMetadata)

-- | The role (DataPipelineDefaultResourceRole) assumed by an Amazon EC2
-- instance to carry out the copy task from Amazon RDS to Amazon S3. For
-- more information, see
-- <https://docs.aws.amazon.com/datapipeline/latest/DeveloperGuide/dp-iam-roles.html Role templates>
-- for data pipelines.
rDSMetadata_resourceRole :: Lens.Lens' RDSMetadata (Prelude.Maybe Prelude.Text)
rDSMetadata_resourceRole = Lens.lens (\RDSMetadata' {resourceRole} -> resourceRole) (\s@RDSMetadata' {} a -> s {resourceRole = a} :: RDSMetadata)

-- | The SQL query that is supplied during CreateDataSourceFromRDS. Returns
-- only if @Verbose@ is true in @GetDataSourceInput@.
rDSMetadata_selectSqlQuery :: Lens.Lens' RDSMetadata (Prelude.Maybe Prelude.Text)
rDSMetadata_selectSqlQuery = Lens.lens (\RDSMetadata' {selectSqlQuery} -> selectSqlQuery) (\s@RDSMetadata' {} a -> s {selectSqlQuery = a} :: RDSMetadata)

-- | The role (DataPipelineDefaultRole) assumed by the Data Pipeline service
-- to monitor the progress of the copy task from Amazon RDS to Amazon S3.
-- For more information, see
-- <https://docs.aws.amazon.com/datapipeline/latest/DeveloperGuide/dp-iam-roles.html Role templates>
-- for data pipelines.
rDSMetadata_serviceRole :: Lens.Lens' RDSMetadata (Prelude.Maybe Prelude.Text)
rDSMetadata_serviceRole = Lens.lens (\RDSMetadata' {serviceRole} -> serviceRole) (\s@RDSMetadata' {} a -> s {serviceRole = a} :: RDSMetadata)

instance Data.FromJSON RDSMetadata where
  parseJSON =
    Data.withObject
      "RDSMetadata"
      ( \x ->
          RDSMetadata'
            Prelude.<$> (x Data..:? "DataPipelineId")
            Prelude.<*> (x Data..:? "Database")
            Prelude.<*> (x Data..:? "DatabaseUserName")
            Prelude.<*> (x Data..:? "ResourceRole")
            Prelude.<*> (x Data..:? "SelectSqlQuery")
            Prelude.<*> (x Data..:? "ServiceRole")
      )

instance Prelude.Hashable RDSMetadata where
  hashWithSalt _salt RDSMetadata' {..} =
    _salt
      `Prelude.hashWithSalt` dataPipelineId
      `Prelude.hashWithSalt` database
      `Prelude.hashWithSalt` databaseUserName
      `Prelude.hashWithSalt` resourceRole
      `Prelude.hashWithSalt` selectSqlQuery
      `Prelude.hashWithSalt` serviceRole

instance Prelude.NFData RDSMetadata where
  rnf RDSMetadata' {..} =
    Prelude.rnf dataPipelineId
      `Prelude.seq` Prelude.rnf database
      `Prelude.seq` Prelude.rnf databaseUserName
      `Prelude.seq` Prelude.rnf resourceRole
      `Prelude.seq` Prelude.rnf selectSqlQuery
      `Prelude.seq` Prelude.rnf serviceRole
