{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MachineLearning.Types.RDSMetadata
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MachineLearning.Types.RDSMetadata where

import Network.AWS.Lens
import Network.AWS.MachineLearning.Types.RDSDatabase
import Network.AWS.Prelude

-- | The datasource details that are specific to Amazon RDS.
--
--
--
-- /See:/ 'rdsMetadata' smart constructor.
data RDSMetadata = RDSMetadata'
  { _rmSelectSqlQuery :: !(Maybe Text),
    _rmDataPipelineId :: !(Maybe Text),
    _rmDatabase :: !(Maybe RDSDatabase),
    _rmDatabaseUserName :: !(Maybe Text),
    _rmResourceRole :: !(Maybe Text),
    _rmServiceRole :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'RDSMetadata' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rmSelectSqlQuery' - The SQL query that is supplied during 'CreateDataSourceFromRDS' . Returns only if @Verbose@ is true in @GetDataSourceInput@ .
--
-- * 'rmDataPipelineId' - The ID of the Data Pipeline instance that is used to carry to copy data from Amazon RDS to Amazon S3. You can use the ID to find details about the instance in the Data Pipeline console.
--
-- * 'rmDatabase' - The database details required to connect to an Amazon RDS.
--
-- * 'rmDatabaseUserName' - Undocumented member.
--
-- * 'rmResourceRole' - The role (DataPipelineDefaultResourceRole) assumed by an Amazon EC2 instance to carry out the copy task from Amazon RDS to Amazon S3. For more information, see <http://docs.aws.amazon.com/datapipeline/latest/DeveloperGuide/dp-iam-roles.html Role templates> for data pipelines.
--
-- * 'rmServiceRole' - The role (DataPipelineDefaultRole) assumed by the Data Pipeline service to monitor the progress of the copy task from Amazon RDS to Amazon S3. For more information, see <http://docs.aws.amazon.com/datapipeline/latest/DeveloperGuide/dp-iam-roles.html Role templates> for data pipelines.
rdsMetadata ::
  RDSMetadata
rdsMetadata =
  RDSMetadata'
    { _rmSelectSqlQuery = Nothing,
      _rmDataPipelineId = Nothing,
      _rmDatabase = Nothing,
      _rmDatabaseUserName = Nothing,
      _rmResourceRole = Nothing,
      _rmServiceRole = Nothing
    }

-- | The SQL query that is supplied during 'CreateDataSourceFromRDS' . Returns only if @Verbose@ is true in @GetDataSourceInput@ .
rmSelectSqlQuery :: Lens' RDSMetadata (Maybe Text)
rmSelectSqlQuery = lens _rmSelectSqlQuery (\s a -> s {_rmSelectSqlQuery = a})

-- | The ID of the Data Pipeline instance that is used to carry to copy data from Amazon RDS to Amazon S3. You can use the ID to find details about the instance in the Data Pipeline console.
rmDataPipelineId :: Lens' RDSMetadata (Maybe Text)
rmDataPipelineId = lens _rmDataPipelineId (\s a -> s {_rmDataPipelineId = a})

-- | The database details required to connect to an Amazon RDS.
rmDatabase :: Lens' RDSMetadata (Maybe RDSDatabase)
rmDatabase = lens _rmDatabase (\s a -> s {_rmDatabase = a})

-- | Undocumented member.
rmDatabaseUserName :: Lens' RDSMetadata (Maybe Text)
rmDatabaseUserName = lens _rmDatabaseUserName (\s a -> s {_rmDatabaseUserName = a})

-- | The role (DataPipelineDefaultResourceRole) assumed by an Amazon EC2 instance to carry out the copy task from Amazon RDS to Amazon S3. For more information, see <http://docs.aws.amazon.com/datapipeline/latest/DeveloperGuide/dp-iam-roles.html Role templates> for data pipelines.
rmResourceRole :: Lens' RDSMetadata (Maybe Text)
rmResourceRole = lens _rmResourceRole (\s a -> s {_rmResourceRole = a})

-- | The role (DataPipelineDefaultRole) assumed by the Data Pipeline service to monitor the progress of the copy task from Amazon RDS to Amazon S3. For more information, see <http://docs.aws.amazon.com/datapipeline/latest/DeveloperGuide/dp-iam-roles.html Role templates> for data pipelines.
rmServiceRole :: Lens' RDSMetadata (Maybe Text)
rmServiceRole = lens _rmServiceRole (\s a -> s {_rmServiceRole = a})

instance FromJSON RDSMetadata where
  parseJSON =
    withObject
      "RDSMetadata"
      ( \x ->
          RDSMetadata'
            <$> (x .:? "SelectSqlQuery")
            <*> (x .:? "DataPipelineId")
            <*> (x .:? "Database")
            <*> (x .:? "DatabaseUserName")
            <*> (x .:? "ResourceRole")
            <*> (x .:? "ServiceRole")
      )

instance Hashable RDSMetadata

instance NFData RDSMetadata
