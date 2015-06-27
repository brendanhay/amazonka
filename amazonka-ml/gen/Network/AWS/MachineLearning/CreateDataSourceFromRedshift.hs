{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.MachineLearning.CreateDataSourceFromRedshift
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Creates a @DataSource@ from
-- <http://aws.amazon.com/redshift/ Amazon Redshift>. A @DataSource@
-- references data that can be used to perform either CreateMLModel,
-- CreateEvaluation or CreateBatchPrediction operations.
--
-- @CreateDataSourceFromRedshift@ is an asynchronous operation. In response
-- to @CreateDataSourceFromRedshift@, Amazon Machine Learning (Amazon ML)
-- immediately returns and sets the @DataSource@ status to @PENDING@. After
-- the @DataSource@ is created and ready for use, Amazon ML sets the
-- @Status@ parameter to @COMPLETED@. @DataSource@ in @COMPLETED@ or
-- @PENDING@ status can only be used to perform CreateMLModel,
-- CreateEvaluation, or CreateBatchPrediction operations.
--
-- If Amazon ML cannot accept the input source, it sets the @Status@
-- parameter to @FAILED@ and includes an error message in the @Message@
-- attribute of the GetDataSource operation response.
--
-- The observations should exist in the database hosted on an Amazon
-- Redshift cluster and should be specified by a @SelectSqlQuery@. Amazon
-- ML executes
-- <http://docs.aws.amazon.com/redshift/latest/dg/t_Unloading_tables.html Unload>
-- command in Amazon Redshift to transfer the result set of
-- @SelectSqlQuery@ to @S3StagingLocation.@
--
-- After the @DataSource@ is created, it\'s ready for use in evaluations
-- and batch predictions. If you plan to use the @DataSource@ to train an
-- @MLModel@, the @DataSource@ requires another item -- a recipe. A recipe
-- describes the observation variables that participate in training an
-- @MLModel@. A recipe describes how each input variable will be used in
-- training. Will the variable be included or excluded from training? Will
-- the variable be manipulated, for example, combined with another variable
-- or split apart into word combinations? The recipe provides answers to
-- these questions. For more information, see the Amazon Machine Learning
-- Developer Guide.
--
-- <http://http://docs.aws.amazon.com/machine-learning/latest/APIReference/API_CreateDataSourceFromRedshift.html>
module Network.AWS.MachineLearning.CreateDataSourceFromRedshift
    (
    -- * Request
      CreateDataSourceFromRedshift
    -- ** Request constructor
    , createDataSourceFromRedshift
    -- ** Request lenses
    , cdsfrDataSourceName
    , cdsfrComputeStatistics
    , cdsfrDataSourceId
    , cdsfrDataSpec
    , cdsfrRoleARN

    -- * Response
    , CreateDataSourceFromRedshiftResponse
    -- ** Response constructor
    , createDataSourceFromRedshiftResponse
    -- ** Response lenses
    , cdsfrrDataSourceId
    , cdsfrrStatus
    ) where

import           Network.AWS.MachineLearning.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'createDataSourceFromRedshift' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cdsfrDataSourceName'
--
-- * 'cdsfrComputeStatistics'
--
-- * 'cdsfrDataSourceId'
--
-- * 'cdsfrDataSpec'
--
-- * 'cdsfrRoleARN'
data CreateDataSourceFromRedshift = CreateDataSourceFromRedshift'
    { _cdsfrDataSourceName    :: Maybe Text
    , _cdsfrComputeStatistics :: Maybe Bool
    , _cdsfrDataSourceId      :: Text
    , _cdsfrDataSpec          :: RedshiftDataSpec
    , _cdsfrRoleARN           :: Text
    } deriving (Eq,Read,Show)

-- | 'CreateDataSourceFromRedshift' smart constructor.
createDataSourceFromRedshift :: Text -> RedshiftDataSpec -> Text -> CreateDataSourceFromRedshift
createDataSourceFromRedshift pDataSourceId pDataSpec pRoleARN =
    CreateDataSourceFromRedshift'
    { _cdsfrDataSourceName = Nothing
    , _cdsfrComputeStatistics = Nothing
    , _cdsfrDataSourceId = pDataSourceId
    , _cdsfrDataSpec = pDataSpec
    , _cdsfrRoleARN = pRoleARN
    }

-- | A user-supplied name or description of the @DataSource@.
cdsfrDataSourceName :: Lens' CreateDataSourceFromRedshift (Maybe Text)
cdsfrDataSourceName = lens _cdsfrDataSourceName (\ s a -> s{_cdsfrDataSourceName = a});

-- | The compute statistics for a @DataSource@. The statistics are generated
-- from the observation data referenced by a @DataSource@. Amazon ML uses
-- the statistics internally during @MLModel@ training. This parameter must
-- be set to @true@ if the @@DataSource@@ needs to be used for @MLModel@
-- training
cdsfrComputeStatistics :: Lens' CreateDataSourceFromRedshift (Maybe Bool)
cdsfrComputeStatistics = lens _cdsfrComputeStatistics (\ s a -> s{_cdsfrComputeStatistics = a});

-- | A user-supplied ID that uniquely identifies the @DataSource@.
cdsfrDataSourceId :: Lens' CreateDataSourceFromRedshift Text
cdsfrDataSourceId = lens _cdsfrDataSourceId (\ s a -> s{_cdsfrDataSourceId = a});

-- | The data specification of an Amazon Redshift @DataSource@:
--
-- -   DatabaseInformation -
--
--     -   @DatabaseName @ - Name of the Amazon Redshift database.
--     -   @ ClusterIdentifier @ - Unique ID for the Amazon Redshift
--         cluster.
-- -   DatabaseCredentials - AWS Identity abd Access Management (IAM)
--     credentials that are used to connect to the Amazon Redshift
--     database.
--
-- -   SelectSqlQuery - Query that is used to retrieve the observation data
--     for the @Datasource@.
--
-- -   S3StagingLocation - Amazon Simple Storage Service (Amazon S3)
--     location for staging Amazon Redshift data. The data retrieved from
--     Amazon Relational Database Service (Amazon RDS) using
--     @SelectSqlQuery@ is stored in this location.
--
-- -   DataSchemaUri - Amazon S3 location of the @DataSchema@.
--
-- -   DataSchema - A JSON string representing the schema. This is not
--     required if @DataSchemaUri@ is specified.
--
-- -   DataRearrangement - A JSON string representing the splitting
--     requirement of a @Datasource@.
--
--     Sample -
--     @ \"{\\\"randomSeed\\\":\\\"some-random-seed\\\", \\\"splitting\\\":{\\\"percentBegin\\\":10,\\\"percentEnd\\\":60}}\"@
--
cdsfrDataSpec :: Lens' CreateDataSourceFromRedshift RedshiftDataSpec
cdsfrDataSpec = lens _cdsfrDataSpec (\ s a -> s{_cdsfrDataSpec = a});

-- | A fully specified role Amazon Resource Name (ARN). Amazon ML assumes the
-- role on behalf of the user to create the following:
--
-- -   A security group to allow Amazon ML to execute the @SelectSqlQuery@
--     query on an Amazon Redshift cluster
--
-- -   An Amazon S3 bucket policy to grant Amazon ML read\/write
--     permissions on the @S3StagingLocation@
--
cdsfrRoleARN :: Lens' CreateDataSourceFromRedshift Text
cdsfrRoleARN = lens _cdsfrRoleARN (\ s a -> s{_cdsfrRoleARN = a});

instance AWSRequest CreateDataSourceFromRedshift
         where
        type Sv CreateDataSourceFromRedshift =
             MachineLearning
        type Rs CreateDataSourceFromRedshift =
             CreateDataSourceFromRedshiftResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 CreateDataSourceFromRedshiftResponse' <$>
                   (x .?> "DataSourceId") <*> (pure (fromEnum s)))

instance ToHeaders CreateDataSourceFromRedshift where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonML_20141212.CreateDataSourceFromRedshift" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateDataSourceFromRedshift where
        toJSON CreateDataSourceFromRedshift'{..}
          = object
              ["DataSourceName" .= _cdsfrDataSourceName,
               "ComputeStatistics" .= _cdsfrComputeStatistics,
               "DataSourceId" .= _cdsfrDataSourceId,
               "DataSpec" .= _cdsfrDataSpec,
               "RoleARN" .= _cdsfrRoleARN]

instance ToPath CreateDataSourceFromRedshift where
        toPath = const "/"

instance ToQuery CreateDataSourceFromRedshift where
        toQuery = const mempty

-- | Represents the output of a CreateDataSourceFromRedshift operation, and
-- is an acknowledgement that Amazon ML received the request.
--
-- The CreateDataSourceFromRedshift operation is asynchronous. You can poll
-- for updates by using the GetBatchPrediction operation and checking the
-- @Status@ parameter.
--
-- /See:/ 'createDataSourceFromRedshiftResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cdsfrrDataSourceId'
--
-- * 'cdsfrrStatus'
data CreateDataSourceFromRedshiftResponse = CreateDataSourceFromRedshiftResponse'
    { _cdsfrrDataSourceId :: Maybe Text
    , _cdsfrrStatus       :: !Int
    } deriving (Eq,Read,Show)

-- | 'CreateDataSourceFromRedshiftResponse' smart constructor.
createDataSourceFromRedshiftResponse :: Int -> CreateDataSourceFromRedshiftResponse
createDataSourceFromRedshiftResponse pStatus =
    CreateDataSourceFromRedshiftResponse'
    { _cdsfrrDataSourceId = Nothing
    , _cdsfrrStatus = pStatus
    }

-- | A user-supplied ID that uniquely identifies the datasource. This value
-- should be identical to the value of the @DataSourceID@ in the request.
cdsfrrDataSourceId :: Lens' CreateDataSourceFromRedshiftResponse (Maybe Text)
cdsfrrDataSourceId = lens _cdsfrrDataSourceId (\ s a -> s{_cdsfrrDataSourceId = a});

-- | FIXME: Undocumented member.
cdsfrrStatus :: Lens' CreateDataSourceFromRedshiftResponse Int
cdsfrrStatus = lens _cdsfrrStatus (\ s a -> s{_cdsfrrStatus = a});
