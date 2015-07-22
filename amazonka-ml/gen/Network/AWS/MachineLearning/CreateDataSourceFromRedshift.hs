{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MachineLearning.CreateDataSourceFromRedshift
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Creates a @DataSource@ from
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
    , cdsfrrqDataSourceName
    , cdsfrrqComputeStatistics
    , cdsfrrqDataSourceId
    , cdsfrrqDataSpec
    , cdsfrrqRoleARN

    -- * Response
    , CreateDataSourceFromRedshiftResponse
    -- ** Response constructor
    , createDataSourceFromRedshiftResponse
    -- ** Response lenses
    , cdsfrrsDataSourceId
    , cdsfrrsStatus
    ) where

import           Network.AWS.MachineLearning.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'createDataSourceFromRedshift' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cdsfrrqDataSourceName'
--
-- * 'cdsfrrqComputeStatistics'
--
-- * 'cdsfrrqDataSourceId'
--
-- * 'cdsfrrqDataSpec'
--
-- * 'cdsfrrqRoleARN'
data CreateDataSourceFromRedshift = CreateDataSourceFromRedshift'
    { _cdsfrrqDataSourceName    :: !(Maybe Text)
    , _cdsfrrqComputeStatistics :: !(Maybe Bool)
    , _cdsfrrqDataSourceId      :: !Text
    , _cdsfrrqDataSpec          :: !RedshiftDataSpec
    , _cdsfrrqRoleARN           :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateDataSourceFromRedshift' smart constructor.
createDataSourceFromRedshift :: Text -> RedshiftDataSpec -> Text -> CreateDataSourceFromRedshift
createDataSourceFromRedshift pDataSourceId pDataSpec pRoleARN =
    CreateDataSourceFromRedshift'
    { _cdsfrrqDataSourceName = Nothing
    , _cdsfrrqComputeStatistics = Nothing
    , _cdsfrrqDataSourceId = pDataSourceId
    , _cdsfrrqDataSpec = pDataSpec
    , _cdsfrrqRoleARN = pRoleARN
    }

-- | A user-supplied name or description of the @DataSource@.
cdsfrrqDataSourceName :: Lens' CreateDataSourceFromRedshift (Maybe Text)
cdsfrrqDataSourceName = lens _cdsfrrqDataSourceName (\ s a -> s{_cdsfrrqDataSourceName = a});

-- | The compute statistics for a @DataSource@. The statistics are generated
-- from the observation data referenced by a @DataSource@. Amazon ML uses
-- the statistics internally during @MLModel@ training. This parameter must
-- be set to @true@ if the @@DataSource@@ needs to be used for @MLModel@
-- training
cdsfrrqComputeStatistics :: Lens' CreateDataSourceFromRedshift (Maybe Bool)
cdsfrrqComputeStatistics = lens _cdsfrrqComputeStatistics (\ s a -> s{_cdsfrrqComputeStatistics = a});

-- | A user-supplied ID that uniquely identifies the @DataSource@.
cdsfrrqDataSourceId :: Lens' CreateDataSourceFromRedshift Text
cdsfrrqDataSourceId = lens _cdsfrrqDataSourceId (\ s a -> s{_cdsfrrqDataSourceId = a});

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
cdsfrrqDataSpec :: Lens' CreateDataSourceFromRedshift RedshiftDataSpec
cdsfrrqDataSpec = lens _cdsfrrqDataSpec (\ s a -> s{_cdsfrrqDataSpec = a});

-- | A fully specified role Amazon Resource Name (ARN). Amazon ML assumes the
-- role on behalf of the user to create the following:
--
-- -   A security group to allow Amazon ML to execute the @SelectSqlQuery@
--     query on an Amazon Redshift cluster
--
-- -   An Amazon S3 bucket policy to grant Amazon ML read\/write
--     permissions on the @S3StagingLocation@
--
cdsfrrqRoleARN :: Lens' CreateDataSourceFromRedshift Text
cdsfrrqRoleARN = lens _cdsfrrqRoleARN (\ s a -> s{_cdsfrrqRoleARN = a});

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
              ["DataSourceName" .= _cdsfrrqDataSourceName,
               "ComputeStatistics" .= _cdsfrrqComputeStatistics,
               "DataSourceId" .= _cdsfrrqDataSourceId,
               "DataSpec" .= _cdsfrrqDataSpec,
               "RoleARN" .= _cdsfrrqRoleARN]

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
-- * 'cdsfrrsDataSourceId'
--
-- * 'cdsfrrsStatus'
data CreateDataSourceFromRedshiftResponse = CreateDataSourceFromRedshiftResponse'
    { _cdsfrrsDataSourceId :: !(Maybe Text)
    , _cdsfrrsStatus       :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateDataSourceFromRedshiftResponse' smart constructor.
createDataSourceFromRedshiftResponse :: Int -> CreateDataSourceFromRedshiftResponse
createDataSourceFromRedshiftResponse pStatus =
    CreateDataSourceFromRedshiftResponse'
    { _cdsfrrsDataSourceId = Nothing
    , _cdsfrrsStatus = pStatus
    }

-- | A user-supplied ID that uniquely identifies the datasource. This value
-- should be identical to the value of the @DataSourceID@ in the request.
cdsfrrsDataSourceId :: Lens' CreateDataSourceFromRedshiftResponse (Maybe Text)
cdsfrrsDataSourceId = lens _cdsfrrsDataSourceId (\ s a -> s{_cdsfrrsDataSourceId = a});

-- | FIXME: Undocumented member.
cdsfrrsStatus :: Lens' CreateDataSourceFromRedshiftResponse Int
cdsfrrsStatus = lens _cdsfrrsStatus (\ s a -> s{_cdsfrrsStatus = a});
