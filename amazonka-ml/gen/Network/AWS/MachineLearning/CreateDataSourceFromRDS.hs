{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MachineLearning.CreateDataSourceFromRDS
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Creates a @DataSource@ object from an
-- <http://aws.amazon.com/rds/ Amazon Relational Database Service> (Amazon
-- RDS). A @DataSource@ references data that can be used to perform
-- CreateMLModel, CreateEvaluation, or CreateBatchPrediction operations.
--
-- @CreateDataSourceFromRDS@ is an asynchronous operation. In response to
-- @CreateDataSourceFromRDS@, Amazon Machine Learning (Amazon ML)
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
-- <http://http://docs.aws.amazon.com/machine-learning/latest/APIReference/API_CreateDataSourceFromRDS.html>
module Network.AWS.MachineLearning.CreateDataSourceFromRDS
    (
    -- * Request
      CreateDataSourceFromRDS
    -- ** Request constructor
    , createDataSourceFromRDS
    -- ** Request lenses
    , cdsfrdsDataSourceName
    , cdsfrdsComputeStatistics
    , cdsfrdsDataSourceId
    , cdsfrdsRDSData
    , cdsfrdsRoleARN

    -- * Response
    , CreateDataSourceFromRDSResponse
    -- ** Response constructor
    , createDataSourceFromRDSResponse
    -- ** Response lenses
    , cdsfrdsrsDataSourceId
    , cdsfrdsrsStatus
    ) where

import           Network.AWS.MachineLearning.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'createDataSourceFromRDS' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cdsfrdsDataSourceName'
--
-- * 'cdsfrdsComputeStatistics'
--
-- * 'cdsfrdsDataSourceId'
--
-- * 'cdsfrdsRDSData'
--
-- * 'cdsfrdsRoleARN'
data CreateDataSourceFromRDS = CreateDataSourceFromRDS'
    { _cdsfrdsDataSourceName    :: !(Maybe Text)
    , _cdsfrdsComputeStatistics :: !(Maybe Bool)
    , _cdsfrdsDataSourceId      :: !Text
    , _cdsfrdsRDSData           :: !RDSDataSpec
    , _cdsfrdsRoleARN           :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateDataSourceFromRDS' smart constructor.
createDataSourceFromRDS :: Text -> RDSDataSpec -> Text -> CreateDataSourceFromRDS
createDataSourceFromRDS pDataSourceId_ pRDSData_ pRoleARN_ =
    CreateDataSourceFromRDS'
    { _cdsfrdsDataSourceName = Nothing
    , _cdsfrdsComputeStatistics = Nothing
    , _cdsfrdsDataSourceId = pDataSourceId_
    , _cdsfrdsRDSData = pRDSData_
    , _cdsfrdsRoleARN = pRoleARN_
    }

-- | A user-supplied name or description of the @DataSource@.
cdsfrdsDataSourceName :: Lens' CreateDataSourceFromRDS (Maybe Text)
cdsfrdsDataSourceName = lens _cdsfrdsDataSourceName (\ s a -> s{_cdsfrdsDataSourceName = a});

-- | The compute statistics for a @DataSource@. The statistics are generated
-- from the observation data referenced by a @DataSource@. Amazon ML uses
-- the statistics internally during an @MLModel@ training. This parameter
-- must be set to @true@ if the @@DataSource@@ needs to be used for
-- @MLModel@ training.
cdsfrdsComputeStatistics :: Lens' CreateDataSourceFromRDS (Maybe Bool)
cdsfrdsComputeStatistics = lens _cdsfrdsComputeStatistics (\ s a -> s{_cdsfrdsComputeStatistics = a});

-- | A user-supplied ID that uniquely identifies the @DataSource@. Typically,
-- an Amazon Resource Number (ARN) becomes the ID for a @DataSource@.
cdsfrdsDataSourceId :: Lens' CreateDataSourceFromRDS Text
cdsfrdsDataSourceId = lens _cdsfrdsDataSourceId (\ s a -> s{_cdsfrdsDataSourceId = a});

-- | The data specification of an Amazon RDS @DataSource@:
--
-- -   DatabaseInformation -
--
--     -   @DatabaseName @ - Name of the Amazon RDS database.
--     -   @ InstanceIdentifier @ - Unique identifier for the Amazon RDS
--         database instance.
-- -   DatabaseCredentials - AWS Identity and Access Management (IAM)
--     credentials that are used to connect to the Amazon RDS database.
--
-- -   ResourceRole - Role (DataPipelineDefaultResourceRole) assumed by an
--     Amazon Elastic Compute Cloud (EC2) instance to carry out the copy
--     task from Amazon RDS to Amazon S3. For more information, see
--     <http://docs.aws.amazon.com/datapipeline/latest/DeveloperGuide/dp-iam-roles.html Role templates>
--     for data pipelines.
--
-- -   ServiceRole - Role (DataPipelineDefaultRole) assumed by the AWS Data
--     Pipeline service to monitor the progress of the copy task from
--     Amazon RDS to Amazon Simple Storage Service (S3). For more
--     information, see
--     <http://docs.aws.amazon.com/datapipeline/latest/DeveloperGuide/dp-iam-roles.html Role templates>
--     for data pipelines.
--
-- -   SecurityInfo - Security information to use to access an Amazon RDS
--     instance. You need to set up appropriate ingress rules for the
--     security entity IDs provided to allow access to the Amazon RDS
--     instance. Specify a [@SubnetId@, @SecurityGroupIds@] pair for a
--     VPC-based Amazon RDS instance.
--
-- -   SelectSqlQuery - Query that is used to retrieve the observation data
--     for the @Datasource@.
--
-- -   S3StagingLocation - Amazon S3 location for staging RDS data. The
--     data retrieved from Amazon RDS using @SelectSqlQuery@ is stored in
--     this location.
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
cdsfrdsRDSData :: Lens' CreateDataSourceFromRDS RDSDataSpec
cdsfrdsRDSData = lens _cdsfrdsRDSData (\ s a -> s{_cdsfrdsRDSData = a});

-- | The role that Amazon ML assumes on behalf of the user to create and
-- activate a data pipeline in the user’s account and copy data (using the
-- @SelectSqlQuery@) query from Amazon RDS to Amazon S3.
--
cdsfrdsRoleARN :: Lens' CreateDataSourceFromRDS Text
cdsfrdsRoleARN = lens _cdsfrdsRoleARN (\ s a -> s{_cdsfrdsRoleARN = a});

instance AWSRequest CreateDataSourceFromRDS where
        type Sv CreateDataSourceFromRDS = MachineLearning
        type Rs CreateDataSourceFromRDS =
             CreateDataSourceFromRDSResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 CreateDataSourceFromRDSResponse' <$>
                   (x .?> "DataSourceId") <*> (pure (fromEnum s)))

instance ToHeaders CreateDataSourceFromRDS where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonML_20141212.CreateDataSourceFromRDS" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateDataSourceFromRDS where
        toJSON CreateDataSourceFromRDS'{..}
          = object
              ["DataSourceName" .= _cdsfrdsDataSourceName,
               "ComputeStatistics" .= _cdsfrdsComputeStatistics,
               "DataSourceId" .= _cdsfrdsDataSourceId,
               "RDSData" .= _cdsfrdsRDSData,
               "RoleARN" .= _cdsfrdsRoleARN]

instance ToPath CreateDataSourceFromRDS where
        toPath = const mempty

instance ToQuery CreateDataSourceFromRDS where
        toQuery = const mempty

-- | Represents the output of a CreateDataSourceFromRDS operation, and is an
-- acknowledgement that Amazon ML received the request.
--
-- The CreateDataSourceFromRDS operation is asynchronous. You can poll for
-- updates by using the GetBatchPrediction operation and checking the
-- @Status@ parameter. You can inspect the @Message@ when @Status@ shows up
-- as @FAILED@. You can also check the progress of the copy operation by
-- going to the @DataPipeline@ console and looking up the pipeline using
-- the pipelineId from the describe call.
--
-- /See:/ 'createDataSourceFromRDSResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cdsfrdsrsDataSourceId'
--
-- * 'cdsfrdsrsStatus'
data CreateDataSourceFromRDSResponse = CreateDataSourceFromRDSResponse'
    { _cdsfrdsrsDataSourceId :: !(Maybe Text)
    , _cdsfrdsrsStatus       :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateDataSourceFromRDSResponse' smart constructor.
createDataSourceFromRDSResponse :: Int -> CreateDataSourceFromRDSResponse
createDataSourceFromRDSResponse pStatus_ =
    CreateDataSourceFromRDSResponse'
    { _cdsfrdsrsDataSourceId = Nothing
    , _cdsfrdsrsStatus = pStatus_
    }

-- | A user-supplied ID that uniquely identifies the datasource. This value
-- should be identical to the value of the @DataSourceID@ in the request.
cdsfrdsrsDataSourceId :: Lens' CreateDataSourceFromRDSResponse (Maybe Text)
cdsfrdsrsDataSourceId = lens _cdsfrdsrsDataSourceId (\ s a -> s{_cdsfrdsrsDataSourceId = a});

-- | FIXME: Undocumented member.
cdsfrdsrsStatus :: Lens' CreateDataSourceFromRDSResponse Int
cdsfrdsrsStatus = lens _cdsfrdsrsStatus (\ s a -> s{_cdsfrdsrsStatus = a});
