{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MachineLearning.CreateDataSourceFromRedshift
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a 'DataSource' from <http://aws.amazon.com/redshift/ Amazon Redshift>. A 'DataSource' references data that can be used to perform either < CreateMLModel>, < CreateEvaluation> or < CreateBatchPrediction> operations.
--
-- 'CreateDataSourceFromRedshift' is an asynchronous operation. In response to 'CreateDataSourceFromRedshift', Amazon Machine Learning (Amazon ML) immediately returns and sets the 'DataSource' status to 'PENDING'. After the 'DataSource' is created and ready for use, Amazon ML sets the 'Status' parameter to 'COMPLETED'. 'DataSource' in 'COMPLETED' or 'PENDING' status can only be used to perform < CreateMLModel>, < CreateEvaluation>, or < CreateBatchPrediction> operations.
--
-- If Amazon ML cannot accept the input source, it sets the 'Status' parameter to 'FAILED' and includes an error message in the 'Message' attribute of the < GetDataSource> operation response.
--
-- The observations should exist in the database hosted on an Amazon Redshift cluster and should be specified by a 'SelectSqlQuery'. Amazon ML executes <http://docs.aws.amazon.com/redshift/latest/dg/t_Unloading_tables.html Unload> command in Amazon Redshift to transfer the result set of 'SelectSqlQuery' to 'S3StagingLocation.'
--
-- After the 'DataSource' is created, it\'s ready for use in evaluations and batch predictions. If you plan to use the 'DataSource' to train an 'MLModel', the 'DataSource' requires another item -- a recipe. A recipe describes the observation variables that participate in training an 'MLModel'. A recipe describes how each input variable will be used in training. Will the variable be included or excluded from training? Will the variable be manipulated, for example, combined with another variable or split apart into word combinations? The recipe provides answers to these questions. For more information, see the Amazon Machine Learning Developer Guide.
module Network.AWS.MachineLearning.CreateDataSourceFromRedshift
    (
    -- * Creating a Request
      createDataSourceFromRedshift
    , CreateDataSourceFromRedshift
    -- * Request Lenses
    , cdsfrDataSourceName
    , cdsfrComputeStatistics
    , cdsfrDataSourceId
    , cdsfrDataSpec
    , cdsfrRoleARN

    -- * Destructuring the Response
    , createDataSourceFromRedshiftResponse
    , CreateDataSourceFromRedshiftResponse
    -- * Response Lenses
    , cdsfrrsDataSourceId
    , cdsfrrsResponseStatus
    ) where

import           Network.AWS.Lens
import           Network.AWS.MachineLearning.Types
import           Network.AWS.MachineLearning.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'createDataSourceFromRedshift' smart constructor.
data CreateDataSourceFromRedshift = CreateDataSourceFromRedshift'
    { _cdsfrDataSourceName    :: !(Maybe Text)
    , _cdsfrComputeStatistics :: !(Maybe Bool)
    , _cdsfrDataSourceId      :: !Text
    , _cdsfrDataSpec          :: !RedshiftDataSpec
    , _cdsfrRoleARN           :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CreateDataSourceFromRedshift' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
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
createDataSourceFromRedshift
    :: Text -- ^ 'cdsfrDataSourceId'
    -> RedshiftDataSpec -- ^ 'cdsfrDataSpec'
    -> Text -- ^ 'cdsfrRoleARN'
    -> CreateDataSourceFromRedshift
createDataSourceFromRedshift pDataSourceId_ pDataSpec_ pRoleARN_ =
    CreateDataSourceFromRedshift'
    { _cdsfrDataSourceName = Nothing
    , _cdsfrComputeStatistics = Nothing
    , _cdsfrDataSourceId = pDataSourceId_
    , _cdsfrDataSpec = pDataSpec_
    , _cdsfrRoleARN = pRoleARN_
    }

-- | A user-supplied name or description of the 'DataSource'.
cdsfrDataSourceName :: Lens' CreateDataSourceFromRedshift (Maybe Text)
cdsfrDataSourceName = lens _cdsfrDataSourceName (\ s a -> s{_cdsfrDataSourceName = a});

-- | The compute statistics for a 'DataSource'. The statistics are generated from the observation data referenced by a 'DataSource'. Amazon ML uses the statistics internally during 'MLModel' training. This parameter must be set to 'true' if the ''DataSource'' needs to be used for 'MLModel' training
cdsfrComputeStatistics :: Lens' CreateDataSourceFromRedshift (Maybe Bool)
cdsfrComputeStatistics = lens _cdsfrComputeStatistics (\ s a -> s{_cdsfrComputeStatistics = a});

-- | A user-supplied ID that uniquely identifies the 'DataSource'.
cdsfrDataSourceId :: Lens' CreateDataSourceFromRedshift Text
cdsfrDataSourceId = lens _cdsfrDataSourceId (\ s a -> s{_cdsfrDataSourceId = a});

-- | The data specification of an Amazon Redshift 'DataSource':
--
-- -   DatabaseInformation -
--
--     -   'DatabaseName ' - Name of the Amazon Redshift database.
--     -   ' ClusterIdentifier ' - Unique ID for the Amazon Redshift cluster.
-- -   DatabaseCredentials - AWS Identity abd Access Management (IAM) credentials that are used to connect to the Amazon Redshift database.
--
-- -   SelectSqlQuery - Query that is used to retrieve the observation data for the 'Datasource'.
--
-- -   S3StagingLocation - Amazon Simple Storage Service (Amazon S3) location for staging Amazon Redshift data. The data retrieved from Amazon Relational Database Service (Amazon RDS) using 'SelectSqlQuery' is stored in this location.
--
-- -   DataSchemaUri - Amazon S3 location of the 'DataSchema'.
--
-- -   DataSchema - A JSON string representing the schema. This is not required if 'DataSchemaUri' is specified.
--
-- -   DataRearrangement - A JSON string representing the splitting requirement of a 'Datasource'.
--
--     Sample - ' \"{\\\"splitting\\\":{\\\"percentBegin\\\":10,\\\"percentEnd\\\":60}}\"'
--
cdsfrDataSpec :: Lens' CreateDataSourceFromRedshift RedshiftDataSpec
cdsfrDataSpec = lens _cdsfrDataSpec (\ s a -> s{_cdsfrDataSpec = a});

-- | A fully specified role Amazon Resource Name (ARN). Amazon ML assumes the role on behalf of the user to create the following:
--
-- -   A security group to allow Amazon ML to execute the 'SelectSqlQuery' query on an Amazon Redshift cluster
--
-- -   An Amazon S3 bucket policy to grant Amazon ML read\/write permissions on the 'S3StagingLocation'
--
cdsfrRoleARN :: Lens' CreateDataSourceFromRedshift Text
cdsfrRoleARN = lens _cdsfrRoleARN (\ s a -> s{_cdsfrRoleARN = a});

instance AWSRequest CreateDataSourceFromRedshift
         where
        type Rs CreateDataSourceFromRedshift =
             CreateDataSourceFromRedshiftResponse
        request = postJSON machineLearning
        response
          = receiveJSON
              (\ s h x ->
                 CreateDataSourceFromRedshiftResponse' <$>
                   (x .?> "DataSourceId") <*> (pure (fromEnum s)))

instance Hashable CreateDataSourceFromRedshift

instance NFData CreateDataSourceFromRedshift

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
              (catMaybes
                 [("DataSourceName" .=) <$> _cdsfrDataSourceName,
                  ("ComputeStatistics" .=) <$> _cdsfrComputeStatistics,
                  Just ("DataSourceId" .= _cdsfrDataSourceId),
                  Just ("DataSpec" .= _cdsfrDataSpec),
                  Just ("RoleARN" .= _cdsfrRoleARN)])

instance ToPath CreateDataSourceFromRedshift where
        toPath = const "/"

instance ToQuery CreateDataSourceFromRedshift where
        toQuery = const mempty

-- | Represents the output of a < CreateDataSourceFromRedshift> operation, and is an acknowledgement that Amazon ML received the request.
--
-- The < CreateDataSourceFromRedshift> operation is asynchronous. You can poll for updates by using the < GetBatchPrediction> operation and checking the 'Status' parameter.
--
-- /See:/ 'createDataSourceFromRedshiftResponse' smart constructor.
data CreateDataSourceFromRedshiftResponse = CreateDataSourceFromRedshiftResponse'
    { _cdsfrrsDataSourceId   :: !(Maybe Text)
    , _cdsfrrsResponseStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CreateDataSourceFromRedshiftResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cdsfrrsDataSourceId'
--
-- * 'cdsfrrsResponseStatus'
createDataSourceFromRedshiftResponse
    :: Int -- ^ 'cdsfrrsResponseStatus'
    -> CreateDataSourceFromRedshiftResponse
createDataSourceFromRedshiftResponse pResponseStatus_ =
    CreateDataSourceFromRedshiftResponse'
    { _cdsfrrsDataSourceId = Nothing
    , _cdsfrrsResponseStatus = pResponseStatus_
    }

-- | A user-supplied ID that uniquely identifies the datasource. This value should be identical to the value of the 'DataSourceID' in the request.
cdsfrrsDataSourceId :: Lens' CreateDataSourceFromRedshiftResponse (Maybe Text)
cdsfrrsDataSourceId = lens _cdsfrrsDataSourceId (\ s a -> s{_cdsfrrsDataSourceId = a});

-- | The response status code.
cdsfrrsResponseStatus :: Lens' CreateDataSourceFromRedshiftResponse Int
cdsfrrsResponseStatus = lens _cdsfrrsResponseStatus (\ s a -> s{_cdsfrrsResponseStatus = a});

instance NFData CreateDataSourceFromRedshiftResponse
