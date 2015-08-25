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
-- Module      : Network.AWS.MachineLearning.CreateDataSourceFromS
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a 'DataSource' object. A 'DataSource' references data that can
-- be used to perform CreateMLModel, CreateEvaluation, or
-- CreateBatchPrediction operations.
--
-- 'CreateDataSourceFromS3' is an asynchronous operation. In response to
-- 'CreateDataSourceFromS3', Amazon Machine Learning (Amazon ML)
-- immediately returns and sets the 'DataSource' status to 'PENDING'. After
-- the 'DataSource' is created and ready for use, Amazon ML sets the
-- 'Status' parameter to 'COMPLETED'. 'DataSource' in 'COMPLETED' or
-- 'PENDING' status can only be used to perform CreateMLModel,
-- CreateEvaluation or CreateBatchPrediction operations.
--
-- If Amazon ML cannot accept the input source, it sets the 'Status'
-- parameter to 'FAILED' and includes an error message in the 'Message'
-- attribute of the GetDataSource operation response.
--
-- The observation data used in a 'DataSource' should be ready to use; that
-- is, it should have a consistent structure, and missing data values
-- should be kept to a minimum. The observation data must reside in one or
-- more CSV files in an Amazon Simple Storage Service (Amazon S3) bucket,
-- along with a schema that describes the data items by name and type. The
-- same schema must be used for all of the data files referenced by the
-- 'DataSource'.
--
-- After the 'DataSource' has been created, it\'s ready to use in
-- evaluations and batch predictions. If you plan to use the 'DataSource'
-- to train an 'MLModel', the 'DataSource' requires another item: a recipe.
-- A recipe describes the observation variables that participate in
-- training an 'MLModel'. A recipe describes how each input variable will
-- be used in training. Will the variable be included or excluded from
-- training? Will the variable be manipulated, for example, combined with
-- another variable, or split apart into word combinations? The recipe
-- provides answers to these questions. For more information, see the
-- <http://docs.aws.amazon.com/machine-learning/latest/dg Amazon Machine Learning Developer Guide>.
--
-- /See:/ <http://http://docs.aws.amazon.com/machine-learning/latest/APIReference/API_CreateDataSourceFromS.html AWS API Reference> for CreateDataSourceFromS.
module Network.AWS.MachineLearning.CreateDataSourceFromS
    (
    -- * Creating a Request
      createDataSourceFromS
    , CreateDataSourceFromS
    -- * Request Lenses
    , cdsfsDataSourceName
    , cdsfsComputeStatistics
    , cdsfsDataSourceId
    , cdsfsDataSpec

    -- * Destructuring the Response
    , createDataSourceFromSResponse
    , CreateDataSourceFromSResponse
    -- * Response Lenses
    , cdsfsrsDataSourceId
    , cdsfsrsStatus
    ) where

import           Network.AWS.MachineLearning.Types
import           Network.AWS.MachineLearning.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'createDataSourceFromS' smart constructor.
data CreateDataSourceFromS = CreateDataSourceFromS'
    { _cdsfsDataSourceName    :: !(Maybe Text)
    , _cdsfsComputeStatistics :: !(Maybe Bool)
    , _cdsfsDataSourceId      :: !Text
    , _cdsfsDataSpec          :: !S3DataSpec
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CreateDataSourceFromS' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cdsfsDataSourceName'
--
-- * 'cdsfsComputeStatistics'
--
-- * 'cdsfsDataSourceId'
--
-- * 'cdsfsDataSpec'
createDataSourceFromS
    :: Text -- ^ 'cdsfsDataSourceId'
    -> S3DataSpec -- ^ 'cdsfsDataSpec'
    -> CreateDataSourceFromS
createDataSourceFromS pDataSourceId_ pDataSpec_ =
    CreateDataSourceFromS'
    { _cdsfsDataSourceName = Nothing
    , _cdsfsComputeStatistics = Nothing
    , _cdsfsDataSourceId = pDataSourceId_
    , _cdsfsDataSpec = pDataSpec_
    }

-- | A user-supplied name or description of the 'DataSource'.
cdsfsDataSourceName :: Lens' CreateDataSourceFromS (Maybe Text)
cdsfsDataSourceName = lens _cdsfsDataSourceName (\ s a -> s{_cdsfsDataSourceName = a});

-- | The compute statistics for a 'DataSource'. The statistics are generated
-- from the observation data referenced by a 'DataSource'. Amazon ML uses
-- the statistics internally during an 'MLModel' training. This parameter
-- must be set to 'true' if the ''DataSource'' needs to be used for
-- 'MLModel' training
cdsfsComputeStatistics :: Lens' CreateDataSourceFromS (Maybe Bool)
cdsfsComputeStatistics = lens _cdsfsComputeStatistics (\ s a -> s{_cdsfsComputeStatistics = a});

-- | A user-supplied identifier that uniquely identifies the 'DataSource'.
cdsfsDataSourceId :: Lens' CreateDataSourceFromS Text
cdsfsDataSourceId = lens _cdsfsDataSourceId (\ s a -> s{_cdsfsDataSourceId = a});

-- | The data specification of a 'DataSource':
--
-- -   DataLocationS3 - Amazon Simple Storage Service (Amazon S3) location
--     of the observation data.
--
-- -   DataSchemaLocationS3 - Amazon S3 location of the 'DataSchema'.
--
-- -   DataSchema - A JSON string representing the schema. This is not
--     required if 'DataSchemaUri' is specified.
--
-- -   DataRearrangement - A JSON string representing the splitting
--     requirement of a 'Datasource'.
--
--     Sample -
--     ' \"{\\\"randomSeed\\\":\\\"some-random-seed\\\", \\\"splitting\\\":{\\\"percentBegin\\\":10,\\\"percentEnd\\\":60}}\"'
--
cdsfsDataSpec :: Lens' CreateDataSourceFromS S3DataSpec
cdsfsDataSpec = lens _cdsfsDataSpec (\ s a -> s{_cdsfsDataSpec = a});

instance AWSRequest CreateDataSourceFromS where
        type Rs CreateDataSourceFromS =
             CreateDataSourceFromSResponse
        request = postJSON machineLearning
        response
          = receiveJSON
              (\ s h x ->
                 CreateDataSourceFromSResponse' <$>
                   (x .?> "DataSourceId") <*> (pure (fromEnum s)))

instance ToHeaders CreateDataSourceFromS where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonML_20141212.CreateDataSourceFromS" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateDataSourceFromS where
        toJSON CreateDataSourceFromS'{..}
          = object
              (catMaybes
                 [("DataSourceName" .=) <$> _cdsfsDataSourceName,
                  ("ComputeStatistics" .=) <$> _cdsfsComputeStatistics,
                  Just ("DataSourceId" .= _cdsfsDataSourceId),
                  Just ("DataSpec" .= _cdsfsDataSpec)])

instance ToPath CreateDataSourceFromS where
        toPath = const "/"

instance ToQuery CreateDataSourceFromS where
        toQuery = const mempty

-- | Represents the output of a CreateDataSourceFromS3 operation, and is an
-- acknowledgement that Amazon ML received the request.
--
-- The CreateDataSourceFromS3 operation is asynchronous. You can poll for
-- updates by using the GetBatchPrediction operation and checking the
-- 'Status' parameter.
--
-- /See:/ 'createDataSourceFromSResponse' smart constructor.
data CreateDataSourceFromSResponse = CreateDataSourceFromSResponse'
    { _cdsfsrsDataSourceId :: !(Maybe Text)
    , _cdsfsrsStatus       :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CreateDataSourceFromSResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cdsfsrsDataSourceId'
--
-- * 'cdsfsrsStatus'
createDataSourceFromSResponse
    :: Int -- ^ 'cdsfsrsStatus'
    -> CreateDataSourceFromSResponse
createDataSourceFromSResponse pStatus_ =
    CreateDataSourceFromSResponse'
    { _cdsfsrsDataSourceId = Nothing
    , _cdsfsrsStatus = pStatus_
    }

-- | A user-supplied ID that uniquely identifies the datasource. This value
-- should be identical to the value of the 'DataSourceID' in the request.
cdsfsrsDataSourceId :: Lens' CreateDataSourceFromSResponse (Maybe Text)
cdsfsrsDataSourceId = lens _cdsfsrsDataSourceId (\ s a -> s{_cdsfsrsDataSourceId = a});

-- | The response status code.
cdsfsrsStatus :: Lens' CreateDataSourceFromSResponse Int
cdsfsrsStatus = lens _cdsfsrsStatus (\ s a -> s{_cdsfsrsStatus = a});
