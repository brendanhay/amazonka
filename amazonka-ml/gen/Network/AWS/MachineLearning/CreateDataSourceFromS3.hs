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
-- Module      : Network.AWS.MachineLearning.CreateDataSourceFromS3
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a @DataSource@ object. A @DataSource@ references data that can be used to perform @CreateMLModel@ , @CreateEvaluation@ , or @CreateBatchPrediction@ operations.
--
--
-- @CreateDataSourceFromS3@ is an asynchronous operation. In response to @CreateDataSourceFromS3@ , Amazon Machine Learning (Amazon ML) immediately returns and sets the @DataSource@ status to @PENDING@ . After the @DataSource@ has been created and is ready for use, Amazon ML sets the @Status@ parameter to @COMPLETED@ . @DataSource@ in the @COMPLETED@ or @PENDING@ state can be used to perform only @CreateMLModel@ , @CreateEvaluation@ or @CreateBatchPrediction@ operations.
--
-- If Amazon ML can't accept the input source, it sets the @Status@ parameter to @FAILED@ and includes an error message in the @Message@ attribute of the @GetDataSource@ operation response.
--
-- The observation data used in a @DataSource@ should be ready to use; that is, it should have a consistent structure, and missing data values should be kept to a minimum. The observation data must reside in one or more .csv files in an Amazon Simple Storage Service (Amazon S3) location, along with a schema that describes the data items by name and type. The same schema must be used for all of the data files referenced by the @DataSource@ .
--
-- After the @DataSource@ has been created, it's ready to use in evaluations and batch predictions. If you plan to use the @DataSource@ to train an @MLModel@ , the @DataSource@ also needs a recipe. A recipe describes how each input variable will be used in training an @MLModel@ . Will the variable be included or excluded from training? Will the variable be manipulated; for example, will it be combined with another variable or will it be split apart into word combinations? The recipe provides answers to these questions.
--
module Network.AWS.MachineLearning.CreateDataSourceFromS3
    (
    -- * Creating a Request
      createDataSourceFromS3
    , CreateDataSourceFromS3
    -- * Request Lenses
    , cdsfsDataSourceName
    , cdsfsComputeStatistics
    , cdsfsDataSourceId
    , cdsfsDataSpec

    -- * Destructuring the Response
    , createDataSourceFromS3Response
    , CreateDataSourceFromS3Response
    -- * Response Lenses
    , cdsfsrsDataSourceId
    , cdsfsrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.MachineLearning.Types
import Network.AWS.MachineLearning.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createDataSourceFromS3' smart constructor.
data CreateDataSourceFromS3 = CreateDataSourceFromS3'
  { _cdsfsDataSourceName    :: !(Maybe Text)
  , _cdsfsComputeStatistics :: !(Maybe Bool)
  , _cdsfsDataSourceId      :: !Text
  , _cdsfsDataSpec          :: !S3DataSpec
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateDataSourceFromS3' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cdsfsDataSourceName' - A user-supplied name or description of the @DataSource@ .
--
-- * 'cdsfsComputeStatistics' - The compute statistics for a @DataSource@ . The statistics are generated from the observation data referenced by a @DataSource@ . Amazon ML uses the statistics internally during @MLModel@ training. This parameter must be set to @true@ if the DataSourceneeds to be used for @MLModel@ training.
--
-- * 'cdsfsDataSourceId' - A user-supplied identifier that uniquely identifies the @DataSource@ .
--
-- * 'cdsfsDataSpec' - The data specification of a @DataSource@ :     * DataLocationS3 - The Amazon S3 location of the observation data.     * DataSchemaLocationS3 - The Amazon S3 location of the @DataSchema@ .     * DataSchema - A JSON string representing the schema. This is not required if @DataSchemaUri@ is specified.      * DataRearrangement - A JSON string that represents the splitting and rearrangement requirements for the @Datasource@ .  Sample - @"{\"splitting\":{\"percentBegin\":10,\"percentEnd\":60}}"@
createDataSourceFromS3
    :: Text -- ^ 'cdsfsDataSourceId'
    -> S3DataSpec -- ^ 'cdsfsDataSpec'
    -> CreateDataSourceFromS3
createDataSourceFromS3 pDataSourceId_ pDataSpec_ =
  CreateDataSourceFromS3'
    { _cdsfsDataSourceName = Nothing
    , _cdsfsComputeStatistics = Nothing
    , _cdsfsDataSourceId = pDataSourceId_
    , _cdsfsDataSpec = pDataSpec_
    }


-- | A user-supplied name or description of the @DataSource@ .
cdsfsDataSourceName :: Lens' CreateDataSourceFromS3 (Maybe Text)
cdsfsDataSourceName = lens _cdsfsDataSourceName (\ s a -> s{_cdsfsDataSourceName = a})

-- | The compute statistics for a @DataSource@ . The statistics are generated from the observation data referenced by a @DataSource@ . Amazon ML uses the statistics internally during @MLModel@ training. This parameter must be set to @true@ if the DataSourceneeds to be used for @MLModel@ training.
cdsfsComputeStatistics :: Lens' CreateDataSourceFromS3 (Maybe Bool)
cdsfsComputeStatistics = lens _cdsfsComputeStatistics (\ s a -> s{_cdsfsComputeStatistics = a})

-- | A user-supplied identifier that uniquely identifies the @DataSource@ .
cdsfsDataSourceId :: Lens' CreateDataSourceFromS3 Text
cdsfsDataSourceId = lens _cdsfsDataSourceId (\ s a -> s{_cdsfsDataSourceId = a})

-- | The data specification of a @DataSource@ :     * DataLocationS3 - The Amazon S3 location of the observation data.     * DataSchemaLocationS3 - The Amazon S3 location of the @DataSchema@ .     * DataSchema - A JSON string representing the schema. This is not required if @DataSchemaUri@ is specified.      * DataRearrangement - A JSON string that represents the splitting and rearrangement requirements for the @Datasource@ .  Sample - @"{\"splitting\":{\"percentBegin\":10,\"percentEnd\":60}}"@
cdsfsDataSpec :: Lens' CreateDataSourceFromS3 S3DataSpec
cdsfsDataSpec = lens _cdsfsDataSpec (\ s a -> s{_cdsfsDataSpec = a})

instance AWSRequest CreateDataSourceFromS3 where
        type Rs CreateDataSourceFromS3 =
             CreateDataSourceFromS3Response
        request = postJSON machineLearning
        response
          = receiveJSON
              (\ s h x ->
                 CreateDataSourceFromS3Response' <$>
                   (x .?> "DataSourceId") <*> (pure (fromEnum s)))

instance Hashable CreateDataSourceFromS3 where

instance NFData CreateDataSourceFromS3 where

instance ToHeaders CreateDataSourceFromS3 where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonML_20141212.CreateDataSourceFromS3" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateDataSourceFromS3 where
        toJSON CreateDataSourceFromS3'{..}
          = object
              (catMaybes
                 [("DataSourceName" .=) <$> _cdsfsDataSourceName,
                  ("ComputeStatistics" .=) <$> _cdsfsComputeStatistics,
                  Just ("DataSourceId" .= _cdsfsDataSourceId),
                  Just ("DataSpec" .= _cdsfsDataSpec)])

instance ToPath CreateDataSourceFromS3 where
        toPath = const "/"

instance ToQuery CreateDataSourceFromS3 where
        toQuery = const mempty

-- | Represents the output of a @CreateDataSourceFromS3@ operation, and is an acknowledgement that Amazon ML received the request.
--
--
-- The @CreateDataSourceFromS3@ operation is asynchronous. You can poll for updates by using the @GetBatchPrediction@ operation and checking the @Status@ parameter.
--
--
-- /See:/ 'createDataSourceFromS3Response' smart constructor.
data CreateDataSourceFromS3Response = CreateDataSourceFromS3Response'
  { _cdsfsrsDataSourceId   :: !(Maybe Text)
  , _cdsfsrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateDataSourceFromS3Response' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cdsfsrsDataSourceId' - A user-supplied ID that uniquely identifies the @DataSource@ . This value should be identical to the value of the @DataSourceID@ in the request.
--
-- * 'cdsfsrsResponseStatus' - -- | The response status code.
createDataSourceFromS3Response
    :: Int -- ^ 'cdsfsrsResponseStatus'
    -> CreateDataSourceFromS3Response
createDataSourceFromS3Response pResponseStatus_ =
  CreateDataSourceFromS3Response'
    {_cdsfsrsDataSourceId = Nothing, _cdsfsrsResponseStatus = pResponseStatus_}


-- | A user-supplied ID that uniquely identifies the @DataSource@ . This value should be identical to the value of the @DataSourceID@ in the request.
cdsfsrsDataSourceId :: Lens' CreateDataSourceFromS3Response (Maybe Text)
cdsfsrsDataSourceId = lens _cdsfsrsDataSourceId (\ s a -> s{_cdsfsrsDataSourceId = a})

-- | -- | The response status code.
cdsfsrsResponseStatus :: Lens' CreateDataSourceFromS3Response Int
cdsfsrsResponseStatus = lens _cdsfsrsResponseStatus (\ s a -> s{_cdsfsrsResponseStatus = a})

instance NFData CreateDataSourceFromS3Response where
