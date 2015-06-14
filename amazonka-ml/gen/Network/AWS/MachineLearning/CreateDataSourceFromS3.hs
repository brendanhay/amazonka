{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.MachineLearning.CreateDataSourceFromS3
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

-- | Creates a @DataSource@ object. A @DataSource@ references data that can
-- be used to perform CreateMLModel, CreateEvaluation, or
-- CreateBatchPrediction operations.
--
-- @CreateDataSourceFromS3@ is an asynchronous operation. In response to
-- @CreateDataSourceFromS3@, Amazon Machine Learning (Amazon ML)
-- immediately returns and sets the @DataSource@ status to @PENDING@. After
-- the @DataSource@ is created and ready for use, Amazon ML sets the
-- @Status@ parameter to @COMPLETED@. @DataSource@ in @COMPLETED@ or
-- @PENDING@ status can only be used to perform CreateMLModel,
-- CreateEvaluation or CreateBatchPrediction operations.
--
-- If Amazon ML cannot accept the input source, it sets the @Status@
-- parameter to @FAILED@ and includes an error message in the @Message@
-- attribute of the GetDataSource operation response.
--
-- The observation data used in a @DataSource@ should be ready to use; that
-- is, it should have a consistent structure, and missing data values
-- should be kept to a minimum. The observation data must reside in one or
-- more CSV files in an Amazon Simple Storage Service (Amazon S3) bucket,
-- along with a schema that describes the data items by name and type. The
-- same schema must be used for all of the data files referenced by the
-- @DataSource@.
--
-- After the @DataSource@ has been created, it\'s ready to use in
-- evaluations and batch predictions. If you plan to use the @DataSource@
-- to train an @MLModel@, the @DataSource@ requires another item: a recipe.
-- A recipe describes the observation variables that participate in
-- training an @MLModel@. A recipe describes how each input variable will
-- be used in training. Will the variable be included or excluded from
-- training? Will the variable be manipulated, for example, combined with
-- another variable, or split apart into word combinations? The recipe
-- provides answers to these questions. For more information, see the
-- <http://docs.aws.amazon.com/machine-learning/latest/dg Amazon Machine Learning Developer Guide>.
--
-- <http://http://docs.aws.amazon.com/machine-learning/latest/APIReference/API_CreateDataSourceFromS3.html>
module Network.AWS.MachineLearning.CreateDataSourceFromS3
    (
    -- * Request
      CreateDataSourceFromS3
    -- ** Request constructor
    , createDataSourceFromS3
    -- ** Request lenses
    , cdsfsDataSourceName
    , cdsfsComputeStatistics
    , cdsfsDataSourceId
    , cdsfsDataSpec

    -- * Response
    , CreateDataSourceFromS3Response
    -- ** Response constructor
    , createDataSourceFromS3Response
    -- ** Response lenses
    , cdsfsrDataSourceId
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.MachineLearning.Types

-- | /See:/ 'createDataSourceFromS3' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cdsfsDataSourceName'
--
-- * 'cdsfsComputeStatistics'
--
-- * 'cdsfsDataSourceId'
--
-- * 'cdsfsDataSpec'
data CreateDataSourceFromS3 = CreateDataSourceFromS3'{_cdsfsDataSourceName :: Maybe Text, _cdsfsComputeStatistics :: Maybe Bool, _cdsfsDataSourceId :: Text, _cdsfsDataSpec :: S3DataSpec} deriving (Eq, Read, Show)

-- | 'CreateDataSourceFromS3' smart constructor.
createDataSourceFromS3 :: Text -> S3DataSpec -> CreateDataSourceFromS3
createDataSourceFromS3 pDataSourceId pDataSpec = CreateDataSourceFromS3'{_cdsfsDataSourceName = Nothing, _cdsfsComputeStatistics = Nothing, _cdsfsDataSourceId = pDataSourceId, _cdsfsDataSpec = pDataSpec};

-- | A user-supplied name or description of the @DataSource@.
cdsfsDataSourceName :: Lens' CreateDataSourceFromS3 (Maybe Text)
cdsfsDataSourceName = lens _cdsfsDataSourceName (\ s a -> s{_cdsfsDataSourceName = a});

-- | The compute statistics for a @DataSource@. The statistics are generated
-- from the observation data referenced by a @DataSource@. Amazon ML uses
-- the statistics internally during an @MLModel@ training. This parameter
-- must be set to @true@ if the @@DataSource@@ needs to be used for
-- @MLModel@ training
cdsfsComputeStatistics :: Lens' CreateDataSourceFromS3 (Maybe Bool)
cdsfsComputeStatistics = lens _cdsfsComputeStatistics (\ s a -> s{_cdsfsComputeStatistics = a});

-- | A user-supplied identifier that uniquely identifies the @DataSource@.
cdsfsDataSourceId :: Lens' CreateDataSourceFromS3 Text
cdsfsDataSourceId = lens _cdsfsDataSourceId (\ s a -> s{_cdsfsDataSourceId = a});

-- | The data specification of a @DataSource@:
--
-- -   DataLocationS3 - Amazon Simple Storage Service (Amazon S3) location
--     of the observation data.
--
-- -   DataSchemaLocationS3 - Amazon S3 location of the @DataSchema@.
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
cdsfsDataSpec :: Lens' CreateDataSourceFromS3 S3DataSpec
cdsfsDataSpec = lens _cdsfsDataSpec (\ s a -> s{_cdsfsDataSpec = a});

instance AWSRequest CreateDataSourceFromS3 where
        type Sv CreateDataSourceFromS3 = MachineLearning
        type Rs CreateDataSourceFromS3 =
             CreateDataSourceFromS3Response
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 CreateDataSourceFromS3Response' <$>
                   x .?> "DataSourceId")

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
              ["DataSourceName" .= _cdsfsDataSourceName,
               "ComputeStatistics" .= _cdsfsComputeStatistics,
               "DataSourceId" .= _cdsfsDataSourceId,
               "DataSpec" .= _cdsfsDataSpec]

instance ToPath CreateDataSourceFromS3 where
        toPath = const "/"

instance ToQuery CreateDataSourceFromS3 where
        toQuery = const mempty

-- | /See:/ 'createDataSourceFromS3Response' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cdsfsrDataSourceId'
newtype CreateDataSourceFromS3Response = CreateDataSourceFromS3Response'{_cdsfsrDataSourceId :: Maybe Text} deriving (Eq, Read, Show)

-- | 'CreateDataSourceFromS3Response' smart constructor.
createDataSourceFromS3Response :: CreateDataSourceFromS3Response
createDataSourceFromS3Response = CreateDataSourceFromS3Response'{_cdsfsrDataSourceId = Nothing};

-- | A user-supplied ID that uniquely identifies the datasource. This value
-- should be identical to the value of the @DataSourceID@ in the request.
cdsfsrDataSourceId :: Lens' CreateDataSourceFromS3Response (Maybe Text)
cdsfsrDataSourceId = lens _cdsfsrDataSourceId (\ s a -> s{_cdsfsrDataSourceId = a});
