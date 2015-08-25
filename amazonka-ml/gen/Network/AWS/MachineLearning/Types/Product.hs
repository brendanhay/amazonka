{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MachineLearning.Types.Product
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MachineLearning.Types.Product where

import           Network.AWS.MachineLearning.Types.Sum
import           Network.AWS.Prelude

-- | Represents the output of GetBatchPrediction operation.
--
-- The content consists of the detailed metadata, the status, and the data
-- file information of a /Batch Prediction/.
--
-- /See:/ 'batchPrediction' smart constructor.
data BatchPrediction = BatchPrediction'
    { _bpStatus                      :: !(Maybe EntityStatus)
    , _bpLastUpdatedAt               :: !(Maybe POSIX)
    , _bpCreatedAt                   :: !(Maybe POSIX)
    , _bpInputDataLocationS3         :: !(Maybe Text)
    , _bpMLModelId                   :: !(Maybe Text)
    , _bpBatchPredictionDataSourceId :: !(Maybe Text)
    , _bpBatchPredictionId           :: !(Maybe Text)
    , _bpName                        :: !(Maybe Text)
    , _bpCreatedByIAMUser            :: !(Maybe Text)
    , _bpMessage                     :: !(Maybe Text)
    , _bpOutputURI                   :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'BatchPrediction' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bpStatus'
--
-- * 'bpLastUpdatedAt'
--
-- * 'bpCreatedAt'
--
-- * 'bpInputDataLocationS3'
--
-- * 'bpMLModelId'
--
-- * 'bpBatchPredictionDataSourceId'
--
-- * 'bpBatchPredictionId'
--
-- * 'bpName'
--
-- * 'bpCreatedByIAMUser'
--
-- * 'bpMessage'
--
-- * 'bpOutputURI'
batchPrediction
    :: BatchPrediction
batchPrediction =
    BatchPrediction'
    { _bpStatus = Nothing
    , _bpLastUpdatedAt = Nothing
    , _bpCreatedAt = Nothing
    , _bpInputDataLocationS3 = Nothing
    , _bpMLModelId = Nothing
    , _bpBatchPredictionDataSourceId = Nothing
    , _bpBatchPredictionId = Nothing
    , _bpName = Nothing
    , _bpCreatedByIAMUser = Nothing
    , _bpMessage = Nothing
    , _bpOutputURI = Nothing
    }

-- | The status of the 'BatchPrediction'. This element can have one of the
-- following values:
--
-- -   'PENDING' - Amazon Machine Learning (Amazon ML) submitted a request
--     to generate predictions for a batch of observations.
-- -   'INPROGRESS' - The process is underway.
-- -   'FAILED' - The request to peform a batch prediction did not run to
--     completion. It is not usable.
-- -   'COMPLETED' - The batch prediction process completed successfully.
-- -   'DELETED' - The 'BatchPrediction' is marked as deleted. It is not
--     usable.
bpStatus :: Lens' BatchPrediction (Maybe EntityStatus)
bpStatus = lens _bpStatus (\ s a -> s{_bpStatus = a});

-- | The time of the most recent edit to the 'BatchPrediction'. The time is
-- expressed in epoch time.
bpLastUpdatedAt :: Lens' BatchPrediction (Maybe UTCTime)
bpLastUpdatedAt = lens _bpLastUpdatedAt (\ s a -> s{_bpLastUpdatedAt = a}) . mapping _Time;

-- | The time that the 'BatchPrediction' was created. The time is expressed
-- in epoch time.
bpCreatedAt :: Lens' BatchPrediction (Maybe UTCTime)
bpCreatedAt = lens _bpCreatedAt (\ s a -> s{_bpCreatedAt = a}) . mapping _Time;

-- | The location of the data file or directory in Amazon Simple Storage
-- Service (Amazon S3).
bpInputDataLocationS3 :: Lens' BatchPrediction (Maybe Text)
bpInputDataLocationS3 = lens _bpInputDataLocationS3 (\ s a -> s{_bpInputDataLocationS3 = a});

-- | The ID of the 'MLModel' that generated predictions for the
-- 'BatchPrediction' request.
bpMLModelId :: Lens' BatchPrediction (Maybe Text)
bpMLModelId = lens _bpMLModelId (\ s a -> s{_bpMLModelId = a});

-- | The ID of the 'DataSource' that points to the group of observations to
-- predict.
bpBatchPredictionDataSourceId :: Lens' BatchPrediction (Maybe Text)
bpBatchPredictionDataSourceId = lens _bpBatchPredictionDataSourceId (\ s a -> s{_bpBatchPredictionDataSourceId = a});

-- | The ID assigned to the 'BatchPrediction' at creation. This value should
-- be identical to the value of the 'BatchPredictionID' in the request.
bpBatchPredictionId :: Lens' BatchPrediction (Maybe Text)
bpBatchPredictionId = lens _bpBatchPredictionId (\ s a -> s{_bpBatchPredictionId = a});

-- | A user-supplied name or description of the 'BatchPrediction'.
bpName :: Lens' BatchPrediction (Maybe Text)
bpName = lens _bpName (\ s a -> s{_bpName = a});

-- | The AWS user account that invoked the 'BatchPrediction'. The account
-- type can be either an AWS root account or an AWS Identity and Access
-- Management (IAM) user account.
bpCreatedByIAMUser :: Lens' BatchPrediction (Maybe Text)
bpCreatedByIAMUser = lens _bpCreatedByIAMUser (\ s a -> s{_bpCreatedByIAMUser = a});

-- | A description of the most recent details about processing the batch
-- prediction request.
bpMessage :: Lens' BatchPrediction (Maybe Text)
bpMessage = lens _bpMessage (\ s a -> s{_bpMessage = a});

-- | The location of an Amazon S3 bucket or directory to receive the
-- operation results. The following substrings are not allowed in the s3
-- key portion of the \"outputURI\" field: \':\', \'\/\/\', \'\/.\/\',
-- \'\/..\/\'.
bpOutputURI :: Lens' BatchPrediction (Maybe Text)
bpOutputURI = lens _bpOutputURI (\ s a -> s{_bpOutputURI = a});

instance FromJSON BatchPrediction where
        parseJSON
          = withObject "BatchPrediction"
              (\ x ->
                 BatchPrediction' <$>
                   (x .:? "Status") <*> (x .:? "LastUpdatedAt") <*>
                     (x .:? "CreatedAt")
                     <*> (x .:? "InputDataLocationS3")
                     <*> (x .:? "MLModelId")
                     <*> (x .:? "BatchPredictionDataSourceId")
                     <*> (x .:? "BatchPredictionId")
                     <*> (x .:? "Name")
                     <*> (x .:? "CreatedByIamUser")
                     <*> (x .:? "Message")
                     <*> (x .:? "OutputUri"))

-- | Represents the output of the GetDataSource operation.
--
-- The content consists of the detailed metadata and data file information
-- and the current status of the 'DataSource'.
--
-- /See:/ 'dataSource' smart constructor.
data DataSource = DataSource'
    { _dsStatus            :: !(Maybe EntityStatus)
    , _dsNumberOfFiles     :: !(Maybe Integer)
    , _dsLastUpdatedAt     :: !(Maybe POSIX)
    , _dsCreatedAt         :: !(Maybe POSIX)
    , _dsRDSMetadata       :: !(Maybe RDSMetadata)
    , _dsDataSourceId      :: !(Maybe Text)
    , _dsDataSizeInBytes   :: !(Maybe Integer)
    , _dsName              :: !(Maybe Text)
    , _dsCreatedByIAMUser  :: !(Maybe Text)
    , _dsDataLocationS3    :: !(Maybe Text)
    , _dsComputeStatistics :: !(Maybe Bool)
    , _dsMessage           :: !(Maybe Text)
    , _dsRedshiftMetadata  :: !(Maybe RedshiftMetadata)
    , _dsRoleARN           :: !(Maybe Text)
    , _dsDataRearrangement :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DataSource' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsStatus'
--
-- * 'dsNumberOfFiles'
--
-- * 'dsLastUpdatedAt'
--
-- * 'dsCreatedAt'
--
-- * 'dsRDSMetadata'
--
-- * 'dsDataSourceId'
--
-- * 'dsDataSizeInBytes'
--
-- * 'dsName'
--
-- * 'dsCreatedByIAMUser'
--
-- * 'dsDataLocationS3'
--
-- * 'dsComputeStatistics'
--
-- * 'dsMessage'
--
-- * 'dsRedshiftMetadata'
--
-- * 'dsRoleARN'
--
-- * 'dsDataRearrangement'
dataSource
    :: DataSource
dataSource =
    DataSource'
    { _dsStatus = Nothing
    , _dsNumberOfFiles = Nothing
    , _dsLastUpdatedAt = Nothing
    , _dsCreatedAt = Nothing
    , _dsRDSMetadata = Nothing
    , _dsDataSourceId = Nothing
    , _dsDataSizeInBytes = Nothing
    , _dsName = Nothing
    , _dsCreatedByIAMUser = Nothing
    , _dsDataLocationS3 = Nothing
    , _dsComputeStatistics = Nothing
    , _dsMessage = Nothing
    , _dsRedshiftMetadata = Nothing
    , _dsRoleARN = Nothing
    , _dsDataRearrangement = Nothing
    }

-- | The current status of the 'DataSource'. This element can have one of the
-- following values:
--
-- -   PENDING - Amazon Machine Learning (Amazon ML) submitted a request to
--     create a 'DataSource'.
-- -   INPROGRESS - The creation process is underway.
-- -   FAILED - The request to create a 'DataSource' did not run to
--     completion. It is not usable.
-- -   COMPLETED - The creation process completed successfully.
-- -   DELETED - The 'DataSource' is marked as deleted. It is not usable.
dsStatus :: Lens' DataSource (Maybe EntityStatus)
dsStatus = lens _dsStatus (\ s a -> s{_dsStatus = a});

-- | The number of data files referenced by the 'DataSource'.
dsNumberOfFiles :: Lens' DataSource (Maybe Integer)
dsNumberOfFiles = lens _dsNumberOfFiles (\ s a -> s{_dsNumberOfFiles = a});

-- | The time of the most recent edit to the 'BatchPrediction'. The time is
-- expressed in epoch time.
dsLastUpdatedAt :: Lens' DataSource (Maybe UTCTime)
dsLastUpdatedAt = lens _dsLastUpdatedAt (\ s a -> s{_dsLastUpdatedAt = a}) . mapping _Time;

-- | The time that the 'DataSource' was created. The time is expressed in
-- epoch time.
dsCreatedAt :: Lens' DataSource (Maybe UTCTime)
dsCreatedAt = lens _dsCreatedAt (\ s a -> s{_dsCreatedAt = a}) . mapping _Time;

-- | Undocumented member.
dsRDSMetadata :: Lens' DataSource (Maybe RDSMetadata)
dsRDSMetadata = lens _dsRDSMetadata (\ s a -> s{_dsRDSMetadata = a});

-- | The ID that is assigned to the 'DataSource' during creation.
dsDataSourceId :: Lens' DataSource (Maybe Text)
dsDataSourceId = lens _dsDataSourceId (\ s a -> s{_dsDataSourceId = a});

-- | The total number of observations contained in the data files that the
-- 'DataSource' references.
dsDataSizeInBytes :: Lens' DataSource (Maybe Integer)
dsDataSizeInBytes = lens _dsDataSizeInBytes (\ s a -> s{_dsDataSizeInBytes = a});

-- | A user-supplied name or description of the 'DataSource'.
dsName :: Lens' DataSource (Maybe Text)
dsName = lens _dsName (\ s a -> s{_dsName = a});

-- | The AWS user account from which the 'DataSource' was created. The
-- account type can be either an AWS root account or an AWS Identity and
-- Access Management (IAM) user account.
dsCreatedByIAMUser :: Lens' DataSource (Maybe Text)
dsCreatedByIAMUser = lens _dsCreatedByIAMUser (\ s a -> s{_dsCreatedByIAMUser = a});

-- | The location and name of the data in Amazon Simple Storage Service
-- (Amazon S3) that is used by a 'DataSource'.
dsDataLocationS3 :: Lens' DataSource (Maybe Text)
dsDataLocationS3 = lens _dsDataLocationS3 (\ s a -> s{_dsDataLocationS3 = a});

-- | The parameter is 'true' if statistics need to be generated from the
-- observation data.
dsComputeStatistics :: Lens' DataSource (Maybe Bool)
dsComputeStatistics = lens _dsComputeStatistics (\ s a -> s{_dsComputeStatistics = a});

-- | A description of the most recent details about creating the
-- 'DataSource'.
dsMessage :: Lens' DataSource (Maybe Text)
dsMessage = lens _dsMessage (\ s a -> s{_dsMessage = a});

-- | Undocumented member.
dsRedshiftMetadata :: Lens' DataSource (Maybe RedshiftMetadata)
dsRedshiftMetadata = lens _dsRedshiftMetadata (\ s a -> s{_dsRedshiftMetadata = a});

-- | Undocumented member.
dsRoleARN :: Lens' DataSource (Maybe Text)
dsRoleARN = lens _dsRoleARN (\ s a -> s{_dsRoleARN = a});

-- | A JSON string that represents the splitting requirement of a
-- 'Datasource'.
dsDataRearrangement :: Lens' DataSource (Maybe Text)
dsDataRearrangement = lens _dsDataRearrangement (\ s a -> s{_dsDataRearrangement = a});

instance FromJSON DataSource where
        parseJSON
          = withObject "DataSource"
              (\ x ->
                 DataSource' <$>
                   (x .:? "Status") <*> (x .:? "NumberOfFiles") <*>
                     (x .:? "LastUpdatedAt")
                     <*> (x .:? "CreatedAt")
                     <*> (x .:? "RDSMetadata")
                     <*> (x .:? "DataSourceId")
                     <*> (x .:? "DataSizeInBytes")
                     <*> (x .:? "Name")
                     <*> (x .:? "CreatedByIamUser")
                     <*> (x .:? "DataLocationS3")
                     <*> (x .:? "ComputeStatistics")
                     <*> (x .:? "Message")
                     <*> (x .:? "RedshiftMetadata")
                     <*> (x .:? "RoleARN")
                     <*> (x .:? "DataRearrangement"))

-- | Represents the output of GetEvaluation operation.
--
-- The content consists of the detailed metadata and data file information
-- and the current status of the 'Evaluation'.
--
-- /See:/ 'evaluation' smart constructor.
data Evaluation = Evaluation'
    { _eStatus                 :: !(Maybe EntityStatus)
    , _ePerformanceMetrics     :: !(Maybe PerformanceMetrics)
    , _eLastUpdatedAt          :: !(Maybe POSIX)
    , _eCreatedAt              :: !(Maybe POSIX)
    , _eInputDataLocationS3    :: !(Maybe Text)
    , _eMLModelId              :: !(Maybe Text)
    , _eName                   :: !(Maybe Text)
    , _eCreatedByIAMUser       :: !(Maybe Text)
    , _eMessage                :: !(Maybe Text)
    , _eEvaluationId           :: !(Maybe Text)
    , _eEvaluationDataSourceId :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'Evaluation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'eStatus'
--
-- * 'ePerformanceMetrics'
--
-- * 'eLastUpdatedAt'
--
-- * 'eCreatedAt'
--
-- * 'eInputDataLocationS3'
--
-- * 'eMLModelId'
--
-- * 'eName'
--
-- * 'eCreatedByIAMUser'
--
-- * 'eMessage'
--
-- * 'eEvaluationId'
--
-- * 'eEvaluationDataSourceId'
evaluation
    :: Evaluation
evaluation =
    Evaluation'
    { _eStatus = Nothing
    , _ePerformanceMetrics = Nothing
    , _eLastUpdatedAt = Nothing
    , _eCreatedAt = Nothing
    , _eInputDataLocationS3 = Nothing
    , _eMLModelId = Nothing
    , _eName = Nothing
    , _eCreatedByIAMUser = Nothing
    , _eMessage = Nothing
    , _eEvaluationId = Nothing
    , _eEvaluationDataSourceId = Nothing
    }

-- | The status of the evaluation. This element can have one of the following
-- values:
--
-- -   'PENDING' - Amazon Machine Learning (Amazon ML) submitted a request
--     to evaluate an 'MLModel'.
-- -   'INPROGRESS' - The evaluation is underway.
-- -   'FAILED' - The request to evaluate an 'MLModel' did not run to
--     completion. It is not usable.
-- -   'COMPLETED' - The evaluation process completed successfully.
-- -   'DELETED' - The 'Evaluation' is marked as deleted. It is not usable.
eStatus :: Lens' Evaluation (Maybe EntityStatus)
eStatus = lens _eStatus (\ s a -> s{_eStatus = a});

-- | Measurements of how well the 'MLModel' performed, using observations
-- referenced by the 'DataSource'. One of the following metrics is
-- returned, based on the type of the MLModel:
--
-- -   BinaryAUC: A binary 'MLModel' uses the Area Under the Curve (AUC)
--     technique to measure performance.
--
-- -   RegressionRMSE: A regression 'MLModel' uses the Root Mean Square
--     Error (RMSE) technique to measure performance. RMSE measures the
--     difference between predicted and actual values for a single
--     variable.
--
-- -   MulticlassAvgFScore: A multiclass 'MLModel' uses the F1 score
--     technique to measure performance.
--
-- For more information about performance metrics, please see the
-- <http://docs.aws.amazon.com/machine-learning/latest/dg Amazon Machine Learning Developer Guide>.
ePerformanceMetrics :: Lens' Evaluation (Maybe PerformanceMetrics)
ePerformanceMetrics = lens _ePerformanceMetrics (\ s a -> s{_ePerformanceMetrics = a});

-- | The time of the most recent edit to the 'Evaluation'. The time is
-- expressed in epoch time.
eLastUpdatedAt :: Lens' Evaluation (Maybe UTCTime)
eLastUpdatedAt = lens _eLastUpdatedAt (\ s a -> s{_eLastUpdatedAt = a}) . mapping _Time;

-- | The time that the 'Evaluation' was created. The time is expressed in
-- epoch time.
eCreatedAt :: Lens' Evaluation (Maybe UTCTime)
eCreatedAt = lens _eCreatedAt (\ s a -> s{_eCreatedAt = a}) . mapping _Time;

-- | The location and name of the data in Amazon Simple Storage Server
-- (Amazon S3) that is used in the evaluation.
eInputDataLocationS3 :: Lens' Evaluation (Maybe Text)
eInputDataLocationS3 = lens _eInputDataLocationS3 (\ s a -> s{_eInputDataLocationS3 = a});

-- | The ID of the 'MLModel' that is the focus of the evaluation.
eMLModelId :: Lens' Evaluation (Maybe Text)
eMLModelId = lens _eMLModelId (\ s a -> s{_eMLModelId = a});

-- | A user-supplied name or description of the 'Evaluation'.
eName :: Lens' Evaluation (Maybe Text)
eName = lens _eName (\ s a -> s{_eName = a});

-- | The AWS user account that invoked the evaluation. The account type can
-- be either an AWS root account or an AWS Identity and Access Management
-- (IAM) user account.
eCreatedByIAMUser :: Lens' Evaluation (Maybe Text)
eCreatedByIAMUser = lens _eCreatedByIAMUser (\ s a -> s{_eCreatedByIAMUser = a});

-- | A description of the most recent details about evaluating the 'MLModel'.
eMessage :: Lens' Evaluation (Maybe Text)
eMessage = lens _eMessage (\ s a -> s{_eMessage = a});

-- | The ID that is assigned to the 'Evaluation' at creation.
eEvaluationId :: Lens' Evaluation (Maybe Text)
eEvaluationId = lens _eEvaluationId (\ s a -> s{_eEvaluationId = a});

-- | The ID of the 'DataSource' that is used to evaluate the 'MLModel'.
eEvaluationDataSourceId :: Lens' Evaluation (Maybe Text)
eEvaluationDataSourceId = lens _eEvaluationDataSourceId (\ s a -> s{_eEvaluationDataSourceId = a});

instance FromJSON Evaluation where
        parseJSON
          = withObject "Evaluation"
              (\ x ->
                 Evaluation' <$>
                   (x .:? "Status") <*> (x .:? "PerformanceMetrics") <*>
                     (x .:? "LastUpdatedAt")
                     <*> (x .:? "CreatedAt")
                     <*> (x .:? "InputDataLocationS3")
                     <*> (x .:? "MLModelId")
                     <*> (x .:? "Name")
                     <*> (x .:? "CreatedByIamUser")
                     <*> (x .:? "Message")
                     <*> (x .:? "EvaluationId")
                     <*> (x .:? "EvaluationDataSourceId"))

-- | Represents the output of a GetMLModel operation.
--
-- The content consists of the detailed metadata and the current status of
-- the 'MLModel'.
--
-- /See:/ 'mLModel' smart constructor.
data MLModel = MLModel'
    { _mlmStatus                      :: !(Maybe EntityStatus)
    , _mlmTrainingParameters          :: !(Maybe (Map Text Text))
    , _mlmLastUpdatedAt               :: !(Maybe POSIX)
    , _mlmCreatedAt                   :: !(Maybe POSIX)
    , _mlmScoreThresholdLastUpdatedAt :: !(Maybe POSIX)
    , _mlmInputDataLocationS3         :: !(Maybe Text)
    , _mlmSizeInBytes                 :: !(Maybe Integer)
    , _mlmMLModelId                   :: !(Maybe Text)
    , _mlmScoreThreshold              :: !(Maybe Double)
    , _mlmName                        :: !(Maybe Text)
    , _mlmAlgorithm                   :: !(Maybe Algorithm)
    , _mlmCreatedByIAMUser            :: !(Maybe Text)
    , _mlmEndpointInfo                :: !(Maybe RealtimeEndpointInfo)
    , _mlmTrainingDataSourceId        :: !(Maybe Text)
    , _mlmMessage                     :: !(Maybe Text)
    , _mlmMLModelType                 :: !(Maybe MLModelType)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'MLModel' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mlmStatus'
--
-- * 'mlmTrainingParameters'
--
-- * 'mlmLastUpdatedAt'
--
-- * 'mlmCreatedAt'
--
-- * 'mlmScoreThresholdLastUpdatedAt'
--
-- * 'mlmInputDataLocationS3'
--
-- * 'mlmSizeInBytes'
--
-- * 'mlmMLModelId'
--
-- * 'mlmScoreThreshold'
--
-- * 'mlmName'
--
-- * 'mlmAlgorithm'
--
-- * 'mlmCreatedByIAMUser'
--
-- * 'mlmEndpointInfo'
--
-- * 'mlmTrainingDataSourceId'
--
-- * 'mlmMessage'
--
-- * 'mlmMLModelType'
mLModel
    :: MLModel
mLModel =
    MLModel'
    { _mlmStatus = Nothing
    , _mlmTrainingParameters = Nothing
    , _mlmLastUpdatedAt = Nothing
    , _mlmCreatedAt = Nothing
    , _mlmScoreThresholdLastUpdatedAt = Nothing
    , _mlmInputDataLocationS3 = Nothing
    , _mlmSizeInBytes = Nothing
    , _mlmMLModelId = Nothing
    , _mlmScoreThreshold = Nothing
    , _mlmName = Nothing
    , _mlmAlgorithm = Nothing
    , _mlmCreatedByIAMUser = Nothing
    , _mlmEndpointInfo = Nothing
    , _mlmTrainingDataSourceId = Nothing
    , _mlmMessage = Nothing
    , _mlmMLModelType = Nothing
    }

-- | The current status of an 'MLModel'. This element can have one of the
-- following values:
--
-- -   PENDING - Amazon Machine Learning (Amazon ML) submitted a request to
--     create an 'MLModel'.
-- -   INPROGRESS - The creation process is underway.
-- -   FAILED - The request to create an 'MLModel' did not run to
--     completion. It is not usable.
-- -   COMPLETED - The creation process completed successfully.
-- -   DELETED - The 'MLModel' is marked as deleted. It is not usable.
mlmStatus :: Lens' MLModel (Maybe EntityStatus)
mlmStatus = lens _mlmStatus (\ s a -> s{_mlmStatus = a});

-- | A list of the training parameters in the 'MLModel'. The list is
-- implemented as a map of key\/value pairs.
--
-- The following is the current set of training parameters:
--
-- -   'sgd.l1RegularizationAmount' - Coefficient regularization L1 norm.
--     It controls overfitting the data by penalizing large coefficients.
--     This tends to drive coefficients to zero, resulting in a sparse
--     feature set. If you use this parameter, specify a small value, such
--     as 1.0E-04 or 1.0E-08.
--
--     The value is a double that ranges from 0 to MAX_DOUBLE. The default
--     is not to use L1 normalization. The parameter cannot be used when
--     'L2' is specified. Use this parameter sparingly.
--
-- -   'sgd.l2RegularizationAmount' - Coefficient regularization L2 norm.
--     It controls overfitting the data by penalizing large coefficients.
--     This tends to drive coefficients to small, nonzero values. If you
--     use this parameter, specify a small value, such as 1.0E-04 or
--     1.0E-08.
--
--     The valus is a double that ranges from 0 to MAX_DOUBLE. The default
--     is not to use L2 normalization. This cannot be used when 'L1' is
--     specified. Use this parameter sparingly.
--
-- -   'sgd.maxPasses' - Number of times that the training process
--     traverses the observations to build the 'MLModel'. The value is an
--     integer that ranges from 1 to 10000. The default value is 10.
--
-- -   'sgd.maxMLModelSizeInBytes' - Maximum allowed size of the model.
--     Depending on the input data, the model size might affect
--     performance.
--
--     The value is an integer that ranges from 100000 to 2147483648. The
--     default value is 33554432.
--
mlmTrainingParameters :: Lens' MLModel (HashMap Text Text)
mlmTrainingParameters = lens _mlmTrainingParameters (\ s a -> s{_mlmTrainingParameters = a}) . _Default . _Map;

-- | The time of the most recent edit to the 'MLModel'. The time is expressed
-- in epoch time.
mlmLastUpdatedAt :: Lens' MLModel (Maybe UTCTime)
mlmLastUpdatedAt = lens _mlmLastUpdatedAt (\ s a -> s{_mlmLastUpdatedAt = a}) . mapping _Time;

-- | The time that the 'MLModel' was created. The time is expressed in epoch
-- time.
mlmCreatedAt :: Lens' MLModel (Maybe UTCTime)
mlmCreatedAt = lens _mlmCreatedAt (\ s a -> s{_mlmCreatedAt = a}) . mapping _Time;

-- | The time of the most recent edit to the 'ScoreThreshold'. The time is
-- expressed in epoch time.
mlmScoreThresholdLastUpdatedAt :: Lens' MLModel (Maybe UTCTime)
mlmScoreThresholdLastUpdatedAt = lens _mlmScoreThresholdLastUpdatedAt (\ s a -> s{_mlmScoreThresholdLastUpdatedAt = a}) . mapping _Time;

-- | The location of the data file or directory in Amazon Simple Storage
-- Service (Amazon S3).
mlmInputDataLocationS3 :: Lens' MLModel (Maybe Text)
mlmInputDataLocationS3 = lens _mlmInputDataLocationS3 (\ s a -> s{_mlmInputDataLocationS3 = a});

-- | Undocumented member.
mlmSizeInBytes :: Lens' MLModel (Maybe Integer)
mlmSizeInBytes = lens _mlmSizeInBytes (\ s a -> s{_mlmSizeInBytes = a});

-- | The ID assigned to the 'MLModel' at creation.
mlmMLModelId :: Lens' MLModel (Maybe Text)
mlmMLModelId = lens _mlmMLModelId (\ s a -> s{_mlmMLModelId = a});

-- | Undocumented member.
mlmScoreThreshold :: Lens' MLModel (Maybe Double)
mlmScoreThreshold = lens _mlmScoreThreshold (\ s a -> s{_mlmScoreThreshold = a});

-- | A user-supplied name or description of the 'MLModel'.
mlmName :: Lens' MLModel (Maybe Text)
mlmName = lens _mlmName (\ s a -> s{_mlmName = a});

-- | The algorithm used to train the 'MLModel'. The following algorithm is
-- supported:
--
-- -   SGD -- Stochastic gradient descent. The goal of SGD is to minimize
--     the gradient of the loss function.
mlmAlgorithm :: Lens' MLModel (Maybe Algorithm)
mlmAlgorithm = lens _mlmAlgorithm (\ s a -> s{_mlmAlgorithm = a});

-- | The AWS user account from which the 'MLModel' was created. The account
-- type can be either an AWS root account or an AWS Identity and Access
-- Management (IAM) user account.
mlmCreatedByIAMUser :: Lens' MLModel (Maybe Text)
mlmCreatedByIAMUser = lens _mlmCreatedByIAMUser (\ s a -> s{_mlmCreatedByIAMUser = a});

-- | The current endpoint of the 'MLModel'.
mlmEndpointInfo :: Lens' MLModel (Maybe RealtimeEndpointInfo)
mlmEndpointInfo = lens _mlmEndpointInfo (\ s a -> s{_mlmEndpointInfo = a});

-- | The ID of the training 'DataSource'. The CreateMLModel operation uses
-- the 'TrainingDataSourceId'.
mlmTrainingDataSourceId :: Lens' MLModel (Maybe Text)
mlmTrainingDataSourceId = lens _mlmTrainingDataSourceId (\ s a -> s{_mlmTrainingDataSourceId = a});

-- | A description of the most recent details about accessing the 'MLModel'.
mlmMessage :: Lens' MLModel (Maybe Text)
mlmMessage = lens _mlmMessage (\ s a -> s{_mlmMessage = a});

-- | Identifies the 'MLModel' category. The following are the available
-- types:
--
-- -   REGRESSION - Produces a numeric result. For example, \"What listing
--     price should a house have?\".
-- -   BINARY - Produces one of two possible results. For example, \"Is
--     this a child-friendly web site?\".
-- -   MULTICLASS - Produces more than two possible results. For example,
--     \"Is this a HIGH, LOW or MEDIUM risk trade?\".
mlmMLModelType :: Lens' MLModel (Maybe MLModelType)
mlmMLModelType = lens _mlmMLModelType (\ s a -> s{_mlmMLModelType = a});

instance FromJSON MLModel where
        parseJSON
          = withObject "MLModel"
              (\ x ->
                 MLModel' <$>
                   (x .:? "Status") <*>
                     (x .:? "TrainingParameters" .!= mempty)
                     <*> (x .:? "LastUpdatedAt")
                     <*> (x .:? "CreatedAt")
                     <*> (x .:? "ScoreThresholdLastUpdatedAt")
                     <*> (x .:? "InputDataLocationS3")
                     <*> (x .:? "SizeInBytes")
                     <*> (x .:? "MLModelId")
                     <*> (x .:? "ScoreThreshold")
                     <*> (x .:? "Name")
                     <*> (x .:? "Algorithm")
                     <*> (x .:? "CreatedByIamUser")
                     <*> (x .:? "EndpointInfo")
                     <*> (x .:? "TrainingDataSourceId")
                     <*> (x .:? "Message")
                     <*> (x .:? "MLModelType"))

-- | Measurements of how well the 'MLModel' performed on known observations.
-- One of the following metrics is returned, based on the type of the
-- 'MLModel':
--
-- -   BinaryAUC: The binary 'MLModel' uses the Area Under the Curve (AUC)
--     technique to measure performance.
--
-- -   RegressionRMSE: The regression 'MLModel' uses the Root Mean Square
--     Error (RMSE) technique to measure performance. RMSE measures the
--     difference between predicted and actual values for a single
--     variable.
--
-- -   MulticlassAvgFScore: The multiclass 'MLModel' uses the F1 score
--     technique to measure performance.
--
-- For more information about performance metrics, please see the
-- <http://docs.aws.amazon.com/machine-learning/latest/dg Amazon Machine Learning Developer Guide>.
--
-- /See:/ 'performanceMetrics' smart constructor.
newtype PerformanceMetrics = PerformanceMetrics'
    { _pmProperties :: Maybe (Map Text Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'PerformanceMetrics' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pmProperties'
performanceMetrics
    :: PerformanceMetrics
performanceMetrics =
    PerformanceMetrics'
    { _pmProperties = Nothing
    }

-- | Undocumented member.
pmProperties :: Lens' PerformanceMetrics (HashMap Text Text)
pmProperties = lens _pmProperties (\ s a -> s{_pmProperties = a}) . _Default . _Map;

instance FromJSON PerformanceMetrics where
        parseJSON
          = withObject "PerformanceMetrics"
              (\ x ->
                 PerformanceMetrics' <$>
                   (x .:? "Properties" .!= mempty))

-- | The output from a 'Predict' operation:
--
-- -   'Details' - Contains the following attributes:
--     DetailsAttributes.PREDICTIVE_MODEL_TYPE - REGRESSION | BINARY |
--     MULTICLASS DetailsAttributes.ALGORITHM - SGD
--
-- -   'PredictedLabel' - Present for either a BINARY or MULTICLASS
--     'MLModel' request.
--
-- -   'PredictedScores' - Contains the raw classification score
--     corresponding to each label.
--
-- -   'PredictedValue' - Present for a REGRESSION 'MLModel' request.
--
--
-- /See:/ 'prediction' smart constructor.
data Prediction = Prediction'
    { _pPredictedValue  :: !(Maybe Double)
    , _pPredictedLabel  :: !(Maybe Text)
    , _pPredictedScores :: !(Maybe (Map Text Double))
    , _pDetails         :: !(Maybe (Map DetailsAttributes Text))
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'Prediction' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pPredictedValue'
--
-- * 'pPredictedLabel'
--
-- * 'pPredictedScores'
--
-- * 'pDetails'
prediction
    :: Prediction
prediction =
    Prediction'
    { _pPredictedValue = Nothing
    , _pPredictedLabel = Nothing
    , _pPredictedScores = Nothing
    , _pDetails = Nothing
    }

-- | The prediction value for REGRESSION 'MLModel'.
pPredictedValue :: Lens' Prediction (Maybe Double)
pPredictedValue = lens _pPredictedValue (\ s a -> s{_pPredictedValue = a});

-- | The prediction label for either a BINARY or MULTICLASS 'MLModel'.
pPredictedLabel :: Lens' Prediction (Maybe Text)
pPredictedLabel = lens _pPredictedLabel (\ s a -> s{_pPredictedLabel = a});

-- | Undocumented member.
pPredictedScores :: Lens' Prediction (HashMap Text Double)
pPredictedScores = lens _pPredictedScores (\ s a -> s{_pPredictedScores = a}) . _Default . _Map;

-- | Undocumented member.
pDetails :: Lens' Prediction (HashMap DetailsAttributes Text)
pDetails = lens _pDetails (\ s a -> s{_pDetails = a}) . _Default . _Map;

instance FromJSON Prediction where
        parseJSON
          = withObject "Prediction"
              (\ x ->
                 Prediction' <$>
                   (x .:? "predictedValue") <*> (x .:? "predictedLabel")
                     <*> (x .:? "predictedScores" .!= mempty)
                     <*> (x .:? "details" .!= mempty))

-- | The data specification of an Amazon Relational Database Service (Amazon
-- RDS) 'DataSource'.
--
-- /See:/ 'rdsDataSpec' smart constructor.
data RDSDataSpec = RDSDataSpec'
    { _rdsdsDataSchemaURI       :: !(Maybe Text)
    , _rdsdsDataSchema          :: !(Maybe Text)
    , _rdsdsDataRearrangement   :: !(Maybe Text)
    , _rdsdsDatabaseInformation :: !RDSDatabase
    , _rdsdsSelectSqlQuery      :: !Text
    , _rdsdsDatabaseCredentials :: !RDSDatabaseCredentials
    , _rdsdsS3StagingLocation   :: !Text
    , _rdsdsResourceRole        :: !Text
    , _rdsdsServiceRole         :: !Text
    , _rdsdsSubnetId            :: !Text
    , _rdsdsSecurityGroupIds    :: ![Text]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'RDSDataSpec' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rdsdsDataSchemaURI'
--
-- * 'rdsdsDataSchema'
--
-- * 'rdsdsDataRearrangement'
--
-- * 'rdsdsDatabaseInformation'
--
-- * 'rdsdsSelectSqlQuery'
--
-- * 'rdsdsDatabaseCredentials'
--
-- * 'rdsdsS3StagingLocation'
--
-- * 'rdsdsResourceRole'
--
-- * 'rdsdsServiceRole'
--
-- * 'rdsdsSubnetId'
--
-- * 'rdsdsSecurityGroupIds'
rdsDataSpec
    :: RDSDatabase -- ^ 'rdsdsDatabaseInformation'
    -> Text -- ^ 'rdsdsSelectSqlQuery'
    -> RDSDatabaseCredentials -- ^ 'rdsdsDatabaseCredentials'
    -> Text -- ^ 'rdsdsS3StagingLocation'
    -> Text -- ^ 'rdsdsResourceRole'
    -> Text -- ^ 'rdsdsServiceRole'
    -> Text -- ^ 'rdsdsSubnetId'
    -> RDSDataSpec
rdsDataSpec pDatabaseInformation_ pSelectSqlQuery_ pDatabaseCredentials_ pS3StagingLocation_ pResourceRole_ pServiceRole_ pSubnetId_ =
    RDSDataSpec'
    { _rdsdsDataSchemaURI = Nothing
    , _rdsdsDataSchema = Nothing
    , _rdsdsDataRearrangement = Nothing
    , _rdsdsDatabaseInformation = pDatabaseInformation_
    , _rdsdsSelectSqlQuery = pSelectSqlQuery_
    , _rdsdsDatabaseCredentials = pDatabaseCredentials_
    , _rdsdsS3StagingLocation = pS3StagingLocation_
    , _rdsdsResourceRole = pResourceRole_
    , _rdsdsServiceRole = pServiceRole_
    , _rdsdsSubnetId = pSubnetId_
    , _rdsdsSecurityGroupIds = mempty
    }

-- | The Amazon S3 location of the 'DataSchema'.
rdsdsDataSchemaURI :: Lens' RDSDataSpec (Maybe Text)
rdsdsDataSchemaURI = lens _rdsdsDataSchemaURI (\ s a -> s{_rdsdsDataSchemaURI = a});

-- | A JSON string that represents the schema. This is not required if
-- 'DataSchemaUri' is specified.
rdsdsDataSchema :: Lens' RDSDataSpec (Maybe Text)
rdsdsDataSchema = lens _rdsdsDataSchema (\ s a -> s{_rdsdsDataSchema = a});

-- | DataRearrangement - A JSON string that represents the splitting
-- requirement of a 'DataSource'.
--
-- Sample -
-- ' \"{\\\"randomSeed\\\":\\\"some-random-seed\\\", \\\"splitting\\\":{\\\"percentBegin\\\":10,\\\"percentEnd\\\":60}}\"'
rdsdsDataRearrangement :: Lens' RDSDataSpec (Maybe Text)
rdsdsDataRearrangement = lens _rdsdsDataRearrangement (\ s a -> s{_rdsdsDataRearrangement = a});

-- | Describes the 'DatabaseName' and 'InstanceIdentifier' of an an Amazon
-- RDS database.
rdsdsDatabaseInformation :: Lens' RDSDataSpec RDSDatabase
rdsdsDatabaseInformation = lens _rdsdsDatabaseInformation (\ s a -> s{_rdsdsDatabaseInformation = a});

-- | The query that is used to retrieve the observation data for the
-- 'DataSource'.
rdsdsSelectSqlQuery :: Lens' RDSDataSpec Text
rdsdsSelectSqlQuery = lens _rdsdsSelectSqlQuery (\ s a -> s{_rdsdsSelectSqlQuery = a});

-- | The AWS Identity and Access Management (IAM) credentials that are used
-- connect to the Amazon RDS database.
rdsdsDatabaseCredentials :: Lens' RDSDataSpec RDSDatabaseCredentials
rdsdsDatabaseCredentials = lens _rdsdsDatabaseCredentials (\ s a -> s{_rdsdsDatabaseCredentials = a});

-- | The Amazon S3 location for staging Amazon RDS data. The data retrieved
-- from Amazon RDS using 'SelectSqlQuery' is stored in this location.
rdsdsS3StagingLocation :: Lens' RDSDataSpec Text
rdsdsS3StagingLocation = lens _rdsdsS3StagingLocation (\ s a -> s{_rdsdsS3StagingLocation = a});

-- | The role (DataPipelineDefaultResourceRole) assumed by an Amazon Elastic
-- Compute Cloud (Amazon EC2) instance to carry out the copy operation from
-- Amazon RDS to an Amazon S3 task. For more information, see
-- <http://docs.aws.amazon.com/datapipeline/latest/DeveloperGuide/dp-iam-roles.html Role templates>
-- for data pipelines.
rdsdsResourceRole :: Lens' RDSDataSpec Text
rdsdsResourceRole = lens _rdsdsResourceRole (\ s a -> s{_rdsdsResourceRole = a});

-- | The role (DataPipelineDefaultRole) assumed by AWS Data Pipeline service
-- to monitor the progress of the copy task from Amazon RDS to Amazon S3.
-- For more information, see
-- <http://docs.aws.amazon.com/datapipeline/latest/DeveloperGuide/dp-iam-roles.html Role templates>
-- for data pipelines.
rdsdsServiceRole :: Lens' RDSDataSpec Text
rdsdsServiceRole = lens _rdsdsServiceRole (\ s a -> s{_rdsdsServiceRole = a});

-- | The subnet ID to be used to access a VPC-based RDS DB instance. This
-- attribute is used by Data Pipeline to carry out the copy task from
-- Amazon RDS to Amazon S3.
rdsdsSubnetId :: Lens' RDSDataSpec Text
rdsdsSubnetId = lens _rdsdsSubnetId (\ s a -> s{_rdsdsSubnetId = a});

-- | The security group IDs to be used to access a VPC-based RDS DB instance.
-- Ensure that there are appropriate ingress rules set up to allow access
-- to the RDS DB instance. This attribute is used by Data Pipeline to carry
-- out the copy operation from Amazon RDS to an Amazon S3 task.
rdsdsSecurityGroupIds :: Lens' RDSDataSpec [Text]
rdsdsSecurityGroupIds = lens _rdsdsSecurityGroupIds (\ s a -> s{_rdsdsSecurityGroupIds = a}) . _Coerce;

instance ToJSON RDSDataSpec where
        toJSON RDSDataSpec'{..}
          = object
              (catMaybes
                 [("DataSchemaUri" .=) <$> _rdsdsDataSchemaURI,
                  ("DataSchema" .=) <$> _rdsdsDataSchema,
                  ("DataRearrangement" .=) <$> _rdsdsDataRearrangement,
                  Just
                    ("DatabaseInformation" .= _rdsdsDatabaseInformation),
                  Just ("SelectSqlQuery" .= _rdsdsSelectSqlQuery),
                  Just
                    ("DatabaseCredentials" .= _rdsdsDatabaseCredentials),
                  Just
                    ("S3StagingLocation" .= _rdsdsS3StagingLocation),
                  Just ("ResourceRole" .= _rdsdsResourceRole),
                  Just ("ServiceRole" .= _rdsdsServiceRole),
                  Just ("SubnetId" .= _rdsdsSubnetId),
                  Just ("SecurityGroupIds" .= _rdsdsSecurityGroupIds)])

-- | The database details of an Amazon RDS database.
--
-- /See:/ 'rdsDatabase' smart constructor.
data RDSDatabase = RDSDatabase'
    { _rdsdInstanceIdentifier :: !Text
    , _rdsdDatabaseName       :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'RDSDatabase' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rdsdInstanceIdentifier'
--
-- * 'rdsdDatabaseName'
rdsDatabase
    :: Text -- ^ 'rdsdInstanceIdentifier'
    -> Text -- ^ 'rdsdDatabaseName'
    -> RDSDatabase
rdsDatabase pInstanceIdentifier_ pDatabaseName_ =
    RDSDatabase'
    { _rdsdInstanceIdentifier = pInstanceIdentifier_
    , _rdsdDatabaseName = pDatabaseName_
    }

-- | The ID of an RDS DB instance.
rdsdInstanceIdentifier :: Lens' RDSDatabase Text
rdsdInstanceIdentifier = lens _rdsdInstanceIdentifier (\ s a -> s{_rdsdInstanceIdentifier = a});

-- | Undocumented member.
rdsdDatabaseName :: Lens' RDSDatabase Text
rdsdDatabaseName = lens _rdsdDatabaseName (\ s a -> s{_rdsdDatabaseName = a});

instance FromJSON RDSDatabase where
        parseJSON
          = withObject "RDSDatabase"
              (\ x ->
                 RDSDatabase' <$>
                   (x .: "InstanceIdentifier") <*>
                     (x .: "DatabaseName"))

instance ToJSON RDSDatabase where
        toJSON RDSDatabase'{..}
          = object
              (catMaybes
                 [Just
                    ("InstanceIdentifier" .= _rdsdInstanceIdentifier),
                  Just ("DatabaseName" .= _rdsdDatabaseName)])

-- | The database credentials to connect to a database on an RDS DB instance.
--
-- /See:/ 'rdsDatabaseCredentials' smart constructor.
data RDSDatabaseCredentials = RDSDatabaseCredentials'
    { _rdsdcUsername :: !Text
    , _rdsdcPassword :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'RDSDatabaseCredentials' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rdsdcUsername'
--
-- * 'rdsdcPassword'
rdsDatabaseCredentials
    :: Text -- ^ 'rdsdcUsername'
    -> Text -- ^ 'rdsdcPassword'
    -> RDSDatabaseCredentials
rdsDatabaseCredentials pUsername_ pPassword_ =
    RDSDatabaseCredentials'
    { _rdsdcUsername = pUsername_
    , _rdsdcPassword = pPassword_
    }

-- | Undocumented member.
rdsdcUsername :: Lens' RDSDatabaseCredentials Text
rdsdcUsername = lens _rdsdcUsername (\ s a -> s{_rdsdcUsername = a});

-- | Undocumented member.
rdsdcPassword :: Lens' RDSDatabaseCredentials Text
rdsdcPassword = lens _rdsdcPassword (\ s a -> s{_rdsdcPassword = a});

instance ToJSON RDSDatabaseCredentials where
        toJSON RDSDatabaseCredentials'{..}
          = object
              (catMaybes
                 [Just ("Username" .= _rdsdcUsername),
                  Just ("Password" .= _rdsdcPassword)])

-- | The datasource details that are specific to Amazon RDS.
--
-- /See:/ 'rdsMetadata' smart constructor.
data RDSMetadata = RDSMetadata'
    { _rmSelectSqlQuery   :: !(Maybe Text)
    , _rmDataPipelineId   :: !(Maybe Text)
    , _rmDatabase         :: !(Maybe RDSDatabase)
    , _rmDatabaseUserName :: !(Maybe Text)
    , _rmResourceRole     :: !(Maybe Text)
    , _rmServiceRole      :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'RDSMetadata' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rmSelectSqlQuery'
--
-- * 'rmDataPipelineId'
--
-- * 'rmDatabase'
--
-- * 'rmDatabaseUserName'
--
-- * 'rmResourceRole'
--
-- * 'rmServiceRole'
rdsMetadata
    :: RDSMetadata
rdsMetadata =
    RDSMetadata'
    { _rmSelectSqlQuery = Nothing
    , _rmDataPipelineId = Nothing
    , _rmDatabase = Nothing
    , _rmDatabaseUserName = Nothing
    , _rmResourceRole = Nothing
    , _rmServiceRole = Nothing
    }

-- | The SQL query that is supplied during CreateDataSourceFromRDS. Returns
-- only if 'Verbose' is true in 'GetDataSourceInput'.
rmSelectSqlQuery :: Lens' RDSMetadata (Maybe Text)
rmSelectSqlQuery = lens _rmSelectSqlQuery (\ s a -> s{_rmSelectSqlQuery = a});

-- | The ID of the Data Pipeline instance that is used to carry to copy data
-- from Amazon RDS to Amazon S3. You can use the ID to find details about
-- the instance in the Data Pipeline console.
rmDataPipelineId :: Lens' RDSMetadata (Maybe Text)
rmDataPipelineId = lens _rmDataPipelineId (\ s a -> s{_rmDataPipelineId = a});

-- | The database details required to connect to an Amazon RDS.
rmDatabase :: Lens' RDSMetadata (Maybe RDSDatabase)
rmDatabase = lens _rmDatabase (\ s a -> s{_rmDatabase = a});

-- | Undocumented member.
rmDatabaseUserName :: Lens' RDSMetadata (Maybe Text)
rmDatabaseUserName = lens _rmDatabaseUserName (\ s a -> s{_rmDatabaseUserName = a});

-- | The role (DataPipelineDefaultResourceRole) assumed by an Amazon EC2
-- instance to carry out the copy task from Amazon RDS to Amazon S3. For
-- more information, see
-- <http://docs.aws.amazon.com/datapipeline/latest/DeveloperGuide/dp-iam-roles.html Role templates>
-- for data pipelines.
rmResourceRole :: Lens' RDSMetadata (Maybe Text)
rmResourceRole = lens _rmResourceRole (\ s a -> s{_rmResourceRole = a});

-- | The role (DataPipelineDefaultRole) assumed by the Data Pipeline service
-- to monitor the progress of the copy task from Amazon RDS to Amazon S3.
-- For more information, see
-- <http://docs.aws.amazon.com/datapipeline/latest/DeveloperGuide/dp-iam-roles.html Role templates>
-- for data pipelines.
rmServiceRole :: Lens' RDSMetadata (Maybe Text)
rmServiceRole = lens _rmServiceRole (\ s a -> s{_rmServiceRole = a});

instance FromJSON RDSMetadata where
        parseJSON
          = withObject "RDSMetadata"
              (\ x ->
                 RDSMetadata' <$>
                   (x .:? "SelectSqlQuery") <*> (x .:? "DataPipelineId")
                     <*> (x .:? "Database")
                     <*> (x .:? "DatabaseUserName")
                     <*> (x .:? "ResourceRole")
                     <*> (x .:? "ServiceRole"))

-- | Describes the real-time endpoint information for an 'MLModel'.
--
-- /See:/ 'realtimeEndpointInfo' smart constructor.
data RealtimeEndpointInfo = RealtimeEndpointInfo'
    { _reiCreatedAt             :: !(Maybe POSIX)
    , _reiEndpointURL           :: !(Maybe Text)
    , _reiEndpointStatus        :: !(Maybe RealtimeEndpointStatus)
    , _reiPeakRequestsPerSecond :: !(Maybe Int)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'RealtimeEndpointInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'reiCreatedAt'
--
-- * 'reiEndpointURL'
--
-- * 'reiEndpointStatus'
--
-- * 'reiPeakRequestsPerSecond'
realtimeEndpointInfo
    :: RealtimeEndpointInfo
realtimeEndpointInfo =
    RealtimeEndpointInfo'
    { _reiCreatedAt = Nothing
    , _reiEndpointURL = Nothing
    , _reiEndpointStatus = Nothing
    , _reiPeakRequestsPerSecond = Nothing
    }

-- | The time that the request to create the real-time endpoint for the
-- 'MLModel' was received. The time is expressed in epoch time.
reiCreatedAt :: Lens' RealtimeEndpointInfo (Maybe UTCTime)
reiCreatedAt = lens _reiCreatedAt (\ s a -> s{_reiCreatedAt = a}) . mapping _Time;

-- | The URI that specifies where to send real-time prediction requests for
-- the 'MLModel'.
--
-- Note
--
-- The application must wait until the real-time endpoint is ready before
-- using this URI.
reiEndpointURL :: Lens' RealtimeEndpointInfo (Maybe Text)
reiEndpointURL = lens _reiEndpointURL (\ s a -> s{_reiEndpointURL = a});

-- | The current status of the real-time endpoint for the 'MLModel'. This
-- element can have one of the following values:
--
-- -   NONE - Endpoint does not exist or was previously deleted.
-- -   READY - Endpoint is ready to be used for real-time predictions.
-- -   UPDATING - Updating\/creating the endpoint.
reiEndpointStatus :: Lens' RealtimeEndpointInfo (Maybe RealtimeEndpointStatus)
reiEndpointStatus = lens _reiEndpointStatus (\ s a -> s{_reiEndpointStatus = a});

-- | The maximum processing rate for the real-time endpoint for 'MLModel',
-- measured in incoming requests per second.
reiPeakRequestsPerSecond :: Lens' RealtimeEndpointInfo (Maybe Int)
reiPeakRequestsPerSecond = lens _reiPeakRequestsPerSecond (\ s a -> s{_reiPeakRequestsPerSecond = a});

instance FromJSON RealtimeEndpointInfo where
        parseJSON
          = withObject "RealtimeEndpointInfo"
              (\ x ->
                 RealtimeEndpointInfo' <$>
                   (x .:? "CreatedAt") <*> (x .:? "EndpointUrl") <*>
                     (x .:? "EndpointStatus")
                     <*> (x .:? "PeakRequestsPerSecond"))

-- | Describes the data specification of an Amazon Redshift 'DataSource'.
--
-- /See:/ 'redshiftDataSpec' smart constructor.
data RedshiftDataSpec = RedshiftDataSpec'
    { _rDataSchemaURI       :: !(Maybe Text)
    , _rDataSchema          :: !(Maybe Text)
    , _rDataRearrangement   :: !(Maybe Text)
    , _rDatabaseInformation :: !RedshiftDatabase
    , _rSelectSqlQuery      :: !Text
    , _rDatabaseCredentials :: !RedshiftDatabaseCredentials
    , _rS3StagingLocation   :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'RedshiftDataSpec' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rDataSchemaURI'
--
-- * 'rDataSchema'
--
-- * 'rDataRearrangement'
--
-- * 'rDatabaseInformation'
--
-- * 'rSelectSqlQuery'
--
-- * 'rDatabaseCredentials'
--
-- * 'rS3StagingLocation'
redshiftDataSpec
    :: RedshiftDatabase -- ^ 'rDatabaseInformation'
    -> Text -- ^ 'rSelectSqlQuery'
    -> RedshiftDatabaseCredentials -- ^ 'rDatabaseCredentials'
    -> Text -- ^ 'rS3StagingLocation'
    -> RedshiftDataSpec
redshiftDataSpec pDatabaseInformation_ pSelectSqlQuery_ pDatabaseCredentials_ pS3StagingLocation_ =
    RedshiftDataSpec'
    { _rDataSchemaURI = Nothing
    , _rDataSchema = Nothing
    , _rDataRearrangement = Nothing
    , _rDatabaseInformation = pDatabaseInformation_
    , _rSelectSqlQuery = pSelectSqlQuery_
    , _rDatabaseCredentials = pDatabaseCredentials_
    , _rS3StagingLocation = pS3StagingLocation_
    }

-- | Describes the schema location for an Amazon Redshift 'DataSource'.
rDataSchemaURI :: Lens' RedshiftDataSpec (Maybe Text)
rDataSchemaURI = lens _rDataSchemaURI (\ s a -> s{_rDataSchemaURI = a});

-- | Describes the schema for an Amazon Redshift 'DataSource'.
rDataSchema :: Lens' RedshiftDataSpec (Maybe Text)
rDataSchema = lens _rDataSchema (\ s a -> s{_rDataSchema = a});

-- | Describes the splitting specifications for a 'DataSource'.
rDataRearrangement :: Lens' RedshiftDataSpec (Maybe Text)
rDataRearrangement = lens _rDataRearrangement (\ s a -> s{_rDataRearrangement = a});

-- | Describes the 'DatabaseName' and 'ClusterIdentifier' for an Amazon
-- Redshift 'DataSource'.
rDatabaseInformation :: Lens' RedshiftDataSpec RedshiftDatabase
rDatabaseInformation = lens _rDatabaseInformation (\ s a -> s{_rDatabaseInformation = a});

-- | Describes the SQL Query to execute on an Amazon Redshift database for an
-- Amazon Redshift 'DataSource'.
rSelectSqlQuery :: Lens' RedshiftDataSpec Text
rSelectSqlQuery = lens _rSelectSqlQuery (\ s a -> s{_rSelectSqlQuery = a});

-- | Describes AWS Identity and Access Management (IAM) credentials that are
-- used connect to the Amazon Redshift database.
rDatabaseCredentials :: Lens' RedshiftDataSpec RedshiftDatabaseCredentials
rDatabaseCredentials = lens _rDatabaseCredentials (\ s a -> s{_rDatabaseCredentials = a});

-- | Describes an Amazon S3 location to store the result set of the
-- 'SelectSqlQuery' query.
rS3StagingLocation :: Lens' RedshiftDataSpec Text
rS3StagingLocation = lens _rS3StagingLocation (\ s a -> s{_rS3StagingLocation = a});

instance ToJSON RedshiftDataSpec where
        toJSON RedshiftDataSpec'{..}
          = object
              (catMaybes
                 [("DataSchemaUri" .=) <$> _rDataSchemaURI,
                  ("DataSchema" .=) <$> _rDataSchema,
                  ("DataRearrangement" .=) <$> _rDataRearrangement,
                  Just
                    ("DatabaseInformation" .= _rDatabaseInformation),
                  Just ("SelectSqlQuery" .= _rSelectSqlQuery),
                  Just
                    ("DatabaseCredentials" .= _rDatabaseCredentials),
                  Just ("S3StagingLocation" .= _rS3StagingLocation)])

-- | Describes the database details required to connect to an Amazon Redshift
-- database.
--
-- /See:/ 'redshiftDatabase' smart constructor.
data RedshiftDatabase = RedshiftDatabase'
    { _rdDatabaseName      :: !Text
    , _rdClusterIdentifier :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'RedshiftDatabase' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rdDatabaseName'
--
-- * 'rdClusterIdentifier'
redshiftDatabase
    :: Text -- ^ 'rdDatabaseName'
    -> Text -- ^ 'rdClusterIdentifier'
    -> RedshiftDatabase
redshiftDatabase pDatabaseName_ pClusterIdentifier_ =
    RedshiftDatabase'
    { _rdDatabaseName = pDatabaseName_
    , _rdClusterIdentifier = pClusterIdentifier_
    }

-- | Undocumented member.
rdDatabaseName :: Lens' RedshiftDatabase Text
rdDatabaseName = lens _rdDatabaseName (\ s a -> s{_rdDatabaseName = a});

-- | Undocumented member.
rdClusterIdentifier :: Lens' RedshiftDatabase Text
rdClusterIdentifier = lens _rdClusterIdentifier (\ s a -> s{_rdClusterIdentifier = a});

instance FromJSON RedshiftDatabase where
        parseJSON
          = withObject "RedshiftDatabase"
              (\ x ->
                 RedshiftDatabase' <$>
                   (x .: "DatabaseName") <*> (x .: "ClusterIdentifier"))

instance ToJSON RedshiftDatabase where
        toJSON RedshiftDatabase'{..}
          = object
              (catMaybes
                 [Just ("DatabaseName" .= _rdDatabaseName),
                  Just ("ClusterIdentifier" .= _rdClusterIdentifier)])

-- | Describes the database credentials for connecting to a database on an
-- Amazon Redshift cluster.
--
-- /See:/ 'redshiftDatabaseCredentials' smart constructor.
data RedshiftDatabaseCredentials = RedshiftDatabaseCredentials'
    { _rdcUsername :: !Text
    , _rdcPassword :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'RedshiftDatabaseCredentials' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rdcUsername'
--
-- * 'rdcPassword'
redshiftDatabaseCredentials
    :: Text -- ^ 'rdcUsername'
    -> Text -- ^ 'rdcPassword'
    -> RedshiftDatabaseCredentials
redshiftDatabaseCredentials pUsername_ pPassword_ =
    RedshiftDatabaseCredentials'
    { _rdcUsername = pUsername_
    , _rdcPassword = pPassword_
    }

-- | Undocumented member.
rdcUsername :: Lens' RedshiftDatabaseCredentials Text
rdcUsername = lens _rdcUsername (\ s a -> s{_rdcUsername = a});

-- | Undocumented member.
rdcPassword :: Lens' RedshiftDatabaseCredentials Text
rdcPassword = lens _rdcPassword (\ s a -> s{_rdcPassword = a});

instance ToJSON RedshiftDatabaseCredentials where
        toJSON RedshiftDatabaseCredentials'{..}
          = object
              (catMaybes
                 [Just ("Username" .= _rdcUsername),
                  Just ("Password" .= _rdcPassword)])

-- | Describes the 'DataSource' details specific to Amazon Redshift.
--
-- /See:/ 'redshiftMetadata' smart constructor.
data RedshiftMetadata = RedshiftMetadata'
    { _redSelectSqlQuery   :: !(Maybe Text)
    , _redRedshiftDatabase :: !(Maybe RedshiftDatabase)
    , _redDatabaseUserName :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'RedshiftMetadata' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'redSelectSqlQuery'
--
-- * 'redRedshiftDatabase'
--
-- * 'redDatabaseUserName'
redshiftMetadata
    :: RedshiftMetadata
redshiftMetadata =
    RedshiftMetadata'
    { _redSelectSqlQuery = Nothing
    , _redRedshiftDatabase = Nothing
    , _redDatabaseUserName = Nothing
    }

-- | The SQL query that is specified during CreateDataSourceFromRedshift.
-- Returns only if 'Verbose' is true in GetDataSourceInput.
redSelectSqlQuery :: Lens' RedshiftMetadata (Maybe Text)
redSelectSqlQuery = lens _redSelectSqlQuery (\ s a -> s{_redSelectSqlQuery = a});

-- | Undocumented member.
redRedshiftDatabase :: Lens' RedshiftMetadata (Maybe RedshiftDatabase)
redRedshiftDatabase = lens _redRedshiftDatabase (\ s a -> s{_redRedshiftDatabase = a});

-- | Undocumented member.
redDatabaseUserName :: Lens' RedshiftMetadata (Maybe Text)
redDatabaseUserName = lens _redDatabaseUserName (\ s a -> s{_redDatabaseUserName = a});

instance FromJSON RedshiftMetadata where
        parseJSON
          = withObject "RedshiftMetadata"
              (\ x ->
                 RedshiftMetadata' <$>
                   (x .:? "SelectSqlQuery") <*>
                     (x .:? "RedshiftDatabase")
                     <*> (x .:? "DatabaseUserName"))

-- | Describes the data specification of a 'DataSource'.
--
-- /See:/ 's3DataSpec' smart constructor.
data S3DataSpec = S3DataSpec'
    { _sdsDataSchema           :: !(Maybe Text)
    , _sdsDataSchemaLocationS3 :: !(Maybe Text)
    , _sdsDataRearrangement    :: !(Maybe Text)
    , _sdsDataLocationS3       :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'S3DataSpec' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sdsDataSchema'
--
-- * 'sdsDataSchemaLocationS3'
--
-- * 'sdsDataRearrangement'
--
-- * 'sdsDataLocationS3'
s3DataSpec
    :: Text -- ^ 'sdsDataLocationS3'
    -> S3DataSpec
s3DataSpec pDataLocationS3_ =
    S3DataSpec'
    { _sdsDataSchema = Nothing
    , _sdsDataSchemaLocationS3 = Nothing
    , _sdsDataRearrangement = Nothing
    , _sdsDataLocationS3 = pDataLocationS3_
    }

-- | Describes the schema for an Amazon S3 'DataSource'.
sdsDataSchema :: Lens' S3DataSpec (Maybe Text)
sdsDataSchema = lens _sdsDataSchema (\ s a -> s{_sdsDataSchema = a});

-- | Describes the schema Location in Amazon S3.
sdsDataSchemaLocationS3 :: Lens' S3DataSpec (Maybe Text)
sdsDataSchemaLocationS3 = lens _sdsDataSchemaLocationS3 (\ s a -> s{_sdsDataSchemaLocationS3 = a});

-- | Describes the splitting requirement of a 'Datasource'.
sdsDataRearrangement :: Lens' S3DataSpec (Maybe Text)
sdsDataRearrangement = lens _sdsDataRearrangement (\ s a -> s{_sdsDataRearrangement = a});

-- | The location of the data file(s) used by a 'DataSource'. The URI
-- specifies a data file or an Amazon Simple Storage Service (Amazon S3)
-- directory or bucket containing data files.
sdsDataLocationS3 :: Lens' S3DataSpec Text
sdsDataLocationS3 = lens _sdsDataLocationS3 (\ s a -> s{_sdsDataLocationS3 = a});

instance ToJSON S3DataSpec where
        toJSON S3DataSpec'{..}
          = object
              (catMaybes
                 [("DataSchema" .=) <$> _sdsDataSchema,
                  ("DataSchemaLocationS3" .=) <$>
                    _sdsDataSchemaLocationS3,
                  ("DataRearrangement" .=) <$> _sdsDataRearrangement,
                  Just ("DataLocationS3" .= _sdsDataLocationS3)])
