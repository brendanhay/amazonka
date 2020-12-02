{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MachineLearning.Types.MLModel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MachineLearning.Types.MLModel where

import Network.AWS.Lens
import Network.AWS.MachineLearning.Types.Algorithm
import Network.AWS.MachineLearning.Types.EntityStatus
import Network.AWS.MachineLearning.Types.MLModelType
import Network.AWS.MachineLearning.Types.RealtimeEndpointInfo
import Network.AWS.Prelude

-- | Represents the output of a @GetMLModel@ operation.
--
--
-- The content consists of the detailed metadata and the current status of the @MLModel@ .
--
--
-- /See:/ 'mLModel' smart constructor.
data MLModel = MLModel'
  { _mlmStatus :: !(Maybe EntityStatus),
    _mlmLastUpdatedAt :: !(Maybe POSIX),
    _mlmTrainingParameters :: !(Maybe (Map Text (Text))),
    _mlmScoreThresholdLastUpdatedAt :: !(Maybe POSIX),
    _mlmCreatedAt :: !(Maybe POSIX),
    _mlmComputeTime :: !(Maybe Integer),
    _mlmInputDataLocationS3 :: !(Maybe Text),
    _mlmMLModelId :: !(Maybe Text),
    _mlmSizeInBytes :: !(Maybe Integer),
    _mlmStartedAt :: !(Maybe POSIX),
    _mlmScoreThreshold :: !(Maybe Double),
    _mlmFinishedAt :: !(Maybe POSIX),
    _mlmAlgorithm :: !(Maybe Algorithm),
    _mlmCreatedByIAMUser :: !(Maybe Text),
    _mlmName :: !(Maybe Text),
    _mlmEndpointInfo :: !(Maybe RealtimeEndpointInfo),
    _mlmTrainingDataSourceId :: !(Maybe Text),
    _mlmMessage :: !(Maybe Text),
    _mlmMLModelType :: !(Maybe MLModelType)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'MLModel' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mlmStatus' - The current status of an @MLModel@ . This element can have one of the following values:      * @PENDING@ - Amazon Machine Learning (Amazon ML) submitted a request to create an @MLModel@ .    * @INPROGRESS@ - The creation process is underway.    * @FAILED@ - The request to create an @MLModel@ didn't run to completion. The model isn't usable.    * @COMPLETED@ - The creation process completed successfully.    * @DELETED@ - The @MLModel@ is marked as deleted. It isn't usable.
--
-- * 'mlmLastUpdatedAt' - The time of the most recent edit to the @MLModel@ . The time is expressed in epoch time.
--
-- * 'mlmTrainingParameters' - A list of the training parameters in the @MLModel@ . The list is implemented as a map of key-value pairs. The following is the current set of training parameters:      * @sgd.maxMLModelSizeInBytes@ - The maximum allowed size of the model. Depending on the input data, the size of the model might affect its performance. The value is an integer that ranges from @100000@ to @2147483648@ . The default value is @33554432@ .     * @sgd.maxPasses@ - The number of times that the training process traverses the observations to build the @MLModel@ . The value is an integer that ranges from @1@ to @10000@ . The default value is @10@ .     * @sgd.shuffleType@ - Whether Amazon ML shuffles the training data. Shuffling the data improves a model's ability to find the optimal solution for a variety of data types. The valid values are @auto@ and @none@ . The default value is @none@ .     * @sgd.l1RegularizationAmount@ - The coefficient regularization L1 norm, which controls overfitting the data by penalizing large coefficients. This parameter tends to drive coefficients to zero, resulting in sparse feature set. If you use this parameter, start by specifying a small value, such as @1.0E-08@ . The value is a double that ranges from @0@ to @MAX_DOUBLE@ . The default is to not use L1 normalization. This parameter can't be used when @L2@ is specified. Use this parameter sparingly.     * @sgd.l2RegularizationAmount@ - The coefficient regularization L2 norm, which controls overfitting the data by penalizing large coefficients. This tends to drive coefficients to small, nonzero values. If you use this parameter, start by specifying a small value, such as @1.0E-08@ . The value is a double that ranges from @0@ to @MAX_DOUBLE@ . The default is to not use L2 normalization. This parameter can't be used when @L1@ is specified. Use this parameter sparingly.
--
-- * 'mlmScoreThresholdLastUpdatedAt' - The time of the most recent edit to the @ScoreThreshold@ . The time is expressed in epoch time.
--
-- * 'mlmCreatedAt' - The time that the @MLModel@ was created. The time is expressed in epoch time.
--
-- * 'mlmComputeTime' - Undocumented member.
--
-- * 'mlmInputDataLocationS3' - The location of the data file or directory in Amazon Simple Storage Service (Amazon S3).
--
-- * 'mlmMLModelId' - The ID assigned to the @MLModel@ at creation.
--
-- * 'mlmSizeInBytes' - Undocumented member.
--
-- * 'mlmStartedAt' - Undocumented member.
--
-- * 'mlmScoreThreshold' - Undocumented member.
--
-- * 'mlmFinishedAt' - Undocumented member.
--
-- * 'mlmAlgorithm' - The algorithm used to train the @MLModel@ . The following algorithm is supported:     * @SGD@ -- Stochastic gradient descent. The goal of @SGD@ is to minimize the gradient of the loss function.
--
-- * 'mlmCreatedByIAMUser' - The AWS user account from which the @MLModel@ was created. The account type can be either an AWS root account or an AWS Identity and Access Management (IAM) user account.
--
-- * 'mlmName' - A user-supplied name or description of the @MLModel@ .
--
-- * 'mlmEndpointInfo' - The current endpoint of the @MLModel@ .
--
-- * 'mlmTrainingDataSourceId' - The ID of the training @DataSource@ . The @CreateMLModel@ operation uses the @TrainingDataSourceId@ .
--
-- * 'mlmMessage' - A description of the most recent details about accessing the @MLModel@ .
--
-- * 'mlmMLModelType' - Identifies the @MLModel@ category. The following are the available types:     * @REGRESSION@ - Produces a numeric result. For example, "What price should a house be listed at?"    * @BINARY@ - Produces one of two possible results. For example, "Is this a child-friendly web site?".    * @MULTICLASS@ - Produces one of several possible results. For example, "Is this a HIGH-, LOW-, or MEDIUM-risk trade?".
mLModel ::
  MLModel
mLModel =
  MLModel'
    { _mlmStatus = Nothing,
      _mlmLastUpdatedAt = Nothing,
      _mlmTrainingParameters = Nothing,
      _mlmScoreThresholdLastUpdatedAt = Nothing,
      _mlmCreatedAt = Nothing,
      _mlmComputeTime = Nothing,
      _mlmInputDataLocationS3 = Nothing,
      _mlmMLModelId = Nothing,
      _mlmSizeInBytes = Nothing,
      _mlmStartedAt = Nothing,
      _mlmScoreThreshold = Nothing,
      _mlmFinishedAt = Nothing,
      _mlmAlgorithm = Nothing,
      _mlmCreatedByIAMUser = Nothing,
      _mlmName = Nothing,
      _mlmEndpointInfo = Nothing,
      _mlmTrainingDataSourceId = Nothing,
      _mlmMessage = Nothing,
      _mlmMLModelType = Nothing
    }

-- | The current status of an @MLModel@ . This element can have one of the following values:      * @PENDING@ - Amazon Machine Learning (Amazon ML) submitted a request to create an @MLModel@ .    * @INPROGRESS@ - The creation process is underway.    * @FAILED@ - The request to create an @MLModel@ didn't run to completion. The model isn't usable.    * @COMPLETED@ - The creation process completed successfully.    * @DELETED@ - The @MLModel@ is marked as deleted. It isn't usable.
mlmStatus :: Lens' MLModel (Maybe EntityStatus)
mlmStatus = lens _mlmStatus (\s a -> s {_mlmStatus = a})

-- | The time of the most recent edit to the @MLModel@ . The time is expressed in epoch time.
mlmLastUpdatedAt :: Lens' MLModel (Maybe UTCTime)
mlmLastUpdatedAt = lens _mlmLastUpdatedAt (\s a -> s {_mlmLastUpdatedAt = a}) . mapping _Time

-- | A list of the training parameters in the @MLModel@ . The list is implemented as a map of key-value pairs. The following is the current set of training parameters:      * @sgd.maxMLModelSizeInBytes@ - The maximum allowed size of the model. Depending on the input data, the size of the model might affect its performance. The value is an integer that ranges from @100000@ to @2147483648@ . The default value is @33554432@ .     * @sgd.maxPasses@ - The number of times that the training process traverses the observations to build the @MLModel@ . The value is an integer that ranges from @1@ to @10000@ . The default value is @10@ .     * @sgd.shuffleType@ - Whether Amazon ML shuffles the training data. Shuffling the data improves a model's ability to find the optimal solution for a variety of data types. The valid values are @auto@ and @none@ . The default value is @none@ .     * @sgd.l1RegularizationAmount@ - The coefficient regularization L1 norm, which controls overfitting the data by penalizing large coefficients. This parameter tends to drive coefficients to zero, resulting in sparse feature set. If you use this parameter, start by specifying a small value, such as @1.0E-08@ . The value is a double that ranges from @0@ to @MAX_DOUBLE@ . The default is to not use L1 normalization. This parameter can't be used when @L2@ is specified. Use this parameter sparingly.     * @sgd.l2RegularizationAmount@ - The coefficient regularization L2 norm, which controls overfitting the data by penalizing large coefficients. This tends to drive coefficients to small, nonzero values. If you use this parameter, start by specifying a small value, such as @1.0E-08@ . The value is a double that ranges from @0@ to @MAX_DOUBLE@ . The default is to not use L2 normalization. This parameter can't be used when @L1@ is specified. Use this parameter sparingly.
mlmTrainingParameters :: Lens' MLModel (HashMap Text (Text))
mlmTrainingParameters = lens _mlmTrainingParameters (\s a -> s {_mlmTrainingParameters = a}) . _Default . _Map

-- | The time of the most recent edit to the @ScoreThreshold@ . The time is expressed in epoch time.
mlmScoreThresholdLastUpdatedAt :: Lens' MLModel (Maybe UTCTime)
mlmScoreThresholdLastUpdatedAt = lens _mlmScoreThresholdLastUpdatedAt (\s a -> s {_mlmScoreThresholdLastUpdatedAt = a}) . mapping _Time

-- | The time that the @MLModel@ was created. The time is expressed in epoch time.
mlmCreatedAt :: Lens' MLModel (Maybe UTCTime)
mlmCreatedAt = lens _mlmCreatedAt (\s a -> s {_mlmCreatedAt = a}) . mapping _Time

-- | Undocumented member.
mlmComputeTime :: Lens' MLModel (Maybe Integer)
mlmComputeTime = lens _mlmComputeTime (\s a -> s {_mlmComputeTime = a})

-- | The location of the data file or directory in Amazon Simple Storage Service (Amazon S3).
mlmInputDataLocationS3 :: Lens' MLModel (Maybe Text)
mlmInputDataLocationS3 = lens _mlmInputDataLocationS3 (\s a -> s {_mlmInputDataLocationS3 = a})

-- | The ID assigned to the @MLModel@ at creation.
mlmMLModelId :: Lens' MLModel (Maybe Text)
mlmMLModelId = lens _mlmMLModelId (\s a -> s {_mlmMLModelId = a})

-- | Undocumented member.
mlmSizeInBytes :: Lens' MLModel (Maybe Integer)
mlmSizeInBytes = lens _mlmSizeInBytes (\s a -> s {_mlmSizeInBytes = a})

-- | Undocumented member.
mlmStartedAt :: Lens' MLModel (Maybe UTCTime)
mlmStartedAt = lens _mlmStartedAt (\s a -> s {_mlmStartedAt = a}) . mapping _Time

-- | Undocumented member.
mlmScoreThreshold :: Lens' MLModel (Maybe Double)
mlmScoreThreshold = lens _mlmScoreThreshold (\s a -> s {_mlmScoreThreshold = a})

-- | Undocumented member.
mlmFinishedAt :: Lens' MLModel (Maybe UTCTime)
mlmFinishedAt = lens _mlmFinishedAt (\s a -> s {_mlmFinishedAt = a}) . mapping _Time

-- | The algorithm used to train the @MLModel@ . The following algorithm is supported:     * @SGD@ -- Stochastic gradient descent. The goal of @SGD@ is to minimize the gradient of the loss function.
mlmAlgorithm :: Lens' MLModel (Maybe Algorithm)
mlmAlgorithm = lens _mlmAlgorithm (\s a -> s {_mlmAlgorithm = a})

-- | The AWS user account from which the @MLModel@ was created. The account type can be either an AWS root account or an AWS Identity and Access Management (IAM) user account.
mlmCreatedByIAMUser :: Lens' MLModel (Maybe Text)
mlmCreatedByIAMUser = lens _mlmCreatedByIAMUser (\s a -> s {_mlmCreatedByIAMUser = a})

-- | A user-supplied name or description of the @MLModel@ .
mlmName :: Lens' MLModel (Maybe Text)
mlmName = lens _mlmName (\s a -> s {_mlmName = a})

-- | The current endpoint of the @MLModel@ .
mlmEndpointInfo :: Lens' MLModel (Maybe RealtimeEndpointInfo)
mlmEndpointInfo = lens _mlmEndpointInfo (\s a -> s {_mlmEndpointInfo = a})

-- | The ID of the training @DataSource@ . The @CreateMLModel@ operation uses the @TrainingDataSourceId@ .
mlmTrainingDataSourceId :: Lens' MLModel (Maybe Text)
mlmTrainingDataSourceId = lens _mlmTrainingDataSourceId (\s a -> s {_mlmTrainingDataSourceId = a})

-- | A description of the most recent details about accessing the @MLModel@ .
mlmMessage :: Lens' MLModel (Maybe Text)
mlmMessage = lens _mlmMessage (\s a -> s {_mlmMessage = a})

-- | Identifies the @MLModel@ category. The following are the available types:     * @REGRESSION@ - Produces a numeric result. For example, "What price should a house be listed at?"    * @BINARY@ - Produces one of two possible results. For example, "Is this a child-friendly web site?".    * @MULTICLASS@ - Produces one of several possible results. For example, "Is this a HIGH-, LOW-, or MEDIUM-risk trade?".
mlmMLModelType :: Lens' MLModel (Maybe MLModelType)
mlmMLModelType = lens _mlmMLModelType (\s a -> s {_mlmMLModelType = a})

instance FromJSON MLModel where
  parseJSON =
    withObject
      "MLModel"
      ( \x ->
          MLModel'
            <$> (x .:? "Status")
            <*> (x .:? "LastUpdatedAt")
            <*> (x .:? "TrainingParameters" .!= mempty)
            <*> (x .:? "ScoreThresholdLastUpdatedAt")
            <*> (x .:? "CreatedAt")
            <*> (x .:? "ComputeTime")
            <*> (x .:? "InputDataLocationS3")
            <*> (x .:? "MLModelId")
            <*> (x .:? "SizeInBytes")
            <*> (x .:? "StartedAt")
            <*> (x .:? "ScoreThreshold")
            <*> (x .:? "FinishedAt")
            <*> (x .:? "Algorithm")
            <*> (x .:? "CreatedByIamUser")
            <*> (x .:? "Name")
            <*> (x .:? "EndpointInfo")
            <*> (x .:? "TrainingDataSourceId")
            <*> (x .:? "Message")
            <*> (x .:? "MLModelType")
      )

instance Hashable MLModel

instance NFData MLModel
