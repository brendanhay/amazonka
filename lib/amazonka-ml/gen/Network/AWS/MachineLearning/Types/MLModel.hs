{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MachineLearning.Types.MLModel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MachineLearning.Types.MLModel
  ( MLModel (..),

    -- * Smart constructor
    mkMLModel,

    -- * Lenses
    mlmStatus,
    mlmLastUpdatedAt,
    mlmTrainingParameters,
    mlmScoreThresholdLastUpdatedAt,
    mlmCreatedAt,
    mlmComputeTime,
    mlmInputDataLocationS3,
    mlmMLModelId,
    mlmSizeInBytes,
    mlmStartedAt,
    mlmScoreThreshold,
    mlmFinishedAt,
    mlmAlgorithm,
    mlmCreatedByIAMUser,
    mlmName,
    mlmEndpointInfo,
    mlmTrainingDataSourceId,
    mlmMessage,
    mlmMLModelType,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MachineLearning.Types.Algorithm
import Network.AWS.MachineLearning.Types.EntityStatus
import Network.AWS.MachineLearning.Types.MLModelType
import Network.AWS.MachineLearning.Types.RealtimeEndpointInfo
import qualified Network.AWS.Prelude as Lude

-- | Represents the output of a @GetMLModel@ operation.
--
-- The content consists of the detailed metadata and the current status of the @MLModel@ .
--
-- /See:/ 'mkMLModel' smart constructor.
data MLModel = MLModel'
  { status :: Lude.Maybe EntityStatus,
    lastUpdatedAt :: Lude.Maybe Lude.Timestamp,
    trainingParameters ::
      Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    scoreThresholdLastUpdatedAt :: Lude.Maybe Lude.Timestamp,
    createdAt :: Lude.Maybe Lude.Timestamp,
    computeTime :: Lude.Maybe Lude.Integer,
    inputDataLocationS3 :: Lude.Maybe Lude.Text,
    mLModelId :: Lude.Maybe Lude.Text,
    sizeInBytes :: Lude.Maybe Lude.Integer,
    startedAt :: Lude.Maybe Lude.Timestamp,
    scoreThreshold :: Lude.Maybe Lude.Double,
    finishedAt :: Lude.Maybe Lude.Timestamp,
    algorithm :: Lude.Maybe Algorithm,
    createdByIAMUser :: Lude.Maybe Lude.Text,
    name :: Lude.Maybe Lude.Text,
    endpointInfo :: Lude.Maybe RealtimeEndpointInfo,
    trainingDataSourceId :: Lude.Maybe Lude.Text,
    message :: Lude.Maybe Lude.Text,
    mLModelType :: Lude.Maybe MLModelType
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'MLModel' with the minimum fields required to make a request.
--
-- * 'algorithm' - The algorithm used to train the @MLModel@ . The following algorithm is supported:
--
--
--     * @SGD@ -- Stochastic gradient descent. The goal of @SGD@ is to minimize the gradient of the loss function.
--
-- * 'computeTime' - Undocumented field.
-- * 'createdAt' - The time that the @MLModel@ was created. The time is expressed in epoch time.
-- * 'createdByIAMUser' - The AWS user account from which the @MLModel@ was created. The account type can be either an AWS root account or an AWS Identity and Access Management (IAM) user account.
-- * 'endpointInfo' - The current endpoint of the @MLModel@ .
-- * 'finishedAt' - Undocumented field.
-- * 'inputDataLocationS3' - The location of the data file or directory in Amazon Simple Storage Service (Amazon S3).
-- * 'lastUpdatedAt' - The time of the most recent edit to the @MLModel@ . The time is expressed in epoch time.
-- * 'mLModelId' - The ID assigned to the @MLModel@ at creation.
-- * 'mLModelType' - Identifies the @MLModel@ category. The following are the available types:
--
--
--     * @REGRESSION@ - Produces a numeric result. For example, "What price should a house be listed at?"
--
--     * @BINARY@ - Produces one of two possible results. For example, "Is this a child-friendly web site?".
--
--     * @MULTICLASS@ - Produces one of several possible results. For example, "Is this a HIGH-, LOW-, or MEDIUM-risk trade?".
--
-- * 'message' - A description of the most recent details about accessing the @MLModel@ .
-- * 'name' - A user-supplied name or description of the @MLModel@ .
-- * 'scoreThreshold' - Undocumented field.
-- * 'scoreThresholdLastUpdatedAt' - The time of the most recent edit to the @ScoreThreshold@ . The time is expressed in epoch time.
-- * 'sizeInBytes' - Undocumented field.
-- * 'startedAt' - Undocumented field.
-- * 'status' - The current status of an @MLModel@ . This element can have one of the following values:
--
--
--     * @PENDING@ - Amazon Machine Learning (Amazon ML) submitted a request to create an @MLModel@ .
--
--     * @INPROGRESS@ - The creation process is underway.
--
--     * @FAILED@ - The request to create an @MLModel@ didn't run to completion. The model isn't usable.
--
--     * @COMPLETED@ - The creation process completed successfully.
--
--     * @DELETED@ - The @MLModel@ is marked as deleted. It isn't usable.
--
-- * 'trainingDataSourceId' - The ID of the training @DataSource@ . The @CreateMLModel@ operation uses the @TrainingDataSourceId@ .
-- * 'trainingParameters' - A list of the training parameters in the @MLModel@ . The list is implemented as a map of key-value pairs.
--
-- The following is the current set of training parameters:
--
--     * @sgd.maxMLModelSizeInBytes@ - The maximum allowed size of the model. Depending on the input data, the size of the model might affect its performance.
-- The value is an integer that ranges from @100000@ to @2147483648@ . The default value is @33554432@ .
--
--
--     * @sgd.maxPasses@ - The number of times that the training process traverses the observations to build the @MLModel@ . The value is an integer that ranges from @1@ to @10000@ . The default value is @10@ .
--
--
--     * @sgd.shuffleType@ - Whether Amazon ML shuffles the training data. Shuffling the data improves a model's ability to find the optimal solution for a variety of data types. The valid values are @auto@ and @none@ . The default value is @none@ .
--
--
--     * @sgd.l1RegularizationAmount@ - The coefficient regularization L1 norm, which controls overfitting the data by penalizing large coefficients. This parameter tends to drive coefficients to zero, resulting in sparse feature set. If you use this parameter, start by specifying a small value, such as @1.0E-08@ .
-- The value is a double that ranges from @0@ to @MAX_DOUBLE@ . The default is to not use L1 normalization. This parameter can't be used when @L2@ is specified. Use this parameter sparingly.
--
--
--     * @sgd.l2RegularizationAmount@ - The coefficient regularization L2 norm, which controls overfitting the data by penalizing large coefficients. This tends to drive coefficients to small, nonzero values. If you use this parameter, start by specifying a small value, such as @1.0E-08@ .
-- The value is a double that ranges from @0@ to @MAX_DOUBLE@ . The default is to not use L2 normalization. This parameter can't be used when @L1@ is specified. Use this parameter sparingly.
mkMLModel ::
  MLModel
mkMLModel =
  MLModel'
    { status = Lude.Nothing,
      lastUpdatedAt = Lude.Nothing,
      trainingParameters = Lude.Nothing,
      scoreThresholdLastUpdatedAt = Lude.Nothing,
      createdAt = Lude.Nothing,
      computeTime = Lude.Nothing,
      inputDataLocationS3 = Lude.Nothing,
      mLModelId = Lude.Nothing,
      sizeInBytes = Lude.Nothing,
      startedAt = Lude.Nothing,
      scoreThreshold = Lude.Nothing,
      finishedAt = Lude.Nothing,
      algorithm = Lude.Nothing,
      createdByIAMUser = Lude.Nothing,
      name = Lude.Nothing,
      endpointInfo = Lude.Nothing,
      trainingDataSourceId = Lude.Nothing,
      message = Lude.Nothing,
      mLModelType = Lude.Nothing
    }

-- | The current status of an @MLModel@ . This element can have one of the following values:
--
--
--     * @PENDING@ - Amazon Machine Learning (Amazon ML) submitted a request to create an @MLModel@ .
--
--     * @INPROGRESS@ - The creation process is underway.
--
--     * @FAILED@ - The request to create an @MLModel@ didn't run to completion. The model isn't usable.
--
--     * @COMPLETED@ - The creation process completed successfully.
--
--     * @DELETED@ - The @MLModel@ is marked as deleted. It isn't usable.
--
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mlmStatus :: Lens.Lens' MLModel (Lude.Maybe EntityStatus)
mlmStatus = Lens.lens (status :: MLModel -> Lude.Maybe EntityStatus) (\s a -> s {status = a} :: MLModel)
{-# DEPRECATED mlmStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The time of the most recent edit to the @MLModel@ . The time is expressed in epoch time.
--
-- /Note:/ Consider using 'lastUpdatedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mlmLastUpdatedAt :: Lens.Lens' MLModel (Lude.Maybe Lude.Timestamp)
mlmLastUpdatedAt = Lens.lens (lastUpdatedAt :: MLModel -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastUpdatedAt = a} :: MLModel)
{-# DEPRECATED mlmLastUpdatedAt "Use generic-lens or generic-optics with 'lastUpdatedAt' instead." #-}

-- | A list of the training parameters in the @MLModel@ . The list is implemented as a map of key-value pairs.
--
-- The following is the current set of training parameters:
--
--     * @sgd.maxMLModelSizeInBytes@ - The maximum allowed size of the model. Depending on the input data, the size of the model might affect its performance.
-- The value is an integer that ranges from @100000@ to @2147483648@ . The default value is @33554432@ .
--
--
--     * @sgd.maxPasses@ - The number of times that the training process traverses the observations to build the @MLModel@ . The value is an integer that ranges from @1@ to @10000@ . The default value is @10@ .
--
--
--     * @sgd.shuffleType@ - Whether Amazon ML shuffles the training data. Shuffling the data improves a model's ability to find the optimal solution for a variety of data types. The valid values are @auto@ and @none@ . The default value is @none@ .
--
--
--     * @sgd.l1RegularizationAmount@ - The coefficient regularization L1 norm, which controls overfitting the data by penalizing large coefficients. This parameter tends to drive coefficients to zero, resulting in sparse feature set. If you use this parameter, start by specifying a small value, such as @1.0E-08@ .
-- The value is a double that ranges from @0@ to @MAX_DOUBLE@ . The default is to not use L1 normalization. This parameter can't be used when @L2@ is specified. Use this parameter sparingly.
--
--
--     * @sgd.l2RegularizationAmount@ - The coefficient regularization L2 norm, which controls overfitting the data by penalizing large coefficients. This tends to drive coefficients to small, nonzero values. If you use this parameter, start by specifying a small value, such as @1.0E-08@ .
-- The value is a double that ranges from @0@ to @MAX_DOUBLE@ . The default is to not use L2 normalization. This parameter can't be used when @L1@ is specified. Use this parameter sparingly.
--
--
--
-- /Note:/ Consider using 'trainingParameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mlmTrainingParameters :: Lens.Lens' MLModel (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
mlmTrainingParameters = Lens.lens (trainingParameters :: MLModel -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {trainingParameters = a} :: MLModel)
{-# DEPRECATED mlmTrainingParameters "Use generic-lens or generic-optics with 'trainingParameters' instead." #-}

-- | The time of the most recent edit to the @ScoreThreshold@ . The time is expressed in epoch time.
--
-- /Note:/ Consider using 'scoreThresholdLastUpdatedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mlmScoreThresholdLastUpdatedAt :: Lens.Lens' MLModel (Lude.Maybe Lude.Timestamp)
mlmScoreThresholdLastUpdatedAt = Lens.lens (scoreThresholdLastUpdatedAt :: MLModel -> Lude.Maybe Lude.Timestamp) (\s a -> s {scoreThresholdLastUpdatedAt = a} :: MLModel)
{-# DEPRECATED mlmScoreThresholdLastUpdatedAt "Use generic-lens or generic-optics with 'scoreThresholdLastUpdatedAt' instead." #-}

-- | The time that the @MLModel@ was created. The time is expressed in epoch time.
--
-- /Note:/ Consider using 'createdAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mlmCreatedAt :: Lens.Lens' MLModel (Lude.Maybe Lude.Timestamp)
mlmCreatedAt = Lens.lens (createdAt :: MLModel -> Lude.Maybe Lude.Timestamp) (\s a -> s {createdAt = a} :: MLModel)
{-# DEPRECATED mlmCreatedAt "Use generic-lens or generic-optics with 'createdAt' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'computeTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mlmComputeTime :: Lens.Lens' MLModel (Lude.Maybe Lude.Integer)
mlmComputeTime = Lens.lens (computeTime :: MLModel -> Lude.Maybe Lude.Integer) (\s a -> s {computeTime = a} :: MLModel)
{-# DEPRECATED mlmComputeTime "Use generic-lens or generic-optics with 'computeTime' instead." #-}

-- | The location of the data file or directory in Amazon Simple Storage Service (Amazon S3).
--
-- /Note:/ Consider using 'inputDataLocationS3' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mlmInputDataLocationS3 :: Lens.Lens' MLModel (Lude.Maybe Lude.Text)
mlmInputDataLocationS3 = Lens.lens (inputDataLocationS3 :: MLModel -> Lude.Maybe Lude.Text) (\s a -> s {inputDataLocationS3 = a} :: MLModel)
{-# DEPRECATED mlmInputDataLocationS3 "Use generic-lens or generic-optics with 'inputDataLocationS3' instead." #-}

-- | The ID assigned to the @MLModel@ at creation.
--
-- /Note:/ Consider using 'mLModelId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mlmMLModelId :: Lens.Lens' MLModel (Lude.Maybe Lude.Text)
mlmMLModelId = Lens.lens (mLModelId :: MLModel -> Lude.Maybe Lude.Text) (\s a -> s {mLModelId = a} :: MLModel)
{-# DEPRECATED mlmMLModelId "Use generic-lens or generic-optics with 'mLModelId' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'sizeInBytes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mlmSizeInBytes :: Lens.Lens' MLModel (Lude.Maybe Lude.Integer)
mlmSizeInBytes = Lens.lens (sizeInBytes :: MLModel -> Lude.Maybe Lude.Integer) (\s a -> s {sizeInBytes = a} :: MLModel)
{-# DEPRECATED mlmSizeInBytes "Use generic-lens or generic-optics with 'sizeInBytes' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'startedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mlmStartedAt :: Lens.Lens' MLModel (Lude.Maybe Lude.Timestamp)
mlmStartedAt = Lens.lens (startedAt :: MLModel -> Lude.Maybe Lude.Timestamp) (\s a -> s {startedAt = a} :: MLModel)
{-# DEPRECATED mlmStartedAt "Use generic-lens or generic-optics with 'startedAt' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'scoreThreshold' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mlmScoreThreshold :: Lens.Lens' MLModel (Lude.Maybe Lude.Double)
mlmScoreThreshold = Lens.lens (scoreThreshold :: MLModel -> Lude.Maybe Lude.Double) (\s a -> s {scoreThreshold = a} :: MLModel)
{-# DEPRECATED mlmScoreThreshold "Use generic-lens or generic-optics with 'scoreThreshold' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'finishedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mlmFinishedAt :: Lens.Lens' MLModel (Lude.Maybe Lude.Timestamp)
mlmFinishedAt = Lens.lens (finishedAt :: MLModel -> Lude.Maybe Lude.Timestamp) (\s a -> s {finishedAt = a} :: MLModel)
{-# DEPRECATED mlmFinishedAt "Use generic-lens or generic-optics with 'finishedAt' instead." #-}

-- | The algorithm used to train the @MLModel@ . The following algorithm is supported:
--
--
--     * @SGD@ -- Stochastic gradient descent. The goal of @SGD@ is to minimize the gradient of the loss function.
--
--
-- /Note:/ Consider using 'algorithm' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mlmAlgorithm :: Lens.Lens' MLModel (Lude.Maybe Algorithm)
mlmAlgorithm = Lens.lens (algorithm :: MLModel -> Lude.Maybe Algorithm) (\s a -> s {algorithm = a} :: MLModel)
{-# DEPRECATED mlmAlgorithm "Use generic-lens or generic-optics with 'algorithm' instead." #-}

-- | The AWS user account from which the @MLModel@ was created. The account type can be either an AWS root account or an AWS Identity and Access Management (IAM) user account.
--
-- /Note:/ Consider using 'createdByIAMUser' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mlmCreatedByIAMUser :: Lens.Lens' MLModel (Lude.Maybe Lude.Text)
mlmCreatedByIAMUser = Lens.lens (createdByIAMUser :: MLModel -> Lude.Maybe Lude.Text) (\s a -> s {createdByIAMUser = a} :: MLModel)
{-# DEPRECATED mlmCreatedByIAMUser "Use generic-lens or generic-optics with 'createdByIAMUser' instead." #-}

-- | A user-supplied name or description of the @MLModel@ .
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mlmName :: Lens.Lens' MLModel (Lude.Maybe Lude.Text)
mlmName = Lens.lens (name :: MLModel -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: MLModel)
{-# DEPRECATED mlmName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The current endpoint of the @MLModel@ .
--
-- /Note:/ Consider using 'endpointInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mlmEndpointInfo :: Lens.Lens' MLModel (Lude.Maybe RealtimeEndpointInfo)
mlmEndpointInfo = Lens.lens (endpointInfo :: MLModel -> Lude.Maybe RealtimeEndpointInfo) (\s a -> s {endpointInfo = a} :: MLModel)
{-# DEPRECATED mlmEndpointInfo "Use generic-lens or generic-optics with 'endpointInfo' instead." #-}

-- | The ID of the training @DataSource@ . The @CreateMLModel@ operation uses the @TrainingDataSourceId@ .
--
-- /Note:/ Consider using 'trainingDataSourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mlmTrainingDataSourceId :: Lens.Lens' MLModel (Lude.Maybe Lude.Text)
mlmTrainingDataSourceId = Lens.lens (trainingDataSourceId :: MLModel -> Lude.Maybe Lude.Text) (\s a -> s {trainingDataSourceId = a} :: MLModel)
{-# DEPRECATED mlmTrainingDataSourceId "Use generic-lens or generic-optics with 'trainingDataSourceId' instead." #-}

-- | A description of the most recent details about accessing the @MLModel@ .
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mlmMessage :: Lens.Lens' MLModel (Lude.Maybe Lude.Text)
mlmMessage = Lens.lens (message :: MLModel -> Lude.Maybe Lude.Text) (\s a -> s {message = a} :: MLModel)
{-# DEPRECATED mlmMessage "Use generic-lens or generic-optics with 'message' instead." #-}

-- | Identifies the @MLModel@ category. The following are the available types:
--
--
--     * @REGRESSION@ - Produces a numeric result. For example, "What price should a house be listed at?"
--
--     * @BINARY@ - Produces one of two possible results. For example, "Is this a child-friendly web site?".
--
--     * @MULTICLASS@ - Produces one of several possible results. For example, "Is this a HIGH-, LOW-, or MEDIUM-risk trade?".
--
--
-- /Note:/ Consider using 'mLModelType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mlmMLModelType :: Lens.Lens' MLModel (Lude.Maybe MLModelType)
mlmMLModelType = Lens.lens (mLModelType :: MLModel -> Lude.Maybe MLModelType) (\s a -> s {mLModelType = a} :: MLModel)
{-# DEPRECATED mlmMLModelType "Use generic-lens or generic-optics with 'mLModelType' instead." #-}

instance Lude.FromJSON MLModel where
  parseJSON =
    Lude.withObject
      "MLModel"
      ( \x ->
          MLModel'
            Lude.<$> (x Lude..:? "Status")
            Lude.<*> (x Lude..:? "LastUpdatedAt")
            Lude.<*> (x Lude..:? "TrainingParameters" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "ScoreThresholdLastUpdatedAt")
            Lude.<*> (x Lude..:? "CreatedAt")
            Lude.<*> (x Lude..:? "ComputeTime")
            Lude.<*> (x Lude..:? "InputDataLocationS3")
            Lude.<*> (x Lude..:? "MLModelId")
            Lude.<*> (x Lude..:? "SizeInBytes")
            Lude.<*> (x Lude..:? "StartedAt")
            Lude.<*> (x Lude..:? "ScoreThreshold")
            Lude.<*> (x Lude..:? "FinishedAt")
            Lude.<*> (x Lude..:? "Algorithm")
            Lude.<*> (x Lude..:? "CreatedByIamUser")
            Lude.<*> (x Lude..:? "Name")
            Lude.<*> (x Lude..:? "EndpointInfo")
            Lude.<*> (x Lude..:? "TrainingDataSourceId")
            Lude.<*> (x Lude..:? "Message")
            Lude.<*> (x Lude..:? "MLModelType")
      )
