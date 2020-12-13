{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MachineLearning.GetMLModel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns an @MLModel@ that includes detailed metadata, data source information, and the current status of the @MLModel@ .
--
-- @GetMLModel@ provides results in normal or verbose format.
module Network.AWS.MachineLearning.GetMLModel
  ( -- * Creating a request
    GetMLModel (..),
    mkGetMLModel,

    -- ** Request lenses
    gmlmVerbose,
    gmlmMLModelId,

    -- * Destructuring the response
    GetMLModelResponse (..),
    mkGetMLModelResponse,

    -- ** Response lenses
    gmlmrsStatus,
    gmlmrsLastUpdatedAt,
    gmlmrsTrainingParameters,
    gmlmrsScoreThresholdLastUpdatedAt,
    gmlmrsCreatedAt,
    gmlmrsComputeTime,
    gmlmrsRecipe,
    gmlmrsInputDataLocationS3,
    gmlmrsMLModelId,
    gmlmrsSizeInBytes,
    gmlmrsSchema,
    gmlmrsStartedAt,
    gmlmrsScoreThreshold,
    gmlmrsFinishedAt,
    gmlmrsCreatedByIAMUser,
    gmlmrsName,
    gmlmrsLogURI,
    gmlmrsEndpointInfo,
    gmlmrsTrainingDataSourceId,
    gmlmrsMessage,
    gmlmrsMLModelType,
    gmlmrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MachineLearning.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetMLModel' smart constructor.
data GetMLModel = GetMLModel'
  { -- | Specifies whether the @GetMLModel@ operation should return @Recipe@ .
    --
    -- If true, @Recipe@ is returned.
    -- If false, @Recipe@ is not returned.
    verbose :: Lude.Maybe Lude.Bool,
    -- | The ID assigned to the @MLModel@ at creation.
    mLModelId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetMLModel' with the minimum fields required to make a request.
--
-- * 'verbose' - Specifies whether the @GetMLModel@ operation should return @Recipe@ .
--
-- If true, @Recipe@ is returned.
-- If false, @Recipe@ is not returned.
-- * 'mLModelId' - The ID assigned to the @MLModel@ at creation.
mkGetMLModel ::
  -- | 'mLModelId'
  Lude.Text ->
  GetMLModel
mkGetMLModel pMLModelId_ =
  GetMLModel' {verbose = Lude.Nothing, mLModelId = pMLModelId_}

-- | Specifies whether the @GetMLModel@ operation should return @Recipe@ .
--
-- If true, @Recipe@ is returned.
-- If false, @Recipe@ is not returned.
--
-- /Note:/ Consider using 'verbose' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmlmVerbose :: Lens.Lens' GetMLModel (Lude.Maybe Lude.Bool)
gmlmVerbose = Lens.lens (verbose :: GetMLModel -> Lude.Maybe Lude.Bool) (\s a -> s {verbose = a} :: GetMLModel)
{-# DEPRECATED gmlmVerbose "Use generic-lens or generic-optics with 'verbose' instead." #-}

-- | The ID assigned to the @MLModel@ at creation.
--
-- /Note:/ Consider using 'mLModelId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmlmMLModelId :: Lens.Lens' GetMLModel Lude.Text
gmlmMLModelId = Lens.lens (mLModelId :: GetMLModel -> Lude.Text) (\s a -> s {mLModelId = a} :: GetMLModel)
{-# DEPRECATED gmlmMLModelId "Use generic-lens or generic-optics with 'mLModelId' instead." #-}

instance Lude.AWSRequest GetMLModel where
  type Rs GetMLModel = GetMLModelResponse
  request = Req.postJSON machineLearningService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetMLModelResponse'
            Lude.<$> (x Lude..?> "Status")
            Lude.<*> (x Lude..?> "LastUpdatedAt")
            Lude.<*> (x Lude..?> "TrainingParameters" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "ScoreThresholdLastUpdatedAt")
            Lude.<*> (x Lude..?> "CreatedAt")
            Lude.<*> (x Lude..?> "ComputeTime")
            Lude.<*> (x Lude..?> "Recipe")
            Lude.<*> (x Lude..?> "InputDataLocationS3")
            Lude.<*> (x Lude..?> "MLModelId")
            Lude.<*> (x Lude..?> "SizeInBytes")
            Lude.<*> (x Lude..?> "Schema")
            Lude.<*> (x Lude..?> "StartedAt")
            Lude.<*> (x Lude..?> "ScoreThreshold")
            Lude.<*> (x Lude..?> "FinishedAt")
            Lude.<*> (x Lude..?> "CreatedByIamUser")
            Lude.<*> (x Lude..?> "Name")
            Lude.<*> (x Lude..?> "LogUri")
            Lude.<*> (x Lude..?> "EndpointInfo")
            Lude.<*> (x Lude..?> "TrainingDataSourceId")
            Lude.<*> (x Lude..?> "Message")
            Lude.<*> (x Lude..?> "MLModelType")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetMLModel where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AmazonML_20141212.GetMLModel" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetMLModel where
  toJSON GetMLModel' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Verbose" Lude..=) Lude.<$> verbose,
            Lude.Just ("MLModelId" Lude..= mLModelId)
          ]
      )

instance Lude.ToPath GetMLModel where
  toPath = Lude.const "/"

instance Lude.ToQuery GetMLModel where
  toQuery = Lude.const Lude.mempty

-- | Represents the output of a @GetMLModel@ operation, and provides detailed information about a @MLModel@ .
--
-- /See:/ 'mkGetMLModelResponse' smart constructor.
data GetMLModelResponse = GetMLModelResponse'
  { -- | The current status of the @MLModel@ . This element can have one of the following values:
    --
    --
    --     * @PENDING@ - Amazon Machine Learning (Amazon ML) submitted a request to describe a @MLModel@ .
    --
    --     * @INPROGRESS@ - The request is processing.
    --
    --     * @FAILED@ - The request did not run to completion. The ML model isn't usable.
    --
    --     * @COMPLETED@ - The request completed successfully.
    --
    --     * @DELETED@ - The @MLModel@ is marked as deleted. It isn't usable.
    status :: Lude.Maybe EntityStatus,
    -- | The time of the most recent edit to the @MLModel@ . The time is expressed in epoch time.
    lastUpdatedAt :: Lude.Maybe Lude.Timestamp,
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
    --     * @sgd.shuffleType@ - Whether Amazon ML shuffles the training data. Shuffling data improves a model's ability to find the optimal solution for a variety of data types. The valid values are @auto@ and @none@ . The default value is @none@ . We strongly recommend that you shuffle your data.
    --
    --
    --     * @sgd.l1RegularizationAmount@ - The coefficient regularization L1 norm. It controls overfitting the data by penalizing large coefficients. This tends to drive coefficients to zero, resulting in a sparse feature set. If you use this parameter, start by specifying a small value, such as @1.0E-08@ .
    -- The value is a double that ranges from @0@ to @MAX_DOUBLE@ . The default is to not use L1 normalization. This parameter can't be used when @L2@ is specified. Use this parameter sparingly.
    --
    --
    --     * @sgd.l2RegularizationAmount@ - The coefficient regularization L2 norm. It controls overfitting the data by penalizing large coefficients. This tends to drive coefficients to small, nonzero values. If you use this parameter, start by specifying a small value, such as @1.0E-08@ .
    -- The value is a double that ranges from @0@ to @MAX_DOUBLE@ . The default is to not use L2 normalization. This parameter can't be used when @L1@ is specified. Use this parameter sparingly.
    trainingParameters :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    -- | The time of the most recent edit to the @ScoreThreshold@ . The time is expressed in epoch time.
    scoreThresholdLastUpdatedAt :: Lude.Maybe Lude.Timestamp,
    -- | The time that the @MLModel@ was created. The time is expressed in epoch time.
    createdAt :: Lude.Maybe Lude.Timestamp,
    -- | The approximate CPU time in milliseconds that Amazon Machine Learning spent processing the @MLModel@ , normalized and scaled on computation resources. @ComputeTime@ is only available if the @MLModel@ is in the @COMPLETED@ state.
    computeTime :: Lude.Maybe Lude.Integer,
    -- | The recipe to use when training the @MLModel@ . The @Recipe@ provides detailed information about the observation data to use during training, and manipulations to perform on the observation data during training.
    recipe :: Lude.Maybe Lude.Text,
    -- | The location of the data file or directory in Amazon Simple Storage Service (Amazon S3).
    inputDataLocationS3 :: Lude.Maybe Lude.Text,
    -- | The MLModel ID, which is same as the @MLModelId@ in the request.
    mLModelId :: Lude.Maybe Lude.Text,
    sizeInBytes :: Lude.Maybe Lude.Integer,
    -- | The schema used by all of the data files referenced by the @DataSource@ .
    schema :: Lude.Maybe Lude.Text,
    -- | The epoch time when Amazon Machine Learning marked the @MLModel@ as @INPROGRESS@ . @StartedAt@ isn't available if the @MLModel@ is in the @PENDING@ state.
    startedAt :: Lude.Maybe Lude.Timestamp,
    -- | The scoring threshold is used in binary classification @MLModel@ models. It marks the boundary between a positive prediction and a negative prediction.
    --
    -- Output values greater than or equal to the threshold receive a positive result from the MLModel, such as @true@ . Output values less than the threshold receive a negative response from the MLModel, such as @false@ .
    scoreThreshold :: Lude.Maybe Lude.Double,
    -- | The epoch time when Amazon Machine Learning marked the @MLModel@ as @COMPLETED@ or @FAILED@ . @FinishedAt@ is only available when the @MLModel@ is in the @COMPLETED@ or @FAILED@ state.
    finishedAt :: Lude.Maybe Lude.Timestamp,
    -- | The AWS user account from which the @MLModel@ was created. The account type can be either an AWS root account or an AWS Identity and Access Management (IAM) user account.
    createdByIAMUser :: Lude.Maybe Lude.Text,
    -- | A user-supplied name or description of the @MLModel@ .
    name :: Lude.Maybe Lude.Text,
    -- | A link to the file that contains logs of the @CreateMLModel@ operation.
    logURI :: Lude.Maybe Lude.Text,
    -- | The current endpoint of the @MLModel@
    endpointInfo :: Lude.Maybe RealtimeEndpointInfo,
    -- | The ID of the training @DataSource@ .
    trainingDataSourceId :: Lude.Maybe Lude.Text,
    -- | A description of the most recent details about accessing the @MLModel@ .
    message :: Lude.Maybe Lude.Text,
    -- | Identifies the @MLModel@ category. The following are the available types:
    --
    --
    --     * REGRESSION -- Produces a numeric result. For example, "What price should a house be listed at?"
    --
    --     * BINARY -- Produces one of two possible results. For example, "Is this an e-commerce website?"
    --
    --     * MULTICLASS -- Produces one of several possible results. For example, "Is this a HIGH, LOW or MEDIUM risk trade?"
    mLModelType :: Lude.Maybe MLModelType,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetMLModelResponse' with the minimum fields required to make a request.
--
-- * 'status' - The current status of the @MLModel@ . This element can have one of the following values:
--
--
--     * @PENDING@ - Amazon Machine Learning (Amazon ML) submitted a request to describe a @MLModel@ .
--
--     * @INPROGRESS@ - The request is processing.
--
--     * @FAILED@ - The request did not run to completion. The ML model isn't usable.
--
--     * @COMPLETED@ - The request completed successfully.
--
--     * @DELETED@ - The @MLModel@ is marked as deleted. It isn't usable.
--
-- * 'lastUpdatedAt' - The time of the most recent edit to the @MLModel@ . The time is expressed in epoch time.
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
--     * @sgd.shuffleType@ - Whether Amazon ML shuffles the training data. Shuffling data improves a model's ability to find the optimal solution for a variety of data types. The valid values are @auto@ and @none@ . The default value is @none@ . We strongly recommend that you shuffle your data.
--
--
--     * @sgd.l1RegularizationAmount@ - The coefficient regularization L1 norm. It controls overfitting the data by penalizing large coefficients. This tends to drive coefficients to zero, resulting in a sparse feature set. If you use this parameter, start by specifying a small value, such as @1.0E-08@ .
-- The value is a double that ranges from @0@ to @MAX_DOUBLE@ . The default is to not use L1 normalization. This parameter can't be used when @L2@ is specified. Use this parameter sparingly.
--
--
--     * @sgd.l2RegularizationAmount@ - The coefficient regularization L2 norm. It controls overfitting the data by penalizing large coefficients. This tends to drive coefficients to small, nonzero values. If you use this parameter, start by specifying a small value, such as @1.0E-08@ .
-- The value is a double that ranges from @0@ to @MAX_DOUBLE@ . The default is to not use L2 normalization. This parameter can't be used when @L1@ is specified. Use this parameter sparingly.
--
--
-- * 'scoreThresholdLastUpdatedAt' - The time of the most recent edit to the @ScoreThreshold@ . The time is expressed in epoch time.
-- * 'createdAt' - The time that the @MLModel@ was created. The time is expressed in epoch time.
-- * 'computeTime' - The approximate CPU time in milliseconds that Amazon Machine Learning spent processing the @MLModel@ , normalized and scaled on computation resources. @ComputeTime@ is only available if the @MLModel@ is in the @COMPLETED@ state.
-- * 'recipe' - The recipe to use when training the @MLModel@ . The @Recipe@ provides detailed information about the observation data to use during training, and manipulations to perform on the observation data during training.
-- * 'inputDataLocationS3' - The location of the data file or directory in Amazon Simple Storage Service (Amazon S3).
-- * 'mLModelId' - The MLModel ID, which is same as the @MLModelId@ in the request.
-- * 'sizeInBytes' -
-- * 'schema' - The schema used by all of the data files referenced by the @DataSource@ .
-- * 'startedAt' - The epoch time when Amazon Machine Learning marked the @MLModel@ as @INPROGRESS@ . @StartedAt@ isn't available if the @MLModel@ is in the @PENDING@ state.
-- * 'scoreThreshold' - The scoring threshold is used in binary classification @MLModel@ models. It marks the boundary between a positive prediction and a negative prediction.
--
-- Output values greater than or equal to the threshold receive a positive result from the MLModel, such as @true@ . Output values less than the threshold receive a negative response from the MLModel, such as @false@ .
-- * 'finishedAt' - The epoch time when Amazon Machine Learning marked the @MLModel@ as @COMPLETED@ or @FAILED@ . @FinishedAt@ is only available when the @MLModel@ is in the @COMPLETED@ or @FAILED@ state.
-- * 'createdByIAMUser' - The AWS user account from which the @MLModel@ was created. The account type can be either an AWS root account or an AWS Identity and Access Management (IAM) user account.
-- * 'name' - A user-supplied name or description of the @MLModel@ .
-- * 'logURI' - A link to the file that contains logs of the @CreateMLModel@ operation.
-- * 'endpointInfo' - The current endpoint of the @MLModel@
-- * 'trainingDataSourceId' - The ID of the training @DataSource@ .
-- * 'message' - A description of the most recent details about accessing the @MLModel@ .
-- * 'mLModelType' - Identifies the @MLModel@ category. The following are the available types:
--
--
--     * REGRESSION -- Produces a numeric result. For example, "What price should a house be listed at?"
--
--     * BINARY -- Produces one of two possible results. For example, "Is this an e-commerce website?"
--
--     * MULTICLASS -- Produces one of several possible results. For example, "Is this a HIGH, LOW or MEDIUM risk trade?"
--
-- * 'responseStatus' - The response status code.
mkGetMLModelResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetMLModelResponse
mkGetMLModelResponse pResponseStatus_ =
  GetMLModelResponse'
    { status = Lude.Nothing,
      lastUpdatedAt = Lude.Nothing,
      trainingParameters = Lude.Nothing,
      scoreThresholdLastUpdatedAt = Lude.Nothing,
      createdAt = Lude.Nothing,
      computeTime = Lude.Nothing,
      recipe = Lude.Nothing,
      inputDataLocationS3 = Lude.Nothing,
      mLModelId = Lude.Nothing,
      sizeInBytes = Lude.Nothing,
      schema = Lude.Nothing,
      startedAt = Lude.Nothing,
      scoreThreshold = Lude.Nothing,
      finishedAt = Lude.Nothing,
      createdByIAMUser = Lude.Nothing,
      name = Lude.Nothing,
      logURI = Lude.Nothing,
      endpointInfo = Lude.Nothing,
      trainingDataSourceId = Lude.Nothing,
      message = Lude.Nothing,
      mLModelType = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The current status of the @MLModel@ . This element can have one of the following values:
--
--
--     * @PENDING@ - Amazon Machine Learning (Amazon ML) submitted a request to describe a @MLModel@ .
--
--     * @INPROGRESS@ - The request is processing.
--
--     * @FAILED@ - The request did not run to completion. The ML model isn't usable.
--
--     * @COMPLETED@ - The request completed successfully.
--
--     * @DELETED@ - The @MLModel@ is marked as deleted. It isn't usable.
--
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmlmrsStatus :: Lens.Lens' GetMLModelResponse (Lude.Maybe EntityStatus)
gmlmrsStatus = Lens.lens (status :: GetMLModelResponse -> Lude.Maybe EntityStatus) (\s a -> s {status = a} :: GetMLModelResponse)
{-# DEPRECATED gmlmrsStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The time of the most recent edit to the @MLModel@ . The time is expressed in epoch time.
--
-- /Note:/ Consider using 'lastUpdatedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmlmrsLastUpdatedAt :: Lens.Lens' GetMLModelResponse (Lude.Maybe Lude.Timestamp)
gmlmrsLastUpdatedAt = Lens.lens (lastUpdatedAt :: GetMLModelResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastUpdatedAt = a} :: GetMLModelResponse)
{-# DEPRECATED gmlmrsLastUpdatedAt "Use generic-lens or generic-optics with 'lastUpdatedAt' instead." #-}

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
--     * @sgd.shuffleType@ - Whether Amazon ML shuffles the training data. Shuffling data improves a model's ability to find the optimal solution for a variety of data types. The valid values are @auto@ and @none@ . The default value is @none@ . We strongly recommend that you shuffle your data.
--
--
--     * @sgd.l1RegularizationAmount@ - The coefficient regularization L1 norm. It controls overfitting the data by penalizing large coefficients. This tends to drive coefficients to zero, resulting in a sparse feature set. If you use this parameter, start by specifying a small value, such as @1.0E-08@ .
-- The value is a double that ranges from @0@ to @MAX_DOUBLE@ . The default is to not use L1 normalization. This parameter can't be used when @L2@ is specified. Use this parameter sparingly.
--
--
--     * @sgd.l2RegularizationAmount@ - The coefficient regularization L2 norm. It controls overfitting the data by penalizing large coefficients. This tends to drive coefficients to small, nonzero values. If you use this parameter, start by specifying a small value, such as @1.0E-08@ .
-- The value is a double that ranges from @0@ to @MAX_DOUBLE@ . The default is to not use L2 normalization. This parameter can't be used when @L1@ is specified. Use this parameter sparingly.
--
--
--
-- /Note:/ Consider using 'trainingParameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmlmrsTrainingParameters :: Lens.Lens' GetMLModelResponse (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
gmlmrsTrainingParameters = Lens.lens (trainingParameters :: GetMLModelResponse -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {trainingParameters = a} :: GetMLModelResponse)
{-# DEPRECATED gmlmrsTrainingParameters "Use generic-lens or generic-optics with 'trainingParameters' instead." #-}

-- | The time of the most recent edit to the @ScoreThreshold@ . The time is expressed in epoch time.
--
-- /Note:/ Consider using 'scoreThresholdLastUpdatedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmlmrsScoreThresholdLastUpdatedAt :: Lens.Lens' GetMLModelResponse (Lude.Maybe Lude.Timestamp)
gmlmrsScoreThresholdLastUpdatedAt = Lens.lens (scoreThresholdLastUpdatedAt :: GetMLModelResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {scoreThresholdLastUpdatedAt = a} :: GetMLModelResponse)
{-# DEPRECATED gmlmrsScoreThresholdLastUpdatedAt "Use generic-lens or generic-optics with 'scoreThresholdLastUpdatedAt' instead." #-}

-- | The time that the @MLModel@ was created. The time is expressed in epoch time.
--
-- /Note:/ Consider using 'createdAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmlmrsCreatedAt :: Lens.Lens' GetMLModelResponse (Lude.Maybe Lude.Timestamp)
gmlmrsCreatedAt = Lens.lens (createdAt :: GetMLModelResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {createdAt = a} :: GetMLModelResponse)
{-# DEPRECATED gmlmrsCreatedAt "Use generic-lens or generic-optics with 'createdAt' instead." #-}

-- | The approximate CPU time in milliseconds that Amazon Machine Learning spent processing the @MLModel@ , normalized and scaled on computation resources. @ComputeTime@ is only available if the @MLModel@ is in the @COMPLETED@ state.
--
-- /Note:/ Consider using 'computeTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmlmrsComputeTime :: Lens.Lens' GetMLModelResponse (Lude.Maybe Lude.Integer)
gmlmrsComputeTime = Lens.lens (computeTime :: GetMLModelResponse -> Lude.Maybe Lude.Integer) (\s a -> s {computeTime = a} :: GetMLModelResponse)
{-# DEPRECATED gmlmrsComputeTime "Use generic-lens or generic-optics with 'computeTime' instead." #-}

-- | The recipe to use when training the @MLModel@ . The @Recipe@ provides detailed information about the observation data to use during training, and manipulations to perform on the observation data during training.
--
-- /Note:/ Consider using 'recipe' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmlmrsRecipe :: Lens.Lens' GetMLModelResponse (Lude.Maybe Lude.Text)
gmlmrsRecipe = Lens.lens (recipe :: GetMLModelResponse -> Lude.Maybe Lude.Text) (\s a -> s {recipe = a} :: GetMLModelResponse)
{-# DEPRECATED gmlmrsRecipe "Use generic-lens or generic-optics with 'recipe' instead." #-}

-- | The location of the data file or directory in Amazon Simple Storage Service (Amazon S3).
--
-- /Note:/ Consider using 'inputDataLocationS3' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmlmrsInputDataLocationS3 :: Lens.Lens' GetMLModelResponse (Lude.Maybe Lude.Text)
gmlmrsInputDataLocationS3 = Lens.lens (inputDataLocationS3 :: GetMLModelResponse -> Lude.Maybe Lude.Text) (\s a -> s {inputDataLocationS3 = a} :: GetMLModelResponse)
{-# DEPRECATED gmlmrsInputDataLocationS3 "Use generic-lens or generic-optics with 'inputDataLocationS3' instead." #-}

-- | The MLModel ID, which is same as the @MLModelId@ in the request.
--
-- /Note:/ Consider using 'mLModelId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmlmrsMLModelId :: Lens.Lens' GetMLModelResponse (Lude.Maybe Lude.Text)
gmlmrsMLModelId = Lens.lens (mLModelId :: GetMLModelResponse -> Lude.Maybe Lude.Text) (\s a -> s {mLModelId = a} :: GetMLModelResponse)
{-# DEPRECATED gmlmrsMLModelId "Use generic-lens or generic-optics with 'mLModelId' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'sizeInBytes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmlmrsSizeInBytes :: Lens.Lens' GetMLModelResponse (Lude.Maybe Lude.Integer)
gmlmrsSizeInBytes = Lens.lens (sizeInBytes :: GetMLModelResponse -> Lude.Maybe Lude.Integer) (\s a -> s {sizeInBytes = a} :: GetMLModelResponse)
{-# DEPRECATED gmlmrsSizeInBytes "Use generic-lens or generic-optics with 'sizeInBytes' instead." #-}

-- | The schema used by all of the data files referenced by the @DataSource@ .
--
-- /Note:/ Consider using 'schema' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmlmrsSchema :: Lens.Lens' GetMLModelResponse (Lude.Maybe Lude.Text)
gmlmrsSchema = Lens.lens (schema :: GetMLModelResponse -> Lude.Maybe Lude.Text) (\s a -> s {schema = a} :: GetMLModelResponse)
{-# DEPRECATED gmlmrsSchema "Use generic-lens or generic-optics with 'schema' instead." #-}

-- | The epoch time when Amazon Machine Learning marked the @MLModel@ as @INPROGRESS@ . @StartedAt@ isn't available if the @MLModel@ is in the @PENDING@ state.
--
-- /Note:/ Consider using 'startedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmlmrsStartedAt :: Lens.Lens' GetMLModelResponse (Lude.Maybe Lude.Timestamp)
gmlmrsStartedAt = Lens.lens (startedAt :: GetMLModelResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {startedAt = a} :: GetMLModelResponse)
{-# DEPRECATED gmlmrsStartedAt "Use generic-lens or generic-optics with 'startedAt' instead." #-}

-- | The scoring threshold is used in binary classification @MLModel@ models. It marks the boundary between a positive prediction and a negative prediction.
--
-- Output values greater than or equal to the threshold receive a positive result from the MLModel, such as @true@ . Output values less than the threshold receive a negative response from the MLModel, such as @false@ .
--
-- /Note:/ Consider using 'scoreThreshold' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmlmrsScoreThreshold :: Lens.Lens' GetMLModelResponse (Lude.Maybe Lude.Double)
gmlmrsScoreThreshold = Lens.lens (scoreThreshold :: GetMLModelResponse -> Lude.Maybe Lude.Double) (\s a -> s {scoreThreshold = a} :: GetMLModelResponse)
{-# DEPRECATED gmlmrsScoreThreshold "Use generic-lens or generic-optics with 'scoreThreshold' instead." #-}

-- | The epoch time when Amazon Machine Learning marked the @MLModel@ as @COMPLETED@ or @FAILED@ . @FinishedAt@ is only available when the @MLModel@ is in the @COMPLETED@ or @FAILED@ state.
--
-- /Note:/ Consider using 'finishedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmlmrsFinishedAt :: Lens.Lens' GetMLModelResponse (Lude.Maybe Lude.Timestamp)
gmlmrsFinishedAt = Lens.lens (finishedAt :: GetMLModelResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {finishedAt = a} :: GetMLModelResponse)
{-# DEPRECATED gmlmrsFinishedAt "Use generic-lens or generic-optics with 'finishedAt' instead." #-}

-- | The AWS user account from which the @MLModel@ was created. The account type can be either an AWS root account or an AWS Identity and Access Management (IAM) user account.
--
-- /Note:/ Consider using 'createdByIAMUser' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmlmrsCreatedByIAMUser :: Lens.Lens' GetMLModelResponse (Lude.Maybe Lude.Text)
gmlmrsCreatedByIAMUser = Lens.lens (createdByIAMUser :: GetMLModelResponse -> Lude.Maybe Lude.Text) (\s a -> s {createdByIAMUser = a} :: GetMLModelResponse)
{-# DEPRECATED gmlmrsCreatedByIAMUser "Use generic-lens or generic-optics with 'createdByIAMUser' instead." #-}

-- | A user-supplied name or description of the @MLModel@ .
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmlmrsName :: Lens.Lens' GetMLModelResponse (Lude.Maybe Lude.Text)
gmlmrsName = Lens.lens (name :: GetMLModelResponse -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: GetMLModelResponse)
{-# DEPRECATED gmlmrsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | A link to the file that contains logs of the @CreateMLModel@ operation.
--
-- /Note:/ Consider using 'logURI' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmlmrsLogURI :: Lens.Lens' GetMLModelResponse (Lude.Maybe Lude.Text)
gmlmrsLogURI = Lens.lens (logURI :: GetMLModelResponse -> Lude.Maybe Lude.Text) (\s a -> s {logURI = a} :: GetMLModelResponse)
{-# DEPRECATED gmlmrsLogURI "Use generic-lens or generic-optics with 'logURI' instead." #-}

-- | The current endpoint of the @MLModel@
--
-- /Note:/ Consider using 'endpointInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmlmrsEndpointInfo :: Lens.Lens' GetMLModelResponse (Lude.Maybe RealtimeEndpointInfo)
gmlmrsEndpointInfo = Lens.lens (endpointInfo :: GetMLModelResponse -> Lude.Maybe RealtimeEndpointInfo) (\s a -> s {endpointInfo = a} :: GetMLModelResponse)
{-# DEPRECATED gmlmrsEndpointInfo "Use generic-lens or generic-optics with 'endpointInfo' instead." #-}

-- | The ID of the training @DataSource@ .
--
-- /Note:/ Consider using 'trainingDataSourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmlmrsTrainingDataSourceId :: Lens.Lens' GetMLModelResponse (Lude.Maybe Lude.Text)
gmlmrsTrainingDataSourceId = Lens.lens (trainingDataSourceId :: GetMLModelResponse -> Lude.Maybe Lude.Text) (\s a -> s {trainingDataSourceId = a} :: GetMLModelResponse)
{-# DEPRECATED gmlmrsTrainingDataSourceId "Use generic-lens or generic-optics with 'trainingDataSourceId' instead." #-}

-- | A description of the most recent details about accessing the @MLModel@ .
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmlmrsMessage :: Lens.Lens' GetMLModelResponse (Lude.Maybe Lude.Text)
gmlmrsMessage = Lens.lens (message :: GetMLModelResponse -> Lude.Maybe Lude.Text) (\s a -> s {message = a} :: GetMLModelResponse)
{-# DEPRECATED gmlmrsMessage "Use generic-lens or generic-optics with 'message' instead." #-}

-- | Identifies the @MLModel@ category. The following are the available types:
--
--
--     * REGRESSION -- Produces a numeric result. For example, "What price should a house be listed at?"
--
--     * BINARY -- Produces one of two possible results. For example, "Is this an e-commerce website?"
--
--     * MULTICLASS -- Produces one of several possible results. For example, "Is this a HIGH, LOW or MEDIUM risk trade?"
--
--
-- /Note:/ Consider using 'mLModelType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmlmrsMLModelType :: Lens.Lens' GetMLModelResponse (Lude.Maybe MLModelType)
gmlmrsMLModelType = Lens.lens (mLModelType :: GetMLModelResponse -> Lude.Maybe MLModelType) (\s a -> s {mLModelType = a} :: GetMLModelResponse)
{-# DEPRECATED gmlmrsMLModelType "Use generic-lens or generic-optics with 'mLModelType' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmlmrsResponseStatus :: Lens.Lens' GetMLModelResponse Lude.Int
gmlmrsResponseStatus = Lens.lens (responseStatus :: GetMLModelResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetMLModelResponse)
{-# DEPRECATED gmlmrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
