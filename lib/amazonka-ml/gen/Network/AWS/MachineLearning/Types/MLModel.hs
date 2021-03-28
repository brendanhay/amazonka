{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MachineLearning.Types.MLModel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MachineLearning.Types.MLModel
  ( MLModel (..)
  -- * Smart constructor
  , mkMLModel
  -- * Lenses
  , mlmAlgorithm
  , mlmComputeTime
  , mlmCreatedAt
  , mlmCreatedByIamUser
  , mlmEndpointInfo
  , mlmFinishedAt
  , mlmInputDataLocationS3
  , mlmLastUpdatedAt
  , mlmMLModelId
  , mlmMLModelType
  , mlmMessage
  , mlmName
  , mlmScoreThreshold
  , mlmScoreThresholdLastUpdatedAt
  , mlmSizeInBytes
  , mlmStartedAt
  , mlmStatus
  , mlmTrainingDataSourceId
  , mlmTrainingParameters
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MachineLearning.Types.Algorithm as Types
import qualified Network.AWS.MachineLearning.Types.CreatedByIamUser as Types
import qualified Network.AWS.MachineLearning.Types.EntityStatus as Types
import qualified Network.AWS.MachineLearning.Types.InputDataLocationS3 as Types
import qualified Network.AWS.MachineLearning.Types.MLModelId as Types
import qualified Network.AWS.MachineLearning.Types.MLModelType as Types
import qualified Network.AWS.MachineLearning.Types.Message as Types
import qualified Network.AWS.MachineLearning.Types.Name as Types
import qualified Network.AWS.MachineLearning.Types.RealtimeEndpointInfo as Types
import qualified Network.AWS.MachineLearning.Types.StringType as Types
import qualified Network.AWS.MachineLearning.Types.TrainingDataSourceId as Types
import qualified Network.AWS.Prelude as Core

-- | Represents the output of a @GetMLModel@ operation. 
--
-- The content consists of the detailed metadata and the current status of the @MLModel@ .
--
-- /See:/ 'mkMLModel' smart constructor.
data MLModel = MLModel'
  { algorithm :: Core.Maybe Types.Algorithm
    -- ^ The algorithm used to train the @MLModel@ . The following algorithm is supported:
--
--
--     * @SGD@ -- Stochastic gradient descent. The goal of @SGD@ is to minimize the gradient of the loss function. 
--
  , computeTime :: Core.Maybe Core.Integer
  , createdAt :: Core.Maybe Core.NominalDiffTime
    -- ^ The time that the @MLModel@ was created. The time is expressed in epoch time.
  , createdByIamUser :: Core.Maybe Types.CreatedByIamUser
    -- ^ The AWS user account from which the @MLModel@ was created. The account type can be either an AWS root account or an AWS Identity and Access Management (IAM) user account.
  , endpointInfo :: Core.Maybe Types.RealtimeEndpointInfo
    -- ^ The current endpoint of the @MLModel@ .
  , finishedAt :: Core.Maybe Core.NominalDiffTime
  , inputDataLocationS3 :: Core.Maybe Types.InputDataLocationS3
    -- ^ The location of the data file or directory in Amazon Simple Storage Service (Amazon S3).
  , lastUpdatedAt :: Core.Maybe Core.NominalDiffTime
    -- ^ The time of the most recent edit to the @MLModel@ . The time is expressed in epoch time.
  , mLModelId :: Core.Maybe Types.MLModelId
    -- ^ The ID assigned to the @MLModel@ at creation.
  , mLModelType :: Core.Maybe Types.MLModelType
    -- ^ Identifies the @MLModel@ category. The following are the available types:
--
--
--     * @REGRESSION@ - Produces a numeric result. For example, "What price should a house be listed at?"
--
--     * @BINARY@ - Produces one of two possible results. For example, "Is this a child-friendly web site?".
--
--     * @MULTICLASS@ - Produces one of several possible results. For example, "Is this a HIGH-, LOW-, or MEDIUM-risk trade?".
--
  , message :: Core.Maybe Types.Message
    -- ^ A description of the most recent details about accessing the @MLModel@ .
  , name :: Core.Maybe Types.Name
    -- ^ A user-supplied name or description of the @MLModel@ .
  , scoreThreshold :: Core.Maybe Core.Double
  , scoreThresholdLastUpdatedAt :: Core.Maybe Core.NominalDiffTime
    -- ^ The time of the most recent edit to the @ScoreThreshold@ . The time is expressed in epoch time.
  , sizeInBytes :: Core.Maybe Core.Integer
  , startedAt :: Core.Maybe Core.NominalDiffTime
  , status :: Core.Maybe Types.EntityStatus
    -- ^ The current status of an @MLModel@ . This element can have one of the following values: 
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
  , trainingDataSourceId :: Core.Maybe Types.TrainingDataSourceId
    -- ^ The ID of the training @DataSource@ . The @CreateMLModel@ operation uses the @TrainingDataSourceId@ .
  , trainingParameters :: Core.Maybe (Core.HashMap Types.StringType Types.StringType)
    -- ^ A list of the training parameters in the @MLModel@ . The list is implemented as a map of key-value pairs.
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
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'MLModel' value with any optional fields omitted.
mkMLModel
    :: MLModel
mkMLModel
  = MLModel'{algorithm = Core.Nothing, computeTime = Core.Nothing,
             createdAt = Core.Nothing, createdByIamUser = Core.Nothing,
             endpointInfo = Core.Nothing, finishedAt = Core.Nothing,
             inputDataLocationS3 = Core.Nothing, lastUpdatedAt = Core.Nothing,
             mLModelId = Core.Nothing, mLModelType = Core.Nothing,
             message = Core.Nothing, name = Core.Nothing,
             scoreThreshold = Core.Nothing,
             scoreThresholdLastUpdatedAt = Core.Nothing,
             sizeInBytes = Core.Nothing, startedAt = Core.Nothing,
             status = Core.Nothing, trainingDataSourceId = Core.Nothing,
             trainingParameters = Core.Nothing}

-- | The algorithm used to train the @MLModel@ . The following algorithm is supported:
--
--
--     * @SGD@ -- Stochastic gradient descent. The goal of @SGD@ is to minimize the gradient of the loss function. 
--
--
-- /Note:/ Consider using 'algorithm' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mlmAlgorithm :: Lens.Lens' MLModel (Core.Maybe Types.Algorithm)
mlmAlgorithm = Lens.field @"algorithm"
{-# INLINEABLE mlmAlgorithm #-}
{-# DEPRECATED algorithm "Use generic-lens or generic-optics with 'algorithm' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'computeTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mlmComputeTime :: Lens.Lens' MLModel (Core.Maybe Core.Integer)
mlmComputeTime = Lens.field @"computeTime"
{-# INLINEABLE mlmComputeTime #-}
{-# DEPRECATED computeTime "Use generic-lens or generic-optics with 'computeTime' instead"  #-}

-- | The time that the @MLModel@ was created. The time is expressed in epoch time.
--
-- /Note:/ Consider using 'createdAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mlmCreatedAt :: Lens.Lens' MLModel (Core.Maybe Core.NominalDiffTime)
mlmCreatedAt = Lens.field @"createdAt"
{-# INLINEABLE mlmCreatedAt #-}
{-# DEPRECATED createdAt "Use generic-lens or generic-optics with 'createdAt' instead"  #-}

-- | The AWS user account from which the @MLModel@ was created. The account type can be either an AWS root account or an AWS Identity and Access Management (IAM) user account.
--
-- /Note:/ Consider using 'createdByIamUser' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mlmCreatedByIamUser :: Lens.Lens' MLModel (Core.Maybe Types.CreatedByIamUser)
mlmCreatedByIamUser = Lens.field @"createdByIamUser"
{-# INLINEABLE mlmCreatedByIamUser #-}
{-# DEPRECATED createdByIamUser "Use generic-lens or generic-optics with 'createdByIamUser' instead"  #-}

-- | The current endpoint of the @MLModel@ .
--
-- /Note:/ Consider using 'endpointInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mlmEndpointInfo :: Lens.Lens' MLModel (Core.Maybe Types.RealtimeEndpointInfo)
mlmEndpointInfo = Lens.field @"endpointInfo"
{-# INLINEABLE mlmEndpointInfo #-}
{-# DEPRECATED endpointInfo "Use generic-lens or generic-optics with 'endpointInfo' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'finishedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mlmFinishedAt :: Lens.Lens' MLModel (Core.Maybe Core.NominalDiffTime)
mlmFinishedAt = Lens.field @"finishedAt"
{-# INLINEABLE mlmFinishedAt #-}
{-# DEPRECATED finishedAt "Use generic-lens or generic-optics with 'finishedAt' instead"  #-}

-- | The location of the data file or directory in Amazon Simple Storage Service (Amazon S3).
--
-- /Note:/ Consider using 'inputDataLocationS3' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mlmInputDataLocationS3 :: Lens.Lens' MLModel (Core.Maybe Types.InputDataLocationS3)
mlmInputDataLocationS3 = Lens.field @"inputDataLocationS3"
{-# INLINEABLE mlmInputDataLocationS3 #-}
{-# DEPRECATED inputDataLocationS3 "Use generic-lens or generic-optics with 'inputDataLocationS3' instead"  #-}

-- | The time of the most recent edit to the @MLModel@ . The time is expressed in epoch time.
--
-- /Note:/ Consider using 'lastUpdatedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mlmLastUpdatedAt :: Lens.Lens' MLModel (Core.Maybe Core.NominalDiffTime)
mlmLastUpdatedAt = Lens.field @"lastUpdatedAt"
{-# INLINEABLE mlmLastUpdatedAt #-}
{-# DEPRECATED lastUpdatedAt "Use generic-lens or generic-optics with 'lastUpdatedAt' instead"  #-}

-- | The ID assigned to the @MLModel@ at creation.
--
-- /Note:/ Consider using 'mLModelId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mlmMLModelId :: Lens.Lens' MLModel (Core.Maybe Types.MLModelId)
mlmMLModelId = Lens.field @"mLModelId"
{-# INLINEABLE mlmMLModelId #-}
{-# DEPRECATED mLModelId "Use generic-lens or generic-optics with 'mLModelId' instead"  #-}

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
mlmMLModelType :: Lens.Lens' MLModel (Core.Maybe Types.MLModelType)
mlmMLModelType = Lens.field @"mLModelType"
{-# INLINEABLE mlmMLModelType #-}
{-# DEPRECATED mLModelType "Use generic-lens or generic-optics with 'mLModelType' instead"  #-}

-- | A description of the most recent details about accessing the @MLModel@ .
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mlmMessage :: Lens.Lens' MLModel (Core.Maybe Types.Message)
mlmMessage = Lens.field @"message"
{-# INLINEABLE mlmMessage #-}
{-# DEPRECATED message "Use generic-lens or generic-optics with 'message' instead"  #-}

-- | A user-supplied name or description of the @MLModel@ .
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mlmName :: Lens.Lens' MLModel (Core.Maybe Types.Name)
mlmName = Lens.field @"name"
{-# INLINEABLE mlmName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'scoreThreshold' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mlmScoreThreshold :: Lens.Lens' MLModel (Core.Maybe Core.Double)
mlmScoreThreshold = Lens.field @"scoreThreshold"
{-# INLINEABLE mlmScoreThreshold #-}
{-# DEPRECATED scoreThreshold "Use generic-lens or generic-optics with 'scoreThreshold' instead"  #-}

-- | The time of the most recent edit to the @ScoreThreshold@ . The time is expressed in epoch time.
--
-- /Note:/ Consider using 'scoreThresholdLastUpdatedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mlmScoreThresholdLastUpdatedAt :: Lens.Lens' MLModel (Core.Maybe Core.NominalDiffTime)
mlmScoreThresholdLastUpdatedAt = Lens.field @"scoreThresholdLastUpdatedAt"
{-# INLINEABLE mlmScoreThresholdLastUpdatedAt #-}
{-# DEPRECATED scoreThresholdLastUpdatedAt "Use generic-lens or generic-optics with 'scoreThresholdLastUpdatedAt' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'sizeInBytes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mlmSizeInBytes :: Lens.Lens' MLModel (Core.Maybe Core.Integer)
mlmSizeInBytes = Lens.field @"sizeInBytes"
{-# INLINEABLE mlmSizeInBytes #-}
{-# DEPRECATED sizeInBytes "Use generic-lens or generic-optics with 'sizeInBytes' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'startedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mlmStartedAt :: Lens.Lens' MLModel (Core.Maybe Core.NominalDiffTime)
mlmStartedAt = Lens.field @"startedAt"
{-# INLINEABLE mlmStartedAt #-}
{-# DEPRECATED startedAt "Use generic-lens or generic-optics with 'startedAt' instead"  #-}

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
mlmStatus :: Lens.Lens' MLModel (Core.Maybe Types.EntityStatus)
mlmStatus = Lens.field @"status"
{-# INLINEABLE mlmStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

-- | The ID of the training @DataSource@ . The @CreateMLModel@ operation uses the @TrainingDataSourceId@ .
--
-- /Note:/ Consider using 'trainingDataSourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mlmTrainingDataSourceId :: Lens.Lens' MLModel (Core.Maybe Types.TrainingDataSourceId)
mlmTrainingDataSourceId = Lens.field @"trainingDataSourceId"
{-# INLINEABLE mlmTrainingDataSourceId #-}
{-# DEPRECATED trainingDataSourceId "Use generic-lens or generic-optics with 'trainingDataSourceId' instead"  #-}

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
mlmTrainingParameters :: Lens.Lens' MLModel (Core.Maybe (Core.HashMap Types.StringType Types.StringType))
mlmTrainingParameters = Lens.field @"trainingParameters"
{-# INLINEABLE mlmTrainingParameters #-}
{-# DEPRECATED trainingParameters "Use generic-lens or generic-optics with 'trainingParameters' instead"  #-}

instance Core.FromJSON MLModel where
        parseJSON
          = Core.withObject "MLModel" Core.$
              \ x ->
                MLModel' Core.<$>
                  (x Core..:? "Algorithm") Core.<*> x Core..:? "ComputeTime" Core.<*>
                    x Core..:? "CreatedAt"
                    Core.<*> x Core..:? "CreatedByIamUser"
                    Core.<*> x Core..:? "EndpointInfo"
                    Core.<*> x Core..:? "FinishedAt"
                    Core.<*> x Core..:? "InputDataLocationS3"
                    Core.<*> x Core..:? "LastUpdatedAt"
                    Core.<*> x Core..:? "MLModelId"
                    Core.<*> x Core..:? "MLModelType"
                    Core.<*> x Core..:? "Message"
                    Core.<*> x Core..:? "Name"
                    Core.<*> x Core..:? "ScoreThreshold"
                    Core.<*> x Core..:? "ScoreThresholdLastUpdatedAt"
                    Core.<*> x Core..:? "SizeInBytes"
                    Core.<*> x Core..:? "StartedAt"
                    Core.<*> x Core..:? "Status"
                    Core.<*> x Core..:? "TrainingDataSourceId"
                    Core.<*> x Core..:? "TrainingParameters"
