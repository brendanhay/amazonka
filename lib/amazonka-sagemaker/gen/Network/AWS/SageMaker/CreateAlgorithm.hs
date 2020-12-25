{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.CreateAlgorithm
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Create a machine learning algorithm that you can use in Amazon SageMaker and list in the AWS Marketplace.
module Network.AWS.SageMaker.CreateAlgorithm
  ( -- * Creating a request
    CreateAlgorithm (..),
    mkCreateAlgorithm,

    -- ** Request lenses
    caAlgorithmName,
    caTrainingSpecification,
    caAlgorithmDescription,
    caCertifyForMarketplace,
    caInferenceSpecification,
    caValidationSpecification,

    -- * Destructuring the response
    CreateAlgorithmResponse (..),
    mkCreateAlgorithmResponse,

    -- ** Response lenses
    carfrsAlgorithmArn,
    carfrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SageMaker.Types as Types

-- | /See:/ 'mkCreateAlgorithm' smart constructor.
data CreateAlgorithm = CreateAlgorithm'
  { -- | The name of the algorithm.
    algorithmName :: Types.EntityName,
    -- | Specifies details about training jobs run by this algorithm, including the following:
    --
    --
    --     * The Amazon ECR path of the container and the version digest of the algorithm.
    --
    --
    --     * The hyperparameters that the algorithm supports.
    --
    --
    --     * The instance types that the algorithm supports for training.
    --
    --
    --     * Whether the algorithm supports distributed training.
    --
    --
    --     * The metrics that the algorithm emits to Amazon CloudWatch.
    --
    --
    --     * Which metrics that the algorithm emits can be used as the objective metric for hyperparameter tuning jobs.
    --
    --
    --     * The input channels that the algorithm supports for training data. For example, an algorithm might support @train@ , @validation@ , and @test@ channels.
    trainingSpecification :: Types.TrainingSpecification,
    -- | A description of the algorithm.
    algorithmDescription :: Core.Maybe Types.EntityDescription,
    -- | Whether to certify the algorithm so that it can be listed in AWS Marketplace.
    certifyForMarketplace :: Core.Maybe Core.Bool,
    -- | Specifies details about inference jobs that the algorithm runs, including the following:
    --
    --
    --     * The Amazon ECR paths of containers that contain the inference code and model artifacts.
    --
    --
    --     * The instance types that the algorithm supports for transform jobs and real-time endpoints used for inference.
    --
    --
    --     * The input and output content formats that the algorithm supports for inference.
    inferenceSpecification :: Core.Maybe Types.InferenceSpecification,
    -- | Specifies configurations for one or more training jobs and that Amazon SageMaker runs to test the algorithm's training code and, optionally, one or more batch transform jobs that Amazon SageMaker runs to test the algorithm's inference code.
    validationSpecification :: Core.Maybe Types.AlgorithmValidationSpecification
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateAlgorithm' value with any optional fields omitted.
mkCreateAlgorithm ::
  -- | 'algorithmName'
  Types.EntityName ->
  -- | 'trainingSpecification'
  Types.TrainingSpecification ->
  CreateAlgorithm
mkCreateAlgorithm algorithmName trainingSpecification =
  CreateAlgorithm'
    { algorithmName,
      trainingSpecification,
      algorithmDescription = Core.Nothing,
      certifyForMarketplace = Core.Nothing,
      inferenceSpecification = Core.Nothing,
      validationSpecification = Core.Nothing
    }

-- | The name of the algorithm.
--
-- /Note:/ Consider using 'algorithmName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caAlgorithmName :: Lens.Lens' CreateAlgorithm Types.EntityName
caAlgorithmName = Lens.field @"algorithmName"
{-# DEPRECATED caAlgorithmName "Use generic-lens or generic-optics with 'algorithmName' instead." #-}

-- | Specifies details about training jobs run by this algorithm, including the following:
--
--
--     * The Amazon ECR path of the container and the version digest of the algorithm.
--
--
--     * The hyperparameters that the algorithm supports.
--
--
--     * The instance types that the algorithm supports for training.
--
--
--     * Whether the algorithm supports distributed training.
--
--
--     * The metrics that the algorithm emits to Amazon CloudWatch.
--
--
--     * Which metrics that the algorithm emits can be used as the objective metric for hyperparameter tuning jobs.
--
--
--     * The input channels that the algorithm supports for training data. For example, an algorithm might support @train@ , @validation@ , and @test@ channels.
--
--
--
-- /Note:/ Consider using 'trainingSpecification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caTrainingSpecification :: Lens.Lens' CreateAlgorithm Types.TrainingSpecification
caTrainingSpecification = Lens.field @"trainingSpecification"
{-# DEPRECATED caTrainingSpecification "Use generic-lens or generic-optics with 'trainingSpecification' instead." #-}

-- | A description of the algorithm.
--
-- /Note:/ Consider using 'algorithmDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caAlgorithmDescription :: Lens.Lens' CreateAlgorithm (Core.Maybe Types.EntityDescription)
caAlgorithmDescription = Lens.field @"algorithmDescription"
{-# DEPRECATED caAlgorithmDescription "Use generic-lens or generic-optics with 'algorithmDescription' instead." #-}

-- | Whether to certify the algorithm so that it can be listed in AWS Marketplace.
--
-- /Note:/ Consider using 'certifyForMarketplace' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caCertifyForMarketplace :: Lens.Lens' CreateAlgorithm (Core.Maybe Core.Bool)
caCertifyForMarketplace = Lens.field @"certifyForMarketplace"
{-# DEPRECATED caCertifyForMarketplace "Use generic-lens or generic-optics with 'certifyForMarketplace' instead." #-}

-- | Specifies details about inference jobs that the algorithm runs, including the following:
--
--
--     * The Amazon ECR paths of containers that contain the inference code and model artifacts.
--
--
--     * The instance types that the algorithm supports for transform jobs and real-time endpoints used for inference.
--
--
--     * The input and output content formats that the algorithm supports for inference.
--
--
--
-- /Note:/ Consider using 'inferenceSpecification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caInferenceSpecification :: Lens.Lens' CreateAlgorithm (Core.Maybe Types.InferenceSpecification)
caInferenceSpecification = Lens.field @"inferenceSpecification"
{-# DEPRECATED caInferenceSpecification "Use generic-lens or generic-optics with 'inferenceSpecification' instead." #-}

-- | Specifies configurations for one or more training jobs and that Amazon SageMaker runs to test the algorithm's training code and, optionally, one or more batch transform jobs that Amazon SageMaker runs to test the algorithm's inference code.
--
-- /Note:/ Consider using 'validationSpecification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caValidationSpecification :: Lens.Lens' CreateAlgorithm (Core.Maybe Types.AlgorithmValidationSpecification)
caValidationSpecification = Lens.field @"validationSpecification"
{-# DEPRECATED caValidationSpecification "Use generic-lens or generic-optics with 'validationSpecification' instead." #-}

instance Core.FromJSON CreateAlgorithm where
  toJSON CreateAlgorithm {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("AlgorithmName" Core..= algorithmName),
            Core.Just ("TrainingSpecification" Core..= trainingSpecification),
            ("AlgorithmDescription" Core..=) Core.<$> algorithmDescription,
            ("CertifyForMarketplace" Core..=) Core.<$> certifyForMarketplace,
            ("InferenceSpecification" Core..=) Core.<$> inferenceSpecification,
            ("ValidationSpecification" Core..=)
              Core.<$> validationSpecification
          ]
      )

instance Core.AWSRequest CreateAlgorithm where
  type Rs CreateAlgorithm = CreateAlgorithmResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "SageMaker.CreateAlgorithm")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateAlgorithmResponse'
            Core.<$> (x Core..: "AlgorithmArn") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCreateAlgorithmResponse' smart constructor.
data CreateAlgorithmResponse = CreateAlgorithmResponse'
  { -- | The Amazon Resource Name (ARN) of the new algorithm.
    algorithmArn :: Types.AlgorithmArn,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateAlgorithmResponse' value with any optional fields omitted.
mkCreateAlgorithmResponse ::
  -- | 'algorithmArn'
  Types.AlgorithmArn ->
  -- | 'responseStatus'
  Core.Int ->
  CreateAlgorithmResponse
mkCreateAlgorithmResponse algorithmArn responseStatus =
  CreateAlgorithmResponse' {algorithmArn, responseStatus}

-- | The Amazon Resource Name (ARN) of the new algorithm.
--
-- /Note:/ Consider using 'algorithmArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
carfrsAlgorithmArn :: Lens.Lens' CreateAlgorithmResponse Types.AlgorithmArn
carfrsAlgorithmArn = Lens.field @"algorithmArn"
{-# DEPRECATED carfrsAlgorithmArn "Use generic-lens or generic-optics with 'algorithmArn' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
carfrsResponseStatus :: Lens.Lens' CreateAlgorithmResponse Core.Int
carfrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED carfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
