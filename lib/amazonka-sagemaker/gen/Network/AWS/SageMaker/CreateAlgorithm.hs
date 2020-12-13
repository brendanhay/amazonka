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
    caTrainingSpecification,
    caAlgorithmName,
    caValidationSpecification,
    caInferenceSpecification,
    caAlgorithmDescription,
    caCertifyForMarketplace,

    -- * Destructuring the response
    CreateAlgorithmResponse (..),
    mkCreateAlgorithmResponse,

    -- ** Response lenses
    crsAlgorithmARN,
    crsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SageMaker.Types

-- | /See:/ 'mkCreateAlgorithm' smart constructor.
data CreateAlgorithm = CreateAlgorithm'
  { -- | Specifies details about training jobs run by this algorithm, including the following:
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
    trainingSpecification :: TrainingSpecification,
    -- | The name of the algorithm.
    algorithmName :: Lude.Text,
    -- | Specifies configurations for one or more training jobs and that Amazon SageMaker runs to test the algorithm's training code and, optionally, one or more batch transform jobs that Amazon SageMaker runs to test the algorithm's inference code.
    validationSpecification :: Lude.Maybe AlgorithmValidationSpecification,
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
    inferenceSpecification :: Lude.Maybe InferenceSpecification,
    -- | A description of the algorithm.
    algorithmDescription :: Lude.Maybe Lude.Text,
    -- | Whether to certify the algorithm so that it can be listed in AWS Marketplace.
    certifyForMarketplace :: Lude.Maybe Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateAlgorithm' with the minimum fields required to make a request.
--
-- * 'trainingSpecification' - Specifies details about training jobs run by this algorithm, including the following:
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
-- * 'algorithmName' - The name of the algorithm.
-- * 'validationSpecification' - Specifies configurations for one or more training jobs and that Amazon SageMaker runs to test the algorithm's training code and, optionally, one or more batch transform jobs that Amazon SageMaker runs to test the algorithm's inference code.
-- * 'inferenceSpecification' - Specifies details about inference jobs that the algorithm runs, including the following:
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
-- * 'algorithmDescription' - A description of the algorithm.
-- * 'certifyForMarketplace' - Whether to certify the algorithm so that it can be listed in AWS Marketplace.
mkCreateAlgorithm ::
  -- | 'trainingSpecification'
  TrainingSpecification ->
  -- | 'algorithmName'
  Lude.Text ->
  CreateAlgorithm
mkCreateAlgorithm pTrainingSpecification_ pAlgorithmName_ =
  CreateAlgorithm'
    { trainingSpecification = pTrainingSpecification_,
      algorithmName = pAlgorithmName_,
      validationSpecification = Lude.Nothing,
      inferenceSpecification = Lude.Nothing,
      algorithmDescription = Lude.Nothing,
      certifyForMarketplace = Lude.Nothing
    }

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
caTrainingSpecification :: Lens.Lens' CreateAlgorithm TrainingSpecification
caTrainingSpecification = Lens.lens (trainingSpecification :: CreateAlgorithm -> TrainingSpecification) (\s a -> s {trainingSpecification = a} :: CreateAlgorithm)
{-# DEPRECATED caTrainingSpecification "Use generic-lens or generic-optics with 'trainingSpecification' instead." #-}

-- | The name of the algorithm.
--
-- /Note:/ Consider using 'algorithmName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caAlgorithmName :: Lens.Lens' CreateAlgorithm Lude.Text
caAlgorithmName = Lens.lens (algorithmName :: CreateAlgorithm -> Lude.Text) (\s a -> s {algorithmName = a} :: CreateAlgorithm)
{-# DEPRECATED caAlgorithmName "Use generic-lens or generic-optics with 'algorithmName' instead." #-}

-- | Specifies configurations for one or more training jobs and that Amazon SageMaker runs to test the algorithm's training code and, optionally, one or more batch transform jobs that Amazon SageMaker runs to test the algorithm's inference code.
--
-- /Note:/ Consider using 'validationSpecification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caValidationSpecification :: Lens.Lens' CreateAlgorithm (Lude.Maybe AlgorithmValidationSpecification)
caValidationSpecification = Lens.lens (validationSpecification :: CreateAlgorithm -> Lude.Maybe AlgorithmValidationSpecification) (\s a -> s {validationSpecification = a} :: CreateAlgorithm)
{-# DEPRECATED caValidationSpecification "Use generic-lens or generic-optics with 'validationSpecification' instead." #-}

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
caInferenceSpecification :: Lens.Lens' CreateAlgorithm (Lude.Maybe InferenceSpecification)
caInferenceSpecification = Lens.lens (inferenceSpecification :: CreateAlgorithm -> Lude.Maybe InferenceSpecification) (\s a -> s {inferenceSpecification = a} :: CreateAlgorithm)
{-# DEPRECATED caInferenceSpecification "Use generic-lens or generic-optics with 'inferenceSpecification' instead." #-}

-- | A description of the algorithm.
--
-- /Note:/ Consider using 'algorithmDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caAlgorithmDescription :: Lens.Lens' CreateAlgorithm (Lude.Maybe Lude.Text)
caAlgorithmDescription = Lens.lens (algorithmDescription :: CreateAlgorithm -> Lude.Maybe Lude.Text) (\s a -> s {algorithmDescription = a} :: CreateAlgorithm)
{-# DEPRECATED caAlgorithmDescription "Use generic-lens or generic-optics with 'algorithmDescription' instead." #-}

-- | Whether to certify the algorithm so that it can be listed in AWS Marketplace.
--
-- /Note:/ Consider using 'certifyForMarketplace' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caCertifyForMarketplace :: Lens.Lens' CreateAlgorithm (Lude.Maybe Lude.Bool)
caCertifyForMarketplace = Lens.lens (certifyForMarketplace :: CreateAlgorithm -> Lude.Maybe Lude.Bool) (\s a -> s {certifyForMarketplace = a} :: CreateAlgorithm)
{-# DEPRECATED caCertifyForMarketplace "Use generic-lens or generic-optics with 'certifyForMarketplace' instead." #-}

instance Lude.AWSRequest CreateAlgorithm where
  type Rs CreateAlgorithm = CreateAlgorithmResponse
  request = Req.postJSON sageMakerService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateAlgorithmResponse'
            Lude.<$> (x Lude..:> "AlgorithmArn") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateAlgorithm where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("SageMaker.CreateAlgorithm" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateAlgorithm where
  toJSON CreateAlgorithm' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("TrainingSpecification" Lude..= trainingSpecification),
            Lude.Just ("AlgorithmName" Lude..= algorithmName),
            ("ValidationSpecification" Lude..=)
              Lude.<$> validationSpecification,
            ("InferenceSpecification" Lude..=) Lude.<$> inferenceSpecification,
            ("AlgorithmDescription" Lude..=) Lude.<$> algorithmDescription,
            ("CertifyForMarketplace" Lude..=) Lude.<$> certifyForMarketplace
          ]
      )

instance Lude.ToPath CreateAlgorithm where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateAlgorithm where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateAlgorithmResponse' smart constructor.
data CreateAlgorithmResponse = CreateAlgorithmResponse'
  { -- | The Amazon Resource Name (ARN) of the new algorithm.
    algorithmARN :: Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateAlgorithmResponse' with the minimum fields required to make a request.
--
-- * 'algorithmARN' - The Amazon Resource Name (ARN) of the new algorithm.
-- * 'responseStatus' - The response status code.
mkCreateAlgorithmResponse ::
  -- | 'algorithmARN'
  Lude.Text ->
  -- | 'responseStatus'
  Lude.Int ->
  CreateAlgorithmResponse
mkCreateAlgorithmResponse pAlgorithmARN_ pResponseStatus_ =
  CreateAlgorithmResponse'
    { algorithmARN = pAlgorithmARN_,
      responseStatus = pResponseStatus_
    }

-- | The Amazon Resource Name (ARN) of the new algorithm.
--
-- /Note:/ Consider using 'algorithmARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crsAlgorithmARN :: Lens.Lens' CreateAlgorithmResponse Lude.Text
crsAlgorithmARN = Lens.lens (algorithmARN :: CreateAlgorithmResponse -> Lude.Text) (\s a -> s {algorithmARN = a} :: CreateAlgorithmResponse)
{-# DEPRECATED crsAlgorithmARN "Use generic-lens or generic-optics with 'algorithmARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crsResponseStatus :: Lens.Lens' CreateAlgorithmResponse Lude.Int
crsResponseStatus = Lens.lens (responseStatus :: CreateAlgorithmResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateAlgorithmResponse)
{-# DEPRECATED crsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
