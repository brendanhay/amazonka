{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    caValidationSpecification,
    caInferenceSpecification,
    caAlgorithmDescription,
    caCertifyForMarketplace,
    caAlgorithmName,
    caTrainingSpecification,

    -- * Destructuring the response
    CreateAlgorithmResponse (..),
    mkCreateAlgorithmResponse,

    -- ** Response lenses
    carsResponseStatus,
    carsAlgorithmARN,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SageMaker.Types

-- | /See:/ 'mkCreateAlgorithm' smart constructor.
data CreateAlgorithm = CreateAlgorithm'
  { validationSpecification ::
      Lude.Maybe AlgorithmValidationSpecification,
    inferenceSpecification :: Lude.Maybe InferenceSpecification,
    algorithmDescription :: Lude.Maybe Lude.Text,
    certifyForMarketplace :: Lude.Maybe Lude.Bool,
    algorithmName :: Lude.Text,
    trainingSpecification :: TrainingSpecification
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateAlgorithm' with the minimum fields required to make a request.
--
-- * 'algorithmDescription' - A description of the algorithm.
-- * 'algorithmName' - The name of the algorithm.
-- * 'certifyForMarketplace' - Whether to certify the algorithm so that it can be listed in AWS Marketplace.
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
-- * 'validationSpecification' - Specifies configurations for one or more training jobs and that Amazon SageMaker runs to test the algorithm's training code and, optionally, one or more batch transform jobs that Amazon SageMaker runs to test the algorithm's inference code.
mkCreateAlgorithm ::
  -- | 'algorithmName'
  Lude.Text ->
  -- | 'trainingSpecification'
  TrainingSpecification ->
  CreateAlgorithm
mkCreateAlgorithm pAlgorithmName_ pTrainingSpecification_ =
  CreateAlgorithm'
    { validationSpecification = Lude.Nothing,
      inferenceSpecification = Lude.Nothing,
      algorithmDescription = Lude.Nothing,
      certifyForMarketplace = Lude.Nothing,
      algorithmName = pAlgorithmName_,
      trainingSpecification = pTrainingSpecification_
    }

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

-- | The name of the algorithm.
--
-- /Note:/ Consider using 'algorithmName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caAlgorithmName :: Lens.Lens' CreateAlgorithm Lude.Text
caAlgorithmName = Lens.lens (algorithmName :: CreateAlgorithm -> Lude.Text) (\s a -> s {algorithmName = a} :: CreateAlgorithm)
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
caTrainingSpecification :: Lens.Lens' CreateAlgorithm TrainingSpecification
caTrainingSpecification = Lens.lens (trainingSpecification :: CreateAlgorithm -> TrainingSpecification) (\s a -> s {trainingSpecification = a} :: CreateAlgorithm)
{-# DEPRECATED caTrainingSpecification "Use generic-lens or generic-optics with 'trainingSpecification' instead." #-}

instance Lude.AWSRequest CreateAlgorithm where
  type Rs CreateAlgorithm = CreateAlgorithmResponse
  request = Req.postJSON sageMakerService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateAlgorithmResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s)) Lude.<*> (x Lude..:> "AlgorithmArn")
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
          [ ("ValidationSpecification" Lude..=)
              Lude.<$> validationSpecification,
            ("InferenceSpecification" Lude..=) Lude.<$> inferenceSpecification,
            ("AlgorithmDescription" Lude..=) Lude.<$> algorithmDescription,
            ("CertifyForMarketplace" Lude..=) Lude.<$> certifyForMarketplace,
            Lude.Just ("AlgorithmName" Lude..= algorithmName),
            Lude.Just ("TrainingSpecification" Lude..= trainingSpecification)
          ]
      )

instance Lude.ToPath CreateAlgorithm where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateAlgorithm where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateAlgorithmResponse' smart constructor.
data CreateAlgorithmResponse = CreateAlgorithmResponse'
  { responseStatus ::
      Lude.Int,
    algorithmARN :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateAlgorithmResponse' with the minimum fields required to make a request.
--
-- * 'algorithmARN' - The Amazon Resource Name (ARN) of the new algorithm.
-- * 'responseStatus' - The response status code.
mkCreateAlgorithmResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  -- | 'algorithmARN'
  Lude.Text ->
  CreateAlgorithmResponse
mkCreateAlgorithmResponse pResponseStatus_ pAlgorithmARN_ =
  CreateAlgorithmResponse'
    { responseStatus = pResponseStatus_,
      algorithmARN = pAlgorithmARN_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
carsResponseStatus :: Lens.Lens' CreateAlgorithmResponse Lude.Int
carsResponseStatus = Lens.lens (responseStatus :: CreateAlgorithmResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateAlgorithmResponse)
{-# DEPRECATED carsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | The Amazon Resource Name (ARN) of the new algorithm.
--
-- /Note:/ Consider using 'algorithmARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
carsAlgorithmARN :: Lens.Lens' CreateAlgorithmResponse Lude.Text
carsAlgorithmARN = Lens.lens (algorithmARN :: CreateAlgorithmResponse -> Lude.Text) (\s a -> s {algorithmARN = a} :: CreateAlgorithmResponse)
{-# DEPRECATED carsAlgorithmARN "Use generic-lens or generic-optics with 'algorithmARN' instead." #-}
