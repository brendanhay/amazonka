{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.DescribeAlgorithm
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a description of the specified algorithm that is in your account.
module Network.AWS.SageMaker.DescribeAlgorithm
  ( -- * Creating a request
    DescribeAlgorithm (..),
    mkDescribeAlgorithm,

    -- ** Request lenses
    daAlgorithmName,

    -- * Destructuring the response
    DescribeAlgorithmResponse (..),
    mkDescribeAlgorithmResponse,

    -- ** Response lenses
    drsCreationTime,
    drsAlgorithmARN,
    drsTrainingSpecification,
    drsAlgorithmName,
    drsValidationSpecification,
    drsInferenceSpecification,
    drsAlgorithmDescription,
    drsCertifyForMarketplace,
    drsAlgorithmStatus,
    drsProductId,
    drsAlgorithmStatusDetails,
    drsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SageMaker.Types

-- | /See:/ 'mkDescribeAlgorithm' smart constructor.
newtype DescribeAlgorithm = DescribeAlgorithm'
  { -- | The name of the algorithm to describe.
    algorithmName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeAlgorithm' with the minimum fields required to make a request.
--
-- * 'algorithmName' - The name of the algorithm to describe.
mkDescribeAlgorithm ::
  -- | 'algorithmName'
  Lude.Text ->
  DescribeAlgorithm
mkDescribeAlgorithm pAlgorithmName_ =
  DescribeAlgorithm' {algorithmName = pAlgorithmName_}

-- | The name of the algorithm to describe.
--
-- /Note:/ Consider using 'algorithmName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daAlgorithmName :: Lens.Lens' DescribeAlgorithm Lude.Text
daAlgorithmName = Lens.lens (algorithmName :: DescribeAlgorithm -> Lude.Text) (\s a -> s {algorithmName = a} :: DescribeAlgorithm)
{-# DEPRECATED daAlgorithmName "Use generic-lens or generic-optics with 'algorithmName' instead." #-}

instance Lude.AWSRequest DescribeAlgorithm where
  type Rs DescribeAlgorithm = DescribeAlgorithmResponse
  request = Req.postJSON sageMakerService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeAlgorithmResponse'
            Lude.<$> (x Lude..:> "CreationTime")
            Lude.<*> (x Lude..:> "AlgorithmArn")
            Lude.<*> (x Lude..:> "TrainingSpecification")
            Lude.<*> (x Lude..:> "AlgorithmName")
            Lude.<*> (x Lude..?> "ValidationSpecification")
            Lude.<*> (x Lude..?> "InferenceSpecification")
            Lude.<*> (x Lude..?> "AlgorithmDescription")
            Lude.<*> (x Lude..?> "CertifyForMarketplace")
            Lude.<*> (x Lude..:> "AlgorithmStatus")
            Lude.<*> (x Lude..?> "ProductId")
            Lude.<*> (x Lude..:> "AlgorithmStatusDetails")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeAlgorithm where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("SageMaker.DescribeAlgorithm" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeAlgorithm where
  toJSON DescribeAlgorithm' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("AlgorithmName" Lude..= algorithmName)]
      )

instance Lude.ToPath DescribeAlgorithm where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeAlgorithm where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeAlgorithmResponse' smart constructor.
data DescribeAlgorithmResponse = DescribeAlgorithmResponse'
  { -- | A timestamp specifying when the algorithm was created.
    creationTime :: Lude.Timestamp,
    -- | The Amazon Resource Name (ARN) of the algorithm.
    algorithmARN :: Lude.Text,
    -- | Details about training jobs run by this algorithm.
    trainingSpecification :: TrainingSpecification,
    -- | The name of the algorithm being described.
    algorithmName :: Lude.Text,
    -- | Details about configurations for one or more training jobs that Amazon SageMaker runs to test the algorithm.
    validationSpecification :: Lude.Maybe AlgorithmValidationSpecification,
    -- | Details about inference jobs that the algorithm runs.
    inferenceSpecification :: Lude.Maybe InferenceSpecification,
    -- | A brief summary about the algorithm.
    algorithmDescription :: Lude.Maybe Lude.Text,
    -- | Whether the algorithm is certified to be listed in AWS Marketplace.
    certifyForMarketplace :: Lude.Maybe Lude.Bool,
    -- | The current status of the algorithm.
    algorithmStatus :: AlgorithmStatus,
    -- | The product identifier of the algorithm.
    productId :: Lude.Maybe Lude.Text,
    -- | Details about the current status of the algorithm.
    algorithmStatusDetails :: AlgorithmStatusDetails,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeAlgorithmResponse' with the minimum fields required to make a request.
--
-- * 'creationTime' - A timestamp specifying when the algorithm was created.
-- * 'algorithmARN' - The Amazon Resource Name (ARN) of the algorithm.
-- * 'trainingSpecification' - Details about training jobs run by this algorithm.
-- * 'algorithmName' - The name of the algorithm being described.
-- * 'validationSpecification' - Details about configurations for one or more training jobs that Amazon SageMaker runs to test the algorithm.
-- * 'inferenceSpecification' - Details about inference jobs that the algorithm runs.
-- * 'algorithmDescription' - A brief summary about the algorithm.
-- * 'certifyForMarketplace' - Whether the algorithm is certified to be listed in AWS Marketplace.
-- * 'algorithmStatus' - The current status of the algorithm.
-- * 'productId' - The product identifier of the algorithm.
-- * 'algorithmStatusDetails' - Details about the current status of the algorithm.
-- * 'responseStatus' - The response status code.
mkDescribeAlgorithmResponse ::
  -- | 'creationTime'
  Lude.Timestamp ->
  -- | 'algorithmARN'
  Lude.Text ->
  -- | 'trainingSpecification'
  TrainingSpecification ->
  -- | 'algorithmName'
  Lude.Text ->
  -- | 'algorithmStatus'
  AlgorithmStatus ->
  -- | 'algorithmStatusDetails'
  AlgorithmStatusDetails ->
  -- | 'responseStatus'
  Lude.Int ->
  DescribeAlgorithmResponse
mkDescribeAlgorithmResponse
  pCreationTime_
  pAlgorithmARN_
  pTrainingSpecification_
  pAlgorithmName_
  pAlgorithmStatus_
  pAlgorithmStatusDetails_
  pResponseStatus_ =
    DescribeAlgorithmResponse'
      { creationTime = pCreationTime_,
        algorithmARN = pAlgorithmARN_,
        trainingSpecification = pTrainingSpecification_,
        algorithmName = pAlgorithmName_,
        validationSpecification = Lude.Nothing,
        inferenceSpecification = Lude.Nothing,
        algorithmDescription = Lude.Nothing,
        certifyForMarketplace = Lude.Nothing,
        algorithmStatus = pAlgorithmStatus_,
        productId = Lude.Nothing,
        algorithmStatusDetails = pAlgorithmStatusDetails_,
        responseStatus = pResponseStatus_
      }

-- | A timestamp specifying when the algorithm was created.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsCreationTime :: Lens.Lens' DescribeAlgorithmResponse Lude.Timestamp
drsCreationTime = Lens.lens (creationTime :: DescribeAlgorithmResponse -> Lude.Timestamp) (\s a -> s {creationTime = a} :: DescribeAlgorithmResponse)
{-# DEPRECATED drsCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

-- | The Amazon Resource Name (ARN) of the algorithm.
--
-- /Note:/ Consider using 'algorithmARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsAlgorithmARN :: Lens.Lens' DescribeAlgorithmResponse Lude.Text
drsAlgorithmARN = Lens.lens (algorithmARN :: DescribeAlgorithmResponse -> Lude.Text) (\s a -> s {algorithmARN = a} :: DescribeAlgorithmResponse)
{-# DEPRECATED drsAlgorithmARN "Use generic-lens or generic-optics with 'algorithmARN' instead." #-}

-- | Details about training jobs run by this algorithm.
--
-- /Note:/ Consider using 'trainingSpecification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsTrainingSpecification :: Lens.Lens' DescribeAlgorithmResponse TrainingSpecification
drsTrainingSpecification = Lens.lens (trainingSpecification :: DescribeAlgorithmResponse -> TrainingSpecification) (\s a -> s {trainingSpecification = a} :: DescribeAlgorithmResponse)
{-# DEPRECATED drsTrainingSpecification "Use generic-lens or generic-optics with 'trainingSpecification' instead." #-}

-- | The name of the algorithm being described.
--
-- /Note:/ Consider using 'algorithmName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsAlgorithmName :: Lens.Lens' DescribeAlgorithmResponse Lude.Text
drsAlgorithmName = Lens.lens (algorithmName :: DescribeAlgorithmResponse -> Lude.Text) (\s a -> s {algorithmName = a} :: DescribeAlgorithmResponse)
{-# DEPRECATED drsAlgorithmName "Use generic-lens or generic-optics with 'algorithmName' instead." #-}

-- | Details about configurations for one or more training jobs that Amazon SageMaker runs to test the algorithm.
--
-- /Note:/ Consider using 'validationSpecification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsValidationSpecification :: Lens.Lens' DescribeAlgorithmResponse (Lude.Maybe AlgorithmValidationSpecification)
drsValidationSpecification = Lens.lens (validationSpecification :: DescribeAlgorithmResponse -> Lude.Maybe AlgorithmValidationSpecification) (\s a -> s {validationSpecification = a} :: DescribeAlgorithmResponse)
{-# DEPRECATED drsValidationSpecification "Use generic-lens or generic-optics with 'validationSpecification' instead." #-}

-- | Details about inference jobs that the algorithm runs.
--
-- /Note:/ Consider using 'inferenceSpecification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsInferenceSpecification :: Lens.Lens' DescribeAlgorithmResponse (Lude.Maybe InferenceSpecification)
drsInferenceSpecification = Lens.lens (inferenceSpecification :: DescribeAlgorithmResponse -> Lude.Maybe InferenceSpecification) (\s a -> s {inferenceSpecification = a} :: DescribeAlgorithmResponse)
{-# DEPRECATED drsInferenceSpecification "Use generic-lens or generic-optics with 'inferenceSpecification' instead." #-}

-- | A brief summary about the algorithm.
--
-- /Note:/ Consider using 'algorithmDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsAlgorithmDescription :: Lens.Lens' DescribeAlgorithmResponse (Lude.Maybe Lude.Text)
drsAlgorithmDescription = Lens.lens (algorithmDescription :: DescribeAlgorithmResponse -> Lude.Maybe Lude.Text) (\s a -> s {algorithmDescription = a} :: DescribeAlgorithmResponse)
{-# DEPRECATED drsAlgorithmDescription "Use generic-lens or generic-optics with 'algorithmDescription' instead." #-}

-- | Whether the algorithm is certified to be listed in AWS Marketplace.
--
-- /Note:/ Consider using 'certifyForMarketplace' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsCertifyForMarketplace :: Lens.Lens' DescribeAlgorithmResponse (Lude.Maybe Lude.Bool)
drsCertifyForMarketplace = Lens.lens (certifyForMarketplace :: DescribeAlgorithmResponse -> Lude.Maybe Lude.Bool) (\s a -> s {certifyForMarketplace = a} :: DescribeAlgorithmResponse)
{-# DEPRECATED drsCertifyForMarketplace "Use generic-lens or generic-optics with 'certifyForMarketplace' instead." #-}

-- | The current status of the algorithm.
--
-- /Note:/ Consider using 'algorithmStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsAlgorithmStatus :: Lens.Lens' DescribeAlgorithmResponse AlgorithmStatus
drsAlgorithmStatus = Lens.lens (algorithmStatus :: DescribeAlgorithmResponse -> AlgorithmStatus) (\s a -> s {algorithmStatus = a} :: DescribeAlgorithmResponse)
{-# DEPRECATED drsAlgorithmStatus "Use generic-lens or generic-optics with 'algorithmStatus' instead." #-}

-- | The product identifier of the algorithm.
--
-- /Note:/ Consider using 'productId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsProductId :: Lens.Lens' DescribeAlgorithmResponse (Lude.Maybe Lude.Text)
drsProductId = Lens.lens (productId :: DescribeAlgorithmResponse -> Lude.Maybe Lude.Text) (\s a -> s {productId = a} :: DescribeAlgorithmResponse)
{-# DEPRECATED drsProductId "Use generic-lens or generic-optics with 'productId' instead." #-}

-- | Details about the current status of the algorithm.
--
-- /Note:/ Consider using 'algorithmStatusDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsAlgorithmStatusDetails :: Lens.Lens' DescribeAlgorithmResponse AlgorithmStatusDetails
drsAlgorithmStatusDetails = Lens.lens (algorithmStatusDetails :: DescribeAlgorithmResponse -> AlgorithmStatusDetails) (\s a -> s {algorithmStatusDetails = a} :: DescribeAlgorithmResponse)
{-# DEPRECATED drsAlgorithmStatusDetails "Use generic-lens or generic-optics with 'algorithmStatusDetails' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsResponseStatus :: Lens.Lens' DescribeAlgorithmResponse Lude.Int
drsResponseStatus = Lens.lens (responseStatus :: DescribeAlgorithmResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeAlgorithmResponse)
{-# DEPRECATED drsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
