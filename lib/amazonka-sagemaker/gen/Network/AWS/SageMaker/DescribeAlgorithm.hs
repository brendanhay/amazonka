{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    dAlgorithmName,

    -- * Destructuring the response
    DescribeAlgorithmResponse (..),
    mkDescribeAlgorithmResponse,

    -- ** Response lenses
    daarsValidationSpecification,
    daarsInferenceSpecification,
    daarsAlgorithmDescription,
    daarsCertifyForMarketplace,
    daarsProductId,
    daarsResponseStatus,
    daarsAlgorithmName,
    daarsAlgorithmARN,
    daarsCreationTime,
    daarsTrainingSpecification,
    daarsAlgorithmStatus,
    daarsAlgorithmStatusDetails,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SageMaker.Types

-- | /See:/ 'mkDescribeAlgorithm' smart constructor.
newtype DescribeAlgorithm = DescribeAlgorithm'
  { algorithmName ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
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
dAlgorithmName :: Lens.Lens' DescribeAlgorithm Lude.Text
dAlgorithmName = Lens.lens (algorithmName :: DescribeAlgorithm -> Lude.Text) (\s a -> s {algorithmName = a} :: DescribeAlgorithm)
{-# DEPRECATED dAlgorithmName "Use generic-lens or generic-optics with 'algorithmName' instead." #-}

instance Lude.AWSRequest DescribeAlgorithm where
  type Rs DescribeAlgorithm = DescribeAlgorithmResponse
  request = Req.postJSON sageMakerService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeAlgorithmResponse'
            Lude.<$> (x Lude..?> "ValidationSpecification")
            Lude.<*> (x Lude..?> "InferenceSpecification")
            Lude.<*> (x Lude..?> "AlgorithmDescription")
            Lude.<*> (x Lude..?> "CertifyForMarketplace")
            Lude.<*> (x Lude..?> "ProductId")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
            Lude.<*> (x Lude..:> "AlgorithmName")
            Lude.<*> (x Lude..:> "AlgorithmArn")
            Lude.<*> (x Lude..:> "CreationTime")
            Lude.<*> (x Lude..:> "TrainingSpecification")
            Lude.<*> (x Lude..:> "AlgorithmStatus")
            Lude.<*> (x Lude..:> "AlgorithmStatusDetails")
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
  { validationSpecification ::
      Lude.Maybe
        AlgorithmValidationSpecification,
    inferenceSpecification ::
      Lude.Maybe InferenceSpecification,
    algorithmDescription ::
      Lude.Maybe Lude.Text,
    certifyForMarketplace ::
      Lude.Maybe Lude.Bool,
    productId :: Lude.Maybe Lude.Text,
    responseStatus :: Lude.Int,
    algorithmName :: Lude.Text,
    algorithmARN :: Lude.Text,
    creationTime :: Lude.Timestamp,
    trainingSpecification ::
      TrainingSpecification,
    algorithmStatus :: AlgorithmStatus,
    algorithmStatusDetails ::
      AlgorithmStatusDetails
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeAlgorithmResponse' with the minimum fields required to make a request.
--
-- * 'algorithmARN' - The Amazon Resource Name (ARN) of the algorithm.
-- * 'algorithmDescription' - A brief summary about the algorithm.
-- * 'algorithmName' - The name of the algorithm being described.
-- * 'algorithmStatus' - The current status of the algorithm.
-- * 'algorithmStatusDetails' - Details about the current status of the algorithm.
-- * 'certifyForMarketplace' - Whether the algorithm is certified to be listed in AWS Marketplace.
-- * 'creationTime' - A timestamp specifying when the algorithm was created.
-- * 'inferenceSpecification' - Details about inference jobs that the algorithm runs.
-- * 'productId' - The product identifier of the algorithm.
-- * 'responseStatus' - The response status code.
-- * 'trainingSpecification' - Details about training jobs run by this algorithm.
-- * 'validationSpecification' - Details about configurations for one or more training jobs that Amazon SageMaker runs to test the algorithm.
mkDescribeAlgorithmResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  -- | 'algorithmName'
  Lude.Text ->
  -- | 'algorithmARN'
  Lude.Text ->
  -- | 'creationTime'
  Lude.Timestamp ->
  -- | 'trainingSpecification'
  TrainingSpecification ->
  -- | 'algorithmStatus'
  AlgorithmStatus ->
  -- | 'algorithmStatusDetails'
  AlgorithmStatusDetails ->
  DescribeAlgorithmResponse
mkDescribeAlgorithmResponse
  pResponseStatus_
  pAlgorithmName_
  pAlgorithmARN_
  pCreationTime_
  pTrainingSpecification_
  pAlgorithmStatus_
  pAlgorithmStatusDetails_ =
    DescribeAlgorithmResponse'
      { validationSpecification =
          Lude.Nothing,
        inferenceSpecification = Lude.Nothing,
        algorithmDescription = Lude.Nothing,
        certifyForMarketplace = Lude.Nothing,
        productId = Lude.Nothing,
        responseStatus = pResponseStatus_,
        algorithmName = pAlgorithmName_,
        algorithmARN = pAlgorithmARN_,
        creationTime = pCreationTime_,
        trainingSpecification = pTrainingSpecification_,
        algorithmStatus = pAlgorithmStatus_,
        algorithmStatusDetails = pAlgorithmStatusDetails_
      }

-- | Details about configurations for one or more training jobs that Amazon SageMaker runs to test the algorithm.
--
-- /Note:/ Consider using 'validationSpecification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daarsValidationSpecification :: Lens.Lens' DescribeAlgorithmResponse (Lude.Maybe AlgorithmValidationSpecification)
daarsValidationSpecification = Lens.lens (validationSpecification :: DescribeAlgorithmResponse -> Lude.Maybe AlgorithmValidationSpecification) (\s a -> s {validationSpecification = a} :: DescribeAlgorithmResponse)
{-# DEPRECATED daarsValidationSpecification "Use generic-lens or generic-optics with 'validationSpecification' instead." #-}

-- | Details about inference jobs that the algorithm runs.
--
-- /Note:/ Consider using 'inferenceSpecification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daarsInferenceSpecification :: Lens.Lens' DescribeAlgorithmResponse (Lude.Maybe InferenceSpecification)
daarsInferenceSpecification = Lens.lens (inferenceSpecification :: DescribeAlgorithmResponse -> Lude.Maybe InferenceSpecification) (\s a -> s {inferenceSpecification = a} :: DescribeAlgorithmResponse)
{-# DEPRECATED daarsInferenceSpecification "Use generic-lens or generic-optics with 'inferenceSpecification' instead." #-}

-- | A brief summary about the algorithm.
--
-- /Note:/ Consider using 'algorithmDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daarsAlgorithmDescription :: Lens.Lens' DescribeAlgorithmResponse (Lude.Maybe Lude.Text)
daarsAlgorithmDescription = Lens.lens (algorithmDescription :: DescribeAlgorithmResponse -> Lude.Maybe Lude.Text) (\s a -> s {algorithmDescription = a} :: DescribeAlgorithmResponse)
{-# DEPRECATED daarsAlgorithmDescription "Use generic-lens or generic-optics with 'algorithmDescription' instead." #-}

-- | Whether the algorithm is certified to be listed in AWS Marketplace.
--
-- /Note:/ Consider using 'certifyForMarketplace' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daarsCertifyForMarketplace :: Lens.Lens' DescribeAlgorithmResponse (Lude.Maybe Lude.Bool)
daarsCertifyForMarketplace = Lens.lens (certifyForMarketplace :: DescribeAlgorithmResponse -> Lude.Maybe Lude.Bool) (\s a -> s {certifyForMarketplace = a} :: DescribeAlgorithmResponse)
{-# DEPRECATED daarsCertifyForMarketplace "Use generic-lens or generic-optics with 'certifyForMarketplace' instead." #-}

-- | The product identifier of the algorithm.
--
-- /Note:/ Consider using 'productId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daarsProductId :: Lens.Lens' DescribeAlgorithmResponse (Lude.Maybe Lude.Text)
daarsProductId = Lens.lens (productId :: DescribeAlgorithmResponse -> Lude.Maybe Lude.Text) (\s a -> s {productId = a} :: DescribeAlgorithmResponse)
{-# DEPRECATED daarsProductId "Use generic-lens or generic-optics with 'productId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daarsResponseStatus :: Lens.Lens' DescribeAlgorithmResponse Lude.Int
daarsResponseStatus = Lens.lens (responseStatus :: DescribeAlgorithmResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeAlgorithmResponse)
{-# DEPRECATED daarsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | The name of the algorithm being described.
--
-- /Note:/ Consider using 'algorithmName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daarsAlgorithmName :: Lens.Lens' DescribeAlgorithmResponse Lude.Text
daarsAlgorithmName = Lens.lens (algorithmName :: DescribeAlgorithmResponse -> Lude.Text) (\s a -> s {algorithmName = a} :: DescribeAlgorithmResponse)
{-# DEPRECATED daarsAlgorithmName "Use generic-lens or generic-optics with 'algorithmName' instead." #-}

-- | The Amazon Resource Name (ARN) of the algorithm.
--
-- /Note:/ Consider using 'algorithmARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daarsAlgorithmARN :: Lens.Lens' DescribeAlgorithmResponse Lude.Text
daarsAlgorithmARN = Lens.lens (algorithmARN :: DescribeAlgorithmResponse -> Lude.Text) (\s a -> s {algorithmARN = a} :: DescribeAlgorithmResponse)
{-# DEPRECATED daarsAlgorithmARN "Use generic-lens or generic-optics with 'algorithmARN' instead." #-}

-- | A timestamp specifying when the algorithm was created.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daarsCreationTime :: Lens.Lens' DescribeAlgorithmResponse Lude.Timestamp
daarsCreationTime = Lens.lens (creationTime :: DescribeAlgorithmResponse -> Lude.Timestamp) (\s a -> s {creationTime = a} :: DescribeAlgorithmResponse)
{-# DEPRECATED daarsCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

-- | Details about training jobs run by this algorithm.
--
-- /Note:/ Consider using 'trainingSpecification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daarsTrainingSpecification :: Lens.Lens' DescribeAlgorithmResponse TrainingSpecification
daarsTrainingSpecification = Lens.lens (trainingSpecification :: DescribeAlgorithmResponse -> TrainingSpecification) (\s a -> s {trainingSpecification = a} :: DescribeAlgorithmResponse)
{-# DEPRECATED daarsTrainingSpecification "Use generic-lens or generic-optics with 'trainingSpecification' instead." #-}

-- | The current status of the algorithm.
--
-- /Note:/ Consider using 'algorithmStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daarsAlgorithmStatus :: Lens.Lens' DescribeAlgorithmResponse AlgorithmStatus
daarsAlgorithmStatus = Lens.lens (algorithmStatus :: DescribeAlgorithmResponse -> AlgorithmStatus) (\s a -> s {algorithmStatus = a} :: DescribeAlgorithmResponse)
{-# DEPRECATED daarsAlgorithmStatus "Use generic-lens or generic-optics with 'algorithmStatus' instead." #-}

-- | Details about the current status of the algorithm.
--
-- /Note:/ Consider using 'algorithmStatusDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daarsAlgorithmStatusDetails :: Lens.Lens' DescribeAlgorithmResponse AlgorithmStatusDetails
daarsAlgorithmStatusDetails = Lens.lens (algorithmStatusDetails :: DescribeAlgorithmResponse -> AlgorithmStatusDetails) (\s a -> s {algorithmStatusDetails = a} :: DescribeAlgorithmResponse)
{-# DEPRECATED daarsAlgorithmStatusDetails "Use generic-lens or generic-optics with 'algorithmStatusDetails' instead." #-}
