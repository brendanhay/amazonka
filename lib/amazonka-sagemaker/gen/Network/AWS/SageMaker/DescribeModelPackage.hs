{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.DescribeModelPackage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a description of the specified model package, which is used to create Amazon SageMaker models or list them on AWS Marketplace.
--
-- To create models in Amazon SageMaker, buyers can subscribe to model packages listed on AWS Marketplace.
module Network.AWS.SageMaker.DescribeModelPackage
  ( -- * Creating a request
    DescribeModelPackage (..),
    mkDescribeModelPackage,

    -- ** Request lenses
    dModelPackageName,

    -- * Destructuring the response
    DescribeModelPackageResponse (..),
    mkDescribeModelPackageResponse,

    -- ** Response lenses
    dmprsSourceAlgorithmSpecification,
    dmprsModelPackageDescription,
    dmprsValidationSpecification,
    dmprsInferenceSpecification,
    dmprsCertifyForMarketplace,
    dmprsResponseStatus,
    dmprsModelPackageName,
    dmprsModelPackageARN,
    dmprsCreationTime,
    dmprsModelPackageStatus,
    dmprsModelPackageStatusDetails,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SageMaker.Types

-- | /See:/ 'mkDescribeModelPackage' smart constructor.
newtype DescribeModelPackage = DescribeModelPackage'
  { modelPackageName ::
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

-- | Creates a value of 'DescribeModelPackage' with the minimum fields required to make a request.
--
-- * 'modelPackageName' - The name of the model package to describe.
mkDescribeModelPackage ::
  -- | 'modelPackageName'
  Lude.Text ->
  DescribeModelPackage
mkDescribeModelPackage pModelPackageName_ =
  DescribeModelPackage' {modelPackageName = pModelPackageName_}

-- | The name of the model package to describe.
--
-- /Note:/ Consider using 'modelPackageName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dModelPackageName :: Lens.Lens' DescribeModelPackage Lude.Text
dModelPackageName = Lens.lens (modelPackageName :: DescribeModelPackage -> Lude.Text) (\s a -> s {modelPackageName = a} :: DescribeModelPackage)
{-# DEPRECATED dModelPackageName "Use generic-lens or generic-optics with 'modelPackageName' instead." #-}

instance Lude.AWSRequest DescribeModelPackage where
  type Rs DescribeModelPackage = DescribeModelPackageResponse
  request = Req.postJSON sageMakerService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeModelPackageResponse'
            Lude.<$> (x Lude..?> "SourceAlgorithmSpecification")
            Lude.<*> (x Lude..?> "ModelPackageDescription")
            Lude.<*> (x Lude..?> "ValidationSpecification")
            Lude.<*> (x Lude..?> "InferenceSpecification")
            Lude.<*> (x Lude..?> "CertifyForMarketplace")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
            Lude.<*> (x Lude..:> "ModelPackageName")
            Lude.<*> (x Lude..:> "ModelPackageArn")
            Lude.<*> (x Lude..:> "CreationTime")
            Lude.<*> (x Lude..:> "ModelPackageStatus")
            Lude.<*> (x Lude..:> "ModelPackageStatusDetails")
      )

instance Lude.ToHeaders DescribeModelPackage where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("SageMaker.DescribeModelPackage" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeModelPackage where
  toJSON DescribeModelPackage' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("ModelPackageName" Lude..= modelPackageName)]
      )

instance Lude.ToPath DescribeModelPackage where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeModelPackage where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeModelPackageResponse' smart constructor.
data DescribeModelPackageResponse = DescribeModelPackageResponse'
  { sourceAlgorithmSpecification ::
      Lude.Maybe
        SourceAlgorithmSpecification,
    modelPackageDescription ::
      Lude.Maybe Lude.Text,
    validationSpecification ::
      Lude.Maybe
        ModelPackageValidationSpecification,
    inferenceSpecification ::
      Lude.Maybe InferenceSpecification,
    certifyForMarketplace ::
      Lude.Maybe Lude.Bool,
    responseStatus :: Lude.Int,
    modelPackageName :: Lude.Text,
    modelPackageARN :: Lude.Text,
    creationTime :: Lude.Timestamp,
    modelPackageStatus ::
      ModelPackageStatus,
    modelPackageStatusDetails ::
      ModelPackageStatusDetails
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeModelPackageResponse' with the minimum fields required to make a request.
--
-- * 'certifyForMarketplace' - Whether the model package is certified for listing on AWS Marketplace.
-- * 'creationTime' - A timestamp specifying when the model package was created.
-- * 'inferenceSpecification' - Details about inference jobs that can be run with models based on this model package.
-- * 'modelPackageARN' - The Amazon Resource Name (ARN) of the model package.
-- * 'modelPackageDescription' - A brief summary of the model package.
-- * 'modelPackageName' - The name of the model package being described.
-- * 'modelPackageStatus' - The current status of the model package.
-- * 'modelPackageStatusDetails' - Details about the current status of the model package.
-- * 'responseStatus' - The response status code.
-- * 'sourceAlgorithmSpecification' - Details about the algorithm that was used to create the model package.
-- * 'validationSpecification' - Configurations for one or more transform jobs that Amazon SageMaker runs to test the model package.
mkDescribeModelPackageResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  -- | 'modelPackageName'
  Lude.Text ->
  -- | 'modelPackageARN'
  Lude.Text ->
  -- | 'creationTime'
  Lude.Timestamp ->
  -- | 'modelPackageStatus'
  ModelPackageStatus ->
  -- | 'modelPackageStatusDetails'
  ModelPackageStatusDetails ->
  DescribeModelPackageResponse
mkDescribeModelPackageResponse
  pResponseStatus_
  pModelPackageName_
  pModelPackageARN_
  pCreationTime_
  pModelPackageStatus_
  pModelPackageStatusDetails_ =
    DescribeModelPackageResponse'
      { sourceAlgorithmSpecification =
          Lude.Nothing,
        modelPackageDescription = Lude.Nothing,
        validationSpecification = Lude.Nothing,
        inferenceSpecification = Lude.Nothing,
        certifyForMarketplace = Lude.Nothing,
        responseStatus = pResponseStatus_,
        modelPackageName = pModelPackageName_,
        modelPackageARN = pModelPackageARN_,
        creationTime = pCreationTime_,
        modelPackageStatus = pModelPackageStatus_,
        modelPackageStatusDetails = pModelPackageStatusDetails_
      }

-- | Details about the algorithm that was used to create the model package.
--
-- /Note:/ Consider using 'sourceAlgorithmSpecification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmprsSourceAlgorithmSpecification :: Lens.Lens' DescribeModelPackageResponse (Lude.Maybe SourceAlgorithmSpecification)
dmprsSourceAlgorithmSpecification = Lens.lens (sourceAlgorithmSpecification :: DescribeModelPackageResponse -> Lude.Maybe SourceAlgorithmSpecification) (\s a -> s {sourceAlgorithmSpecification = a} :: DescribeModelPackageResponse)
{-# DEPRECATED dmprsSourceAlgorithmSpecification "Use generic-lens or generic-optics with 'sourceAlgorithmSpecification' instead." #-}

-- | A brief summary of the model package.
--
-- /Note:/ Consider using 'modelPackageDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmprsModelPackageDescription :: Lens.Lens' DescribeModelPackageResponse (Lude.Maybe Lude.Text)
dmprsModelPackageDescription = Lens.lens (modelPackageDescription :: DescribeModelPackageResponse -> Lude.Maybe Lude.Text) (\s a -> s {modelPackageDescription = a} :: DescribeModelPackageResponse)
{-# DEPRECATED dmprsModelPackageDescription "Use generic-lens or generic-optics with 'modelPackageDescription' instead." #-}

-- | Configurations for one or more transform jobs that Amazon SageMaker runs to test the model package.
--
-- /Note:/ Consider using 'validationSpecification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmprsValidationSpecification :: Lens.Lens' DescribeModelPackageResponse (Lude.Maybe ModelPackageValidationSpecification)
dmprsValidationSpecification = Lens.lens (validationSpecification :: DescribeModelPackageResponse -> Lude.Maybe ModelPackageValidationSpecification) (\s a -> s {validationSpecification = a} :: DescribeModelPackageResponse)
{-# DEPRECATED dmprsValidationSpecification "Use generic-lens or generic-optics with 'validationSpecification' instead." #-}

-- | Details about inference jobs that can be run with models based on this model package.
--
-- /Note:/ Consider using 'inferenceSpecification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmprsInferenceSpecification :: Lens.Lens' DescribeModelPackageResponse (Lude.Maybe InferenceSpecification)
dmprsInferenceSpecification = Lens.lens (inferenceSpecification :: DescribeModelPackageResponse -> Lude.Maybe InferenceSpecification) (\s a -> s {inferenceSpecification = a} :: DescribeModelPackageResponse)
{-# DEPRECATED dmprsInferenceSpecification "Use generic-lens or generic-optics with 'inferenceSpecification' instead." #-}

-- | Whether the model package is certified for listing on AWS Marketplace.
--
-- /Note:/ Consider using 'certifyForMarketplace' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmprsCertifyForMarketplace :: Lens.Lens' DescribeModelPackageResponse (Lude.Maybe Lude.Bool)
dmprsCertifyForMarketplace = Lens.lens (certifyForMarketplace :: DescribeModelPackageResponse -> Lude.Maybe Lude.Bool) (\s a -> s {certifyForMarketplace = a} :: DescribeModelPackageResponse)
{-# DEPRECATED dmprsCertifyForMarketplace "Use generic-lens or generic-optics with 'certifyForMarketplace' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmprsResponseStatus :: Lens.Lens' DescribeModelPackageResponse Lude.Int
dmprsResponseStatus = Lens.lens (responseStatus :: DescribeModelPackageResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeModelPackageResponse)
{-# DEPRECATED dmprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | The name of the model package being described.
--
-- /Note:/ Consider using 'modelPackageName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmprsModelPackageName :: Lens.Lens' DescribeModelPackageResponse Lude.Text
dmprsModelPackageName = Lens.lens (modelPackageName :: DescribeModelPackageResponse -> Lude.Text) (\s a -> s {modelPackageName = a} :: DescribeModelPackageResponse)
{-# DEPRECATED dmprsModelPackageName "Use generic-lens or generic-optics with 'modelPackageName' instead." #-}

-- | The Amazon Resource Name (ARN) of the model package.
--
-- /Note:/ Consider using 'modelPackageARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmprsModelPackageARN :: Lens.Lens' DescribeModelPackageResponse Lude.Text
dmprsModelPackageARN = Lens.lens (modelPackageARN :: DescribeModelPackageResponse -> Lude.Text) (\s a -> s {modelPackageARN = a} :: DescribeModelPackageResponse)
{-# DEPRECATED dmprsModelPackageARN "Use generic-lens or generic-optics with 'modelPackageARN' instead." #-}

-- | A timestamp specifying when the model package was created.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmprsCreationTime :: Lens.Lens' DescribeModelPackageResponse Lude.Timestamp
dmprsCreationTime = Lens.lens (creationTime :: DescribeModelPackageResponse -> Lude.Timestamp) (\s a -> s {creationTime = a} :: DescribeModelPackageResponse)
{-# DEPRECATED dmprsCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

-- | The current status of the model package.
--
-- /Note:/ Consider using 'modelPackageStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmprsModelPackageStatus :: Lens.Lens' DescribeModelPackageResponse ModelPackageStatus
dmprsModelPackageStatus = Lens.lens (modelPackageStatus :: DescribeModelPackageResponse -> ModelPackageStatus) (\s a -> s {modelPackageStatus = a} :: DescribeModelPackageResponse)
{-# DEPRECATED dmprsModelPackageStatus "Use generic-lens or generic-optics with 'modelPackageStatus' instead." #-}

-- | Details about the current status of the model package.
--
-- /Note:/ Consider using 'modelPackageStatusDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmprsModelPackageStatusDetails :: Lens.Lens' DescribeModelPackageResponse ModelPackageStatusDetails
dmprsModelPackageStatusDetails = Lens.lens (modelPackageStatusDetails :: DescribeModelPackageResponse -> ModelPackageStatusDetails) (\s a -> s {modelPackageStatusDetails = a} :: DescribeModelPackageResponse)
{-# DEPRECATED dmprsModelPackageStatusDetails "Use generic-lens or generic-optics with 'modelPackageStatusDetails' instead." #-}
