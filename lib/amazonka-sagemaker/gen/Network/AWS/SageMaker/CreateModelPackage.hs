{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.CreateModelPackage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a model package that you can use to create Amazon SageMaker models or list on AWS Marketplace. Buyers can subscribe to model packages listed on AWS Marketplace to create models in Amazon SageMaker.
--
-- To create a model package by specifying a Docker container that contains your inference code and the Amazon S3 location of your model artifacts, provide values for @InferenceSpecification@ . To create a model from an algorithm resource that you created or subscribed to in AWS Marketplace, provide a value for @SourceAlgorithmSpecification@ .
module Network.AWS.SageMaker.CreateModelPackage
  ( -- * Creating a request
    CreateModelPackage (..),
    mkCreateModelPackage,

    -- ** Request lenses
    cmpSourceAlgorithmSpecification,
    cmpModelPackageName,
    cmpModelPackageDescription,
    cmpValidationSpecification,
    cmpInferenceSpecification,
    cmpCertifyForMarketplace,

    -- * Destructuring the response
    CreateModelPackageResponse (..),
    mkCreateModelPackageResponse,

    -- ** Response lenses
    cmprsResponseStatus,
    cmprsModelPackageARN,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SageMaker.Types

-- | /See:/ 'mkCreateModelPackage' smart constructor.
data CreateModelPackage = CreateModelPackage'
  { sourceAlgorithmSpecification ::
      Lude.Maybe SourceAlgorithmSpecification,
    modelPackageName :: Lude.Maybe Lude.Text,
    modelPackageDescription :: Lude.Maybe Lude.Text,
    validationSpecification ::
      Lude.Maybe ModelPackageValidationSpecification,
    inferenceSpecification ::
      Lude.Maybe InferenceSpecification,
    certifyForMarketplace :: Lude.Maybe Lude.Bool
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateModelPackage' with the minimum fields required to make a request.
--
-- * 'certifyForMarketplace' - Whether to certify the model package for listing on AWS Marketplace.
-- * 'inferenceSpecification' - Specifies details about inference jobs that can be run with models based on this model package, including the following:
--
--
--     * The Amazon ECR paths of containers that contain the inference code and model artifacts.
--
--
--     * The instance types that the model package supports for transform jobs and real-time endpoints used for inference.
--
--
--     * The input and output content formats that the model package supports for inference.
--
--
-- * 'modelPackageDescription' - A description of the model package.
-- * 'modelPackageName' - The name of the model package. The name must have 1 to 63 characters. Valid characters are a-z, A-Z, 0-9, and - (hyphen).
-- * 'sourceAlgorithmSpecification' - Details about the algorithm that was used to create the model package.
-- * 'validationSpecification' - Specifies configurations for one or more transform jobs that Amazon SageMaker runs to test the model package.
mkCreateModelPackage ::
  CreateModelPackage
mkCreateModelPackage =
  CreateModelPackage'
    { sourceAlgorithmSpecification = Lude.Nothing,
      modelPackageName = Lude.Nothing,
      modelPackageDescription = Lude.Nothing,
      validationSpecification = Lude.Nothing,
      inferenceSpecification = Lude.Nothing,
      certifyForMarketplace = Lude.Nothing
    }

-- | Details about the algorithm that was used to create the model package.
--
-- /Note:/ Consider using 'sourceAlgorithmSpecification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmpSourceAlgorithmSpecification :: Lens.Lens' CreateModelPackage (Lude.Maybe SourceAlgorithmSpecification)
cmpSourceAlgorithmSpecification = Lens.lens (sourceAlgorithmSpecification :: CreateModelPackage -> Lude.Maybe SourceAlgorithmSpecification) (\s a -> s {sourceAlgorithmSpecification = a} :: CreateModelPackage)
{-# DEPRECATED cmpSourceAlgorithmSpecification "Use generic-lens or generic-optics with 'sourceAlgorithmSpecification' instead." #-}

-- | The name of the model package. The name must have 1 to 63 characters. Valid characters are a-z, A-Z, 0-9, and - (hyphen).
--
-- /Note:/ Consider using 'modelPackageName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmpModelPackageName :: Lens.Lens' CreateModelPackage (Lude.Maybe Lude.Text)
cmpModelPackageName = Lens.lens (modelPackageName :: CreateModelPackage -> Lude.Maybe Lude.Text) (\s a -> s {modelPackageName = a} :: CreateModelPackage)
{-# DEPRECATED cmpModelPackageName "Use generic-lens or generic-optics with 'modelPackageName' instead." #-}

-- | A description of the model package.
--
-- /Note:/ Consider using 'modelPackageDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmpModelPackageDescription :: Lens.Lens' CreateModelPackage (Lude.Maybe Lude.Text)
cmpModelPackageDescription = Lens.lens (modelPackageDescription :: CreateModelPackage -> Lude.Maybe Lude.Text) (\s a -> s {modelPackageDescription = a} :: CreateModelPackage)
{-# DEPRECATED cmpModelPackageDescription "Use generic-lens or generic-optics with 'modelPackageDescription' instead." #-}

-- | Specifies configurations for one or more transform jobs that Amazon SageMaker runs to test the model package.
--
-- /Note:/ Consider using 'validationSpecification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmpValidationSpecification :: Lens.Lens' CreateModelPackage (Lude.Maybe ModelPackageValidationSpecification)
cmpValidationSpecification = Lens.lens (validationSpecification :: CreateModelPackage -> Lude.Maybe ModelPackageValidationSpecification) (\s a -> s {validationSpecification = a} :: CreateModelPackage)
{-# DEPRECATED cmpValidationSpecification "Use generic-lens or generic-optics with 'validationSpecification' instead." #-}

-- | Specifies details about inference jobs that can be run with models based on this model package, including the following:
--
--
--     * The Amazon ECR paths of containers that contain the inference code and model artifacts.
--
--
--     * The instance types that the model package supports for transform jobs and real-time endpoints used for inference.
--
--
--     * The input and output content formats that the model package supports for inference.
--
--
--
-- /Note:/ Consider using 'inferenceSpecification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmpInferenceSpecification :: Lens.Lens' CreateModelPackage (Lude.Maybe InferenceSpecification)
cmpInferenceSpecification = Lens.lens (inferenceSpecification :: CreateModelPackage -> Lude.Maybe InferenceSpecification) (\s a -> s {inferenceSpecification = a} :: CreateModelPackage)
{-# DEPRECATED cmpInferenceSpecification "Use generic-lens or generic-optics with 'inferenceSpecification' instead." #-}

-- | Whether to certify the model package for listing on AWS Marketplace.
--
-- /Note:/ Consider using 'certifyForMarketplace' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmpCertifyForMarketplace :: Lens.Lens' CreateModelPackage (Lude.Maybe Lude.Bool)
cmpCertifyForMarketplace = Lens.lens (certifyForMarketplace :: CreateModelPackage -> Lude.Maybe Lude.Bool) (\s a -> s {certifyForMarketplace = a} :: CreateModelPackage)
{-# DEPRECATED cmpCertifyForMarketplace "Use generic-lens or generic-optics with 'certifyForMarketplace' instead." #-}

instance Lude.AWSRequest CreateModelPackage where
  type Rs CreateModelPackage = CreateModelPackageResponse
  request = Req.postJSON sageMakerService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateModelPackageResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
            Lude.<*> (x Lude..:> "ModelPackageArn")
      )

instance Lude.ToHeaders CreateModelPackage where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("SageMaker.CreateModelPackage" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateModelPackage where
  toJSON CreateModelPackage' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("SourceAlgorithmSpecification" Lude..=)
              Lude.<$> sourceAlgorithmSpecification,
            ("ModelPackageName" Lude..=) Lude.<$> modelPackageName,
            ("ModelPackageDescription" Lude..=)
              Lude.<$> modelPackageDescription,
            ("ValidationSpecification" Lude..=)
              Lude.<$> validationSpecification,
            ("InferenceSpecification" Lude..=) Lude.<$> inferenceSpecification,
            ("CertifyForMarketplace" Lude..=) Lude.<$> certifyForMarketplace
          ]
      )

instance Lude.ToPath CreateModelPackage where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateModelPackage where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateModelPackageResponse' smart constructor.
data CreateModelPackageResponse = CreateModelPackageResponse'
  { responseStatus ::
      Lude.Int,
    modelPackageARN :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateModelPackageResponse' with the minimum fields required to make a request.
--
-- * 'modelPackageARN' - The Amazon Resource Name (ARN) of the new model package.
-- * 'responseStatus' - The response status code.
mkCreateModelPackageResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  -- | 'modelPackageARN'
  Lude.Text ->
  CreateModelPackageResponse
mkCreateModelPackageResponse pResponseStatus_ pModelPackageARN_ =
  CreateModelPackageResponse'
    { responseStatus = pResponseStatus_,
      modelPackageARN = pModelPackageARN_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmprsResponseStatus :: Lens.Lens' CreateModelPackageResponse Lude.Int
cmprsResponseStatus = Lens.lens (responseStatus :: CreateModelPackageResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateModelPackageResponse)
{-# DEPRECATED cmprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | The Amazon Resource Name (ARN) of the new model package.
--
-- /Note:/ Consider using 'modelPackageARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmprsModelPackageARN :: Lens.Lens' CreateModelPackageResponse Lude.Text
cmprsModelPackageARN = Lens.lens (modelPackageARN :: CreateModelPackageResponse -> Lude.Text) (\s a -> s {modelPackageARN = a} :: CreateModelPackageResponse)
{-# DEPRECATED cmprsModelPackageARN "Use generic-lens or generic-optics with 'modelPackageARN' instead." #-}
