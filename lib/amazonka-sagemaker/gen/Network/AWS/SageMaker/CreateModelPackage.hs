{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
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
    cmpCertifyForMarketplace,
    cmpInferenceSpecification,
    cmpModelPackageDescription,
    cmpModelPackageName,
    cmpSourceAlgorithmSpecification,
    cmpValidationSpecification,

    -- * Destructuring the response
    CreateModelPackageResponse (..),
    mkCreateModelPackageResponse,

    -- ** Response lenses
    cmprrsModelPackageArn,
    cmprrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SageMaker.Types as Types

-- | /See:/ 'mkCreateModelPackage' smart constructor.
data CreateModelPackage = CreateModelPackage'
  { -- | Whether to certify the model package for listing on AWS Marketplace.
    certifyForMarketplace :: Core.Maybe Core.Bool,
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
    inferenceSpecification :: Core.Maybe Types.InferenceSpecification,
    -- | A description of the model package.
    modelPackageDescription :: Core.Maybe Types.ModelPackageDescription,
    -- | The name of the model package. The name must have 1 to 63 characters. Valid characters are a-z, A-Z, 0-9, and - (hyphen).
    modelPackageName :: Core.Maybe Types.EntityName,
    -- | Details about the algorithm that was used to create the model package.
    sourceAlgorithmSpecification :: Core.Maybe Types.SourceAlgorithmSpecification,
    -- | Specifies configurations for one or more transform jobs that Amazon SageMaker runs to test the model package.
    validationSpecification :: Core.Maybe Types.ModelPackageValidationSpecification
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateModelPackage' value with any optional fields omitted.
mkCreateModelPackage ::
  CreateModelPackage
mkCreateModelPackage =
  CreateModelPackage'
    { certifyForMarketplace = Core.Nothing,
      inferenceSpecification = Core.Nothing,
      modelPackageDescription = Core.Nothing,
      modelPackageName = Core.Nothing,
      sourceAlgorithmSpecification = Core.Nothing,
      validationSpecification = Core.Nothing
    }

-- | Whether to certify the model package for listing on AWS Marketplace.
--
-- /Note:/ Consider using 'certifyForMarketplace' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmpCertifyForMarketplace :: Lens.Lens' CreateModelPackage (Core.Maybe Core.Bool)
cmpCertifyForMarketplace = Lens.field @"certifyForMarketplace"
{-# DEPRECATED cmpCertifyForMarketplace "Use generic-lens or generic-optics with 'certifyForMarketplace' instead." #-}

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
cmpInferenceSpecification :: Lens.Lens' CreateModelPackage (Core.Maybe Types.InferenceSpecification)
cmpInferenceSpecification = Lens.field @"inferenceSpecification"
{-# DEPRECATED cmpInferenceSpecification "Use generic-lens or generic-optics with 'inferenceSpecification' instead." #-}

-- | A description of the model package.
--
-- /Note:/ Consider using 'modelPackageDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmpModelPackageDescription :: Lens.Lens' CreateModelPackage (Core.Maybe Types.ModelPackageDescription)
cmpModelPackageDescription = Lens.field @"modelPackageDescription"
{-# DEPRECATED cmpModelPackageDescription "Use generic-lens or generic-optics with 'modelPackageDescription' instead." #-}

-- | The name of the model package. The name must have 1 to 63 characters. Valid characters are a-z, A-Z, 0-9, and - (hyphen).
--
-- /Note:/ Consider using 'modelPackageName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmpModelPackageName :: Lens.Lens' CreateModelPackage (Core.Maybe Types.EntityName)
cmpModelPackageName = Lens.field @"modelPackageName"
{-# DEPRECATED cmpModelPackageName "Use generic-lens or generic-optics with 'modelPackageName' instead." #-}

-- | Details about the algorithm that was used to create the model package.
--
-- /Note:/ Consider using 'sourceAlgorithmSpecification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmpSourceAlgorithmSpecification :: Lens.Lens' CreateModelPackage (Core.Maybe Types.SourceAlgorithmSpecification)
cmpSourceAlgorithmSpecification = Lens.field @"sourceAlgorithmSpecification"
{-# DEPRECATED cmpSourceAlgorithmSpecification "Use generic-lens or generic-optics with 'sourceAlgorithmSpecification' instead." #-}

-- | Specifies configurations for one or more transform jobs that Amazon SageMaker runs to test the model package.
--
-- /Note:/ Consider using 'validationSpecification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmpValidationSpecification :: Lens.Lens' CreateModelPackage (Core.Maybe Types.ModelPackageValidationSpecification)
cmpValidationSpecification = Lens.field @"validationSpecification"
{-# DEPRECATED cmpValidationSpecification "Use generic-lens or generic-optics with 'validationSpecification' instead." #-}

instance Core.FromJSON CreateModelPackage where
  toJSON CreateModelPackage {..} =
    Core.object
      ( Core.catMaybes
          [ ("CertifyForMarketplace" Core..=) Core.<$> certifyForMarketplace,
            ("InferenceSpecification" Core..=) Core.<$> inferenceSpecification,
            ("ModelPackageDescription" Core..=)
              Core.<$> modelPackageDescription,
            ("ModelPackageName" Core..=) Core.<$> modelPackageName,
            ("SourceAlgorithmSpecification" Core..=)
              Core.<$> sourceAlgorithmSpecification,
            ("ValidationSpecification" Core..=)
              Core.<$> validationSpecification
          ]
      )

instance Core.AWSRequest CreateModelPackage where
  type Rs CreateModelPackage = CreateModelPackageResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "SageMaker.CreateModelPackage")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateModelPackageResponse'
            Core.<$> (x Core..: "ModelPackageArn")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCreateModelPackageResponse' smart constructor.
data CreateModelPackageResponse = CreateModelPackageResponse'
  { -- | The Amazon Resource Name (ARN) of the new model package.
    modelPackageArn :: Types.ModelPackageArn,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateModelPackageResponse' value with any optional fields omitted.
mkCreateModelPackageResponse ::
  -- | 'modelPackageArn'
  Types.ModelPackageArn ->
  -- | 'responseStatus'
  Core.Int ->
  CreateModelPackageResponse
mkCreateModelPackageResponse modelPackageArn responseStatus =
  CreateModelPackageResponse' {modelPackageArn, responseStatus}

-- | The Amazon Resource Name (ARN) of the new model package.
--
-- /Note:/ Consider using 'modelPackageArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmprrsModelPackageArn :: Lens.Lens' CreateModelPackageResponse Types.ModelPackageArn
cmprrsModelPackageArn = Lens.field @"modelPackageArn"
{-# DEPRECATED cmprrsModelPackageArn "Use generic-lens or generic-optics with 'modelPackageArn' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmprrsResponseStatus :: Lens.Lens' CreateModelPackageResponse Core.Int
cmprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED cmprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
