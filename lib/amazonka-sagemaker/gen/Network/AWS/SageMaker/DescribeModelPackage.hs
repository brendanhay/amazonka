{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
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
    dmprrsModelPackageName,
    dmprrsModelPackageArn,
    dmprrsCreationTime,
    dmprrsModelPackageStatus,
    dmprrsModelPackageStatusDetails,
    dmprrsCertifyForMarketplace,
    dmprrsInferenceSpecification,
    dmprrsModelPackageDescription,
    dmprrsSourceAlgorithmSpecification,
    dmprrsValidationSpecification,
    dmprrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SageMaker.Types as Types

-- | /See:/ 'mkDescribeModelPackage' smart constructor.
newtype DescribeModelPackage = DescribeModelPackage'
  { -- | The name of the model package to describe.
    modelPackageName :: Types.ModelPackageName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeModelPackage' value with any optional fields omitted.
mkDescribeModelPackage ::
  -- | 'modelPackageName'
  Types.ModelPackageName ->
  DescribeModelPackage
mkDescribeModelPackage modelPackageName =
  DescribeModelPackage' {modelPackageName}

-- | The name of the model package to describe.
--
-- /Note:/ Consider using 'modelPackageName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dModelPackageName :: Lens.Lens' DescribeModelPackage Types.ModelPackageName
dModelPackageName = Lens.field @"modelPackageName"
{-# DEPRECATED dModelPackageName "Use generic-lens or generic-optics with 'modelPackageName' instead." #-}

instance Core.FromJSON DescribeModelPackage where
  toJSON DescribeModelPackage {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("ModelPackageName" Core..= modelPackageName)]
      )

instance Core.AWSRequest DescribeModelPackage where
  type Rs DescribeModelPackage = DescribeModelPackageResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "SageMaker.DescribeModelPackage")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeModelPackageResponse'
            Core.<$> (x Core..: "ModelPackageName")
            Core.<*> (x Core..: "ModelPackageArn")
            Core.<*> (x Core..: "CreationTime")
            Core.<*> (x Core..: "ModelPackageStatus")
            Core.<*> (x Core..: "ModelPackageStatusDetails")
            Core.<*> (x Core..:? "CertifyForMarketplace")
            Core.<*> (x Core..:? "InferenceSpecification")
            Core.<*> (x Core..:? "ModelPackageDescription")
            Core.<*> (x Core..:? "SourceAlgorithmSpecification")
            Core.<*> (x Core..:? "ValidationSpecification")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDescribeModelPackageResponse' smart constructor.
data DescribeModelPackageResponse = DescribeModelPackageResponse'
  { -- | The name of the model package being described.
    modelPackageName :: Types.EntityName,
    -- | The Amazon Resource Name (ARN) of the model package.
    modelPackageArn :: Types.ModelPackageArn,
    -- | A timestamp specifying when the model package was created.
    creationTime :: Core.NominalDiffTime,
    -- | The current status of the model package.
    modelPackageStatus :: Types.ModelPackageStatus,
    -- | Details about the current status of the model package.
    modelPackageStatusDetails :: Types.ModelPackageStatusDetails,
    -- | Whether the model package is certified for listing on AWS Marketplace.
    certifyForMarketplace :: Core.Maybe Core.Bool,
    -- | Details about inference jobs that can be run with models based on this model package.
    inferenceSpecification :: Core.Maybe Types.InferenceSpecification,
    -- | A brief summary of the model package.
    modelPackageDescription :: Core.Maybe Types.EntityDescription,
    -- | Details about the algorithm that was used to create the model package.
    sourceAlgorithmSpecification :: Core.Maybe Types.SourceAlgorithmSpecification,
    -- | Configurations for one or more transform jobs that Amazon SageMaker runs to test the model package.
    validationSpecification :: Core.Maybe Types.ModelPackageValidationSpecification,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribeModelPackageResponse' value with any optional fields omitted.
mkDescribeModelPackageResponse ::
  -- | 'modelPackageName'
  Types.EntityName ->
  -- | 'modelPackageArn'
  Types.ModelPackageArn ->
  -- | 'creationTime'
  Core.NominalDiffTime ->
  -- | 'modelPackageStatus'
  Types.ModelPackageStatus ->
  -- | 'modelPackageStatusDetails'
  Types.ModelPackageStatusDetails ->
  -- | 'responseStatus'
  Core.Int ->
  DescribeModelPackageResponse
mkDescribeModelPackageResponse
  modelPackageName
  modelPackageArn
  creationTime
  modelPackageStatus
  modelPackageStatusDetails
  responseStatus =
    DescribeModelPackageResponse'
      { modelPackageName,
        modelPackageArn,
        creationTime,
        modelPackageStatus,
        modelPackageStatusDetails,
        certifyForMarketplace = Core.Nothing,
        inferenceSpecification = Core.Nothing,
        modelPackageDescription = Core.Nothing,
        sourceAlgorithmSpecification = Core.Nothing,
        validationSpecification = Core.Nothing,
        responseStatus
      }

-- | The name of the model package being described.
--
-- /Note:/ Consider using 'modelPackageName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmprrsModelPackageName :: Lens.Lens' DescribeModelPackageResponse Types.EntityName
dmprrsModelPackageName = Lens.field @"modelPackageName"
{-# DEPRECATED dmprrsModelPackageName "Use generic-lens or generic-optics with 'modelPackageName' instead." #-}

-- | The Amazon Resource Name (ARN) of the model package.
--
-- /Note:/ Consider using 'modelPackageArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmprrsModelPackageArn :: Lens.Lens' DescribeModelPackageResponse Types.ModelPackageArn
dmprrsModelPackageArn = Lens.field @"modelPackageArn"
{-# DEPRECATED dmprrsModelPackageArn "Use generic-lens or generic-optics with 'modelPackageArn' instead." #-}

-- | A timestamp specifying when the model package was created.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmprrsCreationTime :: Lens.Lens' DescribeModelPackageResponse Core.NominalDiffTime
dmprrsCreationTime = Lens.field @"creationTime"
{-# DEPRECATED dmprrsCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

-- | The current status of the model package.
--
-- /Note:/ Consider using 'modelPackageStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmprrsModelPackageStatus :: Lens.Lens' DescribeModelPackageResponse Types.ModelPackageStatus
dmprrsModelPackageStatus = Lens.field @"modelPackageStatus"
{-# DEPRECATED dmprrsModelPackageStatus "Use generic-lens or generic-optics with 'modelPackageStatus' instead." #-}

-- | Details about the current status of the model package.
--
-- /Note:/ Consider using 'modelPackageStatusDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmprrsModelPackageStatusDetails :: Lens.Lens' DescribeModelPackageResponse Types.ModelPackageStatusDetails
dmprrsModelPackageStatusDetails = Lens.field @"modelPackageStatusDetails"
{-# DEPRECATED dmprrsModelPackageStatusDetails "Use generic-lens or generic-optics with 'modelPackageStatusDetails' instead." #-}

-- | Whether the model package is certified for listing on AWS Marketplace.
--
-- /Note:/ Consider using 'certifyForMarketplace' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmprrsCertifyForMarketplace :: Lens.Lens' DescribeModelPackageResponse (Core.Maybe Core.Bool)
dmprrsCertifyForMarketplace = Lens.field @"certifyForMarketplace"
{-# DEPRECATED dmprrsCertifyForMarketplace "Use generic-lens or generic-optics with 'certifyForMarketplace' instead." #-}

-- | Details about inference jobs that can be run with models based on this model package.
--
-- /Note:/ Consider using 'inferenceSpecification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmprrsInferenceSpecification :: Lens.Lens' DescribeModelPackageResponse (Core.Maybe Types.InferenceSpecification)
dmprrsInferenceSpecification = Lens.field @"inferenceSpecification"
{-# DEPRECATED dmprrsInferenceSpecification "Use generic-lens or generic-optics with 'inferenceSpecification' instead." #-}

-- | A brief summary of the model package.
--
-- /Note:/ Consider using 'modelPackageDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmprrsModelPackageDescription :: Lens.Lens' DescribeModelPackageResponse (Core.Maybe Types.EntityDescription)
dmprrsModelPackageDescription = Lens.field @"modelPackageDescription"
{-# DEPRECATED dmprrsModelPackageDescription "Use generic-lens or generic-optics with 'modelPackageDescription' instead." #-}

-- | Details about the algorithm that was used to create the model package.
--
-- /Note:/ Consider using 'sourceAlgorithmSpecification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmprrsSourceAlgorithmSpecification :: Lens.Lens' DescribeModelPackageResponse (Core.Maybe Types.SourceAlgorithmSpecification)
dmprrsSourceAlgorithmSpecification = Lens.field @"sourceAlgorithmSpecification"
{-# DEPRECATED dmprrsSourceAlgorithmSpecification "Use generic-lens or generic-optics with 'sourceAlgorithmSpecification' instead." #-}

-- | Configurations for one or more transform jobs that Amazon SageMaker runs to test the model package.
--
-- /Note:/ Consider using 'validationSpecification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmprrsValidationSpecification :: Lens.Lens' DescribeModelPackageResponse (Core.Maybe Types.ModelPackageValidationSpecification)
dmprrsValidationSpecification = Lens.field @"validationSpecification"
{-# DEPRECATED dmprrsValidationSpecification "Use generic-lens or generic-optics with 'validationSpecification' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmprrsResponseStatus :: Lens.Lens' DescribeModelPackageResponse Core.Int
dmprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dmprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
