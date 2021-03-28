{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      DescribeModelPackage (..)
    , mkDescribeModelPackage
    -- ** Request lenses
    , dModelPackageName

    -- * Destructuring the response
    , DescribeModelPackageResponse (..)
    , mkDescribeModelPackageResponse
    -- ** Response lenses
    , dmprrsModelPackageName
    , dmprrsModelPackageArn
    , dmprrsCreationTime
    , dmprrsModelPackageStatus
    , dmprrsModelPackageStatusDetails
    , dmprrsCertifyForMarketplace
    , dmprrsInferenceSpecification
    , dmprrsModelPackageDescription
    , dmprrsSourceAlgorithmSpecification
    , dmprrsValidationSpecification
    , dmprrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SageMaker.Types as Types

-- | /See:/ 'mkDescribeModelPackage' smart constructor.
newtype DescribeModelPackage = DescribeModelPackage'
  { modelPackageName :: Types.ModelPackageName
    -- ^ The name of the model package to describe.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeModelPackage' value with any optional fields omitted.
mkDescribeModelPackage
    :: Types.ModelPackageName -- ^ 'modelPackageName'
    -> DescribeModelPackage
mkDescribeModelPackage modelPackageName
  = DescribeModelPackage'{modelPackageName}

-- | The name of the model package to describe.
--
-- /Note:/ Consider using 'modelPackageName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dModelPackageName :: Lens.Lens' DescribeModelPackage Types.ModelPackageName
dModelPackageName = Lens.field @"modelPackageName"
{-# INLINEABLE dModelPackageName #-}
{-# DEPRECATED modelPackageName "Use generic-lens or generic-optics with 'modelPackageName' instead"  #-}

instance Core.ToQuery DescribeModelPackage where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeModelPackage where
        toHeaders DescribeModelPackage{..}
          = Core.pure ("X-Amz-Target", "SageMaker.DescribeModelPackage")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DescribeModelPackage where
        toJSON DescribeModelPackage{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("ModelPackageName" Core..= modelPackageName)])

instance Core.AWSRequest DescribeModelPackage where
        type Rs DescribeModelPackage = DescribeModelPackageResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeModelPackageResponse' Core.<$>
                   (x Core..: "ModelPackageName") Core.<*> x Core..: "ModelPackageArn"
                     Core.<*> x Core..: "CreationTime"
                     Core.<*> x Core..: "ModelPackageStatus"
                     Core.<*> x Core..: "ModelPackageStatusDetails"
                     Core.<*> x Core..:? "CertifyForMarketplace"
                     Core.<*> x Core..:? "InferenceSpecification"
                     Core.<*> x Core..:? "ModelPackageDescription"
                     Core.<*> x Core..:? "SourceAlgorithmSpecification"
                     Core.<*> x Core..:? "ValidationSpecification"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDescribeModelPackageResponse' smart constructor.
data DescribeModelPackageResponse = DescribeModelPackageResponse'
  { modelPackageName :: Types.EntityName
    -- ^ The name of the model package being described.
  , modelPackageArn :: Types.ModelPackageArn
    -- ^ The Amazon Resource Name (ARN) of the model package.
  , creationTime :: Core.NominalDiffTime
    -- ^ A timestamp specifying when the model package was created.
  , modelPackageStatus :: Types.ModelPackageStatus
    -- ^ The current status of the model package.
  , modelPackageStatusDetails :: Types.ModelPackageStatusDetails
    -- ^ Details about the current status of the model package.
  , certifyForMarketplace :: Core.Maybe Core.Bool
    -- ^ Whether the model package is certified for listing on AWS Marketplace.
  , inferenceSpecification :: Core.Maybe Types.InferenceSpecification
    -- ^ Details about inference jobs that can be run with models based on this model package.
  , modelPackageDescription :: Core.Maybe Types.EntityDescription
    -- ^ A brief summary of the model package.
  , sourceAlgorithmSpecification :: Core.Maybe Types.SourceAlgorithmSpecification
    -- ^ Details about the algorithm that was used to create the model package.
  , validationSpecification :: Core.Maybe Types.ModelPackageValidationSpecification
    -- ^ Configurations for one or more transform jobs that Amazon SageMaker runs to test the model package.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeModelPackageResponse' value with any optional fields omitted.
mkDescribeModelPackageResponse
    :: Types.EntityName -- ^ 'modelPackageName'
    -> Types.ModelPackageArn -- ^ 'modelPackageArn'
    -> Core.NominalDiffTime -- ^ 'creationTime'
    -> Types.ModelPackageStatus -- ^ 'modelPackageStatus'
    -> Types.ModelPackageStatusDetails -- ^ 'modelPackageStatusDetails'
    -> Core.Int -- ^ 'responseStatus'
    -> DescribeModelPackageResponse
mkDescribeModelPackageResponse modelPackageName modelPackageArn
  creationTime modelPackageStatus modelPackageStatusDetails
  responseStatus
  = DescribeModelPackageResponse'{modelPackageName, modelPackageArn,
                                  creationTime, modelPackageStatus, modelPackageStatusDetails,
                                  certifyForMarketplace = Core.Nothing,
                                  inferenceSpecification = Core.Nothing,
                                  modelPackageDescription = Core.Nothing,
                                  sourceAlgorithmSpecification = Core.Nothing,
                                  validationSpecification = Core.Nothing, responseStatus}

-- | The name of the model package being described.
--
-- /Note:/ Consider using 'modelPackageName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmprrsModelPackageName :: Lens.Lens' DescribeModelPackageResponse Types.EntityName
dmprrsModelPackageName = Lens.field @"modelPackageName"
{-# INLINEABLE dmprrsModelPackageName #-}
{-# DEPRECATED modelPackageName "Use generic-lens or generic-optics with 'modelPackageName' instead"  #-}

-- | The Amazon Resource Name (ARN) of the model package.
--
-- /Note:/ Consider using 'modelPackageArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmprrsModelPackageArn :: Lens.Lens' DescribeModelPackageResponse Types.ModelPackageArn
dmprrsModelPackageArn = Lens.field @"modelPackageArn"
{-# INLINEABLE dmprrsModelPackageArn #-}
{-# DEPRECATED modelPackageArn "Use generic-lens or generic-optics with 'modelPackageArn' instead"  #-}

-- | A timestamp specifying when the model package was created.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmprrsCreationTime :: Lens.Lens' DescribeModelPackageResponse Core.NominalDiffTime
dmprrsCreationTime = Lens.field @"creationTime"
{-# INLINEABLE dmprrsCreationTime #-}
{-# DEPRECATED creationTime "Use generic-lens or generic-optics with 'creationTime' instead"  #-}

-- | The current status of the model package.
--
-- /Note:/ Consider using 'modelPackageStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmprrsModelPackageStatus :: Lens.Lens' DescribeModelPackageResponse Types.ModelPackageStatus
dmprrsModelPackageStatus = Lens.field @"modelPackageStatus"
{-# INLINEABLE dmprrsModelPackageStatus #-}
{-# DEPRECATED modelPackageStatus "Use generic-lens or generic-optics with 'modelPackageStatus' instead"  #-}

-- | Details about the current status of the model package.
--
-- /Note:/ Consider using 'modelPackageStatusDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmprrsModelPackageStatusDetails :: Lens.Lens' DescribeModelPackageResponse Types.ModelPackageStatusDetails
dmprrsModelPackageStatusDetails = Lens.field @"modelPackageStatusDetails"
{-# INLINEABLE dmprrsModelPackageStatusDetails #-}
{-# DEPRECATED modelPackageStatusDetails "Use generic-lens or generic-optics with 'modelPackageStatusDetails' instead"  #-}

-- | Whether the model package is certified for listing on AWS Marketplace.
--
-- /Note:/ Consider using 'certifyForMarketplace' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmprrsCertifyForMarketplace :: Lens.Lens' DescribeModelPackageResponse (Core.Maybe Core.Bool)
dmprrsCertifyForMarketplace = Lens.field @"certifyForMarketplace"
{-# INLINEABLE dmprrsCertifyForMarketplace #-}
{-# DEPRECATED certifyForMarketplace "Use generic-lens or generic-optics with 'certifyForMarketplace' instead"  #-}

-- | Details about inference jobs that can be run with models based on this model package.
--
-- /Note:/ Consider using 'inferenceSpecification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmprrsInferenceSpecification :: Lens.Lens' DescribeModelPackageResponse (Core.Maybe Types.InferenceSpecification)
dmprrsInferenceSpecification = Lens.field @"inferenceSpecification"
{-# INLINEABLE dmprrsInferenceSpecification #-}
{-# DEPRECATED inferenceSpecification "Use generic-lens or generic-optics with 'inferenceSpecification' instead"  #-}

-- | A brief summary of the model package.
--
-- /Note:/ Consider using 'modelPackageDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmprrsModelPackageDescription :: Lens.Lens' DescribeModelPackageResponse (Core.Maybe Types.EntityDescription)
dmprrsModelPackageDescription = Lens.field @"modelPackageDescription"
{-# INLINEABLE dmprrsModelPackageDescription #-}
{-# DEPRECATED modelPackageDescription "Use generic-lens or generic-optics with 'modelPackageDescription' instead"  #-}

-- | Details about the algorithm that was used to create the model package.
--
-- /Note:/ Consider using 'sourceAlgorithmSpecification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmprrsSourceAlgorithmSpecification :: Lens.Lens' DescribeModelPackageResponse (Core.Maybe Types.SourceAlgorithmSpecification)
dmprrsSourceAlgorithmSpecification = Lens.field @"sourceAlgorithmSpecification"
{-# INLINEABLE dmprrsSourceAlgorithmSpecification #-}
{-# DEPRECATED sourceAlgorithmSpecification "Use generic-lens or generic-optics with 'sourceAlgorithmSpecification' instead"  #-}

-- | Configurations for one or more transform jobs that Amazon SageMaker runs to test the model package.
--
-- /Note:/ Consider using 'validationSpecification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmprrsValidationSpecification :: Lens.Lens' DescribeModelPackageResponse (Core.Maybe Types.ModelPackageValidationSpecification)
dmprrsValidationSpecification = Lens.field @"validationSpecification"
{-# INLINEABLE dmprrsValidationSpecification #-}
{-# DEPRECATED validationSpecification "Use generic-lens or generic-optics with 'validationSpecification' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmprrsResponseStatus :: Lens.Lens' DescribeModelPackageResponse Core.Int
dmprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dmprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
