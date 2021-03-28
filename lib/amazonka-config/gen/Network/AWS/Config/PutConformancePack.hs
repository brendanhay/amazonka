{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.PutConformancePack
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates or updates a conformance pack. A conformance pack is a collection of AWS Config rules that can be easily deployed in an account and a region and across AWS Organization.
--
-- This API creates a service linked role @AWSServiceRoleForConfigConforms@ in your account. The service linked role is created only when the role does not exist in your account. 
module Network.AWS.Config.PutConformancePack
    (
    -- * Creating a request
      PutConformancePack (..)
    , mkPutConformancePack
    -- ** Request lenses
    , pcpConformancePackName
    , pcpConformancePackInputParameters
    , pcpDeliveryS3Bucket
    , pcpDeliveryS3KeyPrefix
    , pcpTemplateBody
    , pcpTemplateS3Uri

    -- * Destructuring the response
    , PutConformancePackResponse (..)
    , mkPutConformancePackResponse
    -- ** Response lenses
    , pcprrsConformancePackArn
    , pcprrsResponseStatus
    ) where

import qualified Network.AWS.Config.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkPutConformancePack' smart constructor.
data PutConformancePack = PutConformancePack'
  { conformancePackName :: Types.ConformancePackName
    -- ^ Name of the conformance pack you want to create.
  , conformancePackInputParameters :: Core.Maybe [Types.ConformancePackInputParameter]
    -- ^ A list of @ConformancePackInputParameter@ objects.
  , deliveryS3Bucket :: Core.Maybe Types.DeliveryS3Bucket
    -- ^ AWS Config stores intermediate files while processing conformance pack template.
  , deliveryS3KeyPrefix :: Core.Maybe Types.DeliveryS3KeyPrefix
    -- ^ The prefix for the Amazon S3 bucket. 
  , templateBody :: Core.Maybe Types.TemplateBody
    -- ^ A string containing full conformance pack template body. Structure containing the template body with a minimum length of 1 byte and a maximum length of 51,200 bytes.
  , templateS3Uri :: Core.Maybe Types.TemplateS3Uri
    -- ^ Location of file containing the template body (@s3://bucketname/prefix@ ). The uri must point to the conformance pack template (max size: 300 KB) that is located in an Amazon S3 bucket in the same region as the conformance pack. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutConformancePack' value with any optional fields omitted.
mkPutConformancePack
    :: Types.ConformancePackName -- ^ 'conformancePackName'
    -> PutConformancePack
mkPutConformancePack conformancePackName
  = PutConformancePack'{conformancePackName,
                        conformancePackInputParameters = Core.Nothing,
                        deliveryS3Bucket = Core.Nothing,
                        deliveryS3KeyPrefix = Core.Nothing, templateBody = Core.Nothing,
                        templateS3Uri = Core.Nothing}

-- | Name of the conformance pack you want to create.
--
-- /Note:/ Consider using 'conformancePackName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcpConformancePackName :: Lens.Lens' PutConformancePack Types.ConformancePackName
pcpConformancePackName = Lens.field @"conformancePackName"
{-# INLINEABLE pcpConformancePackName #-}
{-# DEPRECATED conformancePackName "Use generic-lens or generic-optics with 'conformancePackName' instead"  #-}

-- | A list of @ConformancePackInputParameter@ objects.
--
-- /Note:/ Consider using 'conformancePackInputParameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcpConformancePackInputParameters :: Lens.Lens' PutConformancePack (Core.Maybe [Types.ConformancePackInputParameter])
pcpConformancePackInputParameters = Lens.field @"conformancePackInputParameters"
{-# INLINEABLE pcpConformancePackInputParameters #-}
{-# DEPRECATED conformancePackInputParameters "Use generic-lens or generic-optics with 'conformancePackInputParameters' instead"  #-}

-- | AWS Config stores intermediate files while processing conformance pack template.
--
-- /Note:/ Consider using 'deliveryS3Bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcpDeliveryS3Bucket :: Lens.Lens' PutConformancePack (Core.Maybe Types.DeliveryS3Bucket)
pcpDeliveryS3Bucket = Lens.field @"deliveryS3Bucket"
{-# INLINEABLE pcpDeliveryS3Bucket #-}
{-# DEPRECATED deliveryS3Bucket "Use generic-lens or generic-optics with 'deliveryS3Bucket' instead"  #-}

-- | The prefix for the Amazon S3 bucket. 
--
-- /Note:/ Consider using 'deliveryS3KeyPrefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcpDeliveryS3KeyPrefix :: Lens.Lens' PutConformancePack (Core.Maybe Types.DeliveryS3KeyPrefix)
pcpDeliveryS3KeyPrefix = Lens.field @"deliveryS3KeyPrefix"
{-# INLINEABLE pcpDeliveryS3KeyPrefix #-}
{-# DEPRECATED deliveryS3KeyPrefix "Use generic-lens or generic-optics with 'deliveryS3KeyPrefix' instead"  #-}

-- | A string containing full conformance pack template body. Structure containing the template body with a minimum length of 1 byte and a maximum length of 51,200 bytes.
--
-- /Note:/ Consider using 'templateBody' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcpTemplateBody :: Lens.Lens' PutConformancePack (Core.Maybe Types.TemplateBody)
pcpTemplateBody = Lens.field @"templateBody"
{-# INLINEABLE pcpTemplateBody #-}
{-# DEPRECATED templateBody "Use generic-lens or generic-optics with 'templateBody' instead"  #-}

-- | Location of file containing the template body (@s3://bucketname/prefix@ ). The uri must point to the conformance pack template (max size: 300 KB) that is located in an Amazon S3 bucket in the same region as the conformance pack. 
--
-- /Note:/ Consider using 'templateS3Uri' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcpTemplateS3Uri :: Lens.Lens' PutConformancePack (Core.Maybe Types.TemplateS3Uri)
pcpTemplateS3Uri = Lens.field @"templateS3Uri"
{-# INLINEABLE pcpTemplateS3Uri #-}
{-# DEPRECATED templateS3Uri "Use generic-lens or generic-optics with 'templateS3Uri' instead"  #-}

instance Core.ToQuery PutConformancePack where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders PutConformancePack where
        toHeaders PutConformancePack{..}
          = Core.pure
              ("X-Amz-Target", "StarlingDoveService.PutConformancePack")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON PutConformancePack where
        toJSON PutConformancePack{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("ConformancePackName" Core..= conformancePackName),
                  ("ConformancePackInputParameters" Core..=) Core.<$>
                    conformancePackInputParameters,
                  ("DeliveryS3Bucket" Core..=) Core.<$> deliveryS3Bucket,
                  ("DeliveryS3KeyPrefix" Core..=) Core.<$> deliveryS3KeyPrefix,
                  ("TemplateBody" Core..=) Core.<$> templateBody,
                  ("TemplateS3Uri" Core..=) Core.<$> templateS3Uri])

instance Core.AWSRequest PutConformancePack where
        type Rs PutConformancePack = PutConformancePackResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 PutConformancePackResponse' Core.<$>
                   (x Core..:? "ConformancePackArn") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkPutConformancePackResponse' smart constructor.
data PutConformancePackResponse = PutConformancePackResponse'
  { conformancePackArn :: Core.Maybe Types.ConformancePackArn
    -- ^ ARN of the conformance pack.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutConformancePackResponse' value with any optional fields omitted.
mkPutConformancePackResponse
    :: Core.Int -- ^ 'responseStatus'
    -> PutConformancePackResponse
mkPutConformancePackResponse responseStatus
  = PutConformancePackResponse'{conformancePackArn = Core.Nothing,
                                responseStatus}

-- | ARN of the conformance pack.
--
-- /Note:/ Consider using 'conformancePackArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcprrsConformancePackArn :: Lens.Lens' PutConformancePackResponse (Core.Maybe Types.ConformancePackArn)
pcprrsConformancePackArn = Lens.field @"conformancePackArn"
{-# INLINEABLE pcprrsConformancePackArn #-}
{-# DEPRECATED conformancePackArn "Use generic-lens or generic-optics with 'conformancePackArn' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcprrsResponseStatus :: Lens.Lens' PutConformancePackResponse Core.Int
pcprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE pcprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
