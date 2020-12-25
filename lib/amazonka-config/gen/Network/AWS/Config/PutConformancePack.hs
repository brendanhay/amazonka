{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    PutConformancePack (..),
    mkPutConformancePack,

    -- ** Request lenses
    pcpConformancePackName,
    pcpConformancePackInputParameters,
    pcpDeliveryS3Bucket,
    pcpDeliveryS3KeyPrefix,
    pcpTemplateBody,
    pcpTemplateS3Uri,

    -- * Destructuring the response
    PutConformancePackResponse (..),
    mkPutConformancePackResponse,

    -- ** Response lenses
    pcprrsConformancePackArn,
    pcprrsResponseStatus,
  )
where

import qualified Network.AWS.Config.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkPutConformancePack' smart constructor.
data PutConformancePack = PutConformancePack'
  { -- | Name of the conformance pack you want to create.
    conformancePackName :: Types.ConformancePackName,
    -- | A list of @ConformancePackInputParameter@ objects.
    conformancePackInputParameters :: Core.Maybe [Types.ConformancePackInputParameter],
    -- | AWS Config stores intermediate files while processing conformance pack template.
    deliveryS3Bucket :: Core.Maybe Types.DeliveryS3Bucket,
    -- | The prefix for the Amazon S3 bucket.
    deliveryS3KeyPrefix :: Core.Maybe Types.DeliveryS3KeyPrefix,
    -- | A string containing full conformance pack template body. Structure containing the template body with a minimum length of 1 byte and a maximum length of 51,200 bytes.
    templateBody :: Core.Maybe Types.TemplateBody,
    -- | Location of file containing the template body (@s3://bucketname/prefix@ ). The uri must point to the conformance pack template (max size: 300 KB) that is located in an Amazon S3 bucket in the same region as the conformance pack.
    templateS3Uri :: Core.Maybe Types.TemplateS3Uri
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutConformancePack' value with any optional fields omitted.
mkPutConformancePack ::
  -- | 'conformancePackName'
  Types.ConformancePackName ->
  PutConformancePack
mkPutConformancePack conformancePackName =
  PutConformancePack'
    { conformancePackName,
      conformancePackInputParameters = Core.Nothing,
      deliveryS3Bucket = Core.Nothing,
      deliveryS3KeyPrefix = Core.Nothing,
      templateBody = Core.Nothing,
      templateS3Uri = Core.Nothing
    }

-- | Name of the conformance pack you want to create.
--
-- /Note:/ Consider using 'conformancePackName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcpConformancePackName :: Lens.Lens' PutConformancePack Types.ConformancePackName
pcpConformancePackName = Lens.field @"conformancePackName"
{-# DEPRECATED pcpConformancePackName "Use generic-lens or generic-optics with 'conformancePackName' instead." #-}

-- | A list of @ConformancePackInputParameter@ objects.
--
-- /Note:/ Consider using 'conformancePackInputParameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcpConformancePackInputParameters :: Lens.Lens' PutConformancePack (Core.Maybe [Types.ConformancePackInputParameter])
pcpConformancePackInputParameters = Lens.field @"conformancePackInputParameters"
{-# DEPRECATED pcpConformancePackInputParameters "Use generic-lens or generic-optics with 'conformancePackInputParameters' instead." #-}

-- | AWS Config stores intermediate files while processing conformance pack template.
--
-- /Note:/ Consider using 'deliveryS3Bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcpDeliveryS3Bucket :: Lens.Lens' PutConformancePack (Core.Maybe Types.DeliveryS3Bucket)
pcpDeliveryS3Bucket = Lens.field @"deliveryS3Bucket"
{-# DEPRECATED pcpDeliveryS3Bucket "Use generic-lens or generic-optics with 'deliveryS3Bucket' instead." #-}

-- | The prefix for the Amazon S3 bucket.
--
-- /Note:/ Consider using 'deliveryS3KeyPrefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcpDeliveryS3KeyPrefix :: Lens.Lens' PutConformancePack (Core.Maybe Types.DeliveryS3KeyPrefix)
pcpDeliveryS3KeyPrefix = Lens.field @"deliveryS3KeyPrefix"
{-# DEPRECATED pcpDeliveryS3KeyPrefix "Use generic-lens or generic-optics with 'deliveryS3KeyPrefix' instead." #-}

-- | A string containing full conformance pack template body. Structure containing the template body with a minimum length of 1 byte and a maximum length of 51,200 bytes.
--
-- /Note:/ Consider using 'templateBody' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcpTemplateBody :: Lens.Lens' PutConformancePack (Core.Maybe Types.TemplateBody)
pcpTemplateBody = Lens.field @"templateBody"
{-# DEPRECATED pcpTemplateBody "Use generic-lens or generic-optics with 'templateBody' instead." #-}

-- | Location of file containing the template body (@s3://bucketname/prefix@ ). The uri must point to the conformance pack template (max size: 300 KB) that is located in an Amazon S3 bucket in the same region as the conformance pack.
--
-- /Note:/ Consider using 'templateS3Uri' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcpTemplateS3Uri :: Lens.Lens' PutConformancePack (Core.Maybe Types.TemplateS3Uri)
pcpTemplateS3Uri = Lens.field @"templateS3Uri"
{-# DEPRECATED pcpTemplateS3Uri "Use generic-lens or generic-optics with 'templateS3Uri' instead." #-}

instance Core.FromJSON PutConformancePack where
  toJSON PutConformancePack {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("ConformancePackName" Core..= conformancePackName),
            ("ConformancePackInputParameters" Core..=)
              Core.<$> conformancePackInputParameters,
            ("DeliveryS3Bucket" Core..=) Core.<$> deliveryS3Bucket,
            ("DeliveryS3KeyPrefix" Core..=) Core.<$> deliveryS3KeyPrefix,
            ("TemplateBody" Core..=) Core.<$> templateBody,
            ("TemplateS3Uri" Core..=) Core.<$> templateS3Uri
          ]
      )

instance Core.AWSRequest PutConformancePack where
  type Rs PutConformancePack = PutConformancePackResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "StarlingDoveService.PutConformancePack")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          PutConformancePackResponse'
            Core.<$> (x Core..:? "ConformancePackArn")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkPutConformancePackResponse' smart constructor.
data PutConformancePackResponse = PutConformancePackResponse'
  { -- | ARN of the conformance pack.
    conformancePackArn :: Core.Maybe Types.ConformancePackArn,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutConformancePackResponse' value with any optional fields omitted.
mkPutConformancePackResponse ::
  -- | 'responseStatus'
  Core.Int ->
  PutConformancePackResponse
mkPutConformancePackResponse responseStatus =
  PutConformancePackResponse'
    { conformancePackArn = Core.Nothing,
      responseStatus
    }

-- | ARN of the conformance pack.
--
-- /Note:/ Consider using 'conformancePackArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcprrsConformancePackArn :: Lens.Lens' PutConformancePackResponse (Core.Maybe Types.ConformancePackArn)
pcprrsConformancePackArn = Lens.field @"conformancePackArn"
{-# DEPRECATED pcprrsConformancePackArn "Use generic-lens or generic-optics with 'conformancePackArn' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcprrsResponseStatus :: Lens.Lens' PutConformancePackResponse Core.Int
pcprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED pcprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
