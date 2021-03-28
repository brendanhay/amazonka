{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.PutOrganizationConformancePack
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deploys conformance packs across member accounts in an AWS Organization.
--
-- Only a master account and a delegated administrator can call this API. When calling this API with a delegated administrator, you must ensure AWS Organizations @ListDelegatedAdministrator@ permissions are added.
-- This API enables organization service access for @config-multiaccountsetup.amazonaws.com@ through the @EnableAWSServiceAccess@ action and creates a service linked role @AWSServiceRoleForConfigMultiAccountSetup@ in the master or delegated administrator account of your organization. The service linked role is created only when the role does not exist in the caller account. To use this API with delegated administrator, register a delegated administrator by calling AWS Organization @register-delegate-admin@ for @config-multiaccountsetup.amazonaws.com@ .
module Network.AWS.Config.PutOrganizationConformancePack
    (
    -- * Creating a request
      PutOrganizationConformancePack (..)
    , mkPutOrganizationConformancePack
    -- ** Request lenses
    , pocpOrganizationConformancePackName
    , pocpConformancePackInputParameters
    , pocpDeliveryS3Bucket
    , pocpDeliveryS3KeyPrefix
    , pocpExcludedAccounts
    , pocpTemplateBody
    , pocpTemplateS3Uri

    -- * Destructuring the response
    , PutOrganizationConformancePackResponse (..)
    , mkPutOrganizationConformancePackResponse
    -- ** Response lenses
    , pocprrsOrganizationConformancePackArn
    , pocprrsResponseStatus
    ) where

import qualified Network.AWS.Config.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkPutOrganizationConformancePack' smart constructor.
data PutOrganizationConformancePack = PutOrganizationConformancePack'
  { organizationConformancePackName :: Types.OrganizationConformancePackName
    -- ^ Name of the organization conformance pack you want to create.
  , conformancePackInputParameters :: Core.Maybe [Types.ConformancePackInputParameter]
    -- ^ A list of @ConformancePackInputParameter@ objects.
  , deliveryS3Bucket :: Core.Maybe Types.DeliveryS3Bucket
    -- ^ Location of an Amazon S3 bucket where AWS Config can deliver evaluation results. AWS Config stores intermediate files while processing conformance pack template. 
--
-- The delivery bucket name should start with awsconfigconforms. For example: "Resource": "arn:aws:s3:::your_bucket_name/*". For more information, see <https://docs.aws.amazon.com/config/latest/developerguide/conformance-pack-organization-apis.html Permissions for cross account bucket access> .
  , deliveryS3KeyPrefix :: Core.Maybe Types.DeliveryS3KeyPrefix
    -- ^ The prefix for the Amazon S3 bucket.
  , excludedAccounts :: Core.Maybe [Types.AccountId]
    -- ^ A list of AWS accounts to be excluded from an organization conformance pack while deploying a conformance pack.
  , templateBody :: Core.Maybe Types.TemplateBody
    -- ^ A string containing full conformance pack template body. Structure containing the template body with a minimum length of 1 byte and a maximum length of 51,200 bytes.
  , templateS3Uri :: Core.Maybe Types.TemplateS3Uri
    -- ^ Location of file containing the template body. The uri must point to the conformance pack template (max size: 300 KB).
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutOrganizationConformancePack' value with any optional fields omitted.
mkPutOrganizationConformancePack
    :: Types.OrganizationConformancePackName -- ^ 'organizationConformancePackName'
    -> PutOrganizationConformancePack
mkPutOrganizationConformancePack organizationConformancePackName
  = PutOrganizationConformancePack'{organizationConformancePackName,
                                    conformancePackInputParameters = Core.Nothing,
                                    deliveryS3Bucket = Core.Nothing,
                                    deliveryS3KeyPrefix = Core.Nothing,
                                    excludedAccounts = Core.Nothing, templateBody = Core.Nothing,
                                    templateS3Uri = Core.Nothing}

-- | Name of the organization conformance pack you want to create.
--
-- /Note:/ Consider using 'organizationConformancePackName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pocpOrganizationConformancePackName :: Lens.Lens' PutOrganizationConformancePack Types.OrganizationConformancePackName
pocpOrganizationConformancePackName = Lens.field @"organizationConformancePackName"
{-# INLINEABLE pocpOrganizationConformancePackName #-}
{-# DEPRECATED organizationConformancePackName "Use generic-lens or generic-optics with 'organizationConformancePackName' instead"  #-}

-- | A list of @ConformancePackInputParameter@ objects.
--
-- /Note:/ Consider using 'conformancePackInputParameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pocpConformancePackInputParameters :: Lens.Lens' PutOrganizationConformancePack (Core.Maybe [Types.ConformancePackInputParameter])
pocpConformancePackInputParameters = Lens.field @"conformancePackInputParameters"
{-# INLINEABLE pocpConformancePackInputParameters #-}
{-# DEPRECATED conformancePackInputParameters "Use generic-lens or generic-optics with 'conformancePackInputParameters' instead"  #-}

-- | Location of an Amazon S3 bucket where AWS Config can deliver evaluation results. AWS Config stores intermediate files while processing conformance pack template. 
--
-- The delivery bucket name should start with awsconfigconforms. For example: "Resource": "arn:aws:s3:::your_bucket_name/*". For more information, see <https://docs.aws.amazon.com/config/latest/developerguide/conformance-pack-organization-apis.html Permissions for cross account bucket access> .
--
-- /Note:/ Consider using 'deliveryS3Bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pocpDeliveryS3Bucket :: Lens.Lens' PutOrganizationConformancePack (Core.Maybe Types.DeliveryS3Bucket)
pocpDeliveryS3Bucket = Lens.field @"deliveryS3Bucket"
{-# INLINEABLE pocpDeliveryS3Bucket #-}
{-# DEPRECATED deliveryS3Bucket "Use generic-lens or generic-optics with 'deliveryS3Bucket' instead"  #-}

-- | The prefix for the Amazon S3 bucket.
--
-- /Note:/ Consider using 'deliveryS3KeyPrefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pocpDeliveryS3KeyPrefix :: Lens.Lens' PutOrganizationConformancePack (Core.Maybe Types.DeliveryS3KeyPrefix)
pocpDeliveryS3KeyPrefix = Lens.field @"deliveryS3KeyPrefix"
{-# INLINEABLE pocpDeliveryS3KeyPrefix #-}
{-# DEPRECATED deliveryS3KeyPrefix "Use generic-lens or generic-optics with 'deliveryS3KeyPrefix' instead"  #-}

-- | A list of AWS accounts to be excluded from an organization conformance pack while deploying a conformance pack.
--
-- /Note:/ Consider using 'excludedAccounts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pocpExcludedAccounts :: Lens.Lens' PutOrganizationConformancePack (Core.Maybe [Types.AccountId])
pocpExcludedAccounts = Lens.field @"excludedAccounts"
{-# INLINEABLE pocpExcludedAccounts #-}
{-# DEPRECATED excludedAccounts "Use generic-lens or generic-optics with 'excludedAccounts' instead"  #-}

-- | A string containing full conformance pack template body. Structure containing the template body with a minimum length of 1 byte and a maximum length of 51,200 bytes.
--
-- /Note:/ Consider using 'templateBody' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pocpTemplateBody :: Lens.Lens' PutOrganizationConformancePack (Core.Maybe Types.TemplateBody)
pocpTemplateBody = Lens.field @"templateBody"
{-# INLINEABLE pocpTemplateBody #-}
{-# DEPRECATED templateBody "Use generic-lens or generic-optics with 'templateBody' instead"  #-}

-- | Location of file containing the template body. The uri must point to the conformance pack template (max size: 300 KB).
--
-- /Note:/ Consider using 'templateS3Uri' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pocpTemplateS3Uri :: Lens.Lens' PutOrganizationConformancePack (Core.Maybe Types.TemplateS3Uri)
pocpTemplateS3Uri = Lens.field @"templateS3Uri"
{-# INLINEABLE pocpTemplateS3Uri #-}
{-# DEPRECATED templateS3Uri "Use generic-lens or generic-optics with 'templateS3Uri' instead"  #-}

instance Core.ToQuery PutOrganizationConformancePack where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders PutOrganizationConformancePack where
        toHeaders PutOrganizationConformancePack{..}
          = Core.pure
              ("X-Amz-Target",
               "StarlingDoveService.PutOrganizationConformancePack")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON PutOrganizationConformancePack where
        toJSON PutOrganizationConformancePack{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just
                    ("OrganizationConformancePackName" Core..=
                       organizationConformancePackName),
                  ("ConformancePackInputParameters" Core..=) Core.<$>
                    conformancePackInputParameters,
                  ("DeliveryS3Bucket" Core..=) Core.<$> deliveryS3Bucket,
                  ("DeliveryS3KeyPrefix" Core..=) Core.<$> deliveryS3KeyPrefix,
                  ("ExcludedAccounts" Core..=) Core.<$> excludedAccounts,
                  ("TemplateBody" Core..=) Core.<$> templateBody,
                  ("TemplateS3Uri" Core..=) Core.<$> templateS3Uri])

instance Core.AWSRequest PutOrganizationConformancePack where
        type Rs PutOrganizationConformancePack =
             PutOrganizationConformancePackResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 PutOrganizationConformancePackResponse' Core.<$>
                   (x Core..:? "OrganizationConformancePackArn") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkPutOrganizationConformancePackResponse' smart constructor.
data PutOrganizationConformancePackResponse = PutOrganizationConformancePackResponse'
  { organizationConformancePackArn :: Core.Maybe Types.StringWithCharLimit256
    -- ^ ARN of the organization conformance pack.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutOrganizationConformancePackResponse' value with any optional fields omitted.
mkPutOrganizationConformancePackResponse
    :: Core.Int -- ^ 'responseStatus'
    -> PutOrganizationConformancePackResponse
mkPutOrganizationConformancePackResponse responseStatus
  = PutOrganizationConformancePackResponse'{organizationConformancePackArn
                                              = Core.Nothing,
                                            responseStatus}

-- | ARN of the organization conformance pack.
--
-- /Note:/ Consider using 'organizationConformancePackArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pocprrsOrganizationConformancePackArn :: Lens.Lens' PutOrganizationConformancePackResponse (Core.Maybe Types.StringWithCharLimit256)
pocprrsOrganizationConformancePackArn = Lens.field @"organizationConformancePackArn"
{-# INLINEABLE pocprrsOrganizationConformancePackArn #-}
{-# DEPRECATED organizationConformancePackArn "Use generic-lens or generic-optics with 'organizationConformancePackArn' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pocprrsResponseStatus :: Lens.Lens' PutOrganizationConformancePackResponse Core.Int
pocprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE pocprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
