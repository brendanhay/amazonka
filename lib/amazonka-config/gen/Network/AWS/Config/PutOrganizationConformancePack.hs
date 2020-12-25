{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    PutOrganizationConformancePack (..),
    mkPutOrganizationConformancePack,

    -- ** Request lenses
    pocpOrganizationConformancePackName,
    pocpConformancePackInputParameters,
    pocpDeliveryS3Bucket,
    pocpDeliveryS3KeyPrefix,
    pocpExcludedAccounts,
    pocpTemplateBody,
    pocpTemplateS3Uri,

    -- * Destructuring the response
    PutOrganizationConformancePackResponse (..),
    mkPutOrganizationConformancePackResponse,

    -- ** Response lenses
    pocprrsOrganizationConformancePackArn,
    pocprrsResponseStatus,
  )
where

import qualified Network.AWS.Config.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkPutOrganizationConformancePack' smart constructor.
data PutOrganizationConformancePack = PutOrganizationConformancePack'
  { -- | Name of the organization conformance pack you want to create.
    organizationConformancePackName :: Types.OrganizationConformancePackName,
    -- | A list of @ConformancePackInputParameter@ objects.
    conformancePackInputParameters :: Core.Maybe [Types.ConformancePackInputParameter],
    -- | Location of an Amazon S3 bucket where AWS Config can deliver evaluation results. AWS Config stores intermediate files while processing conformance pack template.
    --
    -- The delivery bucket name should start with awsconfigconforms. For example: "Resource": "arn:aws:s3:::your_bucket_name/*". For more information, see <https://docs.aws.amazon.com/config/latest/developerguide/conformance-pack-organization-apis.html Permissions for cross account bucket access> .
    deliveryS3Bucket :: Core.Maybe Types.DeliveryS3Bucket,
    -- | The prefix for the Amazon S3 bucket.
    deliveryS3KeyPrefix :: Core.Maybe Types.DeliveryS3KeyPrefix,
    -- | A list of AWS accounts to be excluded from an organization conformance pack while deploying a conformance pack.
    excludedAccounts :: Core.Maybe [Types.AccountId],
    -- | A string containing full conformance pack template body. Structure containing the template body with a minimum length of 1 byte and a maximum length of 51,200 bytes.
    templateBody :: Core.Maybe Types.TemplateBody,
    -- | Location of file containing the template body. The uri must point to the conformance pack template (max size: 300 KB).
    templateS3Uri :: Core.Maybe Types.TemplateS3Uri
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutOrganizationConformancePack' value with any optional fields omitted.
mkPutOrganizationConformancePack ::
  -- | 'organizationConformancePackName'
  Types.OrganizationConformancePackName ->
  PutOrganizationConformancePack
mkPutOrganizationConformancePack organizationConformancePackName =
  PutOrganizationConformancePack'
    { organizationConformancePackName,
      conformancePackInputParameters = Core.Nothing,
      deliveryS3Bucket = Core.Nothing,
      deliveryS3KeyPrefix = Core.Nothing,
      excludedAccounts = Core.Nothing,
      templateBody = Core.Nothing,
      templateS3Uri = Core.Nothing
    }

-- | Name of the organization conformance pack you want to create.
--
-- /Note:/ Consider using 'organizationConformancePackName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pocpOrganizationConformancePackName :: Lens.Lens' PutOrganizationConformancePack Types.OrganizationConformancePackName
pocpOrganizationConformancePackName = Lens.field @"organizationConformancePackName"
{-# DEPRECATED pocpOrganizationConformancePackName "Use generic-lens or generic-optics with 'organizationConformancePackName' instead." #-}

-- | A list of @ConformancePackInputParameter@ objects.
--
-- /Note:/ Consider using 'conformancePackInputParameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pocpConformancePackInputParameters :: Lens.Lens' PutOrganizationConformancePack (Core.Maybe [Types.ConformancePackInputParameter])
pocpConformancePackInputParameters = Lens.field @"conformancePackInputParameters"
{-# DEPRECATED pocpConformancePackInputParameters "Use generic-lens or generic-optics with 'conformancePackInputParameters' instead." #-}

-- | Location of an Amazon S3 bucket where AWS Config can deliver evaluation results. AWS Config stores intermediate files while processing conformance pack template.
--
-- The delivery bucket name should start with awsconfigconforms. For example: "Resource": "arn:aws:s3:::your_bucket_name/*". For more information, see <https://docs.aws.amazon.com/config/latest/developerguide/conformance-pack-organization-apis.html Permissions for cross account bucket access> .
--
-- /Note:/ Consider using 'deliveryS3Bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pocpDeliveryS3Bucket :: Lens.Lens' PutOrganizationConformancePack (Core.Maybe Types.DeliveryS3Bucket)
pocpDeliveryS3Bucket = Lens.field @"deliveryS3Bucket"
{-# DEPRECATED pocpDeliveryS3Bucket "Use generic-lens or generic-optics with 'deliveryS3Bucket' instead." #-}

-- | The prefix for the Amazon S3 bucket.
--
-- /Note:/ Consider using 'deliveryS3KeyPrefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pocpDeliveryS3KeyPrefix :: Lens.Lens' PutOrganizationConformancePack (Core.Maybe Types.DeliveryS3KeyPrefix)
pocpDeliveryS3KeyPrefix = Lens.field @"deliveryS3KeyPrefix"
{-# DEPRECATED pocpDeliveryS3KeyPrefix "Use generic-lens or generic-optics with 'deliveryS3KeyPrefix' instead." #-}

-- | A list of AWS accounts to be excluded from an organization conformance pack while deploying a conformance pack.
--
-- /Note:/ Consider using 'excludedAccounts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pocpExcludedAccounts :: Lens.Lens' PutOrganizationConformancePack (Core.Maybe [Types.AccountId])
pocpExcludedAccounts = Lens.field @"excludedAccounts"
{-# DEPRECATED pocpExcludedAccounts "Use generic-lens or generic-optics with 'excludedAccounts' instead." #-}

-- | A string containing full conformance pack template body. Structure containing the template body with a minimum length of 1 byte and a maximum length of 51,200 bytes.
--
-- /Note:/ Consider using 'templateBody' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pocpTemplateBody :: Lens.Lens' PutOrganizationConformancePack (Core.Maybe Types.TemplateBody)
pocpTemplateBody = Lens.field @"templateBody"
{-# DEPRECATED pocpTemplateBody "Use generic-lens or generic-optics with 'templateBody' instead." #-}

-- | Location of file containing the template body. The uri must point to the conformance pack template (max size: 300 KB).
--
-- /Note:/ Consider using 'templateS3Uri' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pocpTemplateS3Uri :: Lens.Lens' PutOrganizationConformancePack (Core.Maybe Types.TemplateS3Uri)
pocpTemplateS3Uri = Lens.field @"templateS3Uri"
{-# DEPRECATED pocpTemplateS3Uri "Use generic-lens or generic-optics with 'templateS3Uri' instead." #-}

instance Core.FromJSON PutOrganizationConformancePack where
  toJSON PutOrganizationConformancePack {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ( "OrganizationConformancePackName"
                  Core..= organizationConformancePackName
              ),
            ("ConformancePackInputParameters" Core..=)
              Core.<$> conformancePackInputParameters,
            ("DeliveryS3Bucket" Core..=) Core.<$> deliveryS3Bucket,
            ("DeliveryS3KeyPrefix" Core..=) Core.<$> deliveryS3KeyPrefix,
            ("ExcludedAccounts" Core..=) Core.<$> excludedAccounts,
            ("TemplateBody" Core..=) Core.<$> templateBody,
            ("TemplateS3Uri" Core..=) Core.<$> templateS3Uri
          ]
      )

instance Core.AWSRequest PutOrganizationConformancePack where
  type
    Rs PutOrganizationConformancePack =
      PutOrganizationConformancePackResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "StarlingDoveService.PutOrganizationConformancePack"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          PutOrganizationConformancePackResponse'
            Core.<$> (x Core..:? "OrganizationConformancePackArn")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkPutOrganizationConformancePackResponse' smart constructor.
data PutOrganizationConformancePackResponse = PutOrganizationConformancePackResponse'
  { -- | ARN of the organization conformance pack.
    organizationConformancePackArn :: Core.Maybe Types.StringWithCharLimit256,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutOrganizationConformancePackResponse' value with any optional fields omitted.
mkPutOrganizationConformancePackResponse ::
  -- | 'responseStatus'
  Core.Int ->
  PutOrganizationConformancePackResponse
mkPutOrganizationConformancePackResponse responseStatus =
  PutOrganizationConformancePackResponse'
    { organizationConformancePackArn =
        Core.Nothing,
      responseStatus
    }

-- | ARN of the organization conformance pack.
--
-- /Note:/ Consider using 'organizationConformancePackArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pocprrsOrganizationConformancePackArn :: Lens.Lens' PutOrganizationConformancePackResponse (Core.Maybe Types.StringWithCharLimit256)
pocprrsOrganizationConformancePackArn = Lens.field @"organizationConformancePackArn"
{-# DEPRECATED pocprrsOrganizationConformancePackArn "Use generic-lens or generic-optics with 'organizationConformancePackArn' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pocprrsResponseStatus :: Lens.Lens' PutOrganizationConformancePackResponse Core.Int
pocprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED pocprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
