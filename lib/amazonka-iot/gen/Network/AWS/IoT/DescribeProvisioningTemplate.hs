{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.DescribeProvisioningTemplate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a fleet provisioning template.
module Network.AWS.IoT.DescribeProvisioningTemplate
  ( -- * Creating a request
    DescribeProvisioningTemplate (..),
    mkDescribeProvisioningTemplate,

    -- ** Request lenses
    dptTemplateName,

    -- * Destructuring the response
    DescribeProvisioningTemplateResponse (..),
    mkDescribeProvisioningTemplateResponse,

    -- ** Response lenses
    dptrrsCreationDate,
    dptrrsDefaultVersionId,
    dptrrsDescription,
    dptrrsEnabled,
    dptrrsLastModifiedDate,
    dptrrsPreProvisioningHook,
    dptrrsProvisioningRoleArn,
    dptrrsTemplateArn,
    dptrrsTemplateBody,
    dptrrsTemplateName,
    dptrrsResponseStatus,
  )
where

import qualified Network.AWS.IoT.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeProvisioningTemplate' smart constructor.
newtype DescribeProvisioningTemplate = DescribeProvisioningTemplate'
  { -- | The name of the fleet provisioning template.
    templateName :: Types.TemplateName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeProvisioningTemplate' value with any optional fields omitted.
mkDescribeProvisioningTemplate ::
  -- | 'templateName'
  Types.TemplateName ->
  DescribeProvisioningTemplate
mkDescribeProvisioningTemplate templateName =
  DescribeProvisioningTemplate' {templateName}

-- | The name of the fleet provisioning template.
--
-- /Note:/ Consider using 'templateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dptTemplateName :: Lens.Lens' DescribeProvisioningTemplate Types.TemplateName
dptTemplateName = Lens.field @"templateName"
{-# DEPRECATED dptTemplateName "Use generic-lens or generic-optics with 'templateName' instead." #-}

instance Core.AWSRequest DescribeProvisioningTemplate where
  type
    Rs DescribeProvisioningTemplate =
      DescribeProvisioningTemplateResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath =
          Core.rawPath
            ("/provisioning-templates/" Core.<> (Core.toText templateName)),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = ""
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeProvisioningTemplateResponse'
            Core.<$> (x Core..:? "creationDate")
            Core.<*> (x Core..:? "defaultVersionId")
            Core.<*> (x Core..:? "description")
            Core.<*> (x Core..:? "enabled")
            Core.<*> (x Core..:? "lastModifiedDate")
            Core.<*> (x Core..:? "preProvisioningHook")
            Core.<*> (x Core..:? "provisioningRoleArn")
            Core.<*> (x Core..:? "templateArn")
            Core.<*> (x Core..:? "templateBody")
            Core.<*> (x Core..:? "templateName")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDescribeProvisioningTemplateResponse' smart constructor.
data DescribeProvisioningTemplateResponse = DescribeProvisioningTemplateResponse'
  { -- | The date when the fleet provisioning template was created.
    creationDate :: Core.Maybe Core.NominalDiffTime,
    -- | The default fleet template version ID.
    defaultVersionId :: Core.Maybe Core.Int,
    -- | The description of the fleet provisioning template.
    description :: Core.Maybe Types.Description,
    -- | True if the fleet provisioning template is enabled, otherwise false.
    enabled :: Core.Maybe Core.Bool,
    -- | The date when the fleet provisioning template was last modified.
    lastModifiedDate :: Core.Maybe Core.NominalDiffTime,
    -- | Gets information about a pre-provisioned hook.
    preProvisioningHook :: Core.Maybe Types.ProvisioningHook,
    -- | The ARN of the role associated with the provisioning template. This IoT role grants permission to provision a device.
    provisioningRoleArn :: Core.Maybe Types.ProvisioningRoleArn,
    -- | The ARN of the fleet provisioning template.
    templateArn :: Core.Maybe Types.TemplateArn,
    -- | The JSON formatted contents of the fleet provisioning template.
    templateBody :: Core.Maybe Types.TemplateBody,
    -- | The name of the fleet provisioning template.
    templateName :: Core.Maybe Types.TemplateName,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribeProvisioningTemplateResponse' value with any optional fields omitted.
mkDescribeProvisioningTemplateResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeProvisioningTemplateResponse
mkDescribeProvisioningTemplateResponse responseStatus =
  DescribeProvisioningTemplateResponse'
    { creationDate =
        Core.Nothing,
      defaultVersionId = Core.Nothing,
      description = Core.Nothing,
      enabled = Core.Nothing,
      lastModifiedDate = Core.Nothing,
      preProvisioningHook = Core.Nothing,
      provisioningRoleArn = Core.Nothing,
      templateArn = Core.Nothing,
      templateBody = Core.Nothing,
      templateName = Core.Nothing,
      responseStatus
    }

-- | The date when the fleet provisioning template was created.
--
-- /Note:/ Consider using 'creationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dptrrsCreationDate :: Lens.Lens' DescribeProvisioningTemplateResponse (Core.Maybe Core.NominalDiffTime)
dptrrsCreationDate = Lens.field @"creationDate"
{-# DEPRECATED dptrrsCreationDate "Use generic-lens or generic-optics with 'creationDate' instead." #-}

-- | The default fleet template version ID.
--
-- /Note:/ Consider using 'defaultVersionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dptrrsDefaultVersionId :: Lens.Lens' DescribeProvisioningTemplateResponse (Core.Maybe Core.Int)
dptrrsDefaultVersionId = Lens.field @"defaultVersionId"
{-# DEPRECATED dptrrsDefaultVersionId "Use generic-lens or generic-optics with 'defaultVersionId' instead." #-}

-- | The description of the fleet provisioning template.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dptrrsDescription :: Lens.Lens' DescribeProvisioningTemplateResponse (Core.Maybe Types.Description)
dptrrsDescription = Lens.field @"description"
{-# DEPRECATED dptrrsDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | True if the fleet provisioning template is enabled, otherwise false.
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dptrrsEnabled :: Lens.Lens' DescribeProvisioningTemplateResponse (Core.Maybe Core.Bool)
dptrrsEnabled = Lens.field @"enabled"
{-# DEPRECATED dptrrsEnabled "Use generic-lens or generic-optics with 'enabled' instead." #-}

-- | The date when the fleet provisioning template was last modified.
--
-- /Note:/ Consider using 'lastModifiedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dptrrsLastModifiedDate :: Lens.Lens' DescribeProvisioningTemplateResponse (Core.Maybe Core.NominalDiffTime)
dptrrsLastModifiedDate = Lens.field @"lastModifiedDate"
{-# DEPRECATED dptrrsLastModifiedDate "Use generic-lens or generic-optics with 'lastModifiedDate' instead." #-}

-- | Gets information about a pre-provisioned hook.
--
-- /Note:/ Consider using 'preProvisioningHook' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dptrrsPreProvisioningHook :: Lens.Lens' DescribeProvisioningTemplateResponse (Core.Maybe Types.ProvisioningHook)
dptrrsPreProvisioningHook = Lens.field @"preProvisioningHook"
{-# DEPRECATED dptrrsPreProvisioningHook "Use generic-lens or generic-optics with 'preProvisioningHook' instead." #-}

-- | The ARN of the role associated with the provisioning template. This IoT role grants permission to provision a device.
--
-- /Note:/ Consider using 'provisioningRoleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dptrrsProvisioningRoleArn :: Lens.Lens' DescribeProvisioningTemplateResponse (Core.Maybe Types.ProvisioningRoleArn)
dptrrsProvisioningRoleArn = Lens.field @"provisioningRoleArn"
{-# DEPRECATED dptrrsProvisioningRoleArn "Use generic-lens or generic-optics with 'provisioningRoleArn' instead." #-}

-- | The ARN of the fleet provisioning template.
--
-- /Note:/ Consider using 'templateArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dptrrsTemplateArn :: Lens.Lens' DescribeProvisioningTemplateResponse (Core.Maybe Types.TemplateArn)
dptrrsTemplateArn = Lens.field @"templateArn"
{-# DEPRECATED dptrrsTemplateArn "Use generic-lens or generic-optics with 'templateArn' instead." #-}

-- | The JSON formatted contents of the fleet provisioning template.
--
-- /Note:/ Consider using 'templateBody' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dptrrsTemplateBody :: Lens.Lens' DescribeProvisioningTemplateResponse (Core.Maybe Types.TemplateBody)
dptrrsTemplateBody = Lens.field @"templateBody"
{-# DEPRECATED dptrrsTemplateBody "Use generic-lens or generic-optics with 'templateBody' instead." #-}

-- | The name of the fleet provisioning template.
--
-- /Note:/ Consider using 'templateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dptrrsTemplateName :: Lens.Lens' DescribeProvisioningTemplateResponse (Core.Maybe Types.TemplateName)
dptrrsTemplateName = Lens.field @"templateName"
{-# DEPRECATED dptrrsTemplateName "Use generic-lens or generic-optics with 'templateName' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dptrrsResponseStatus :: Lens.Lens' DescribeProvisioningTemplateResponse Core.Int
dptrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dptrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
