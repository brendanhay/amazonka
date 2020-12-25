{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.CreateProvisioningTemplate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a fleet provisioning template.
module Network.AWS.IoT.CreateProvisioningTemplate
  ( -- * Creating a request
    CreateProvisioningTemplate (..),
    mkCreateProvisioningTemplate,

    -- ** Request lenses
    cptTemplateName,
    cptTemplateBody,
    cptProvisioningRoleArn,
    cptDescription,
    cptEnabled,
    cptPreProvisioningHook,
    cptTags,

    -- * Destructuring the response
    CreateProvisioningTemplateResponse (..),
    mkCreateProvisioningTemplateResponse,

    -- ** Response lenses
    cptrrsDefaultVersionId,
    cptrrsTemplateArn,
    cptrrsTemplateName,
    cptrrsResponseStatus,
  )
where

import qualified Network.AWS.IoT.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateProvisioningTemplate' smart constructor.
data CreateProvisioningTemplate = CreateProvisioningTemplate'
  { -- | The name of the fleet provisioning template.
    templateName :: Types.TemplateName,
    -- | The JSON formatted contents of the fleet provisioning template.
    templateBody :: Types.TemplateBody,
    -- | The role ARN for the role associated with the fleet provisioning template. This IoT role grants permission to provision a device.
    provisioningRoleArn :: Types.ProvisioningRoleArn,
    -- | The description of the fleet provisioning template.
    description :: Core.Maybe Types.Description,
    -- | True to enable the fleet provisioning template, otherwise false.
    enabled :: Core.Maybe Core.Bool,
    -- | Creates a pre-provisioning hook template.
    preProvisioningHook :: Core.Maybe Types.ProvisioningHook,
    -- | Metadata which can be used to manage the fleet provisioning template.
    tags :: Core.Maybe [Types.Tag]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateProvisioningTemplate' value with any optional fields omitted.
mkCreateProvisioningTemplate ::
  -- | 'templateName'
  Types.TemplateName ->
  -- | 'templateBody'
  Types.TemplateBody ->
  -- | 'provisioningRoleArn'
  Types.ProvisioningRoleArn ->
  CreateProvisioningTemplate
mkCreateProvisioningTemplate
  templateName
  templateBody
  provisioningRoleArn =
    CreateProvisioningTemplate'
      { templateName,
        templateBody,
        provisioningRoleArn,
        description = Core.Nothing,
        enabled = Core.Nothing,
        preProvisioningHook = Core.Nothing,
        tags = Core.Nothing
      }

-- | The name of the fleet provisioning template.
--
-- /Note:/ Consider using 'templateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cptTemplateName :: Lens.Lens' CreateProvisioningTemplate Types.TemplateName
cptTemplateName = Lens.field @"templateName"
{-# DEPRECATED cptTemplateName "Use generic-lens or generic-optics with 'templateName' instead." #-}

-- | The JSON formatted contents of the fleet provisioning template.
--
-- /Note:/ Consider using 'templateBody' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cptTemplateBody :: Lens.Lens' CreateProvisioningTemplate Types.TemplateBody
cptTemplateBody = Lens.field @"templateBody"
{-# DEPRECATED cptTemplateBody "Use generic-lens or generic-optics with 'templateBody' instead." #-}

-- | The role ARN for the role associated with the fleet provisioning template. This IoT role grants permission to provision a device.
--
-- /Note:/ Consider using 'provisioningRoleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cptProvisioningRoleArn :: Lens.Lens' CreateProvisioningTemplate Types.ProvisioningRoleArn
cptProvisioningRoleArn = Lens.field @"provisioningRoleArn"
{-# DEPRECATED cptProvisioningRoleArn "Use generic-lens or generic-optics with 'provisioningRoleArn' instead." #-}

-- | The description of the fleet provisioning template.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cptDescription :: Lens.Lens' CreateProvisioningTemplate (Core.Maybe Types.Description)
cptDescription = Lens.field @"description"
{-# DEPRECATED cptDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | True to enable the fleet provisioning template, otherwise false.
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cptEnabled :: Lens.Lens' CreateProvisioningTemplate (Core.Maybe Core.Bool)
cptEnabled = Lens.field @"enabled"
{-# DEPRECATED cptEnabled "Use generic-lens or generic-optics with 'enabled' instead." #-}

-- | Creates a pre-provisioning hook template.
--
-- /Note:/ Consider using 'preProvisioningHook' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cptPreProvisioningHook :: Lens.Lens' CreateProvisioningTemplate (Core.Maybe Types.ProvisioningHook)
cptPreProvisioningHook = Lens.field @"preProvisioningHook"
{-# DEPRECATED cptPreProvisioningHook "Use generic-lens or generic-optics with 'preProvisioningHook' instead." #-}

-- | Metadata which can be used to manage the fleet provisioning template.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cptTags :: Lens.Lens' CreateProvisioningTemplate (Core.Maybe [Types.Tag])
cptTags = Lens.field @"tags"
{-# DEPRECATED cptTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Core.FromJSON CreateProvisioningTemplate where
  toJSON CreateProvisioningTemplate {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("templateName" Core..= templateName),
            Core.Just ("templateBody" Core..= templateBody),
            Core.Just ("provisioningRoleArn" Core..= provisioningRoleArn),
            ("description" Core..=) Core.<$> description,
            ("enabled" Core..=) Core.<$> enabled,
            ("preProvisioningHook" Core..=) Core.<$> preProvisioningHook,
            ("tags" Core..=) Core.<$> tags
          ]
      )

instance Core.AWSRequest CreateProvisioningTemplate where
  type
    Rs CreateProvisioningTemplate =
      CreateProvisioningTemplateResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/provisioning-templates",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateProvisioningTemplateResponse'
            Core.<$> (x Core..:? "defaultVersionId")
            Core.<*> (x Core..:? "templateArn")
            Core.<*> (x Core..:? "templateName")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCreateProvisioningTemplateResponse' smart constructor.
data CreateProvisioningTemplateResponse = CreateProvisioningTemplateResponse'
  { -- | The default version of the fleet provisioning template.
    defaultVersionId :: Core.Maybe Core.Int,
    -- | The ARN that identifies the provisioning template.
    templateArn :: Core.Maybe Types.TemplateArn,
    -- | The name of the fleet provisioning template.
    templateName :: Core.Maybe Types.TemplateName,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateProvisioningTemplateResponse' value with any optional fields omitted.
mkCreateProvisioningTemplateResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateProvisioningTemplateResponse
mkCreateProvisioningTemplateResponse responseStatus =
  CreateProvisioningTemplateResponse'
    { defaultVersionId =
        Core.Nothing,
      templateArn = Core.Nothing,
      templateName = Core.Nothing,
      responseStatus
    }

-- | The default version of the fleet provisioning template.
--
-- /Note:/ Consider using 'defaultVersionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cptrrsDefaultVersionId :: Lens.Lens' CreateProvisioningTemplateResponse (Core.Maybe Core.Int)
cptrrsDefaultVersionId = Lens.field @"defaultVersionId"
{-# DEPRECATED cptrrsDefaultVersionId "Use generic-lens or generic-optics with 'defaultVersionId' instead." #-}

-- | The ARN that identifies the provisioning template.
--
-- /Note:/ Consider using 'templateArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cptrrsTemplateArn :: Lens.Lens' CreateProvisioningTemplateResponse (Core.Maybe Types.TemplateArn)
cptrrsTemplateArn = Lens.field @"templateArn"
{-# DEPRECATED cptrrsTemplateArn "Use generic-lens or generic-optics with 'templateArn' instead." #-}

-- | The name of the fleet provisioning template.
--
-- /Note:/ Consider using 'templateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cptrrsTemplateName :: Lens.Lens' CreateProvisioningTemplateResponse (Core.Maybe Types.TemplateName)
cptrrsTemplateName = Lens.field @"templateName"
{-# DEPRECATED cptrrsTemplateName "Use generic-lens or generic-optics with 'templateName' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cptrrsResponseStatus :: Lens.Lens' CreateProvisioningTemplateResponse Core.Int
cptrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED cptrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
