{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.UpdateProvisioningTemplate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a fleet provisioning template.
module Network.AWS.IoT.UpdateProvisioningTemplate
  ( -- * Creating a request
    UpdateProvisioningTemplate (..),
    mkUpdateProvisioningTemplate,

    -- ** Request lenses
    uptTemplateName,
    uptDefaultVersionId,
    uptDescription,
    uptEnabled,
    uptPreProvisioningHook,
    uptProvisioningRoleArn,
    uptRemovePreProvisioningHook,

    -- * Destructuring the response
    UpdateProvisioningTemplateResponse (..),
    mkUpdateProvisioningTemplateResponse,

    -- ** Response lenses
    uptrrsResponseStatus,
  )
where

import qualified Network.AWS.IoT.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateProvisioningTemplate' smart constructor.
data UpdateProvisioningTemplate = UpdateProvisioningTemplate'
  { -- | The name of the fleet provisioning template.
    templateName :: Types.TemplateName,
    -- | The ID of the default provisioning template version.
    defaultVersionId :: Core.Maybe Core.Int,
    -- | The description of the fleet provisioning template.
    description :: Core.Maybe Types.TemplateDescription,
    -- | True to enable the fleet provisioning template, otherwise false.
    enabled :: Core.Maybe Core.Bool,
    -- | Updates the pre-provisioning hook template.
    preProvisioningHook :: Core.Maybe Types.ProvisioningHook,
    -- | The ARN of the role associated with the provisioning template. This IoT role grants permission to provision a device.
    provisioningRoleArn :: Core.Maybe Types.ProvisioningRoleArn,
    -- | Removes pre-provisioning hook template.
    removePreProvisioningHook :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateProvisioningTemplate' value with any optional fields omitted.
mkUpdateProvisioningTemplate ::
  -- | 'templateName'
  Types.TemplateName ->
  UpdateProvisioningTemplate
mkUpdateProvisioningTemplate templateName =
  UpdateProvisioningTemplate'
    { templateName,
      defaultVersionId = Core.Nothing,
      description = Core.Nothing,
      enabled = Core.Nothing,
      preProvisioningHook = Core.Nothing,
      provisioningRoleArn = Core.Nothing,
      removePreProvisioningHook = Core.Nothing
    }

-- | The name of the fleet provisioning template.
--
-- /Note:/ Consider using 'templateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uptTemplateName :: Lens.Lens' UpdateProvisioningTemplate Types.TemplateName
uptTemplateName = Lens.field @"templateName"
{-# DEPRECATED uptTemplateName "Use generic-lens or generic-optics with 'templateName' instead." #-}

-- | The ID of the default provisioning template version.
--
-- /Note:/ Consider using 'defaultVersionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uptDefaultVersionId :: Lens.Lens' UpdateProvisioningTemplate (Core.Maybe Core.Int)
uptDefaultVersionId = Lens.field @"defaultVersionId"
{-# DEPRECATED uptDefaultVersionId "Use generic-lens or generic-optics with 'defaultVersionId' instead." #-}

-- | The description of the fleet provisioning template.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uptDescription :: Lens.Lens' UpdateProvisioningTemplate (Core.Maybe Types.TemplateDescription)
uptDescription = Lens.field @"description"
{-# DEPRECATED uptDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | True to enable the fleet provisioning template, otherwise false.
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uptEnabled :: Lens.Lens' UpdateProvisioningTemplate (Core.Maybe Core.Bool)
uptEnabled = Lens.field @"enabled"
{-# DEPRECATED uptEnabled "Use generic-lens or generic-optics with 'enabled' instead." #-}

-- | Updates the pre-provisioning hook template.
--
-- /Note:/ Consider using 'preProvisioningHook' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uptPreProvisioningHook :: Lens.Lens' UpdateProvisioningTemplate (Core.Maybe Types.ProvisioningHook)
uptPreProvisioningHook = Lens.field @"preProvisioningHook"
{-# DEPRECATED uptPreProvisioningHook "Use generic-lens or generic-optics with 'preProvisioningHook' instead." #-}

-- | The ARN of the role associated with the provisioning template. This IoT role grants permission to provision a device.
--
-- /Note:/ Consider using 'provisioningRoleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uptProvisioningRoleArn :: Lens.Lens' UpdateProvisioningTemplate (Core.Maybe Types.ProvisioningRoleArn)
uptProvisioningRoleArn = Lens.field @"provisioningRoleArn"
{-# DEPRECATED uptProvisioningRoleArn "Use generic-lens or generic-optics with 'provisioningRoleArn' instead." #-}

-- | Removes pre-provisioning hook template.
--
-- /Note:/ Consider using 'removePreProvisioningHook' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uptRemovePreProvisioningHook :: Lens.Lens' UpdateProvisioningTemplate (Core.Maybe Core.Bool)
uptRemovePreProvisioningHook = Lens.field @"removePreProvisioningHook"
{-# DEPRECATED uptRemovePreProvisioningHook "Use generic-lens or generic-optics with 'removePreProvisioningHook' instead." #-}

instance Core.FromJSON UpdateProvisioningTemplate where
  toJSON UpdateProvisioningTemplate {..} =
    Core.object
      ( Core.catMaybes
          [ ("defaultVersionId" Core..=) Core.<$> defaultVersionId,
            ("description" Core..=) Core.<$> description,
            ("enabled" Core..=) Core.<$> enabled,
            ("preProvisioningHook" Core..=) Core.<$> preProvisioningHook,
            ("provisioningRoleArn" Core..=) Core.<$> provisioningRoleArn,
            ("removePreProvisioningHook" Core..=)
              Core.<$> removePreProvisioningHook
          ]
      )

instance Core.AWSRequest UpdateProvisioningTemplate where
  type
    Rs UpdateProvisioningTemplate =
      UpdateProvisioningTemplateResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.PATCH,
        Core._rqPath =
          Core.rawPath
            ("/provisioning-templates/" Core.<> (Core.toText templateName)),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateProvisioningTemplateResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkUpdateProvisioningTemplateResponse' smart constructor.
newtype UpdateProvisioningTemplateResponse = UpdateProvisioningTemplateResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateProvisioningTemplateResponse' value with any optional fields omitted.
mkUpdateProvisioningTemplateResponse ::
  -- | 'responseStatus'
  Core.Int ->
  UpdateProvisioningTemplateResponse
mkUpdateProvisioningTemplateResponse responseStatus =
  UpdateProvisioningTemplateResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uptrrsResponseStatus :: Lens.Lens' UpdateProvisioningTemplateResponse Core.Int
uptrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED uptrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
