{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.CreateProvisioningTemplateVersion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new version of a fleet provisioning template.
module Network.AWS.IoT.CreateProvisioningTemplateVersion
  ( -- * Creating a request
    CreateProvisioningTemplateVersion (..),
    mkCreateProvisioningTemplateVersion,

    -- ** Request lenses
    cptvTemplateName,
    cptvTemplateBody,
    cptvSetAsDefault,

    -- * Destructuring the response
    CreateProvisioningTemplateVersionResponse (..),
    mkCreateProvisioningTemplateVersionResponse,

    -- ** Response lenses
    cptvrrsIsDefaultVersion,
    cptvrrsTemplateArn,
    cptvrrsTemplateName,
    cptvrrsVersionId,
    cptvrrsResponseStatus,
  )
where

import qualified Network.AWS.IoT.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateProvisioningTemplateVersion' smart constructor.
data CreateProvisioningTemplateVersion = CreateProvisioningTemplateVersion'
  { -- | The name of the fleet provisioning template.
    templateName :: Types.TemplateName,
    -- | The JSON formatted contents of the fleet provisioning template.
    templateBody :: Types.TemplateBody,
    -- | Sets a fleet provision template version as the default version.
    setAsDefault :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateProvisioningTemplateVersion' value with any optional fields omitted.
mkCreateProvisioningTemplateVersion ::
  -- | 'templateName'
  Types.TemplateName ->
  -- | 'templateBody'
  Types.TemplateBody ->
  CreateProvisioningTemplateVersion
mkCreateProvisioningTemplateVersion templateName templateBody =
  CreateProvisioningTemplateVersion'
    { templateName,
      templateBody,
      setAsDefault = Core.Nothing
    }

-- | The name of the fleet provisioning template.
--
-- /Note:/ Consider using 'templateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cptvTemplateName :: Lens.Lens' CreateProvisioningTemplateVersion Types.TemplateName
cptvTemplateName = Lens.field @"templateName"
{-# DEPRECATED cptvTemplateName "Use generic-lens or generic-optics with 'templateName' instead." #-}

-- | The JSON formatted contents of the fleet provisioning template.
--
-- /Note:/ Consider using 'templateBody' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cptvTemplateBody :: Lens.Lens' CreateProvisioningTemplateVersion Types.TemplateBody
cptvTemplateBody = Lens.field @"templateBody"
{-# DEPRECATED cptvTemplateBody "Use generic-lens or generic-optics with 'templateBody' instead." #-}

-- | Sets a fleet provision template version as the default version.
--
-- /Note:/ Consider using 'setAsDefault' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cptvSetAsDefault :: Lens.Lens' CreateProvisioningTemplateVersion (Core.Maybe Core.Bool)
cptvSetAsDefault = Lens.field @"setAsDefault"
{-# DEPRECATED cptvSetAsDefault "Use generic-lens or generic-optics with 'setAsDefault' instead." #-}

instance Core.FromJSON CreateProvisioningTemplateVersion where
  toJSON CreateProvisioningTemplateVersion {..} =
    Core.object
      (Core.catMaybes [Core.Just ("templateBody" Core..= templateBody)])

instance Core.AWSRequest CreateProvisioningTemplateVersion where
  type
    Rs CreateProvisioningTemplateVersion =
      CreateProvisioningTemplateVersionResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath =
          Core.rawPath
            ( "/provisioning-templates/" Core.<> (Core.toText templateName)
                Core.<> ("/versions")
            ),
        Core._rqQuery =
          Core.toQueryValue "setAsDefault" Core.<$> setAsDefault,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateProvisioningTemplateVersionResponse'
            Core.<$> (x Core..:? "isDefaultVersion")
            Core.<*> (x Core..:? "templateArn")
            Core.<*> (x Core..:? "templateName")
            Core.<*> (x Core..:? "versionId")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCreateProvisioningTemplateVersionResponse' smart constructor.
data CreateProvisioningTemplateVersionResponse = CreateProvisioningTemplateVersionResponse'
  { -- | True if the fleet provisioning template version is the default version, otherwise false.
    isDefaultVersion :: Core.Maybe Core.Bool,
    -- | The ARN that identifies the provisioning template.
    templateArn :: Core.Maybe Types.TemplateArn,
    -- | The name of the fleet provisioning template.
    templateName :: Core.Maybe Types.TemplateName,
    -- | The version of the fleet provisioning template.
    versionId :: Core.Maybe Core.Int,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateProvisioningTemplateVersionResponse' value with any optional fields omitted.
mkCreateProvisioningTemplateVersionResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateProvisioningTemplateVersionResponse
mkCreateProvisioningTemplateVersionResponse responseStatus =
  CreateProvisioningTemplateVersionResponse'
    { isDefaultVersion =
        Core.Nothing,
      templateArn = Core.Nothing,
      templateName = Core.Nothing,
      versionId = Core.Nothing,
      responseStatus
    }

-- | True if the fleet provisioning template version is the default version, otherwise false.
--
-- /Note:/ Consider using 'isDefaultVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cptvrrsIsDefaultVersion :: Lens.Lens' CreateProvisioningTemplateVersionResponse (Core.Maybe Core.Bool)
cptvrrsIsDefaultVersion = Lens.field @"isDefaultVersion"
{-# DEPRECATED cptvrrsIsDefaultVersion "Use generic-lens or generic-optics with 'isDefaultVersion' instead." #-}

-- | The ARN that identifies the provisioning template.
--
-- /Note:/ Consider using 'templateArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cptvrrsTemplateArn :: Lens.Lens' CreateProvisioningTemplateVersionResponse (Core.Maybe Types.TemplateArn)
cptvrrsTemplateArn = Lens.field @"templateArn"
{-# DEPRECATED cptvrrsTemplateArn "Use generic-lens or generic-optics with 'templateArn' instead." #-}

-- | The name of the fleet provisioning template.
--
-- /Note:/ Consider using 'templateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cptvrrsTemplateName :: Lens.Lens' CreateProvisioningTemplateVersionResponse (Core.Maybe Types.TemplateName)
cptvrrsTemplateName = Lens.field @"templateName"
{-# DEPRECATED cptvrrsTemplateName "Use generic-lens or generic-optics with 'templateName' instead." #-}

-- | The version of the fleet provisioning template.
--
-- /Note:/ Consider using 'versionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cptvrrsVersionId :: Lens.Lens' CreateProvisioningTemplateVersionResponse (Core.Maybe Core.Int)
cptvrrsVersionId = Lens.field @"versionId"
{-# DEPRECATED cptvrrsVersionId "Use generic-lens or generic-optics with 'versionId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cptvrrsResponseStatus :: Lens.Lens' CreateProvisioningTemplateVersionResponse Core.Int
cptvrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED cptvrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
