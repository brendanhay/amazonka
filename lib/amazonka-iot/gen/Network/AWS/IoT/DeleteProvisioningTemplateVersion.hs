{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.DeleteProvisioningTemplateVersion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a fleet provisioning template version.
module Network.AWS.IoT.DeleteProvisioningTemplateVersion
  ( -- * Creating a request
    DeleteProvisioningTemplateVersion (..),
    mkDeleteProvisioningTemplateVersion,

    -- ** Request lenses
    dptvfTemplateName,
    dptvfVersionId,

    -- * Destructuring the response
    DeleteProvisioningTemplateVersionResponse (..),
    mkDeleteProvisioningTemplateVersionResponse,

    -- ** Response lenses
    dptvrfrsResponseStatus,
  )
where

import qualified Network.AWS.IoT.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteProvisioningTemplateVersion' smart constructor.
data DeleteProvisioningTemplateVersion = DeleteProvisioningTemplateVersion'
  { -- | The name of the fleet provisioning template version to delete.
    templateName :: Types.TemplateName,
    -- | The fleet provisioning template version ID to delete.
    versionId :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteProvisioningTemplateVersion' value with any optional fields omitted.
mkDeleteProvisioningTemplateVersion ::
  -- | 'templateName'
  Types.TemplateName ->
  -- | 'versionId'
  Core.Int ->
  DeleteProvisioningTemplateVersion
mkDeleteProvisioningTemplateVersion templateName versionId =
  DeleteProvisioningTemplateVersion' {templateName, versionId}

-- | The name of the fleet provisioning template version to delete.
--
-- /Note:/ Consider using 'templateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dptvfTemplateName :: Lens.Lens' DeleteProvisioningTemplateVersion Types.TemplateName
dptvfTemplateName = Lens.field @"templateName"
{-# DEPRECATED dptvfTemplateName "Use generic-lens or generic-optics with 'templateName' instead." #-}

-- | The fleet provisioning template version ID to delete.
--
-- /Note:/ Consider using 'versionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dptvfVersionId :: Lens.Lens' DeleteProvisioningTemplateVersion Core.Int
dptvfVersionId = Lens.field @"versionId"
{-# DEPRECATED dptvfVersionId "Use generic-lens or generic-optics with 'versionId' instead." #-}

instance Core.AWSRequest DeleteProvisioningTemplateVersion where
  type
    Rs DeleteProvisioningTemplateVersion =
      DeleteProvisioningTemplateVersionResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.DELETE,
        Core._rqPath =
          Core.rawPath
            ( "/provisioning-templates/" Core.<> (Core.toText templateName)
                Core.<> ("/versions/")
                Core.<> (Core.toText versionId)
            ),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = ""
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteProvisioningTemplateVersionResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDeleteProvisioningTemplateVersionResponse' smart constructor.
newtype DeleteProvisioningTemplateVersionResponse = DeleteProvisioningTemplateVersionResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteProvisioningTemplateVersionResponse' value with any optional fields omitted.
mkDeleteProvisioningTemplateVersionResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeleteProvisioningTemplateVersionResponse
mkDeleteProvisioningTemplateVersionResponse responseStatus =
  DeleteProvisioningTemplateVersionResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dptvrfrsResponseStatus :: Lens.Lens' DeleteProvisioningTemplateVersionResponse Core.Int
dptvrfrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dptvrfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
