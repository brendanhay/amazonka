{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.DescribeProvisioningTemplateVersion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a fleet provisioning template version.
module Network.AWS.IoT.DescribeProvisioningTemplateVersion
  ( -- * Creating a request
    DescribeProvisioningTemplateVersion (..),
    mkDescribeProvisioningTemplateVersion,

    -- ** Request lenses
    dptvTemplateName,
    dptvVersionId,

    -- * Destructuring the response
    DescribeProvisioningTemplateVersionResponse (..),
    mkDescribeProvisioningTemplateVersionResponse,

    -- ** Response lenses
    dptvrrsCreationDate,
    dptvrrsIsDefaultVersion,
    dptvrrsTemplateBody,
    dptvrrsVersionId,
    dptvrrsResponseStatus,
  )
where

import qualified Network.AWS.IoT.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeProvisioningTemplateVersion' smart constructor.
data DescribeProvisioningTemplateVersion = DescribeProvisioningTemplateVersion'
  { -- | The template name.
    templateName :: Types.TemplateName,
    -- | The fleet provisioning template version ID.
    versionId :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeProvisioningTemplateVersion' value with any optional fields omitted.
mkDescribeProvisioningTemplateVersion ::
  -- | 'templateName'
  Types.TemplateName ->
  -- | 'versionId'
  Core.Int ->
  DescribeProvisioningTemplateVersion
mkDescribeProvisioningTemplateVersion templateName versionId =
  DescribeProvisioningTemplateVersion' {templateName, versionId}

-- | The template name.
--
-- /Note:/ Consider using 'templateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dptvTemplateName :: Lens.Lens' DescribeProvisioningTemplateVersion Types.TemplateName
dptvTemplateName = Lens.field @"templateName"
{-# DEPRECATED dptvTemplateName "Use generic-lens or generic-optics with 'templateName' instead." #-}

-- | The fleet provisioning template version ID.
--
-- /Note:/ Consider using 'versionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dptvVersionId :: Lens.Lens' DescribeProvisioningTemplateVersion Core.Int
dptvVersionId = Lens.field @"versionId"
{-# DEPRECATED dptvVersionId "Use generic-lens or generic-optics with 'versionId' instead." #-}

instance Core.AWSRequest DescribeProvisioningTemplateVersion where
  type
    Rs DescribeProvisioningTemplateVersion =
      DescribeProvisioningTemplateVersionResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
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
    Response.receiveJSON
      ( \s h x ->
          DescribeProvisioningTemplateVersionResponse'
            Core.<$> (x Core..:? "creationDate")
            Core.<*> (x Core..:? "isDefaultVersion")
            Core.<*> (x Core..:? "templateBody")
            Core.<*> (x Core..:? "versionId")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDescribeProvisioningTemplateVersionResponse' smart constructor.
data DescribeProvisioningTemplateVersionResponse = DescribeProvisioningTemplateVersionResponse'
  { -- | The date when the fleet provisioning template version was created.
    creationDate :: Core.Maybe Core.NominalDiffTime,
    -- | True if the fleet provisioning template version is the default version.
    isDefaultVersion :: Core.Maybe Core.Bool,
    -- | The JSON formatted contents of the fleet provisioning template version.
    templateBody :: Core.Maybe Types.TemplateBody,
    -- | The fleet provisioning template version ID.
    versionId :: Core.Maybe Core.Int,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribeProvisioningTemplateVersionResponse' value with any optional fields omitted.
mkDescribeProvisioningTemplateVersionResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeProvisioningTemplateVersionResponse
mkDescribeProvisioningTemplateVersionResponse responseStatus =
  DescribeProvisioningTemplateVersionResponse'
    { creationDate =
        Core.Nothing,
      isDefaultVersion = Core.Nothing,
      templateBody = Core.Nothing,
      versionId = Core.Nothing,
      responseStatus
    }

-- | The date when the fleet provisioning template version was created.
--
-- /Note:/ Consider using 'creationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dptvrrsCreationDate :: Lens.Lens' DescribeProvisioningTemplateVersionResponse (Core.Maybe Core.NominalDiffTime)
dptvrrsCreationDate = Lens.field @"creationDate"
{-# DEPRECATED dptvrrsCreationDate "Use generic-lens or generic-optics with 'creationDate' instead." #-}

-- | True if the fleet provisioning template version is the default version.
--
-- /Note:/ Consider using 'isDefaultVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dptvrrsIsDefaultVersion :: Lens.Lens' DescribeProvisioningTemplateVersionResponse (Core.Maybe Core.Bool)
dptvrrsIsDefaultVersion = Lens.field @"isDefaultVersion"
{-# DEPRECATED dptvrrsIsDefaultVersion "Use generic-lens or generic-optics with 'isDefaultVersion' instead." #-}

-- | The JSON formatted contents of the fleet provisioning template version.
--
-- /Note:/ Consider using 'templateBody' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dptvrrsTemplateBody :: Lens.Lens' DescribeProvisioningTemplateVersionResponse (Core.Maybe Types.TemplateBody)
dptvrrsTemplateBody = Lens.field @"templateBody"
{-# DEPRECATED dptvrrsTemplateBody "Use generic-lens or generic-optics with 'templateBody' instead." #-}

-- | The fleet provisioning template version ID.
--
-- /Note:/ Consider using 'versionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dptvrrsVersionId :: Lens.Lens' DescribeProvisioningTemplateVersionResponse (Core.Maybe Core.Int)
dptvrrsVersionId = Lens.field @"versionId"
{-# DEPRECATED dptvrrsVersionId "Use generic-lens or generic-optics with 'versionId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dptvrrsResponseStatus :: Lens.Lens' DescribeProvisioningTemplateVersionResponse Core.Int
dptvrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dptvrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
