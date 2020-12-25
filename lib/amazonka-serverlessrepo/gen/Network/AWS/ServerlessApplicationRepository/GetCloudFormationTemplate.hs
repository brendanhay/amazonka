{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServerlessApplicationRepository.GetCloudFormationTemplate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the specified AWS CloudFormation template.
module Network.AWS.ServerlessApplicationRepository.GetCloudFormationTemplate
  ( -- * Creating a request
    GetCloudFormationTemplate (..),
    mkGetCloudFormationTemplate,

    -- ** Request lenses
    gcftApplicationId,
    gcftTemplateId,

    -- * Destructuring the response
    GetCloudFormationTemplateResponse (..),
    mkGetCloudFormationTemplateResponse,

    -- ** Response lenses
    gcftrrsApplicationId,
    gcftrrsCreationTime,
    gcftrrsExpirationTime,
    gcftrrsSemanticVersion,
    gcftrrsStatus,
    gcftrrsTemplateId,
    gcftrrsTemplateUrl,
    gcftrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.ServerlessApplicationRepository.Types as Types

-- | /See:/ 'mkGetCloudFormationTemplate' smart constructor.
data GetCloudFormationTemplate = GetCloudFormationTemplate'
  { -- | The Amazon Resource Name (ARN) of the application.
    applicationId :: Core.Text,
    -- | The UUID returned by CreateCloudFormationTemplate.
    --
    -- Pattern: [0-9a-fA-F]{8}\-[0-9a-fA-F]{4}\-[0-9a-fA-F]{4}\-[0-9a-fA-F]{4}\-[0-9a-fA-F]{12}
    templateId :: Core.Text
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetCloudFormationTemplate' value with any optional fields omitted.
mkGetCloudFormationTemplate ::
  -- | 'applicationId'
  Core.Text ->
  -- | 'templateId'
  Core.Text ->
  GetCloudFormationTemplate
mkGetCloudFormationTemplate applicationId templateId =
  GetCloudFormationTemplate' {applicationId, templateId}

-- | The Amazon Resource Name (ARN) of the application.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcftApplicationId :: Lens.Lens' GetCloudFormationTemplate Core.Text
gcftApplicationId = Lens.field @"applicationId"
{-# DEPRECATED gcftApplicationId "Use generic-lens or generic-optics with 'applicationId' instead." #-}

-- | The UUID returned by CreateCloudFormationTemplate.
--
-- Pattern: [0-9a-fA-F]{8}\-[0-9a-fA-F]{4}\-[0-9a-fA-F]{4}\-[0-9a-fA-F]{4}\-[0-9a-fA-F]{12}
--
-- /Note:/ Consider using 'templateId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcftTemplateId :: Lens.Lens' GetCloudFormationTemplate Core.Text
gcftTemplateId = Lens.field @"templateId"
{-# DEPRECATED gcftTemplateId "Use generic-lens or generic-optics with 'templateId' instead." #-}

instance Core.AWSRequest GetCloudFormationTemplate where
  type
    Rs GetCloudFormationTemplate =
      GetCloudFormationTemplateResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath =
          Core.rawPath
            ( "/applications/" Core.<> (Core.toText applicationId)
                Core.<> ("/templates/")
                Core.<> (Core.toText templateId)
            ),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = ""
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetCloudFormationTemplateResponse'
            Core.<$> (x Core..:? "applicationId")
            Core.<*> (x Core..:? "creationTime")
            Core.<*> (x Core..:? "expirationTime")
            Core.<*> (x Core..:? "semanticVersion")
            Core.<*> (x Core..:? "status")
            Core.<*> (x Core..:? "templateId")
            Core.<*> (x Core..:? "templateUrl")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetCloudFormationTemplateResponse' smart constructor.
data GetCloudFormationTemplateResponse = GetCloudFormationTemplateResponse'
  { -- | The application Amazon Resource Name (ARN).
    applicationId :: Core.Maybe Core.Text,
    -- | The date and time this resource was created.
    creationTime :: Core.Maybe Core.Text,
    -- | The date and time this template expires. Templates
    --
    --  expire 1 hour after creation.
    expirationTime :: Core.Maybe Core.Text,
    -- | The semantic version of the application:
    --
    -- <https://semver.org/ https://semver.org/>
    semanticVersion :: Core.Maybe Core.Text,
    -- | Status of the template creation workflow.
    --
    -- Possible values: PREPARING | ACTIVE | EXPIRED
    status :: Core.Maybe Types.Status,
    -- | The UUID returned by CreateCloudFormationTemplate.
    --
    -- Pattern: [0-9a-fA-F]{8}\-[0-9a-fA-F]{4}\-[0-9a-fA-F]{4}\-[0-9a-fA-F]{4}\-[0-9a-fA-F]{12}
    templateId :: Core.Maybe Core.Text,
    -- | A link to the template that can be used to deploy the application using
    --
    --  AWS CloudFormation.
    templateUrl :: Core.Maybe Core.Text,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetCloudFormationTemplateResponse' value with any optional fields omitted.
mkGetCloudFormationTemplateResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetCloudFormationTemplateResponse
mkGetCloudFormationTemplateResponse responseStatus =
  GetCloudFormationTemplateResponse'
    { applicationId = Core.Nothing,
      creationTime = Core.Nothing,
      expirationTime = Core.Nothing,
      semanticVersion = Core.Nothing,
      status = Core.Nothing,
      templateId = Core.Nothing,
      templateUrl = Core.Nothing,
      responseStatus
    }

-- | The application Amazon Resource Name (ARN).
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcftrrsApplicationId :: Lens.Lens' GetCloudFormationTemplateResponse (Core.Maybe Core.Text)
gcftrrsApplicationId = Lens.field @"applicationId"
{-# DEPRECATED gcftrrsApplicationId "Use generic-lens or generic-optics with 'applicationId' instead." #-}

-- | The date and time this resource was created.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcftrrsCreationTime :: Lens.Lens' GetCloudFormationTemplateResponse (Core.Maybe Core.Text)
gcftrrsCreationTime = Lens.field @"creationTime"
{-# DEPRECATED gcftrrsCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

-- | The date and time this template expires. Templates
--
--  expire 1 hour after creation.
--
-- /Note:/ Consider using 'expirationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcftrrsExpirationTime :: Lens.Lens' GetCloudFormationTemplateResponse (Core.Maybe Core.Text)
gcftrrsExpirationTime = Lens.field @"expirationTime"
{-# DEPRECATED gcftrrsExpirationTime "Use generic-lens or generic-optics with 'expirationTime' instead." #-}

-- | The semantic version of the application:
--
-- <https://semver.org/ https://semver.org/>
--
-- /Note:/ Consider using 'semanticVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcftrrsSemanticVersion :: Lens.Lens' GetCloudFormationTemplateResponse (Core.Maybe Core.Text)
gcftrrsSemanticVersion = Lens.field @"semanticVersion"
{-# DEPRECATED gcftrrsSemanticVersion "Use generic-lens or generic-optics with 'semanticVersion' instead." #-}

-- | Status of the template creation workflow.
--
-- Possible values: PREPARING | ACTIVE | EXPIRED
--
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcftrrsStatus :: Lens.Lens' GetCloudFormationTemplateResponse (Core.Maybe Types.Status)
gcftrrsStatus = Lens.field @"status"
{-# DEPRECATED gcftrrsStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The UUID returned by CreateCloudFormationTemplate.
--
-- Pattern: [0-9a-fA-F]{8}\-[0-9a-fA-F]{4}\-[0-9a-fA-F]{4}\-[0-9a-fA-F]{4}\-[0-9a-fA-F]{12}
--
-- /Note:/ Consider using 'templateId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcftrrsTemplateId :: Lens.Lens' GetCloudFormationTemplateResponse (Core.Maybe Core.Text)
gcftrrsTemplateId = Lens.field @"templateId"
{-# DEPRECATED gcftrrsTemplateId "Use generic-lens or generic-optics with 'templateId' instead." #-}

-- | A link to the template that can be used to deploy the application using
--
--  AWS CloudFormation.
--
-- /Note:/ Consider using 'templateUrl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcftrrsTemplateUrl :: Lens.Lens' GetCloudFormationTemplateResponse (Core.Maybe Core.Text)
gcftrrsTemplateUrl = Lens.field @"templateUrl"
{-# DEPRECATED gcftrrsTemplateUrl "Use generic-lens or generic-optics with 'templateUrl' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcftrrsResponseStatus :: Lens.Lens' GetCloudFormationTemplateResponse Core.Int
gcftrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gcftrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
