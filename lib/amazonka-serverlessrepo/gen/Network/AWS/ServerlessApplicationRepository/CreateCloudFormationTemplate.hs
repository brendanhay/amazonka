{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServerlessApplicationRepository.CreateCloudFormationTemplate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an AWS CloudFormation template.
module Network.AWS.ServerlessApplicationRepository.CreateCloudFormationTemplate
  ( -- * Creating a request
    CreateCloudFormationTemplate (..),
    mkCreateCloudFormationTemplate,

    -- ** Request lenses
    ccftApplicationId,
    ccftSemanticVersion,

    -- * Destructuring the response
    CreateCloudFormationTemplateResponse (..),
    mkCreateCloudFormationTemplateResponse,

    -- ** Response lenses
    ccftrrsApplicationId,
    ccftrrsCreationTime,
    ccftrrsExpirationTime,
    ccftrrsSemanticVersion,
    ccftrrsStatus,
    ccftrrsTemplateId,
    ccftrrsTemplateUrl,
    ccftrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.ServerlessApplicationRepository.Types as Types

-- | /See:/ 'mkCreateCloudFormationTemplate' smart constructor.
data CreateCloudFormationTemplate = CreateCloudFormationTemplate'
  { -- | The Amazon Resource Name (ARN) of the application.
    applicationId :: Core.Text,
    -- | The semantic version of the application:
    --
    -- <https://semver.org/ https://semver.org/>
    semanticVersion :: Core.Maybe Core.Text
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateCloudFormationTemplate' value with any optional fields omitted.
mkCreateCloudFormationTemplate ::
  -- | 'applicationId'
  Core.Text ->
  CreateCloudFormationTemplate
mkCreateCloudFormationTemplate applicationId =
  CreateCloudFormationTemplate'
    { applicationId,
      semanticVersion = Core.Nothing
    }

-- | The Amazon Resource Name (ARN) of the application.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccftApplicationId :: Lens.Lens' CreateCloudFormationTemplate Core.Text
ccftApplicationId = Lens.field @"applicationId"
{-# DEPRECATED ccftApplicationId "Use generic-lens or generic-optics with 'applicationId' instead." #-}

-- | The semantic version of the application:
--
-- <https://semver.org/ https://semver.org/>
--
-- /Note:/ Consider using 'semanticVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccftSemanticVersion :: Lens.Lens' CreateCloudFormationTemplate (Core.Maybe Core.Text)
ccftSemanticVersion = Lens.field @"semanticVersion"
{-# DEPRECATED ccftSemanticVersion "Use generic-lens or generic-optics with 'semanticVersion' instead." #-}

instance Core.FromJSON CreateCloudFormationTemplate where
  toJSON CreateCloudFormationTemplate {..} =
    Core.object
      ( Core.catMaybes
          [("semanticVersion" Core..=) Core.<$> semanticVersion]
      )

instance Core.AWSRequest CreateCloudFormationTemplate where
  type
    Rs CreateCloudFormationTemplate =
      CreateCloudFormationTemplateResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath =
          Core.rawPath
            ( "/applications/" Core.<> (Core.toText applicationId)
                Core.<> ("/templates")
            ),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateCloudFormationTemplateResponse'
            Core.<$> (x Core..:? "applicationId")
            Core.<*> (x Core..:? "creationTime")
            Core.<*> (x Core..:? "expirationTime")
            Core.<*> (x Core..:? "semanticVersion")
            Core.<*> (x Core..:? "status")
            Core.<*> (x Core..:? "templateId")
            Core.<*> (x Core..:? "templateUrl")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCreateCloudFormationTemplateResponse' smart constructor.
data CreateCloudFormationTemplateResponse = CreateCloudFormationTemplateResponse'
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

-- | Creates a 'CreateCloudFormationTemplateResponse' value with any optional fields omitted.
mkCreateCloudFormationTemplateResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateCloudFormationTemplateResponse
mkCreateCloudFormationTemplateResponse responseStatus =
  CreateCloudFormationTemplateResponse'
    { applicationId =
        Core.Nothing,
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
ccftrrsApplicationId :: Lens.Lens' CreateCloudFormationTemplateResponse (Core.Maybe Core.Text)
ccftrrsApplicationId = Lens.field @"applicationId"
{-# DEPRECATED ccftrrsApplicationId "Use generic-lens or generic-optics with 'applicationId' instead." #-}

-- | The date and time this resource was created.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccftrrsCreationTime :: Lens.Lens' CreateCloudFormationTemplateResponse (Core.Maybe Core.Text)
ccftrrsCreationTime = Lens.field @"creationTime"
{-# DEPRECATED ccftrrsCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

-- | The date and time this template expires. Templates
--
--  expire 1 hour after creation.
--
-- /Note:/ Consider using 'expirationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccftrrsExpirationTime :: Lens.Lens' CreateCloudFormationTemplateResponse (Core.Maybe Core.Text)
ccftrrsExpirationTime = Lens.field @"expirationTime"
{-# DEPRECATED ccftrrsExpirationTime "Use generic-lens or generic-optics with 'expirationTime' instead." #-}

-- | The semantic version of the application:
--
-- <https://semver.org/ https://semver.org/>
--
-- /Note:/ Consider using 'semanticVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccftrrsSemanticVersion :: Lens.Lens' CreateCloudFormationTemplateResponse (Core.Maybe Core.Text)
ccftrrsSemanticVersion = Lens.field @"semanticVersion"
{-# DEPRECATED ccftrrsSemanticVersion "Use generic-lens or generic-optics with 'semanticVersion' instead." #-}

-- | Status of the template creation workflow.
--
-- Possible values: PREPARING | ACTIVE | EXPIRED
--
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccftrrsStatus :: Lens.Lens' CreateCloudFormationTemplateResponse (Core.Maybe Types.Status)
ccftrrsStatus = Lens.field @"status"
{-# DEPRECATED ccftrrsStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The UUID returned by CreateCloudFormationTemplate.
--
-- Pattern: [0-9a-fA-F]{8}\-[0-9a-fA-F]{4}\-[0-9a-fA-F]{4}\-[0-9a-fA-F]{4}\-[0-9a-fA-F]{12}
--
-- /Note:/ Consider using 'templateId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccftrrsTemplateId :: Lens.Lens' CreateCloudFormationTemplateResponse (Core.Maybe Core.Text)
ccftrrsTemplateId = Lens.field @"templateId"
{-# DEPRECATED ccftrrsTemplateId "Use generic-lens or generic-optics with 'templateId' instead." #-}

-- | A link to the template that can be used to deploy the application using
--
--  AWS CloudFormation.
--
-- /Note:/ Consider using 'templateUrl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccftrrsTemplateUrl :: Lens.Lens' CreateCloudFormationTemplateResponse (Core.Maybe Core.Text)
ccftrrsTemplateUrl = Lens.field @"templateUrl"
{-# DEPRECATED ccftrrsTemplateUrl "Use generic-lens or generic-optics with 'templateUrl' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccftrrsResponseStatus :: Lens.Lens' CreateCloudFormationTemplateResponse Core.Int
ccftrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ccftrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
