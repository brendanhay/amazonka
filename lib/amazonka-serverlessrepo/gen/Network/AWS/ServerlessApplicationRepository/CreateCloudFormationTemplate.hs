{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      CreateCloudFormationTemplate (..)
    , mkCreateCloudFormationTemplate
    -- ** Request lenses
    , ccftApplicationId
    , ccftSemanticVersion

    -- * Destructuring the response
    , CreateCloudFormationTemplateResponse (..)
    , mkCreateCloudFormationTemplateResponse
    -- ** Response lenses
    , ccftrrsApplicationId
    , ccftrrsCreationTime
    , ccftrrsExpirationTime
    , ccftrrsSemanticVersion
    , ccftrrsStatus
    , ccftrrsTemplateId
    , ccftrrsTemplateUrl
    , ccftrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.ServerlessApplicationRepository.Types as Types

-- | /See:/ 'mkCreateCloudFormationTemplate' smart constructor.
data CreateCloudFormationTemplate = CreateCloudFormationTemplate'
  { applicationId :: Core.Text
    -- ^ The Amazon Resource Name (ARN) of the application.
  , semanticVersion :: Core.Maybe Core.Text
    -- ^ The semantic version of the application:
--
-- <https://semver.org/ https://semver.org/> 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateCloudFormationTemplate' value with any optional fields omitted.
mkCreateCloudFormationTemplate
    :: Core.Text -- ^ 'applicationId'
    -> CreateCloudFormationTemplate
mkCreateCloudFormationTemplate applicationId
  = CreateCloudFormationTemplate'{applicationId,
                                  semanticVersion = Core.Nothing}

-- | The Amazon Resource Name (ARN) of the application.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccftApplicationId :: Lens.Lens' CreateCloudFormationTemplate Core.Text
ccftApplicationId = Lens.field @"applicationId"
{-# INLINEABLE ccftApplicationId #-}
{-# DEPRECATED applicationId "Use generic-lens or generic-optics with 'applicationId' instead"  #-}

-- | The semantic version of the application:
--
-- <https://semver.org/ https://semver.org/> 
--
-- /Note:/ Consider using 'semanticVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccftSemanticVersion :: Lens.Lens' CreateCloudFormationTemplate (Core.Maybe Core.Text)
ccftSemanticVersion = Lens.field @"semanticVersion"
{-# INLINEABLE ccftSemanticVersion #-}
{-# DEPRECATED semanticVersion "Use generic-lens or generic-optics with 'semanticVersion' instead"  #-}

instance Core.ToQuery CreateCloudFormationTemplate where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateCloudFormationTemplate where
        toHeaders CreateCloudFormationTemplate{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CreateCloudFormationTemplate where
        toJSON CreateCloudFormationTemplate{..}
          = Core.object
              (Core.catMaybes
                 [("semanticVersion" Core..=) Core.<$> semanticVersion])

instance Core.AWSRequest CreateCloudFormationTemplate where
        type Rs CreateCloudFormationTemplate =
             CreateCloudFormationTemplateResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST,
                         Core._rqPath =
                           "/applications/" Core.<> Core.toText applicationId Core.<>
                             "/templates",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CreateCloudFormationTemplateResponse' Core.<$>
                   (x Core..:? "applicationId") Core.<*> x Core..:? "creationTime"
                     Core.<*> x Core..:? "expirationTime"
                     Core.<*> x Core..:? "semanticVersion"
                     Core.<*> x Core..:? "status"
                     Core.<*> x Core..:? "templateId"
                     Core.<*> x Core..:? "templateUrl"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateCloudFormationTemplateResponse' smart constructor.
data CreateCloudFormationTemplateResponse = CreateCloudFormationTemplateResponse'
  { applicationId :: Core.Maybe Core.Text
    -- ^ The application Amazon Resource Name (ARN).
  , creationTime :: Core.Maybe Core.Text
    -- ^ The date and time this resource was created.
  , expirationTime :: Core.Maybe Core.Text
    -- ^ The date and time this template expires. Templates
--
--  expire 1 hour after creation.
  , semanticVersion :: Core.Maybe Core.Text
    -- ^ The semantic version of the application:
--
-- <https://semver.org/ https://semver.org/> 
  , status :: Core.Maybe Types.Status
    -- ^ Status of the template creation workflow.
--
-- Possible values: PREPARING | ACTIVE | EXPIRED
--  
  , templateId :: Core.Maybe Core.Text
    -- ^ The UUID returned by CreateCloudFormationTemplate.
--
-- Pattern: [0-9a-fA-F]{8}\-[0-9a-fA-F]{4}\-[0-9a-fA-F]{4}\-[0-9a-fA-F]{4}\-[0-9a-fA-F]{12}
  , templateUrl :: Core.Maybe Core.Text
    -- ^ A link to the template that can be used to deploy the application using
--
--  AWS CloudFormation.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateCloudFormationTemplateResponse' value with any optional fields omitted.
mkCreateCloudFormationTemplateResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateCloudFormationTemplateResponse
mkCreateCloudFormationTemplateResponse responseStatus
  = CreateCloudFormationTemplateResponse'{applicationId =
                                            Core.Nothing,
                                          creationTime = Core.Nothing,
                                          expirationTime = Core.Nothing,
                                          semanticVersion = Core.Nothing, status = Core.Nothing,
                                          templateId = Core.Nothing, templateUrl = Core.Nothing,
                                          responseStatus}

-- | The application Amazon Resource Name (ARN).
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccftrrsApplicationId :: Lens.Lens' CreateCloudFormationTemplateResponse (Core.Maybe Core.Text)
ccftrrsApplicationId = Lens.field @"applicationId"
{-# INLINEABLE ccftrrsApplicationId #-}
{-# DEPRECATED applicationId "Use generic-lens or generic-optics with 'applicationId' instead"  #-}

-- | The date and time this resource was created.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccftrrsCreationTime :: Lens.Lens' CreateCloudFormationTemplateResponse (Core.Maybe Core.Text)
ccftrrsCreationTime = Lens.field @"creationTime"
{-# INLINEABLE ccftrrsCreationTime #-}
{-# DEPRECATED creationTime "Use generic-lens or generic-optics with 'creationTime' instead"  #-}

-- | The date and time this template expires. Templates
--
--  expire 1 hour after creation.
--
-- /Note:/ Consider using 'expirationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccftrrsExpirationTime :: Lens.Lens' CreateCloudFormationTemplateResponse (Core.Maybe Core.Text)
ccftrrsExpirationTime = Lens.field @"expirationTime"
{-# INLINEABLE ccftrrsExpirationTime #-}
{-# DEPRECATED expirationTime "Use generic-lens or generic-optics with 'expirationTime' instead"  #-}

-- | The semantic version of the application:
--
-- <https://semver.org/ https://semver.org/> 
--
-- /Note:/ Consider using 'semanticVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccftrrsSemanticVersion :: Lens.Lens' CreateCloudFormationTemplateResponse (Core.Maybe Core.Text)
ccftrrsSemanticVersion = Lens.field @"semanticVersion"
{-# INLINEABLE ccftrrsSemanticVersion #-}
{-# DEPRECATED semanticVersion "Use generic-lens or generic-optics with 'semanticVersion' instead"  #-}

-- | Status of the template creation workflow.
--
-- Possible values: PREPARING | ACTIVE | EXPIRED
--  
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccftrrsStatus :: Lens.Lens' CreateCloudFormationTemplateResponse (Core.Maybe Types.Status)
ccftrrsStatus = Lens.field @"status"
{-# INLINEABLE ccftrrsStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

-- | The UUID returned by CreateCloudFormationTemplate.
--
-- Pattern: [0-9a-fA-F]{8}\-[0-9a-fA-F]{4}\-[0-9a-fA-F]{4}\-[0-9a-fA-F]{4}\-[0-9a-fA-F]{12}
--
-- /Note:/ Consider using 'templateId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccftrrsTemplateId :: Lens.Lens' CreateCloudFormationTemplateResponse (Core.Maybe Core.Text)
ccftrrsTemplateId = Lens.field @"templateId"
{-# INLINEABLE ccftrrsTemplateId #-}
{-# DEPRECATED templateId "Use generic-lens or generic-optics with 'templateId' instead"  #-}

-- | A link to the template that can be used to deploy the application using
--
--  AWS CloudFormation.
--
-- /Note:/ Consider using 'templateUrl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccftrrsTemplateUrl :: Lens.Lens' CreateCloudFormationTemplateResponse (Core.Maybe Core.Text)
ccftrrsTemplateUrl = Lens.field @"templateUrl"
{-# INLINEABLE ccftrrsTemplateUrl #-}
{-# DEPRECATED templateUrl "Use generic-lens or generic-optics with 'templateUrl' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccftrrsResponseStatus :: Lens.Lens' CreateCloudFormationTemplateResponse Core.Int
ccftrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ccftrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
