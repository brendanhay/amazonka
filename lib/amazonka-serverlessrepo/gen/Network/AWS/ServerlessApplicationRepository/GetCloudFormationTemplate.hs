{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      GetCloudFormationTemplate (..)
    , mkGetCloudFormationTemplate
    -- ** Request lenses
    , gcftApplicationId
    , gcftTemplateId

    -- * Destructuring the response
    , GetCloudFormationTemplateResponse (..)
    , mkGetCloudFormationTemplateResponse
    -- ** Response lenses
    , gcftrrsApplicationId
    , gcftrrsCreationTime
    , gcftrrsExpirationTime
    , gcftrrsSemanticVersion
    , gcftrrsStatus
    , gcftrrsTemplateId
    , gcftrrsTemplateUrl
    , gcftrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.ServerlessApplicationRepository.Types as Types

-- | /See:/ 'mkGetCloudFormationTemplate' smart constructor.
data GetCloudFormationTemplate = GetCloudFormationTemplate'
  { applicationId :: Core.Text
    -- ^ The Amazon Resource Name (ARN) of the application.
  , templateId :: Core.Text
    -- ^ The UUID returned by CreateCloudFormationTemplate.
--
-- Pattern: [0-9a-fA-F]{8}\-[0-9a-fA-F]{4}\-[0-9a-fA-F]{4}\-[0-9a-fA-F]{4}\-[0-9a-fA-F]{12}
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetCloudFormationTemplate' value with any optional fields omitted.
mkGetCloudFormationTemplate
    :: Core.Text -- ^ 'applicationId'
    -> Core.Text -- ^ 'templateId'
    -> GetCloudFormationTemplate
mkGetCloudFormationTemplate applicationId templateId
  = GetCloudFormationTemplate'{applicationId, templateId}

-- | The Amazon Resource Name (ARN) of the application.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcftApplicationId :: Lens.Lens' GetCloudFormationTemplate Core.Text
gcftApplicationId = Lens.field @"applicationId"
{-# INLINEABLE gcftApplicationId #-}
{-# DEPRECATED applicationId "Use generic-lens or generic-optics with 'applicationId' instead"  #-}

-- | The UUID returned by CreateCloudFormationTemplate.
--
-- Pattern: [0-9a-fA-F]{8}\-[0-9a-fA-F]{4}\-[0-9a-fA-F]{4}\-[0-9a-fA-F]{4}\-[0-9a-fA-F]{12}
--
-- /Note:/ Consider using 'templateId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcftTemplateId :: Lens.Lens' GetCloudFormationTemplate Core.Text
gcftTemplateId = Lens.field @"templateId"
{-# INLINEABLE gcftTemplateId #-}
{-# DEPRECATED templateId "Use generic-lens or generic-optics with 'templateId' instead"  #-}

instance Core.ToQuery GetCloudFormationTemplate where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetCloudFormationTemplate where
        toHeaders GetCloudFormationTemplate{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.AWSRequest GetCloudFormationTemplate where
        type Rs GetCloudFormationTemplate =
             GetCloudFormationTemplateResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath =
                           "/applications/" Core.<> Core.toText applicationId Core.<>
                             "/templates/"
                             Core.<> Core.toText templateId,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetCloudFormationTemplateResponse' Core.<$>
                   (x Core..:? "applicationId") Core.<*> x Core..:? "creationTime"
                     Core.<*> x Core..:? "expirationTime"
                     Core.<*> x Core..:? "semanticVersion"
                     Core.<*> x Core..:? "status"
                     Core.<*> x Core..:? "templateId"
                     Core.<*> x Core..:? "templateUrl"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetCloudFormationTemplateResponse' smart constructor.
data GetCloudFormationTemplateResponse = GetCloudFormationTemplateResponse'
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

-- | Creates a 'GetCloudFormationTemplateResponse' value with any optional fields omitted.
mkGetCloudFormationTemplateResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetCloudFormationTemplateResponse
mkGetCloudFormationTemplateResponse responseStatus
  = GetCloudFormationTemplateResponse'{applicationId = Core.Nothing,
                                       creationTime = Core.Nothing, expirationTime = Core.Nothing,
                                       semanticVersion = Core.Nothing, status = Core.Nothing,
                                       templateId = Core.Nothing, templateUrl = Core.Nothing,
                                       responseStatus}

-- | The application Amazon Resource Name (ARN).
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcftrrsApplicationId :: Lens.Lens' GetCloudFormationTemplateResponse (Core.Maybe Core.Text)
gcftrrsApplicationId = Lens.field @"applicationId"
{-# INLINEABLE gcftrrsApplicationId #-}
{-# DEPRECATED applicationId "Use generic-lens or generic-optics with 'applicationId' instead"  #-}

-- | The date and time this resource was created.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcftrrsCreationTime :: Lens.Lens' GetCloudFormationTemplateResponse (Core.Maybe Core.Text)
gcftrrsCreationTime = Lens.field @"creationTime"
{-# INLINEABLE gcftrrsCreationTime #-}
{-# DEPRECATED creationTime "Use generic-lens or generic-optics with 'creationTime' instead"  #-}

-- | The date and time this template expires. Templates
--
--  expire 1 hour after creation.
--
-- /Note:/ Consider using 'expirationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcftrrsExpirationTime :: Lens.Lens' GetCloudFormationTemplateResponse (Core.Maybe Core.Text)
gcftrrsExpirationTime = Lens.field @"expirationTime"
{-# INLINEABLE gcftrrsExpirationTime #-}
{-# DEPRECATED expirationTime "Use generic-lens or generic-optics with 'expirationTime' instead"  #-}

-- | The semantic version of the application:
--
-- <https://semver.org/ https://semver.org/> 
--
-- /Note:/ Consider using 'semanticVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcftrrsSemanticVersion :: Lens.Lens' GetCloudFormationTemplateResponse (Core.Maybe Core.Text)
gcftrrsSemanticVersion = Lens.field @"semanticVersion"
{-# INLINEABLE gcftrrsSemanticVersion #-}
{-# DEPRECATED semanticVersion "Use generic-lens or generic-optics with 'semanticVersion' instead"  #-}

-- | Status of the template creation workflow.
--
-- Possible values: PREPARING | ACTIVE | EXPIRED
--  
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcftrrsStatus :: Lens.Lens' GetCloudFormationTemplateResponse (Core.Maybe Types.Status)
gcftrrsStatus = Lens.field @"status"
{-# INLINEABLE gcftrrsStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

-- | The UUID returned by CreateCloudFormationTemplate.
--
-- Pattern: [0-9a-fA-F]{8}\-[0-9a-fA-F]{4}\-[0-9a-fA-F]{4}\-[0-9a-fA-F]{4}\-[0-9a-fA-F]{12}
--
-- /Note:/ Consider using 'templateId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcftrrsTemplateId :: Lens.Lens' GetCloudFormationTemplateResponse (Core.Maybe Core.Text)
gcftrrsTemplateId = Lens.field @"templateId"
{-# INLINEABLE gcftrrsTemplateId #-}
{-# DEPRECATED templateId "Use generic-lens or generic-optics with 'templateId' instead"  #-}

-- | A link to the template that can be used to deploy the application using
--
--  AWS CloudFormation.
--
-- /Note:/ Consider using 'templateUrl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcftrrsTemplateUrl :: Lens.Lens' GetCloudFormationTemplateResponse (Core.Maybe Core.Text)
gcftrrsTemplateUrl = Lens.field @"templateUrl"
{-# INLINEABLE gcftrrsTemplateUrl #-}
{-# DEPRECATED templateUrl "Use generic-lens or generic-optics with 'templateUrl' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcftrrsResponseStatus :: Lens.Lens' GetCloudFormationTemplateResponse Core.Int
gcftrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gcftrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
