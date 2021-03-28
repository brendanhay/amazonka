{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      DescribeProvisioningTemplate (..)
    , mkDescribeProvisioningTemplate
    -- ** Request lenses
    , dptTemplateName

    -- * Destructuring the response
    , DescribeProvisioningTemplateResponse (..)
    , mkDescribeProvisioningTemplateResponse
    -- ** Response lenses
    , dptrrsCreationDate
    , dptrrsDefaultVersionId
    , dptrrsDescription
    , dptrrsEnabled
    , dptrrsLastModifiedDate
    , dptrrsPreProvisioningHook
    , dptrrsProvisioningRoleArn
    , dptrrsTemplateArn
    , dptrrsTemplateBody
    , dptrrsTemplateName
    , dptrrsResponseStatus
    ) where

import qualified Network.AWS.IoT.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeProvisioningTemplate' smart constructor.
newtype DescribeProvisioningTemplate = DescribeProvisioningTemplate'
  { templateName :: Types.TemplateName
    -- ^ The name of the fleet provisioning template.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeProvisioningTemplate' value with any optional fields omitted.
mkDescribeProvisioningTemplate
    :: Types.TemplateName -- ^ 'templateName'
    -> DescribeProvisioningTemplate
mkDescribeProvisioningTemplate templateName
  = DescribeProvisioningTemplate'{templateName}

-- | The name of the fleet provisioning template.
--
-- /Note:/ Consider using 'templateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dptTemplateName :: Lens.Lens' DescribeProvisioningTemplate Types.TemplateName
dptTemplateName = Lens.field @"templateName"
{-# INLINEABLE dptTemplateName #-}
{-# DEPRECATED templateName "Use generic-lens or generic-optics with 'templateName' instead"  #-}

instance Core.ToQuery DescribeProvisioningTemplate where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeProvisioningTemplate where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DescribeProvisioningTemplate where
        type Rs DescribeProvisioningTemplate =
             DescribeProvisioningTemplateResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath =
                           "/provisioning-templates/" Core.<> Core.toText templateName,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeProvisioningTemplateResponse' Core.<$>
                   (x Core..:? "creationDate") Core.<*> x Core..:? "defaultVersionId"
                     Core.<*> x Core..:? "description"
                     Core.<*> x Core..:? "enabled"
                     Core.<*> x Core..:? "lastModifiedDate"
                     Core.<*> x Core..:? "preProvisioningHook"
                     Core.<*> x Core..:? "provisioningRoleArn"
                     Core.<*> x Core..:? "templateArn"
                     Core.<*> x Core..:? "templateBody"
                     Core.<*> x Core..:? "templateName"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDescribeProvisioningTemplateResponse' smart constructor.
data DescribeProvisioningTemplateResponse = DescribeProvisioningTemplateResponse'
  { creationDate :: Core.Maybe Core.NominalDiffTime
    -- ^ The date when the fleet provisioning template was created.
  , defaultVersionId :: Core.Maybe Core.Int
    -- ^ The default fleet template version ID.
  , description :: Core.Maybe Types.Description
    -- ^ The description of the fleet provisioning template.
  , enabled :: Core.Maybe Core.Bool
    -- ^ True if the fleet provisioning template is enabled, otherwise false.
  , lastModifiedDate :: Core.Maybe Core.NominalDiffTime
    -- ^ The date when the fleet provisioning template was last modified.
  , preProvisioningHook :: Core.Maybe Types.ProvisioningHook
    -- ^ Gets information about a pre-provisioned hook.
  , provisioningRoleArn :: Core.Maybe Types.ProvisioningRoleArn
    -- ^ The ARN of the role associated with the provisioning template. This IoT role grants permission to provision a device.
  , templateArn :: Core.Maybe Types.TemplateArn
    -- ^ The ARN of the fleet provisioning template.
  , templateBody :: Core.Maybe Types.TemplateBody
    -- ^ The JSON formatted contents of the fleet provisioning template.
  , templateName :: Core.Maybe Types.TemplateName
    -- ^ The name of the fleet provisioning template.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeProvisioningTemplateResponse' value with any optional fields omitted.
mkDescribeProvisioningTemplateResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeProvisioningTemplateResponse
mkDescribeProvisioningTemplateResponse responseStatus
  = DescribeProvisioningTemplateResponse'{creationDate =
                                            Core.Nothing,
                                          defaultVersionId = Core.Nothing,
                                          description = Core.Nothing, enabled = Core.Nothing,
                                          lastModifiedDate = Core.Nothing,
                                          preProvisioningHook = Core.Nothing,
                                          provisioningRoleArn = Core.Nothing,
                                          templateArn = Core.Nothing, templateBody = Core.Nothing,
                                          templateName = Core.Nothing, responseStatus}

-- | The date when the fleet provisioning template was created.
--
-- /Note:/ Consider using 'creationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dptrrsCreationDate :: Lens.Lens' DescribeProvisioningTemplateResponse (Core.Maybe Core.NominalDiffTime)
dptrrsCreationDate = Lens.field @"creationDate"
{-# INLINEABLE dptrrsCreationDate #-}
{-# DEPRECATED creationDate "Use generic-lens or generic-optics with 'creationDate' instead"  #-}

-- | The default fleet template version ID.
--
-- /Note:/ Consider using 'defaultVersionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dptrrsDefaultVersionId :: Lens.Lens' DescribeProvisioningTemplateResponse (Core.Maybe Core.Int)
dptrrsDefaultVersionId = Lens.field @"defaultVersionId"
{-# INLINEABLE dptrrsDefaultVersionId #-}
{-# DEPRECATED defaultVersionId "Use generic-lens or generic-optics with 'defaultVersionId' instead"  #-}

-- | The description of the fleet provisioning template.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dptrrsDescription :: Lens.Lens' DescribeProvisioningTemplateResponse (Core.Maybe Types.Description)
dptrrsDescription = Lens.field @"description"
{-# INLINEABLE dptrrsDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | True if the fleet provisioning template is enabled, otherwise false.
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dptrrsEnabled :: Lens.Lens' DescribeProvisioningTemplateResponse (Core.Maybe Core.Bool)
dptrrsEnabled = Lens.field @"enabled"
{-# INLINEABLE dptrrsEnabled #-}
{-# DEPRECATED enabled "Use generic-lens or generic-optics with 'enabled' instead"  #-}

-- | The date when the fleet provisioning template was last modified.
--
-- /Note:/ Consider using 'lastModifiedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dptrrsLastModifiedDate :: Lens.Lens' DescribeProvisioningTemplateResponse (Core.Maybe Core.NominalDiffTime)
dptrrsLastModifiedDate = Lens.field @"lastModifiedDate"
{-# INLINEABLE dptrrsLastModifiedDate #-}
{-# DEPRECATED lastModifiedDate "Use generic-lens or generic-optics with 'lastModifiedDate' instead"  #-}

-- | Gets information about a pre-provisioned hook.
--
-- /Note:/ Consider using 'preProvisioningHook' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dptrrsPreProvisioningHook :: Lens.Lens' DescribeProvisioningTemplateResponse (Core.Maybe Types.ProvisioningHook)
dptrrsPreProvisioningHook = Lens.field @"preProvisioningHook"
{-# INLINEABLE dptrrsPreProvisioningHook #-}
{-# DEPRECATED preProvisioningHook "Use generic-lens or generic-optics with 'preProvisioningHook' instead"  #-}

-- | The ARN of the role associated with the provisioning template. This IoT role grants permission to provision a device.
--
-- /Note:/ Consider using 'provisioningRoleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dptrrsProvisioningRoleArn :: Lens.Lens' DescribeProvisioningTemplateResponse (Core.Maybe Types.ProvisioningRoleArn)
dptrrsProvisioningRoleArn = Lens.field @"provisioningRoleArn"
{-# INLINEABLE dptrrsProvisioningRoleArn #-}
{-# DEPRECATED provisioningRoleArn "Use generic-lens or generic-optics with 'provisioningRoleArn' instead"  #-}

-- | The ARN of the fleet provisioning template.
--
-- /Note:/ Consider using 'templateArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dptrrsTemplateArn :: Lens.Lens' DescribeProvisioningTemplateResponse (Core.Maybe Types.TemplateArn)
dptrrsTemplateArn = Lens.field @"templateArn"
{-# INLINEABLE dptrrsTemplateArn #-}
{-# DEPRECATED templateArn "Use generic-lens or generic-optics with 'templateArn' instead"  #-}

-- | The JSON formatted contents of the fleet provisioning template.
--
-- /Note:/ Consider using 'templateBody' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dptrrsTemplateBody :: Lens.Lens' DescribeProvisioningTemplateResponse (Core.Maybe Types.TemplateBody)
dptrrsTemplateBody = Lens.field @"templateBody"
{-# INLINEABLE dptrrsTemplateBody #-}
{-# DEPRECATED templateBody "Use generic-lens or generic-optics with 'templateBody' instead"  #-}

-- | The name of the fleet provisioning template.
--
-- /Note:/ Consider using 'templateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dptrrsTemplateName :: Lens.Lens' DescribeProvisioningTemplateResponse (Core.Maybe Types.TemplateName)
dptrrsTemplateName = Lens.field @"templateName"
{-# INLINEABLE dptrrsTemplateName #-}
{-# DEPRECATED templateName "Use generic-lens or generic-optics with 'templateName' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dptrrsResponseStatus :: Lens.Lens' DescribeProvisioningTemplateResponse Core.Int
dptrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dptrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
