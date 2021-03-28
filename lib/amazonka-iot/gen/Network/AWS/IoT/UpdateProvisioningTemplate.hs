{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      UpdateProvisioningTemplate (..)
    , mkUpdateProvisioningTemplate
    -- ** Request lenses
    , uptTemplateName
    , uptDefaultVersionId
    , uptDescription
    , uptEnabled
    , uptPreProvisioningHook
    , uptProvisioningRoleArn
    , uptRemovePreProvisioningHook

    -- * Destructuring the response
    , UpdateProvisioningTemplateResponse (..)
    , mkUpdateProvisioningTemplateResponse
    -- ** Response lenses
    , uptrrsResponseStatus
    ) where

import qualified Network.AWS.IoT.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateProvisioningTemplate' smart constructor.
data UpdateProvisioningTemplate = UpdateProvisioningTemplate'
  { templateName :: Types.TemplateName
    -- ^ The name of the fleet provisioning template.
  , defaultVersionId :: Core.Maybe Core.Int
    -- ^ The ID of the default provisioning template version.
  , description :: Core.Maybe Types.TemplateDescription
    -- ^ The description of the fleet provisioning template.
  , enabled :: Core.Maybe Core.Bool
    -- ^ True to enable the fleet provisioning template, otherwise false.
  , preProvisioningHook :: Core.Maybe Types.ProvisioningHook
    -- ^ Updates the pre-provisioning hook template.
  , provisioningRoleArn :: Core.Maybe Types.ProvisioningRoleArn
    -- ^ The ARN of the role associated with the provisioning template. This IoT role grants permission to provision a device.
  , removePreProvisioningHook :: Core.Maybe Core.Bool
    -- ^ Removes pre-provisioning hook template.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateProvisioningTemplate' value with any optional fields omitted.
mkUpdateProvisioningTemplate
    :: Types.TemplateName -- ^ 'templateName'
    -> UpdateProvisioningTemplate
mkUpdateProvisioningTemplate templateName
  = UpdateProvisioningTemplate'{templateName,
                                defaultVersionId = Core.Nothing, description = Core.Nothing,
                                enabled = Core.Nothing, preProvisioningHook = Core.Nothing,
                                provisioningRoleArn = Core.Nothing,
                                removePreProvisioningHook = Core.Nothing}

-- | The name of the fleet provisioning template.
--
-- /Note:/ Consider using 'templateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uptTemplateName :: Lens.Lens' UpdateProvisioningTemplate Types.TemplateName
uptTemplateName = Lens.field @"templateName"
{-# INLINEABLE uptTemplateName #-}
{-# DEPRECATED templateName "Use generic-lens or generic-optics with 'templateName' instead"  #-}

-- | The ID of the default provisioning template version.
--
-- /Note:/ Consider using 'defaultVersionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uptDefaultVersionId :: Lens.Lens' UpdateProvisioningTemplate (Core.Maybe Core.Int)
uptDefaultVersionId = Lens.field @"defaultVersionId"
{-# INLINEABLE uptDefaultVersionId #-}
{-# DEPRECATED defaultVersionId "Use generic-lens or generic-optics with 'defaultVersionId' instead"  #-}

-- | The description of the fleet provisioning template.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uptDescription :: Lens.Lens' UpdateProvisioningTemplate (Core.Maybe Types.TemplateDescription)
uptDescription = Lens.field @"description"
{-# INLINEABLE uptDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | True to enable the fleet provisioning template, otherwise false.
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uptEnabled :: Lens.Lens' UpdateProvisioningTemplate (Core.Maybe Core.Bool)
uptEnabled = Lens.field @"enabled"
{-# INLINEABLE uptEnabled #-}
{-# DEPRECATED enabled "Use generic-lens or generic-optics with 'enabled' instead"  #-}

-- | Updates the pre-provisioning hook template.
--
-- /Note:/ Consider using 'preProvisioningHook' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uptPreProvisioningHook :: Lens.Lens' UpdateProvisioningTemplate (Core.Maybe Types.ProvisioningHook)
uptPreProvisioningHook = Lens.field @"preProvisioningHook"
{-# INLINEABLE uptPreProvisioningHook #-}
{-# DEPRECATED preProvisioningHook "Use generic-lens or generic-optics with 'preProvisioningHook' instead"  #-}

-- | The ARN of the role associated with the provisioning template. This IoT role grants permission to provision a device.
--
-- /Note:/ Consider using 'provisioningRoleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uptProvisioningRoleArn :: Lens.Lens' UpdateProvisioningTemplate (Core.Maybe Types.ProvisioningRoleArn)
uptProvisioningRoleArn = Lens.field @"provisioningRoleArn"
{-# INLINEABLE uptProvisioningRoleArn #-}
{-# DEPRECATED provisioningRoleArn "Use generic-lens or generic-optics with 'provisioningRoleArn' instead"  #-}

-- | Removes pre-provisioning hook template.
--
-- /Note:/ Consider using 'removePreProvisioningHook' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uptRemovePreProvisioningHook :: Lens.Lens' UpdateProvisioningTemplate (Core.Maybe Core.Bool)
uptRemovePreProvisioningHook = Lens.field @"removePreProvisioningHook"
{-# INLINEABLE uptRemovePreProvisioningHook #-}
{-# DEPRECATED removePreProvisioningHook "Use generic-lens or generic-optics with 'removePreProvisioningHook' instead"  #-}

instance Core.ToQuery UpdateProvisioningTemplate where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpdateProvisioningTemplate where
        toHeaders _ = Core.pure Core.mempty

instance Core.FromJSON UpdateProvisioningTemplate where
        toJSON UpdateProvisioningTemplate{..}
          = Core.object
              (Core.catMaybes
                 [("defaultVersionId" Core..=) Core.<$> defaultVersionId,
                  ("description" Core..=) Core.<$> description,
                  ("enabled" Core..=) Core.<$> enabled,
                  ("preProvisioningHook" Core..=) Core.<$> preProvisioningHook,
                  ("provisioningRoleArn" Core..=) Core.<$> provisioningRoleArn,
                  ("removePreProvisioningHook" Core..=) Core.<$>
                    removePreProvisioningHook])

instance Core.AWSRequest UpdateProvisioningTemplate where
        type Rs UpdateProvisioningTemplate =
             UpdateProvisioningTemplateResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.PATCH,
                         Core._rqPath =
                           "/provisioning-templates/" Core.<> Core.toText templateName,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 UpdateProvisioningTemplateResponse' Core.<$>
                   (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkUpdateProvisioningTemplateResponse' smart constructor.
newtype UpdateProvisioningTemplateResponse = UpdateProvisioningTemplateResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateProvisioningTemplateResponse' value with any optional fields omitted.
mkUpdateProvisioningTemplateResponse
    :: Core.Int -- ^ 'responseStatus'
    -> UpdateProvisioningTemplateResponse
mkUpdateProvisioningTemplateResponse responseStatus
  = UpdateProvisioningTemplateResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uptrrsResponseStatus :: Lens.Lens' UpdateProvisioningTemplateResponse Core.Int
uptrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE uptrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
