{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.DeleteProvisioningTemplate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a fleet provisioning template.
module Network.AWS.IoT.DeleteProvisioningTemplate
    (
    -- * Creating a request
      DeleteProvisioningTemplate (..)
    , mkDeleteProvisioningTemplate
    -- ** Request lenses
    , dTemplateName

    -- * Destructuring the response
    , DeleteProvisioningTemplateResponse (..)
    , mkDeleteProvisioningTemplateResponse
    -- ** Response lenses
    , dptrfrsResponseStatus
    ) where

import qualified Network.AWS.IoT.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteProvisioningTemplate' smart constructor.
newtype DeleteProvisioningTemplate = DeleteProvisioningTemplate'
  { templateName :: Types.TemplateName
    -- ^ The name of the fleet provision template to delete.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteProvisioningTemplate' value with any optional fields omitted.
mkDeleteProvisioningTemplate
    :: Types.TemplateName -- ^ 'templateName'
    -> DeleteProvisioningTemplate
mkDeleteProvisioningTemplate templateName
  = DeleteProvisioningTemplate'{templateName}

-- | The name of the fleet provision template to delete.
--
-- /Note:/ Consider using 'templateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dTemplateName :: Lens.Lens' DeleteProvisioningTemplate Types.TemplateName
dTemplateName = Lens.field @"templateName"
{-# INLINEABLE dTemplateName #-}
{-# DEPRECATED templateName "Use generic-lens or generic-optics with 'templateName' instead"  #-}

instance Core.ToQuery DeleteProvisioningTemplate where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteProvisioningTemplate where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DeleteProvisioningTemplate where
        type Rs DeleteProvisioningTemplate =
             DeleteProvisioningTemplateResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.DELETE,
                         Core._rqPath =
                           "/provisioning-templates/" Core.<> Core.toText templateName,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 DeleteProvisioningTemplateResponse' Core.<$>
                   (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteProvisioningTemplateResponse' smart constructor.
newtype DeleteProvisioningTemplateResponse = DeleteProvisioningTemplateResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteProvisioningTemplateResponse' value with any optional fields omitted.
mkDeleteProvisioningTemplateResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteProvisioningTemplateResponse
mkDeleteProvisioningTemplateResponse responseStatus
  = DeleteProvisioningTemplateResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dptrfrsResponseStatus :: Lens.Lens' DeleteProvisioningTemplateResponse Core.Int
dptrfrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dptrfrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
