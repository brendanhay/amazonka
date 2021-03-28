{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.DeleteJobTemplate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Permanently delete a job template you have created.
module Network.AWS.MediaConvert.DeleteJobTemplate
    (
    -- * Creating a request
      DeleteJobTemplate (..)
    , mkDeleteJobTemplate
    -- ** Request lenses
    , djtName

    -- * Destructuring the response
    , DeleteJobTemplateResponse (..)
    , mkDeleteJobTemplateResponse
    -- ** Response lenses
    , djtrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaConvert.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteJobTemplate' smart constructor.
newtype DeleteJobTemplate = DeleteJobTemplate'
  { name :: Core.Text
    -- ^ The name of the job template to be deleted.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteJobTemplate' value with any optional fields omitted.
mkDeleteJobTemplate
    :: Core.Text -- ^ 'name'
    -> DeleteJobTemplate
mkDeleteJobTemplate name = DeleteJobTemplate'{name}

-- | The name of the job template to be deleted.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
djtName :: Lens.Lens' DeleteJobTemplate Core.Text
djtName = Lens.field @"name"
{-# INLINEABLE djtName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

instance Core.ToQuery DeleteJobTemplate where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteJobTemplate where
        toHeaders DeleteJobTemplate{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.AWSRequest DeleteJobTemplate where
        type Rs DeleteJobTemplate = DeleteJobTemplateResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.DELETE,
                         Core._rqPath =
                           "/2017-08-29/jobTemplates/" Core.<> Core.toText name,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 DeleteJobTemplateResponse' Core.<$> (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteJobTemplateResponse' smart constructor.
newtype DeleteJobTemplateResponse = DeleteJobTemplateResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteJobTemplateResponse' value with any optional fields omitted.
mkDeleteJobTemplateResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteJobTemplateResponse
mkDeleteJobTemplateResponse responseStatus
  = DeleteJobTemplateResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
djtrrsResponseStatus :: Lens.Lens' DeleteJobTemplateResponse Core.Int
djtrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE djtrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
