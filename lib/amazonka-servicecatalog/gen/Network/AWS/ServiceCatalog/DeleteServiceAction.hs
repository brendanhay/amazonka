{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.DeleteServiceAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a self-service action.
module Network.AWS.ServiceCatalog.DeleteServiceAction
    (
    -- * Creating a request
      DeleteServiceAction (..)
    , mkDeleteServiceAction
    -- ** Request lenses
    , dsafId
    , dsafAcceptLanguage

    -- * Destructuring the response
    , DeleteServiceActionResponse (..)
    , mkDeleteServiceActionResponse
    -- ** Response lenses
    , dsarfrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.ServiceCatalog.Types as Types

-- | /See:/ 'mkDeleteServiceAction' smart constructor.
data DeleteServiceAction = DeleteServiceAction'
  { id :: Types.Id
    -- ^ The self-service action identifier. For example, @act-fs7abcd89wxyz@ .
  , acceptLanguage :: Core.Maybe Types.AcceptLanguage
    -- ^ The language code.
--
--
--     * @en@ - English (default)
--
--
--     * @jp@ - Japanese
--
--
--     * @zh@ - Chinese
--
--
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteServiceAction' value with any optional fields omitted.
mkDeleteServiceAction
    :: Types.Id -- ^ 'id'
    -> DeleteServiceAction
mkDeleteServiceAction id
  = DeleteServiceAction'{id, acceptLanguage = Core.Nothing}

-- | The self-service action identifier. For example, @act-fs7abcd89wxyz@ .
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsafId :: Lens.Lens' DeleteServiceAction Types.Id
dsafId = Lens.field @"id"
{-# INLINEABLE dsafId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

-- | The language code.
--
--
--     * @en@ - English (default)
--
--
--     * @jp@ - Japanese
--
--
--     * @zh@ - Chinese
--
--
--
-- /Note:/ Consider using 'acceptLanguage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsafAcceptLanguage :: Lens.Lens' DeleteServiceAction (Core.Maybe Types.AcceptLanguage)
dsafAcceptLanguage = Lens.field @"acceptLanguage"
{-# INLINEABLE dsafAcceptLanguage #-}
{-# DEPRECATED acceptLanguage "Use generic-lens or generic-optics with 'acceptLanguage' instead"  #-}

instance Core.ToQuery DeleteServiceAction where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteServiceAction where
        toHeaders DeleteServiceAction{..}
          = Core.pure
              ("X-Amz-Target", "AWS242ServiceCatalogService.DeleteServiceAction")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DeleteServiceAction where
        toJSON DeleteServiceAction{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("Id" Core..= id),
                  ("AcceptLanguage" Core..=) Core.<$> acceptLanguage])

instance Core.AWSRequest DeleteServiceAction where
        type Rs DeleteServiceAction = DeleteServiceActionResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 DeleteServiceActionResponse' Core.<$>
                   (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteServiceActionResponse' smart constructor.
newtype DeleteServiceActionResponse = DeleteServiceActionResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteServiceActionResponse' value with any optional fields omitted.
mkDeleteServiceActionResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteServiceActionResponse
mkDeleteServiceActionResponse responseStatus
  = DeleteServiceActionResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsarfrsResponseStatus :: Lens.Lens' DeleteServiceActionResponse Core.Int
dsarfrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dsarfrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
