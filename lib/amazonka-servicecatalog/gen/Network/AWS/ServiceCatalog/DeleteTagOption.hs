{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.DeleteTagOption
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified TagOption.
--
-- You cannot delete a TagOption if it is associated with a product or portfolio.
module Network.AWS.ServiceCatalog.DeleteTagOption
    (
    -- * Creating a request
      DeleteTagOption (..)
    , mkDeleteTagOption
    -- ** Request lenses
    , dtofId

    -- * Destructuring the response
    , DeleteTagOptionResponse (..)
    , mkDeleteTagOptionResponse
    -- ** Response lenses
    , dtorfrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.ServiceCatalog.Types as Types

-- | /See:/ 'mkDeleteTagOption' smart constructor.
newtype DeleteTagOption = DeleteTagOption'
  { id :: Types.TagOptionId
    -- ^ The TagOption identifier.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteTagOption' value with any optional fields omitted.
mkDeleteTagOption
    :: Types.TagOptionId -- ^ 'id'
    -> DeleteTagOption
mkDeleteTagOption id = DeleteTagOption'{id}

-- | The TagOption identifier.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtofId :: Lens.Lens' DeleteTagOption Types.TagOptionId
dtofId = Lens.field @"id"
{-# INLINEABLE dtofId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

instance Core.ToQuery DeleteTagOption where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteTagOption where
        toHeaders DeleteTagOption{..}
          = Core.pure
              ("X-Amz-Target", "AWS242ServiceCatalogService.DeleteTagOption")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DeleteTagOption where
        toJSON DeleteTagOption{..}
          = Core.object (Core.catMaybes [Core.Just ("Id" Core..= id)])

instance Core.AWSRequest DeleteTagOption where
        type Rs DeleteTagOption = DeleteTagOptionResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 DeleteTagOptionResponse' Core.<$> (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteTagOptionResponse' smart constructor.
newtype DeleteTagOptionResponse = DeleteTagOptionResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteTagOptionResponse' value with any optional fields omitted.
mkDeleteTagOptionResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteTagOptionResponse
mkDeleteTagOptionResponse responseStatus
  = DeleteTagOptionResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtorfrsResponseStatus :: Lens.Lens' DeleteTagOptionResponse Core.Int
dtorfrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dtorfrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
