{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.UpdateTagOption
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the specified TagOption.
module Network.AWS.ServiceCatalog.UpdateTagOption
    (
    -- * Creating a request
      UpdateTagOption (..)
    , mkUpdateTagOption
    -- ** Request lenses
    , utoId
    , utoActive
    , utoValue

    -- * Destructuring the response
    , UpdateTagOptionResponse (..)
    , mkUpdateTagOptionResponse
    -- ** Response lenses
    , utorrsTagOptionDetail
    , utorrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.ServiceCatalog.Types as Types

-- | /See:/ 'mkUpdateTagOption' smart constructor.
data UpdateTagOption = UpdateTagOption'
  { id :: Types.TagOptionId
    -- ^ The TagOption identifier.
  , active :: Core.Maybe Core.Bool
    -- ^ The updated active state.
  , value :: Core.Maybe Types.TagOptionValue
    -- ^ The updated value.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateTagOption' value with any optional fields omitted.
mkUpdateTagOption
    :: Types.TagOptionId -- ^ 'id'
    -> UpdateTagOption
mkUpdateTagOption id
  = UpdateTagOption'{id, active = Core.Nothing, value = Core.Nothing}

-- | The TagOption identifier.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utoId :: Lens.Lens' UpdateTagOption Types.TagOptionId
utoId = Lens.field @"id"
{-# INLINEABLE utoId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

-- | The updated active state.
--
-- /Note:/ Consider using 'active' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utoActive :: Lens.Lens' UpdateTagOption (Core.Maybe Core.Bool)
utoActive = Lens.field @"active"
{-# INLINEABLE utoActive #-}
{-# DEPRECATED active "Use generic-lens or generic-optics with 'active' instead"  #-}

-- | The updated value.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utoValue :: Lens.Lens' UpdateTagOption (Core.Maybe Types.TagOptionValue)
utoValue = Lens.field @"value"
{-# INLINEABLE utoValue #-}
{-# DEPRECATED value "Use generic-lens or generic-optics with 'value' instead"  #-}

instance Core.ToQuery UpdateTagOption where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpdateTagOption where
        toHeaders UpdateTagOption{..}
          = Core.pure
              ("X-Amz-Target", "AWS242ServiceCatalogService.UpdateTagOption")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON UpdateTagOption where
        toJSON UpdateTagOption{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("Id" Core..= id), ("Active" Core..=) Core.<$> active,
                  ("Value" Core..=) Core.<$> value])

instance Core.AWSRequest UpdateTagOption where
        type Rs UpdateTagOption = UpdateTagOptionResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 UpdateTagOptionResponse' Core.<$>
                   (x Core..:? "TagOptionDetail") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkUpdateTagOptionResponse' smart constructor.
data UpdateTagOptionResponse = UpdateTagOptionResponse'
  { tagOptionDetail :: Core.Maybe Types.TagOptionDetail
    -- ^ Information about the TagOption.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateTagOptionResponse' value with any optional fields omitted.
mkUpdateTagOptionResponse
    :: Core.Int -- ^ 'responseStatus'
    -> UpdateTagOptionResponse
mkUpdateTagOptionResponse responseStatus
  = UpdateTagOptionResponse'{tagOptionDetail = Core.Nothing,
                             responseStatus}

-- | Information about the TagOption.
--
-- /Note:/ Consider using 'tagOptionDetail' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utorrsTagOptionDetail :: Lens.Lens' UpdateTagOptionResponse (Core.Maybe Types.TagOptionDetail)
utorrsTagOptionDetail = Lens.field @"tagOptionDetail"
{-# INLINEABLE utorrsTagOptionDetail #-}
{-# DEPRECATED tagOptionDetail "Use generic-lens or generic-optics with 'tagOptionDetail' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utorrsResponseStatus :: Lens.Lens' UpdateTagOptionResponse Core.Int
utorrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE utorrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
