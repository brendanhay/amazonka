{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.CreateTagOption
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a TagOption.
module Network.AWS.ServiceCatalog.CreateTagOption
    (
    -- * Creating a request
      CreateTagOption (..)
    , mkCreateTagOption
    -- ** Request lenses
    , ctoKey
    , ctoValue

    -- * Destructuring the response
    , CreateTagOptionResponse (..)
    , mkCreateTagOptionResponse
    -- ** Response lenses
    , ctorrsTagOptionDetail
    , ctorrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.ServiceCatalog.Types as Types

-- | /See:/ 'mkCreateTagOption' smart constructor.
data CreateTagOption = CreateTagOption'
  { key :: Types.Key
    -- ^ The TagOption key.
  , value :: Types.TagOptionValue
    -- ^ The TagOption value.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateTagOption' value with any optional fields omitted.
mkCreateTagOption
    :: Types.Key -- ^ 'key'
    -> Types.TagOptionValue -- ^ 'value'
    -> CreateTagOption
mkCreateTagOption key value = CreateTagOption'{key, value}

-- | The TagOption key.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctoKey :: Lens.Lens' CreateTagOption Types.Key
ctoKey = Lens.field @"key"
{-# INLINEABLE ctoKey #-}
{-# DEPRECATED key "Use generic-lens or generic-optics with 'key' instead"  #-}

-- | The TagOption value.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctoValue :: Lens.Lens' CreateTagOption Types.TagOptionValue
ctoValue = Lens.field @"value"
{-# INLINEABLE ctoValue #-}
{-# DEPRECATED value "Use generic-lens or generic-optics with 'value' instead"  #-}

instance Core.ToQuery CreateTagOption where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateTagOption where
        toHeaders CreateTagOption{..}
          = Core.pure
              ("X-Amz-Target", "AWS242ServiceCatalogService.CreateTagOption")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CreateTagOption where
        toJSON CreateTagOption{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("Key" Core..= key), Core.Just ("Value" Core..= value)])

instance Core.AWSRequest CreateTagOption where
        type Rs CreateTagOption = CreateTagOptionResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CreateTagOptionResponse' Core.<$>
                   (x Core..:? "TagOptionDetail") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateTagOptionResponse' smart constructor.
data CreateTagOptionResponse = CreateTagOptionResponse'
  { tagOptionDetail :: Core.Maybe Types.TagOptionDetail
    -- ^ Information about the TagOption.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateTagOptionResponse' value with any optional fields omitted.
mkCreateTagOptionResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateTagOptionResponse
mkCreateTagOptionResponse responseStatus
  = CreateTagOptionResponse'{tagOptionDetail = Core.Nothing,
                             responseStatus}

-- | Information about the TagOption.
--
-- /Note:/ Consider using 'tagOptionDetail' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctorrsTagOptionDetail :: Lens.Lens' CreateTagOptionResponse (Core.Maybe Types.TagOptionDetail)
ctorrsTagOptionDetail = Lens.field @"tagOptionDetail"
{-# INLINEABLE ctorrsTagOptionDetail #-}
{-# DEPRECATED tagOptionDetail "Use generic-lens or generic-optics with 'tagOptionDetail' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctorrsResponseStatus :: Lens.Lens' CreateTagOptionResponse Core.Int
ctorrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ctorrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
