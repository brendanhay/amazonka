{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.DescribeTagOption
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about the specified TagOption.
module Network.AWS.ServiceCatalog.DescribeTagOption
    (
    -- * Creating a request
      DescribeTagOption (..)
    , mkDescribeTagOption
    -- ** Request lenses
    , dtoId

    -- * Destructuring the response
    , DescribeTagOptionResponse (..)
    , mkDescribeTagOptionResponse
    -- ** Response lenses
    , dtorrsTagOptionDetail
    , dtorrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.ServiceCatalog.Types as Types

-- | /See:/ 'mkDescribeTagOption' smart constructor.
newtype DescribeTagOption = DescribeTagOption'
  { id :: Types.TagOptionId
    -- ^ The TagOption identifier.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeTagOption' value with any optional fields omitted.
mkDescribeTagOption
    :: Types.TagOptionId -- ^ 'id'
    -> DescribeTagOption
mkDescribeTagOption id = DescribeTagOption'{id}

-- | The TagOption identifier.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtoId :: Lens.Lens' DescribeTagOption Types.TagOptionId
dtoId = Lens.field @"id"
{-# INLINEABLE dtoId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

instance Core.ToQuery DescribeTagOption where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeTagOption where
        toHeaders DescribeTagOption{..}
          = Core.pure
              ("X-Amz-Target", "AWS242ServiceCatalogService.DescribeTagOption")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DescribeTagOption where
        toJSON DescribeTagOption{..}
          = Core.object (Core.catMaybes [Core.Just ("Id" Core..= id)])

instance Core.AWSRequest DescribeTagOption where
        type Rs DescribeTagOption = DescribeTagOptionResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeTagOptionResponse' Core.<$>
                   (x Core..:? "TagOptionDetail") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDescribeTagOptionResponse' smart constructor.
data DescribeTagOptionResponse = DescribeTagOptionResponse'
  { tagOptionDetail :: Core.Maybe Types.TagOptionDetail
    -- ^ Information about the TagOption.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeTagOptionResponse' value with any optional fields omitted.
mkDescribeTagOptionResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeTagOptionResponse
mkDescribeTagOptionResponse responseStatus
  = DescribeTagOptionResponse'{tagOptionDetail = Core.Nothing,
                               responseStatus}

-- | Information about the TagOption.
--
-- /Note:/ Consider using 'tagOptionDetail' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtorrsTagOptionDetail :: Lens.Lens' DescribeTagOptionResponse (Core.Maybe Types.TagOptionDetail)
dtorrsTagOptionDetail = Lens.field @"tagOptionDetail"
{-# INLINEABLE dtorrsTagOptionDetail #-}
{-# DEPRECATED tagOptionDetail "Use generic-lens or generic-optics with 'tagOptionDetail' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtorrsResponseStatus :: Lens.Lens' DescribeTagOptionResponse Core.Int
dtorrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dtorrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
