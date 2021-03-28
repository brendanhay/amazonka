{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.DescribeServiceAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes a self-service action.
module Network.AWS.ServiceCatalog.DescribeServiceAction
    (
    -- * Creating a request
      DescribeServiceAction (..)
    , mkDescribeServiceAction
    -- ** Request lenses
    , dsaId
    , dsaAcceptLanguage

    -- * Destructuring the response
    , DescribeServiceActionResponse (..)
    , mkDescribeServiceActionResponse
    -- ** Response lenses
    , dsarrsServiceActionDetail
    , dsarrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.ServiceCatalog.Types as Types

-- | /See:/ 'mkDescribeServiceAction' smart constructor.
data DescribeServiceAction = DescribeServiceAction'
  { id :: Types.Id
    -- ^ The self-service action identifier.
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

-- | Creates a 'DescribeServiceAction' value with any optional fields omitted.
mkDescribeServiceAction
    :: Types.Id -- ^ 'id'
    -> DescribeServiceAction
mkDescribeServiceAction id
  = DescribeServiceAction'{id, acceptLanguage = Core.Nothing}

-- | The self-service action identifier.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsaId :: Lens.Lens' DescribeServiceAction Types.Id
dsaId = Lens.field @"id"
{-# INLINEABLE dsaId #-}
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
dsaAcceptLanguage :: Lens.Lens' DescribeServiceAction (Core.Maybe Types.AcceptLanguage)
dsaAcceptLanguage = Lens.field @"acceptLanguage"
{-# INLINEABLE dsaAcceptLanguage #-}
{-# DEPRECATED acceptLanguage "Use generic-lens or generic-optics with 'acceptLanguage' instead"  #-}

instance Core.ToQuery DescribeServiceAction where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeServiceAction where
        toHeaders DescribeServiceAction{..}
          = Core.pure
              ("X-Amz-Target",
               "AWS242ServiceCatalogService.DescribeServiceAction")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DescribeServiceAction where
        toJSON DescribeServiceAction{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("Id" Core..= id),
                  ("AcceptLanguage" Core..=) Core.<$> acceptLanguage])

instance Core.AWSRequest DescribeServiceAction where
        type Rs DescribeServiceAction = DescribeServiceActionResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeServiceActionResponse' Core.<$>
                   (x Core..:? "ServiceActionDetail") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDescribeServiceActionResponse' smart constructor.
data DescribeServiceActionResponse = DescribeServiceActionResponse'
  { serviceActionDetail :: Core.Maybe Types.ServiceActionDetail
    -- ^ Detailed information about the self-service action.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeServiceActionResponse' value with any optional fields omitted.
mkDescribeServiceActionResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeServiceActionResponse
mkDescribeServiceActionResponse responseStatus
  = DescribeServiceActionResponse'{serviceActionDetail =
                                     Core.Nothing,
                                   responseStatus}

-- | Detailed information about the self-service action.
--
-- /Note:/ Consider using 'serviceActionDetail' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsarrsServiceActionDetail :: Lens.Lens' DescribeServiceActionResponse (Core.Maybe Types.ServiceActionDetail)
dsarrsServiceActionDetail = Lens.field @"serviceActionDetail"
{-# INLINEABLE dsarrsServiceActionDetail #-}
{-# DEPRECATED serviceActionDetail "Use generic-lens or generic-optics with 'serviceActionDetail' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsarrsResponseStatus :: Lens.Lens' DescribeServiceActionResponse Core.Int
dsarrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dsarrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
