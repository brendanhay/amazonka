{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.DescribeCopyProductStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the status of the specified copy product operation.
module Network.AWS.ServiceCatalog.DescribeCopyProductStatus
    (
    -- * Creating a request
      DescribeCopyProductStatus (..)
    , mkDescribeCopyProductStatus
    -- ** Request lenses
    , dcpsCopyProductToken
    , dcpsAcceptLanguage

    -- * Destructuring the response
    , DescribeCopyProductStatusResponse (..)
    , mkDescribeCopyProductStatusResponse
    -- ** Response lenses
    , dcpsrrsCopyProductStatus
    , dcpsrrsStatusDetail
    , dcpsrrsTargetProductId
    , dcpsrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.ServiceCatalog.Types as Types

-- | /See:/ 'mkDescribeCopyProductStatus' smart constructor.
data DescribeCopyProductStatus = DescribeCopyProductStatus'
  { copyProductToken :: Types.CopyProductToken
    -- ^ The token for the copy product operation. This token is returned by 'CopyProduct' .
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

-- | Creates a 'DescribeCopyProductStatus' value with any optional fields omitted.
mkDescribeCopyProductStatus
    :: Types.CopyProductToken -- ^ 'copyProductToken'
    -> DescribeCopyProductStatus
mkDescribeCopyProductStatus copyProductToken
  = DescribeCopyProductStatus'{copyProductToken,
                               acceptLanguage = Core.Nothing}

-- | The token for the copy product operation. This token is returned by 'CopyProduct' .
--
-- /Note:/ Consider using 'copyProductToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcpsCopyProductToken :: Lens.Lens' DescribeCopyProductStatus Types.CopyProductToken
dcpsCopyProductToken = Lens.field @"copyProductToken"
{-# INLINEABLE dcpsCopyProductToken #-}
{-# DEPRECATED copyProductToken "Use generic-lens or generic-optics with 'copyProductToken' instead"  #-}

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
dcpsAcceptLanguage :: Lens.Lens' DescribeCopyProductStatus (Core.Maybe Types.AcceptLanguage)
dcpsAcceptLanguage = Lens.field @"acceptLanguage"
{-# INLINEABLE dcpsAcceptLanguage #-}
{-# DEPRECATED acceptLanguage "Use generic-lens or generic-optics with 'acceptLanguage' instead"  #-}

instance Core.ToQuery DescribeCopyProductStatus where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeCopyProductStatus where
        toHeaders DescribeCopyProductStatus{..}
          = Core.pure
              ("X-Amz-Target",
               "AWS242ServiceCatalogService.DescribeCopyProductStatus")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DescribeCopyProductStatus where
        toJSON DescribeCopyProductStatus{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("CopyProductToken" Core..= copyProductToken),
                  ("AcceptLanguage" Core..=) Core.<$> acceptLanguage])

instance Core.AWSRequest DescribeCopyProductStatus where
        type Rs DescribeCopyProductStatus =
             DescribeCopyProductStatusResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeCopyProductStatusResponse' Core.<$>
                   (x Core..:? "CopyProductStatus") Core.<*> x Core..:? "StatusDetail"
                     Core.<*> x Core..:? "TargetProductId"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDescribeCopyProductStatusResponse' smart constructor.
data DescribeCopyProductStatusResponse = DescribeCopyProductStatusResponse'
  { copyProductStatus :: Core.Maybe Types.CopyProductStatus
    -- ^ The status of the copy product operation.
  , statusDetail :: Core.Maybe Types.StatusDetail
    -- ^ The status message.
  , targetProductId :: Core.Maybe Types.Id
    -- ^ The identifier of the copied product.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeCopyProductStatusResponse' value with any optional fields omitted.
mkDescribeCopyProductStatusResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeCopyProductStatusResponse
mkDescribeCopyProductStatusResponse responseStatus
  = DescribeCopyProductStatusResponse'{copyProductStatus =
                                         Core.Nothing,
                                       statusDetail = Core.Nothing, targetProductId = Core.Nothing,
                                       responseStatus}

-- | The status of the copy product operation.
--
-- /Note:/ Consider using 'copyProductStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcpsrrsCopyProductStatus :: Lens.Lens' DescribeCopyProductStatusResponse (Core.Maybe Types.CopyProductStatus)
dcpsrrsCopyProductStatus = Lens.field @"copyProductStatus"
{-# INLINEABLE dcpsrrsCopyProductStatus #-}
{-# DEPRECATED copyProductStatus "Use generic-lens or generic-optics with 'copyProductStatus' instead"  #-}

-- | The status message.
--
-- /Note:/ Consider using 'statusDetail' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcpsrrsStatusDetail :: Lens.Lens' DescribeCopyProductStatusResponse (Core.Maybe Types.StatusDetail)
dcpsrrsStatusDetail = Lens.field @"statusDetail"
{-# INLINEABLE dcpsrrsStatusDetail #-}
{-# DEPRECATED statusDetail "Use generic-lens or generic-optics with 'statusDetail' instead"  #-}

-- | The identifier of the copied product.
--
-- /Note:/ Consider using 'targetProductId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcpsrrsTargetProductId :: Lens.Lens' DescribeCopyProductStatusResponse (Core.Maybe Types.Id)
dcpsrrsTargetProductId = Lens.field @"targetProductId"
{-# INLINEABLE dcpsrrsTargetProductId #-}
{-# DEPRECATED targetProductId "Use generic-lens or generic-optics with 'targetProductId' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcpsrrsResponseStatus :: Lens.Lens' DescribeCopyProductStatusResponse Core.Int
dcpsrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dcpsrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
