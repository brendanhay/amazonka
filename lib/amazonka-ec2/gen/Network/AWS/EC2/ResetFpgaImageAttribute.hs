{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.ResetFpgaImageAttribute
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Resets the specified attribute of the specified Amazon FPGA Image (AFI) to its default value. You can only reset the load permission attribute.
module Network.AWS.EC2.ResetFpgaImageAttribute
    (
    -- * Creating a request
      ResetFpgaImageAttribute (..)
    , mkResetFpgaImageAttribute
    -- ** Request lenses
    , rfiaFpgaImageId
    , rfiaAttribute
    , rfiaDryRun

    -- * Destructuring the response
    , ResetFpgaImageAttributeResponse (..)
    , mkResetFpgaImageAttributeResponse
    -- ** Response lenses
    , rfiarrsReturn
    , rfiarrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkResetFpgaImageAttribute' smart constructor.
data ResetFpgaImageAttribute = ResetFpgaImageAttribute'
  { fpgaImageId :: Types.FpgaImageId
    -- ^ The ID of the AFI.
  , attribute :: Core.Maybe Types.ResetFpgaImageAttributeName
    -- ^ The attribute.
  , dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ResetFpgaImageAttribute' value with any optional fields omitted.
mkResetFpgaImageAttribute
    :: Types.FpgaImageId -- ^ 'fpgaImageId'
    -> ResetFpgaImageAttribute
mkResetFpgaImageAttribute fpgaImageId
  = ResetFpgaImageAttribute'{fpgaImageId, attribute = Core.Nothing,
                             dryRun = Core.Nothing}

-- | The ID of the AFI.
--
-- /Note:/ Consider using 'fpgaImageId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rfiaFpgaImageId :: Lens.Lens' ResetFpgaImageAttribute Types.FpgaImageId
rfiaFpgaImageId = Lens.field @"fpgaImageId"
{-# INLINEABLE rfiaFpgaImageId #-}
{-# DEPRECATED fpgaImageId "Use generic-lens or generic-optics with 'fpgaImageId' instead"  #-}

-- | The attribute.
--
-- /Note:/ Consider using 'attribute' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rfiaAttribute :: Lens.Lens' ResetFpgaImageAttribute (Core.Maybe Types.ResetFpgaImageAttributeName)
rfiaAttribute = Lens.field @"attribute"
{-# INLINEABLE rfiaAttribute #-}
{-# DEPRECATED attribute "Use generic-lens or generic-optics with 'attribute' instead"  #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rfiaDryRun :: Lens.Lens' ResetFpgaImageAttribute (Core.Maybe Core.Bool)
rfiaDryRun = Lens.field @"dryRun"
{-# INLINEABLE rfiaDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

instance Core.ToQuery ResetFpgaImageAttribute where
        toQuery ResetFpgaImageAttribute{..}
          = Core.toQueryPair "Action"
              ("ResetFpgaImageAttribute" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<> Core.toQueryPair "FpgaImageId" fpgaImageId
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "Attribute") attribute
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun

instance Core.ToHeaders ResetFpgaImageAttribute where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest ResetFpgaImageAttribute where
        type Rs ResetFpgaImageAttribute = ResetFpgaImageAttributeResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.mempty,
                         Core._rqHeaders =
                           Core.pure
                             ("Content-Type",
                              "application/x-www-form-urlencoded; charset=utf-8")
                             Core.<> Core.toHeaders x,
                         Core._rqBody = Core.toFormBody (Core.toQuery x)}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXML
              (\ s h x ->
                 ResetFpgaImageAttributeResponse' Core.<$>
                   (x Core..@? "return") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkResetFpgaImageAttributeResponse' smart constructor.
data ResetFpgaImageAttributeResponse = ResetFpgaImageAttributeResponse'
  { return :: Core.Maybe Core.Bool
    -- ^ Is @true@ if the request succeeds, and an error otherwise.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ResetFpgaImageAttributeResponse' value with any optional fields omitted.
mkResetFpgaImageAttributeResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ResetFpgaImageAttributeResponse
mkResetFpgaImageAttributeResponse responseStatus
  = ResetFpgaImageAttributeResponse'{return = Core.Nothing,
                                     responseStatus}

-- | Is @true@ if the request succeeds, and an error otherwise.
--
-- /Note:/ Consider using 'return' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rfiarrsReturn :: Lens.Lens' ResetFpgaImageAttributeResponse (Core.Maybe Core.Bool)
rfiarrsReturn = Lens.field @"return"
{-# INLINEABLE rfiarrsReturn #-}
{-# DEPRECATED return "Use generic-lens or generic-optics with 'return' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rfiarrsResponseStatus :: Lens.Lens' ResetFpgaImageAttributeResponse Core.Int
rfiarrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE rfiarrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
