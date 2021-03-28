{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DeleteFpgaImage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified Amazon FPGA Image (AFI).
module Network.AWS.EC2.DeleteFpgaImage
    (
    -- * Creating a request
      DeleteFpgaImage (..)
    , mkDeleteFpgaImage
    -- ** Request lenses
    , dfiFpgaImageId
    , dfiDryRun

    -- * Destructuring the response
    , DeleteFpgaImageResponse (..)
    , mkDeleteFpgaImageResponse
    -- ** Response lenses
    , dfirrsReturn
    , dfirrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteFpgaImage' smart constructor.
data DeleteFpgaImage = DeleteFpgaImage'
  { fpgaImageId :: Types.FpgaImageId
    -- ^ The ID of the AFI.
  , dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteFpgaImage' value with any optional fields omitted.
mkDeleteFpgaImage
    :: Types.FpgaImageId -- ^ 'fpgaImageId'
    -> DeleteFpgaImage
mkDeleteFpgaImage fpgaImageId
  = DeleteFpgaImage'{fpgaImageId, dryRun = Core.Nothing}

-- | The ID of the AFI.
--
-- /Note:/ Consider using 'fpgaImageId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfiFpgaImageId :: Lens.Lens' DeleteFpgaImage Types.FpgaImageId
dfiFpgaImageId = Lens.field @"fpgaImageId"
{-# INLINEABLE dfiFpgaImageId #-}
{-# DEPRECATED fpgaImageId "Use generic-lens or generic-optics with 'fpgaImageId' instead"  #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfiDryRun :: Lens.Lens' DeleteFpgaImage (Core.Maybe Core.Bool)
dfiDryRun = Lens.field @"dryRun"
{-# INLINEABLE dfiDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

instance Core.ToQuery DeleteFpgaImage where
        toQuery DeleteFpgaImage{..}
          = Core.toQueryPair "Action" ("DeleteFpgaImage" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<> Core.toQueryPair "FpgaImageId" fpgaImageId
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun

instance Core.ToHeaders DeleteFpgaImage where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DeleteFpgaImage where
        type Rs DeleteFpgaImage = DeleteFpgaImageResponse
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
                 DeleteFpgaImageResponse' Core.<$>
                   (x Core..@? "return") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteFpgaImageResponse' smart constructor.
data DeleteFpgaImageResponse = DeleteFpgaImageResponse'
  { return :: Core.Maybe Core.Bool
    -- ^ Is @true@ if the request succeeds, and an error otherwise.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteFpgaImageResponse' value with any optional fields omitted.
mkDeleteFpgaImageResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteFpgaImageResponse
mkDeleteFpgaImageResponse responseStatus
  = DeleteFpgaImageResponse'{return = Core.Nothing, responseStatus}

-- | Is @true@ if the request succeeds, and an error otherwise.
--
-- /Note:/ Consider using 'return' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfirrsReturn :: Lens.Lens' DeleteFpgaImageResponse (Core.Maybe Core.Bool)
dfirrsReturn = Lens.field @"return"
{-# INLINEABLE dfirrsReturn #-}
{-# DEPRECATED return "Use generic-lens or generic-optics with 'return' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfirrsResponseStatus :: Lens.Lens' DeleteFpgaImageResponse Core.Int
dfirrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dfirrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
