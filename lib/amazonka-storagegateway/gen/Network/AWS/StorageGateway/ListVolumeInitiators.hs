{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.ListVolumeInitiators
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists iSCSI initiators that are connected to a volume. You can use this operation to determine whether a volume is being used or not. This operation is only supported in the cached volume and stored volume gateway types.
module Network.AWS.StorageGateway.ListVolumeInitiators
    (
    -- * Creating a request
      ListVolumeInitiators (..)
    , mkListVolumeInitiators
    -- ** Request lenses
    , lviVolumeARN

    -- * Destructuring the response
    , ListVolumeInitiatorsResponse (..)
    , mkListVolumeInitiatorsResponse
    -- ** Response lenses
    , lvirrsInitiators
    , lvirrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.StorageGateway.Types as Types

-- | ListVolumeInitiatorsInput
--
-- /See:/ 'mkListVolumeInitiators' smart constructor.
newtype ListVolumeInitiators = ListVolumeInitiators'
  { volumeARN :: Types.VolumeARN
    -- ^ The Amazon Resource Name (ARN) of the volume. Use the 'ListVolumes' operation to return a list of gateway volumes for the gateway.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'ListVolumeInitiators' value with any optional fields omitted.
mkListVolumeInitiators
    :: Types.VolumeARN -- ^ 'volumeARN'
    -> ListVolumeInitiators
mkListVolumeInitiators volumeARN = ListVolumeInitiators'{volumeARN}

-- | The Amazon Resource Name (ARN) of the volume. Use the 'ListVolumes' operation to return a list of gateway volumes for the gateway.
--
-- /Note:/ Consider using 'volumeARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lviVolumeARN :: Lens.Lens' ListVolumeInitiators Types.VolumeARN
lviVolumeARN = Lens.field @"volumeARN"
{-# INLINEABLE lviVolumeARN #-}
{-# DEPRECATED volumeARN "Use generic-lens or generic-optics with 'volumeARN' instead"  #-}

instance Core.ToQuery ListVolumeInitiators where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ListVolumeInitiators where
        toHeaders ListVolumeInitiators{..}
          = Core.pure
              ("X-Amz-Target", "StorageGateway_20130630.ListVolumeInitiators")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ListVolumeInitiators where
        toJSON ListVolumeInitiators{..}
          = Core.object
              (Core.catMaybes [Core.Just ("VolumeARN" Core..= volumeARN)])

instance Core.AWSRequest ListVolumeInitiators where
        type Rs ListVolumeInitiators = ListVolumeInitiatorsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListVolumeInitiatorsResponse' Core.<$>
                   (x Core..:? "Initiators") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | ListVolumeInitiatorsOutput
--
-- /See:/ 'mkListVolumeInitiatorsResponse' smart constructor.
data ListVolumeInitiatorsResponse = ListVolumeInitiatorsResponse'
  { initiators :: Core.Maybe [Types.Initiator]
    -- ^ The host names and port numbers of all iSCSI initiators that are connected to the gateway.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListVolumeInitiatorsResponse' value with any optional fields omitted.
mkListVolumeInitiatorsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListVolumeInitiatorsResponse
mkListVolumeInitiatorsResponse responseStatus
  = ListVolumeInitiatorsResponse'{initiators = Core.Nothing,
                                  responseStatus}

-- | The host names and port numbers of all iSCSI initiators that are connected to the gateway.
--
-- /Note:/ Consider using 'initiators' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lvirrsInitiators :: Lens.Lens' ListVolumeInitiatorsResponse (Core.Maybe [Types.Initiator])
lvirrsInitiators = Lens.field @"initiators"
{-# INLINEABLE lvirrsInitiators #-}
{-# DEPRECATED initiators "Use generic-lens or generic-optics with 'initiators' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lvirrsResponseStatus :: Lens.Lens' ListVolumeInitiatorsResponse Core.Int
lvirrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE lvirrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
