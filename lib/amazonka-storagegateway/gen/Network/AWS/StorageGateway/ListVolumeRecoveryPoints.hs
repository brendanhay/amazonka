{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.ListVolumeRecoveryPoints
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the recovery points for a specified gateway. This operation is only supported in the cached volume gateway type.
--
-- Each cache volume has one recovery point. A volume recovery point is a point in time at which all data of the volume is consistent and from which you can create a snapshot or clone a new cached volume from a source volume. To create a snapshot from a volume recovery point use the 'CreateSnapshotFromVolumeRecoveryPoint' operation.
module Network.AWS.StorageGateway.ListVolumeRecoveryPoints
    (
    -- * Creating a request
      ListVolumeRecoveryPoints (..)
    , mkListVolumeRecoveryPoints
    -- ** Request lenses
    , lvrpGatewayARN

    -- * Destructuring the response
    , ListVolumeRecoveryPointsResponse (..)
    , mkListVolumeRecoveryPointsResponse
    -- ** Response lenses
    , lvrprrsGatewayARN
    , lvrprrsVolumeRecoveryPointInfos
    , lvrprrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.StorageGateway.Types as Types

-- | /See:/ 'mkListVolumeRecoveryPoints' smart constructor.
newtype ListVolumeRecoveryPoints = ListVolumeRecoveryPoints'
  { gatewayARN :: Types.GatewayARN
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'ListVolumeRecoveryPoints' value with any optional fields omitted.
mkListVolumeRecoveryPoints
    :: Types.GatewayARN -- ^ 'gatewayARN'
    -> ListVolumeRecoveryPoints
mkListVolumeRecoveryPoints gatewayARN
  = ListVolumeRecoveryPoints'{gatewayARN}

-- | Undocumented field.
--
-- /Note:/ Consider using 'gatewayARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lvrpGatewayARN :: Lens.Lens' ListVolumeRecoveryPoints Types.GatewayARN
lvrpGatewayARN = Lens.field @"gatewayARN"
{-# INLINEABLE lvrpGatewayARN #-}
{-# DEPRECATED gatewayARN "Use generic-lens or generic-optics with 'gatewayARN' instead"  #-}

instance Core.ToQuery ListVolumeRecoveryPoints where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ListVolumeRecoveryPoints where
        toHeaders ListVolumeRecoveryPoints{..}
          = Core.pure
              ("X-Amz-Target",
               "StorageGateway_20130630.ListVolumeRecoveryPoints")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ListVolumeRecoveryPoints where
        toJSON ListVolumeRecoveryPoints{..}
          = Core.object
              (Core.catMaybes [Core.Just ("GatewayARN" Core..= gatewayARN)])

instance Core.AWSRequest ListVolumeRecoveryPoints where
        type Rs ListVolumeRecoveryPoints = ListVolumeRecoveryPointsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListVolumeRecoveryPointsResponse' Core.<$>
                   (x Core..:? "GatewayARN") Core.<*>
                     x Core..:? "VolumeRecoveryPointInfos"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkListVolumeRecoveryPointsResponse' smart constructor.
data ListVolumeRecoveryPointsResponse = ListVolumeRecoveryPointsResponse'
  { gatewayARN :: Core.Maybe Types.GatewayARN
  , volumeRecoveryPointInfos :: Core.Maybe [Types.VolumeRecoveryPointInfo]
    -- ^ An array of 'VolumeRecoveryPointInfo' objects.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListVolumeRecoveryPointsResponse' value with any optional fields omitted.
mkListVolumeRecoveryPointsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListVolumeRecoveryPointsResponse
mkListVolumeRecoveryPointsResponse responseStatus
  = ListVolumeRecoveryPointsResponse'{gatewayARN = Core.Nothing,
                                      volumeRecoveryPointInfos = Core.Nothing, responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'gatewayARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lvrprrsGatewayARN :: Lens.Lens' ListVolumeRecoveryPointsResponse (Core.Maybe Types.GatewayARN)
lvrprrsGatewayARN = Lens.field @"gatewayARN"
{-# INLINEABLE lvrprrsGatewayARN #-}
{-# DEPRECATED gatewayARN "Use generic-lens or generic-optics with 'gatewayARN' instead"  #-}

-- | An array of 'VolumeRecoveryPointInfo' objects.
--
-- /Note:/ Consider using 'volumeRecoveryPointInfos' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lvrprrsVolumeRecoveryPointInfos :: Lens.Lens' ListVolumeRecoveryPointsResponse (Core.Maybe [Types.VolumeRecoveryPointInfo])
lvrprrsVolumeRecoveryPointInfos = Lens.field @"volumeRecoveryPointInfos"
{-# INLINEABLE lvrprrsVolumeRecoveryPointInfos #-}
{-# DEPRECATED volumeRecoveryPointInfos "Use generic-lens or generic-optics with 'volumeRecoveryPointInfos' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lvrprrsResponseStatus :: Lens.Lens' ListVolumeRecoveryPointsResponse Core.Int
lvrprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE lvrprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
