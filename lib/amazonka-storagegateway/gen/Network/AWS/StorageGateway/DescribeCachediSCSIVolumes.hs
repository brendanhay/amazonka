{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.DescribeCachediSCSIVolumes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a description of the gateway volumes specified in the request. This operation is only supported in the cached volume gateway types.
--
-- The list of gateway volumes in the request must be from one gateway. In the response, AWS Storage Gateway returns volume information sorted by volume Amazon Resource Name (ARN).
module Network.AWS.StorageGateway.DescribeCachediSCSIVolumes
    (
    -- * Creating a request
      DescribeCachediSCSIVolumes (..)
    , mkDescribeCachediSCSIVolumes
    -- ** Request lenses
    , dcscsivVolumeARNs

    -- * Destructuring the response
    , DescribeCachediSCSIVolumesResponse (..)
    , mkDescribeCachediSCSIVolumesResponse
    -- ** Response lenses
    , dcscsivrrsCachediSCSIVolumes
    , dcscsivrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.StorageGateway.Types as Types

-- | /See:/ 'mkDescribeCachediSCSIVolumes' smart constructor.
newtype DescribeCachediSCSIVolumes = DescribeCachediSCSIVolumes'
  { volumeARNs :: [Types.VolumeARN]
    -- ^ An array of strings where each string represents the Amazon Resource Name (ARN) of a cached volume. All of the specified cached volumes must be from the same gateway. Use 'ListVolumes' to get volume ARNs for a gateway.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeCachediSCSIVolumes' value with any optional fields omitted.
mkDescribeCachediSCSIVolumes
    :: DescribeCachediSCSIVolumes
mkDescribeCachediSCSIVolumes
  = DescribeCachediSCSIVolumes'{volumeARNs = Core.mempty}

-- | An array of strings where each string represents the Amazon Resource Name (ARN) of a cached volume. All of the specified cached volumes must be from the same gateway. Use 'ListVolumes' to get volume ARNs for a gateway.
--
-- /Note:/ Consider using 'volumeARNs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcscsivVolumeARNs :: Lens.Lens' DescribeCachediSCSIVolumes [Types.VolumeARN]
dcscsivVolumeARNs = Lens.field @"volumeARNs"
{-# INLINEABLE dcscsivVolumeARNs #-}
{-# DEPRECATED volumeARNs "Use generic-lens or generic-optics with 'volumeARNs' instead"  #-}

instance Core.ToQuery DescribeCachediSCSIVolumes where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeCachediSCSIVolumes where
        toHeaders DescribeCachediSCSIVolumes{..}
          = Core.pure
              ("X-Amz-Target",
               "StorageGateway_20130630.DescribeCachediSCSIVolumes")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DescribeCachediSCSIVolumes where
        toJSON DescribeCachediSCSIVolumes{..}
          = Core.object
              (Core.catMaybes [Core.Just ("VolumeARNs" Core..= volumeARNs)])

instance Core.AWSRequest DescribeCachediSCSIVolumes where
        type Rs DescribeCachediSCSIVolumes =
             DescribeCachediSCSIVolumesResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeCachediSCSIVolumesResponse' Core.<$>
                   (x Core..:? "CachediSCSIVolumes") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | A JSON object containing the following fields:
--
-- /See:/ 'mkDescribeCachediSCSIVolumesResponse' smart constructor.
data DescribeCachediSCSIVolumesResponse = DescribeCachediSCSIVolumesResponse'
  { cachediSCSIVolumes :: Core.Maybe [Types.CachediSCSIVolume]
    -- ^ An array of objects where each object contains metadata about one cached volume.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeCachediSCSIVolumesResponse' value with any optional fields omitted.
mkDescribeCachediSCSIVolumesResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeCachediSCSIVolumesResponse
mkDescribeCachediSCSIVolumesResponse responseStatus
  = DescribeCachediSCSIVolumesResponse'{cachediSCSIVolumes =
                                          Core.Nothing,
                                        responseStatus}

-- | An array of objects where each object contains metadata about one cached volume.
--
-- /Note:/ Consider using 'cachediSCSIVolumes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcscsivrrsCachediSCSIVolumes :: Lens.Lens' DescribeCachediSCSIVolumesResponse (Core.Maybe [Types.CachediSCSIVolume])
dcscsivrrsCachediSCSIVolumes = Lens.field @"cachediSCSIVolumes"
{-# INLINEABLE dcscsivrrsCachediSCSIVolumes #-}
{-# DEPRECATED cachediSCSIVolumes "Use generic-lens or generic-optics with 'cachediSCSIVolumes' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcscsivrrsResponseStatus :: Lens.Lens' DescribeCachediSCSIVolumesResponse Core.Int
dcscsivrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dcscsivrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
