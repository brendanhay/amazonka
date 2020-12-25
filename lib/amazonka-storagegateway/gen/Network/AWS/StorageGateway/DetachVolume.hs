{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.DetachVolume
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disconnects a volume from an iSCSI connection and then detaches the volume from the specified gateway. Detaching and attaching a volume enables you to recover your data from one gateway to a different gateway without creating a snapshot. It also makes it easier to move your volumes from an on-premises gateway to a gateway hosted on an Amazon EC2 instance. This operation is only supported in the volume gateway type.
module Network.AWS.StorageGateway.DetachVolume
  ( -- * Creating a request
    DetachVolume (..),
    mkDetachVolume,

    -- ** Request lenses
    dvVolumeARN,
    dvForceDetach,

    -- * Destructuring the response
    DetachVolumeResponse (..),
    mkDetachVolumeResponse,

    -- ** Response lenses
    dvrrsVolumeARN,
    dvrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.StorageGateway.Types as Types

-- | AttachVolumeInput
--
-- /See:/ 'mkDetachVolume' smart constructor.
data DetachVolume = DetachVolume'
  { -- | The Amazon Resource Name (ARN) of the volume to detach from the gateway.
    volumeARN :: Types.VolumeARN,
    -- | Set to @true@ to forcibly remove the iSCSI connection of the target volume and detach the volume. The default is @false@ . If this value is set to @false@ , you must manually disconnect the iSCSI connection from the target volume.
    --
    -- Valid Values: @true@ | @false@
    forceDetach :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DetachVolume' value with any optional fields omitted.
mkDetachVolume ::
  -- | 'volumeARN'
  Types.VolumeARN ->
  DetachVolume
mkDetachVolume volumeARN =
  DetachVolume' {volumeARN, forceDetach = Core.Nothing}

-- | The Amazon Resource Name (ARN) of the volume to detach from the gateway.
--
-- /Note:/ Consider using 'volumeARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvVolumeARN :: Lens.Lens' DetachVolume Types.VolumeARN
dvVolumeARN = Lens.field @"volumeARN"
{-# DEPRECATED dvVolumeARN "Use generic-lens or generic-optics with 'volumeARN' instead." #-}

-- | Set to @true@ to forcibly remove the iSCSI connection of the target volume and detach the volume. The default is @false@ . If this value is set to @false@ , you must manually disconnect the iSCSI connection from the target volume.
--
-- Valid Values: @true@ | @false@
--
-- /Note:/ Consider using 'forceDetach' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvForceDetach :: Lens.Lens' DetachVolume (Core.Maybe Core.Bool)
dvForceDetach = Lens.field @"forceDetach"
{-# DEPRECATED dvForceDetach "Use generic-lens or generic-optics with 'forceDetach' instead." #-}

instance Core.FromJSON DetachVolume where
  toJSON DetachVolume {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("VolumeARN" Core..= volumeARN),
            ("ForceDetach" Core..=) Core.<$> forceDetach
          ]
      )

instance Core.AWSRequest DetachVolume where
  type Rs DetachVolume = DetachVolumeResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "StorageGateway_20130630.DetachVolume")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DetachVolumeResponse'
            Core.<$> (x Core..:? "VolumeARN") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | AttachVolumeOutput
--
-- /See:/ 'mkDetachVolumeResponse' smart constructor.
data DetachVolumeResponse = DetachVolumeResponse'
  { -- | The Amazon Resource Name (ARN) of the volume that was detached.
    volumeARN :: Core.Maybe Types.VolumeARN,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DetachVolumeResponse' value with any optional fields omitted.
mkDetachVolumeResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DetachVolumeResponse
mkDetachVolumeResponse responseStatus =
  DetachVolumeResponse' {volumeARN = Core.Nothing, responseStatus}

-- | The Amazon Resource Name (ARN) of the volume that was detached.
--
-- /Note:/ Consider using 'volumeARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvrrsVolumeARN :: Lens.Lens' DetachVolumeResponse (Core.Maybe Types.VolumeARN)
dvrrsVolumeARN = Lens.field @"volumeARN"
{-# DEPRECATED dvrrsVolumeARN "Use generic-lens or generic-optics with 'volumeARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvrrsResponseStatus :: Lens.Lens' DetachVolumeResponse Core.Int
dvrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dvrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
