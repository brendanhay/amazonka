{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    ListVolumeInitiators (..),
    mkListVolumeInitiators,

    -- ** Request lenses
    lviVolumeARN,

    -- * Destructuring the response
    ListVolumeInitiatorsResponse (..),
    mkListVolumeInitiatorsResponse,

    -- ** Response lenses
    lvirrsInitiators,
    lvirrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.StorageGateway.Types as Types

-- | ListVolumeInitiatorsInput
--
-- /See:/ 'mkListVolumeInitiators' smart constructor.
newtype ListVolumeInitiators = ListVolumeInitiators'
  { -- | The Amazon Resource Name (ARN) of the volume. Use the 'ListVolumes' operation to return a list of gateway volumes for the gateway.
    volumeARN :: Types.VolumeARN
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'ListVolumeInitiators' value with any optional fields omitted.
mkListVolumeInitiators ::
  -- | 'volumeARN'
  Types.VolumeARN ->
  ListVolumeInitiators
mkListVolumeInitiators volumeARN = ListVolumeInitiators' {volumeARN}

-- | The Amazon Resource Name (ARN) of the volume. Use the 'ListVolumes' operation to return a list of gateway volumes for the gateway.
--
-- /Note:/ Consider using 'volumeARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lviVolumeARN :: Lens.Lens' ListVolumeInitiators Types.VolumeARN
lviVolumeARN = Lens.field @"volumeARN"
{-# DEPRECATED lviVolumeARN "Use generic-lens or generic-optics with 'volumeARN' instead." #-}

instance Core.FromJSON ListVolumeInitiators where
  toJSON ListVolumeInitiators {..} =
    Core.object
      (Core.catMaybes [Core.Just ("VolumeARN" Core..= volumeARN)])

instance Core.AWSRequest ListVolumeInitiators where
  type Rs ListVolumeInitiators = ListVolumeInitiatorsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "StorageGateway_20130630.ListVolumeInitiators")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListVolumeInitiatorsResponse'
            Core.<$> (x Core..:? "Initiators") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | ListVolumeInitiatorsOutput
--
-- /See:/ 'mkListVolumeInitiatorsResponse' smart constructor.
data ListVolumeInitiatorsResponse = ListVolumeInitiatorsResponse'
  { -- | The host names and port numbers of all iSCSI initiators that are connected to the gateway.
    initiators :: Core.Maybe [Types.Initiator],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListVolumeInitiatorsResponse' value with any optional fields omitted.
mkListVolumeInitiatorsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListVolumeInitiatorsResponse
mkListVolumeInitiatorsResponse responseStatus =
  ListVolumeInitiatorsResponse'
    { initiators = Core.Nothing,
      responseStatus
    }

-- | The host names and port numbers of all iSCSI initiators that are connected to the gateway.
--
-- /Note:/ Consider using 'initiators' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lvirrsInitiators :: Lens.Lens' ListVolumeInitiatorsResponse (Core.Maybe [Types.Initiator])
lvirrsInitiators = Lens.field @"initiators"
{-# DEPRECATED lvirrsInitiators "Use generic-lens or generic-optics with 'initiators' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lvirrsResponseStatus :: Lens.Lens' ListVolumeInitiatorsResponse Core.Int
lvirrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED lvirrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
