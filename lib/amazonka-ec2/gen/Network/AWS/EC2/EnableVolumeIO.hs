{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.EnableVolumeIO
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables I/O operations for a volume that had I/O operations disabled because the data on the volume was potentially inconsistent.
module Network.AWS.EC2.EnableVolumeIO
  ( -- * Creating a request
    EnableVolumeIO (..),
    mkEnableVolumeIO,

    -- ** Request lenses
    evioVolumeId,
    evioDryRun,

    -- * Destructuring the response
    EnableVolumeIOResponse (..),
    mkEnableVolumeIOResponse,
  )
where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkEnableVolumeIO' smart constructor.
data EnableVolumeIO = EnableVolumeIO'
  { -- | The ID of the volume.
    volumeId :: Types.VolumeId,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'EnableVolumeIO' value with any optional fields omitted.
mkEnableVolumeIO ::
  -- | 'volumeId'
  Types.VolumeId ->
  EnableVolumeIO
mkEnableVolumeIO volumeId =
  EnableVolumeIO' {volumeId, dryRun = Core.Nothing}

-- | The ID of the volume.
--
-- /Note:/ Consider using 'volumeId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
evioVolumeId :: Lens.Lens' EnableVolumeIO Types.VolumeId
evioVolumeId = Lens.field @"volumeId"
{-# DEPRECATED evioVolumeId "Use generic-lens or generic-optics with 'volumeId' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
evioDryRun :: Lens.Lens' EnableVolumeIO (Core.Maybe Core.Bool)
evioDryRun = Lens.field @"dryRun"
{-# DEPRECATED evioDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

instance Core.AWSRequest EnableVolumeIO where
  type Rs EnableVolumeIO = EnableVolumeIOResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "Content-Type",
              "application/x-www-form-urlencoded; charset=utf-8"
            ),
        Core._rqBody =
          Core.toFormBody
            ( Core.pure ("Action", "EnableVolumeIO")
                Core.<> (Core.pure ("Version", "2016-11-15"))
                Core.<> (Core.toQueryValue "VolumeId" volumeId)
                Core.<> (Core.toQueryValue "DryRun" Core.<$> dryRun)
            )
      }
  response = Response.receiveNull EnableVolumeIOResponse'

-- | /See:/ 'mkEnableVolumeIOResponse' smart constructor.
data EnableVolumeIOResponse = EnableVolumeIOResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'EnableVolumeIOResponse' value with any optional fields omitted.
mkEnableVolumeIOResponse ::
  EnableVolumeIOResponse
mkEnableVolumeIOResponse = EnableVolumeIOResponse'
