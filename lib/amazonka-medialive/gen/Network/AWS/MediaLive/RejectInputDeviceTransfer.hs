{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.RejectInputDeviceTransfer
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Reject the transfer of the specified input device to your AWS account.
module Network.AWS.MediaLive.RejectInputDeviceTransfer
  ( -- * Creating a request
    RejectInputDeviceTransfer (..),
    mkRejectInputDeviceTransfer,

    -- ** Request lenses
    ridtInputDeviceId,

    -- * Destructuring the response
    RejectInputDeviceTransferResponse (..),
    mkRejectInputDeviceTransferResponse,

    -- ** Response lenses
    ridtrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaLive.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Placeholder documentation for RejectInputDeviceTransferRequest
--
-- /See:/ 'mkRejectInputDeviceTransfer' smart constructor.
newtype RejectInputDeviceTransfer = RejectInputDeviceTransfer'
  { -- | The unique ID of the input device to reject. For example, hd-123456789abcdef.
    inputDeviceId :: Core.Text
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'RejectInputDeviceTransfer' value with any optional fields omitted.
mkRejectInputDeviceTransfer ::
  -- | 'inputDeviceId'
  Core.Text ->
  RejectInputDeviceTransfer
mkRejectInputDeviceTransfer inputDeviceId =
  RejectInputDeviceTransfer' {inputDeviceId}

-- | The unique ID of the input device to reject. For example, hd-123456789abcdef.
--
-- /Note:/ Consider using 'inputDeviceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ridtInputDeviceId :: Lens.Lens' RejectInputDeviceTransfer Core.Text
ridtInputDeviceId = Lens.field @"inputDeviceId"
{-# DEPRECATED ridtInputDeviceId "Use generic-lens or generic-optics with 'inputDeviceId' instead." #-}

instance Core.FromJSON RejectInputDeviceTransfer where
  toJSON _ = Core.Object Core.mempty

instance Core.AWSRequest RejectInputDeviceTransfer where
  type
    Rs RejectInputDeviceTransfer =
      RejectInputDeviceTransferResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath =
          Core.rawPath
            ( "/prod/inputDevices/" Core.<> (Core.toText inputDeviceId)
                Core.<> ("/reject")
            ),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          RejectInputDeviceTransferResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | Placeholder documentation for RejectInputDeviceTransferResponse
--
-- /See:/ 'mkRejectInputDeviceTransferResponse' smart constructor.
newtype RejectInputDeviceTransferResponse = RejectInputDeviceTransferResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'RejectInputDeviceTransferResponse' value with any optional fields omitted.
mkRejectInputDeviceTransferResponse ::
  -- | 'responseStatus'
  Core.Int ->
  RejectInputDeviceTransferResponse
mkRejectInputDeviceTransferResponse responseStatus =
  RejectInputDeviceTransferResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ridtrrsResponseStatus :: Lens.Lens' RejectInputDeviceTransferResponse Core.Int
ridtrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ridtrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
