{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.CancelInputDeviceTransfer
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Cancel an input device transfer that you have requested.
module Network.AWS.MediaLive.CancelInputDeviceTransfer
  ( -- * Creating a request
    CancelInputDeviceTransfer (..),
    mkCancelInputDeviceTransfer,

    -- ** Request lenses
    cidtInputDeviceId,

    -- * Destructuring the response
    CancelInputDeviceTransferResponse (..),
    mkCancelInputDeviceTransferResponse,

    -- ** Response lenses
    cidtrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaLive.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Placeholder documentation for CancelInputDeviceTransferRequest
--
-- /See:/ 'mkCancelInputDeviceTransfer' smart constructor.
newtype CancelInputDeviceTransfer = CancelInputDeviceTransfer'
  { -- | The unique ID of the input device to cancel. For example, hd-123456789abcdef.
    inputDeviceId :: Core.Text
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'CancelInputDeviceTransfer' value with any optional fields omitted.
mkCancelInputDeviceTransfer ::
  -- | 'inputDeviceId'
  Core.Text ->
  CancelInputDeviceTransfer
mkCancelInputDeviceTransfer inputDeviceId =
  CancelInputDeviceTransfer' {inputDeviceId}

-- | The unique ID of the input device to cancel. For example, hd-123456789abcdef.
--
-- /Note:/ Consider using 'inputDeviceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cidtInputDeviceId :: Lens.Lens' CancelInputDeviceTransfer Core.Text
cidtInputDeviceId = Lens.field @"inputDeviceId"
{-# DEPRECATED cidtInputDeviceId "Use generic-lens or generic-optics with 'inputDeviceId' instead." #-}

instance Core.FromJSON CancelInputDeviceTransfer where
  toJSON _ = Core.Object Core.mempty

instance Core.AWSRequest CancelInputDeviceTransfer where
  type
    Rs CancelInputDeviceTransfer =
      CancelInputDeviceTransferResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath =
          Core.rawPath
            ( "/prod/inputDevices/" Core.<> (Core.toText inputDeviceId)
                Core.<> ("/cancel")
            ),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          CancelInputDeviceTransferResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | Placeholder documentation for CancelInputDeviceTransferResponse
--
-- /See:/ 'mkCancelInputDeviceTransferResponse' smart constructor.
newtype CancelInputDeviceTransferResponse = CancelInputDeviceTransferResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'CancelInputDeviceTransferResponse' value with any optional fields omitted.
mkCancelInputDeviceTransferResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CancelInputDeviceTransferResponse
mkCancelInputDeviceTransferResponse responseStatus =
  CancelInputDeviceTransferResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cidtrrsResponseStatus :: Lens.Lens' CancelInputDeviceTransferResponse Core.Int
cidtrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED cidtrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
