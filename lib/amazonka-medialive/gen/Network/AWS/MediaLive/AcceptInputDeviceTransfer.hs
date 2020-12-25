{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.AcceptInputDeviceTransfer
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Accept an incoming input device transfer. The ownership of the device will transfer to your AWS account.
module Network.AWS.MediaLive.AcceptInputDeviceTransfer
  ( -- * Creating a request
    AcceptInputDeviceTransfer (..),
    mkAcceptInputDeviceTransfer,

    -- ** Request lenses
    aidtInputDeviceId,

    -- * Destructuring the response
    AcceptInputDeviceTransferResponse (..),
    mkAcceptInputDeviceTransferResponse,

    -- ** Response lenses
    aidtrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaLive.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Placeholder documentation for AcceptInputDeviceTransferRequest
--
-- /See:/ 'mkAcceptInputDeviceTransfer' smart constructor.
newtype AcceptInputDeviceTransfer = AcceptInputDeviceTransfer'
  { -- | The unique ID of the input device to accept. For example, hd-123456789abcdef.
    inputDeviceId :: Core.Text
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'AcceptInputDeviceTransfer' value with any optional fields omitted.
mkAcceptInputDeviceTransfer ::
  -- | 'inputDeviceId'
  Core.Text ->
  AcceptInputDeviceTransfer
mkAcceptInputDeviceTransfer inputDeviceId =
  AcceptInputDeviceTransfer' {inputDeviceId}

-- | The unique ID of the input device to accept. For example, hd-123456789abcdef.
--
-- /Note:/ Consider using 'inputDeviceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aidtInputDeviceId :: Lens.Lens' AcceptInputDeviceTransfer Core.Text
aidtInputDeviceId = Lens.field @"inputDeviceId"
{-# DEPRECATED aidtInputDeviceId "Use generic-lens or generic-optics with 'inputDeviceId' instead." #-}

instance Core.FromJSON AcceptInputDeviceTransfer where
  toJSON _ = Core.Object Core.mempty

instance Core.AWSRequest AcceptInputDeviceTransfer where
  type
    Rs AcceptInputDeviceTransfer =
      AcceptInputDeviceTransferResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath =
          Core.rawPath
            ( "/prod/inputDevices/" Core.<> (Core.toText inputDeviceId)
                Core.<> ("/accept")
            ),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          AcceptInputDeviceTransferResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | Placeholder documentation for AcceptInputDeviceTransferResponse
--
-- /See:/ 'mkAcceptInputDeviceTransferResponse' smart constructor.
newtype AcceptInputDeviceTransferResponse = AcceptInputDeviceTransferResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'AcceptInputDeviceTransferResponse' value with any optional fields omitted.
mkAcceptInputDeviceTransferResponse ::
  -- | 'responseStatus'
  Core.Int ->
  AcceptInputDeviceTransferResponse
mkAcceptInputDeviceTransferResponse responseStatus =
  AcceptInputDeviceTransferResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aidtrrsResponseStatus :: Lens.Lens' AcceptInputDeviceTransferResponse Core.Int
aidtrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED aidtrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
