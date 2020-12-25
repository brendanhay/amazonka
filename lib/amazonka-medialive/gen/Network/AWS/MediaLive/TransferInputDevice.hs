{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.TransferInputDevice
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Start an input device transfer to another AWS account. After you make the request, the other account must accept or reject the transfer.
module Network.AWS.MediaLive.TransferInputDevice
  ( -- * Creating a request
    TransferInputDevice (..),
    mkTransferInputDevice,

    -- ** Request lenses
    tidInputDeviceId,
    tidTargetCustomerId,
    tidTransferMessage,

    -- * Destructuring the response
    TransferInputDeviceResponse (..),
    mkTransferInputDeviceResponse,

    -- ** Response lenses
    tidrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaLive.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | A request to transfer an input device.
--
-- /See:/ 'mkTransferInputDevice' smart constructor.
data TransferInputDevice = TransferInputDevice'
  { -- | The unique ID of this input device. For example, hd-123456789abcdef.
    inputDeviceId :: Core.Text,
    -- | The AWS account ID (12 digits) for the recipient of the device transfer.
    targetCustomerId :: Core.Maybe Core.Text,
    -- | An optional message for the recipient. Maximum 280 characters.
    transferMessage :: Core.Maybe Core.Text
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TransferInputDevice' value with any optional fields omitted.
mkTransferInputDevice ::
  -- | 'inputDeviceId'
  Core.Text ->
  TransferInputDevice
mkTransferInputDevice inputDeviceId =
  TransferInputDevice'
    { inputDeviceId,
      targetCustomerId = Core.Nothing,
      transferMessage = Core.Nothing
    }

-- | The unique ID of this input device. For example, hd-123456789abcdef.
--
-- /Note:/ Consider using 'inputDeviceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tidInputDeviceId :: Lens.Lens' TransferInputDevice Core.Text
tidInputDeviceId = Lens.field @"inputDeviceId"
{-# DEPRECATED tidInputDeviceId "Use generic-lens or generic-optics with 'inputDeviceId' instead." #-}

-- | The AWS account ID (12 digits) for the recipient of the device transfer.
--
-- /Note:/ Consider using 'targetCustomerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tidTargetCustomerId :: Lens.Lens' TransferInputDevice (Core.Maybe Core.Text)
tidTargetCustomerId = Lens.field @"targetCustomerId"
{-# DEPRECATED tidTargetCustomerId "Use generic-lens or generic-optics with 'targetCustomerId' instead." #-}

-- | An optional message for the recipient. Maximum 280 characters.
--
-- /Note:/ Consider using 'transferMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tidTransferMessage :: Lens.Lens' TransferInputDevice (Core.Maybe Core.Text)
tidTransferMessage = Lens.field @"transferMessage"
{-# DEPRECATED tidTransferMessage "Use generic-lens or generic-optics with 'transferMessage' instead." #-}

instance Core.FromJSON TransferInputDevice where
  toJSON TransferInputDevice {..} =
    Core.object
      ( Core.catMaybes
          [ ("targetCustomerId" Core..=) Core.<$> targetCustomerId,
            ("transferMessage" Core..=) Core.<$> transferMessage
          ]
      )

instance Core.AWSRequest TransferInputDevice where
  type Rs TransferInputDevice = TransferInputDeviceResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath =
          Core.rawPath
            ( "/prod/inputDevices/" Core.<> (Core.toText inputDeviceId)
                Core.<> ("/transfer")
            ),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          TransferInputDeviceResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | Placeholder documentation for TransferInputDeviceResponse
--
-- /See:/ 'mkTransferInputDeviceResponse' smart constructor.
newtype TransferInputDeviceResponse = TransferInputDeviceResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'TransferInputDeviceResponse' value with any optional fields omitted.
mkTransferInputDeviceResponse ::
  -- | 'responseStatus'
  Core.Int ->
  TransferInputDeviceResponse
mkTransferInputDeviceResponse responseStatus =
  TransferInputDeviceResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tidrrsResponseStatus :: Lens.Lens' TransferInputDeviceResponse Core.Int
tidrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED tidrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
