{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    tidTransferMessage,
    tidTargetCustomerId,
    tidInputDeviceId,

    -- * Destructuring the response
    TransferInputDeviceResponse (..),
    mkTransferInputDeviceResponse,

    -- ** Response lenses
    tidrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | A request to transfer an input device.
--
-- /See:/ 'mkTransferInputDevice' smart constructor.
data TransferInputDevice = TransferInputDevice'
  { transferMessage ::
      Lude.Maybe Lude.Text,
    targetCustomerId :: Lude.Maybe Lude.Text,
    inputDeviceId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TransferInputDevice' with the minimum fields required to make a request.
--
-- * 'inputDeviceId' - The unique ID of this input device. For example, hd-123456789abcdef.
-- * 'targetCustomerId' - The AWS account ID (12 digits) for the recipient of the device transfer.
-- * 'transferMessage' - An optional message for the recipient. Maximum 280 characters.
mkTransferInputDevice ::
  -- | 'inputDeviceId'
  Lude.Text ->
  TransferInputDevice
mkTransferInputDevice pInputDeviceId_ =
  TransferInputDevice'
    { transferMessage = Lude.Nothing,
      targetCustomerId = Lude.Nothing,
      inputDeviceId = pInputDeviceId_
    }

-- | An optional message for the recipient. Maximum 280 characters.
--
-- /Note:/ Consider using 'transferMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tidTransferMessage :: Lens.Lens' TransferInputDevice (Lude.Maybe Lude.Text)
tidTransferMessage = Lens.lens (transferMessage :: TransferInputDevice -> Lude.Maybe Lude.Text) (\s a -> s {transferMessage = a} :: TransferInputDevice)
{-# DEPRECATED tidTransferMessage "Use generic-lens or generic-optics with 'transferMessage' instead." #-}

-- | The AWS account ID (12 digits) for the recipient of the device transfer.
--
-- /Note:/ Consider using 'targetCustomerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tidTargetCustomerId :: Lens.Lens' TransferInputDevice (Lude.Maybe Lude.Text)
tidTargetCustomerId = Lens.lens (targetCustomerId :: TransferInputDevice -> Lude.Maybe Lude.Text) (\s a -> s {targetCustomerId = a} :: TransferInputDevice)
{-# DEPRECATED tidTargetCustomerId "Use generic-lens or generic-optics with 'targetCustomerId' instead." #-}

-- | The unique ID of this input device. For example, hd-123456789abcdef.
--
-- /Note:/ Consider using 'inputDeviceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tidInputDeviceId :: Lens.Lens' TransferInputDevice Lude.Text
tidInputDeviceId = Lens.lens (inputDeviceId :: TransferInputDevice -> Lude.Text) (\s a -> s {inputDeviceId = a} :: TransferInputDevice)
{-# DEPRECATED tidInputDeviceId "Use generic-lens or generic-optics with 'inputDeviceId' instead." #-}

instance Lude.AWSRequest TransferInputDevice where
  type Rs TransferInputDevice = TransferInputDeviceResponse
  request = Req.postJSON mediaLiveService
  response =
    Res.receiveEmpty
      ( \s h x ->
          TransferInputDeviceResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders TransferInputDevice where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON TransferInputDevice where
  toJSON TransferInputDevice' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("transferMessage" Lude..=) Lude.<$> transferMessage,
            ("targetCustomerId" Lude..=) Lude.<$> targetCustomerId
          ]
      )

instance Lude.ToPath TransferInputDevice where
  toPath TransferInputDevice' {..} =
    Lude.mconcat
      ["/prod/inputDevices/", Lude.toBS inputDeviceId, "/transfer"]

instance Lude.ToQuery TransferInputDevice where
  toQuery = Lude.const Lude.mempty

-- | Placeholder documentation for TransferInputDeviceResponse
--
-- /See:/ 'mkTransferInputDeviceResponse' smart constructor.
newtype TransferInputDeviceResponse = TransferInputDeviceResponse'
  { responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TransferInputDeviceResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkTransferInputDeviceResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  TransferInputDeviceResponse
mkTransferInputDeviceResponse pResponseStatus_ =
  TransferInputDeviceResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tidrsResponseStatus :: Lens.Lens' TransferInputDeviceResponse Lude.Int
tidrsResponseStatus = Lens.lens (responseStatus :: TransferInputDeviceResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: TransferInputDeviceResponse)
{-# DEPRECATED tidrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
