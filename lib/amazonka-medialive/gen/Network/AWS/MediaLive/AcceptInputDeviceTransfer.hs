{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    aidtrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Placeholder documentation for AcceptInputDeviceTransferRequest
--
-- /See:/ 'mkAcceptInputDeviceTransfer' smart constructor.
newtype AcceptInputDeviceTransfer = AcceptInputDeviceTransfer'
  { inputDeviceId ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AcceptInputDeviceTransfer' with the minimum fields required to make a request.
--
-- * 'inputDeviceId' - The unique ID of the input device to accept. For example, hd-123456789abcdef.
mkAcceptInputDeviceTransfer ::
  -- | 'inputDeviceId'
  Lude.Text ->
  AcceptInputDeviceTransfer
mkAcceptInputDeviceTransfer pInputDeviceId_ =
  AcceptInputDeviceTransfer' {inputDeviceId = pInputDeviceId_}

-- | The unique ID of the input device to accept. For example, hd-123456789abcdef.
--
-- /Note:/ Consider using 'inputDeviceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aidtInputDeviceId :: Lens.Lens' AcceptInputDeviceTransfer Lude.Text
aidtInputDeviceId = Lens.lens (inputDeviceId :: AcceptInputDeviceTransfer -> Lude.Text) (\s a -> s {inputDeviceId = a} :: AcceptInputDeviceTransfer)
{-# DEPRECATED aidtInputDeviceId "Use generic-lens or generic-optics with 'inputDeviceId' instead." #-}

instance Lude.AWSRequest AcceptInputDeviceTransfer where
  type
    Rs AcceptInputDeviceTransfer =
      AcceptInputDeviceTransferResponse
  request = Req.postJSON mediaLiveService
  response =
    Res.receiveEmpty
      ( \s h x ->
          AcceptInputDeviceTransferResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders AcceptInputDeviceTransfer where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON AcceptInputDeviceTransfer where
  toJSON = Lude.const (Lude.Object Lude.mempty)

instance Lude.ToPath AcceptInputDeviceTransfer where
  toPath AcceptInputDeviceTransfer' {..} =
    Lude.mconcat
      ["/prod/inputDevices/", Lude.toBS inputDeviceId, "/accept"]

instance Lude.ToQuery AcceptInputDeviceTransfer where
  toQuery = Lude.const Lude.mempty

-- | Placeholder documentation for AcceptInputDeviceTransferResponse
--
-- /See:/ 'mkAcceptInputDeviceTransferResponse' smart constructor.
newtype AcceptInputDeviceTransferResponse = AcceptInputDeviceTransferResponse'
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

-- | Creates a value of 'AcceptInputDeviceTransferResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkAcceptInputDeviceTransferResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  AcceptInputDeviceTransferResponse
mkAcceptInputDeviceTransferResponse pResponseStatus_ =
  AcceptInputDeviceTransferResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aidtrsResponseStatus :: Lens.Lens' AcceptInputDeviceTransferResponse Lude.Int
aidtrsResponseStatus = Lens.lens (responseStatus :: AcceptInputDeviceTransferResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: AcceptInputDeviceTransferResponse)
{-# DEPRECATED aidtrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
