{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    cidtrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Placeholder documentation for CancelInputDeviceTransferRequest
--
-- /See:/ 'mkCancelInputDeviceTransfer' smart constructor.
newtype CancelInputDeviceTransfer = CancelInputDeviceTransfer'
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

-- | Creates a value of 'CancelInputDeviceTransfer' with the minimum fields required to make a request.
--
-- * 'inputDeviceId' - The unique ID of the input device to cancel. For example, hd-123456789abcdef.
mkCancelInputDeviceTransfer ::
  -- | 'inputDeviceId'
  Lude.Text ->
  CancelInputDeviceTransfer
mkCancelInputDeviceTransfer pInputDeviceId_ =
  CancelInputDeviceTransfer' {inputDeviceId = pInputDeviceId_}

-- | The unique ID of the input device to cancel. For example, hd-123456789abcdef.
--
-- /Note:/ Consider using 'inputDeviceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cidtInputDeviceId :: Lens.Lens' CancelInputDeviceTransfer Lude.Text
cidtInputDeviceId = Lens.lens (inputDeviceId :: CancelInputDeviceTransfer -> Lude.Text) (\s a -> s {inputDeviceId = a} :: CancelInputDeviceTransfer)
{-# DEPRECATED cidtInputDeviceId "Use generic-lens or generic-optics with 'inputDeviceId' instead." #-}

instance Lude.AWSRequest CancelInputDeviceTransfer where
  type
    Rs CancelInputDeviceTransfer =
      CancelInputDeviceTransferResponse
  request = Req.postJSON mediaLiveService
  response =
    Res.receiveEmpty
      ( \s h x ->
          CancelInputDeviceTransferResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CancelInputDeviceTransfer where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CancelInputDeviceTransfer where
  toJSON = Lude.const (Lude.Object Lude.mempty)

instance Lude.ToPath CancelInputDeviceTransfer where
  toPath CancelInputDeviceTransfer' {..} =
    Lude.mconcat
      ["/prod/inputDevices/", Lude.toBS inputDeviceId, "/cancel"]

instance Lude.ToQuery CancelInputDeviceTransfer where
  toQuery = Lude.const Lude.mempty

-- | Placeholder documentation for CancelInputDeviceTransferResponse
--
-- /See:/ 'mkCancelInputDeviceTransferResponse' smart constructor.
newtype CancelInputDeviceTransferResponse = CancelInputDeviceTransferResponse'
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

-- | Creates a value of 'CancelInputDeviceTransferResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkCancelInputDeviceTransferResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CancelInputDeviceTransferResponse
mkCancelInputDeviceTransferResponse pResponseStatus_ =
  CancelInputDeviceTransferResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cidtrsResponseStatus :: Lens.Lens' CancelInputDeviceTransferResponse Lude.Int
cidtrsResponseStatus = Lens.lens (responseStatus :: CancelInputDeviceTransferResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CancelInputDeviceTransferResponse)
{-# DEPRECATED cidtrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
