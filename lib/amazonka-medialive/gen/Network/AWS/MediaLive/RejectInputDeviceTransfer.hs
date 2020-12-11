{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    ridtrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Placeholder documentation for RejectInputDeviceTransferRequest
--
-- /See:/ 'mkRejectInputDeviceTransfer' smart constructor.
newtype RejectInputDeviceTransfer = RejectInputDeviceTransfer'
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

-- | Creates a value of 'RejectInputDeviceTransfer' with the minimum fields required to make a request.
--
-- * 'inputDeviceId' - The unique ID of the input device to reject. For example, hd-123456789abcdef.
mkRejectInputDeviceTransfer ::
  -- | 'inputDeviceId'
  Lude.Text ->
  RejectInputDeviceTransfer
mkRejectInputDeviceTransfer pInputDeviceId_ =
  RejectInputDeviceTransfer' {inputDeviceId = pInputDeviceId_}

-- | The unique ID of the input device to reject. For example, hd-123456789abcdef.
--
-- /Note:/ Consider using 'inputDeviceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ridtInputDeviceId :: Lens.Lens' RejectInputDeviceTransfer Lude.Text
ridtInputDeviceId = Lens.lens (inputDeviceId :: RejectInputDeviceTransfer -> Lude.Text) (\s a -> s {inputDeviceId = a} :: RejectInputDeviceTransfer)
{-# DEPRECATED ridtInputDeviceId "Use generic-lens or generic-optics with 'inputDeviceId' instead." #-}

instance Lude.AWSRequest RejectInputDeviceTransfer where
  type
    Rs RejectInputDeviceTransfer =
      RejectInputDeviceTransferResponse
  request = Req.postJSON mediaLiveService
  response =
    Res.receiveEmpty
      ( \s h x ->
          RejectInputDeviceTransferResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders RejectInputDeviceTransfer where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON RejectInputDeviceTransfer where
  toJSON = Lude.const (Lude.Object Lude.mempty)

instance Lude.ToPath RejectInputDeviceTransfer where
  toPath RejectInputDeviceTransfer' {..} =
    Lude.mconcat
      ["/prod/inputDevices/", Lude.toBS inputDeviceId, "/reject"]

instance Lude.ToQuery RejectInputDeviceTransfer where
  toQuery = Lude.const Lude.mempty

-- | Placeholder documentation for RejectInputDeviceTransferResponse
--
-- /See:/ 'mkRejectInputDeviceTransferResponse' smart constructor.
newtype RejectInputDeviceTransferResponse = RejectInputDeviceTransferResponse'
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

-- | Creates a value of 'RejectInputDeviceTransferResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkRejectInputDeviceTransferResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  RejectInputDeviceTransferResponse
mkRejectInputDeviceTransferResponse pResponseStatus_ =
  RejectInputDeviceTransferResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ridtrsResponseStatus :: Lens.Lens' RejectInputDeviceTransferResponse Lude.Int
ridtrsResponseStatus = Lens.lens (responseStatus :: RejectInputDeviceTransferResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: RejectInputDeviceTransferResponse)
{-# DEPRECATED ridtrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
