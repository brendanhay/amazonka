{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.CancelArchival
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Cancels archiving of a virtual tape to the virtual tape shelf (VTS) after the archiving process is initiated. This operation is only supported in the tape gateway type.
module Network.AWS.StorageGateway.CancelArchival
  ( -- * Creating a request
    CancelArchival (..),
    mkCancelArchival,

    -- ** Request lenses
    caGatewayARN,
    caTapeARN,

    -- * Destructuring the response
    CancelArchivalResponse (..),
    mkCancelArchivalResponse,

    -- ** Response lenses
    carsTapeARN,
    carsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.StorageGateway.Types

-- | CancelArchivalInput
--
-- /See:/ 'mkCancelArchival' smart constructor.
data CancelArchival = CancelArchival'
  { gatewayARN :: Lude.Text,
    tapeARN :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CancelArchival' with the minimum fields required to make a request.
--
-- * 'gatewayARN' - Undocumented field.
-- * 'tapeARN' - The Amazon Resource Name (ARN) of the virtual tape you want to cancel archiving for.
mkCancelArchival ::
  -- | 'gatewayARN'
  Lude.Text ->
  -- | 'tapeARN'
  Lude.Text ->
  CancelArchival
mkCancelArchival pGatewayARN_ pTapeARN_ =
  CancelArchival' {gatewayARN = pGatewayARN_, tapeARN = pTapeARN_}

-- | Undocumented field.
--
-- /Note:/ Consider using 'gatewayARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caGatewayARN :: Lens.Lens' CancelArchival Lude.Text
caGatewayARN = Lens.lens (gatewayARN :: CancelArchival -> Lude.Text) (\s a -> s {gatewayARN = a} :: CancelArchival)
{-# DEPRECATED caGatewayARN "Use generic-lens or generic-optics with 'gatewayARN' instead." #-}

-- | The Amazon Resource Name (ARN) of the virtual tape you want to cancel archiving for.
--
-- /Note:/ Consider using 'tapeARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caTapeARN :: Lens.Lens' CancelArchival Lude.Text
caTapeARN = Lens.lens (tapeARN :: CancelArchival -> Lude.Text) (\s a -> s {tapeARN = a} :: CancelArchival)
{-# DEPRECATED caTapeARN "Use generic-lens or generic-optics with 'tapeARN' instead." #-}

instance Lude.AWSRequest CancelArchival where
  type Rs CancelArchival = CancelArchivalResponse
  request = Req.postJSON storageGatewayService
  response =
    Res.receiveJSON
      ( \s h x ->
          CancelArchivalResponse'
            Lude.<$> (x Lude..?> "TapeARN") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CancelArchival where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("StorageGateway_20130630.CancelArchival" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CancelArchival where
  toJSON CancelArchival' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("GatewayARN" Lude..= gatewayARN),
            Lude.Just ("TapeARN" Lude..= tapeARN)
          ]
      )

instance Lude.ToPath CancelArchival where
  toPath = Lude.const "/"

instance Lude.ToQuery CancelArchival where
  toQuery = Lude.const Lude.mempty

-- | CancelArchivalOutput
--
-- /See:/ 'mkCancelArchivalResponse' smart constructor.
data CancelArchivalResponse = CancelArchivalResponse'
  { tapeARN ::
      Lude.Maybe Lude.Text,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CancelArchivalResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'tapeARN' - The Amazon Resource Name (ARN) of the virtual tape for which archiving was canceled.
mkCancelArchivalResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CancelArchivalResponse
mkCancelArchivalResponse pResponseStatus_ =
  CancelArchivalResponse'
    { tapeARN = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The Amazon Resource Name (ARN) of the virtual tape for which archiving was canceled.
--
-- /Note:/ Consider using 'tapeARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
carsTapeARN :: Lens.Lens' CancelArchivalResponse (Lude.Maybe Lude.Text)
carsTapeARN = Lens.lens (tapeARN :: CancelArchivalResponse -> Lude.Maybe Lude.Text) (\s a -> s {tapeARN = a} :: CancelArchivalResponse)
{-# DEPRECATED carsTapeARN "Use generic-lens or generic-optics with 'tapeARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
carsResponseStatus :: Lens.Lens' CancelArchivalResponse Lude.Int
carsResponseStatus = Lens.lens (responseStatus :: CancelArchivalResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CancelArchivalResponse)
{-# DEPRECATED carsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
