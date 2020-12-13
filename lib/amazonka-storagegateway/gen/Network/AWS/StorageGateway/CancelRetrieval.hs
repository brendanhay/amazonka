{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.CancelRetrieval
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Cancels retrieval of a virtual tape from the virtual tape shelf (VTS) to a gateway after the retrieval process is initiated. The virtual tape is returned to the VTS. This operation is only supported in the tape gateway type.
module Network.AWS.StorageGateway.CancelRetrieval
  ( -- * Creating a request
    CancelRetrieval (..),
    mkCancelRetrieval,

    -- ** Request lenses
    crTapeARN,
    crGatewayARN,

    -- * Destructuring the response
    CancelRetrievalResponse (..),
    mkCancelRetrievalResponse,

    -- ** Response lenses
    crrsTapeARN,
    crrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.StorageGateway.Types

-- | CancelRetrievalInput
--
-- /See:/ 'mkCancelRetrieval' smart constructor.
data CancelRetrieval = CancelRetrieval'
  { -- | The Amazon Resource Name (ARN) of the virtual tape you want to cancel retrieval for.
    tapeARN :: Lude.Text,
    gatewayARN :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CancelRetrieval' with the minimum fields required to make a request.
--
-- * 'tapeARN' - The Amazon Resource Name (ARN) of the virtual tape you want to cancel retrieval for.
-- * 'gatewayARN' -
mkCancelRetrieval ::
  -- | 'tapeARN'
  Lude.Text ->
  -- | 'gatewayARN'
  Lude.Text ->
  CancelRetrieval
mkCancelRetrieval pTapeARN_ pGatewayARN_ =
  CancelRetrieval' {tapeARN = pTapeARN_, gatewayARN = pGatewayARN_}

-- | The Amazon Resource Name (ARN) of the virtual tape you want to cancel retrieval for.
--
-- /Note:/ Consider using 'tapeARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crTapeARN :: Lens.Lens' CancelRetrieval Lude.Text
crTapeARN = Lens.lens (tapeARN :: CancelRetrieval -> Lude.Text) (\s a -> s {tapeARN = a} :: CancelRetrieval)
{-# DEPRECATED crTapeARN "Use generic-lens or generic-optics with 'tapeARN' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'gatewayARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crGatewayARN :: Lens.Lens' CancelRetrieval Lude.Text
crGatewayARN = Lens.lens (gatewayARN :: CancelRetrieval -> Lude.Text) (\s a -> s {gatewayARN = a} :: CancelRetrieval)
{-# DEPRECATED crGatewayARN "Use generic-lens or generic-optics with 'gatewayARN' instead." #-}

instance Lude.AWSRequest CancelRetrieval where
  type Rs CancelRetrieval = CancelRetrievalResponse
  request = Req.postJSON storageGatewayService
  response =
    Res.receiveJSON
      ( \s h x ->
          CancelRetrievalResponse'
            Lude.<$> (x Lude..?> "TapeARN") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CancelRetrieval where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("StorageGateway_20130630.CancelRetrieval" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CancelRetrieval where
  toJSON CancelRetrieval' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("TapeARN" Lude..= tapeARN),
            Lude.Just ("GatewayARN" Lude..= gatewayARN)
          ]
      )

instance Lude.ToPath CancelRetrieval where
  toPath = Lude.const "/"

instance Lude.ToQuery CancelRetrieval where
  toQuery = Lude.const Lude.mempty

-- | CancelRetrievalOutput
--
-- /See:/ 'mkCancelRetrievalResponse' smart constructor.
data CancelRetrievalResponse = CancelRetrievalResponse'
  { -- | The Amazon Resource Name (ARN) of the virtual tape for which retrieval was canceled.
    tapeARN :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CancelRetrievalResponse' with the minimum fields required to make a request.
--
-- * 'tapeARN' - The Amazon Resource Name (ARN) of the virtual tape for which retrieval was canceled.
-- * 'responseStatus' - The response status code.
mkCancelRetrievalResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CancelRetrievalResponse
mkCancelRetrievalResponse pResponseStatus_ =
  CancelRetrievalResponse'
    { tapeARN = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The Amazon Resource Name (ARN) of the virtual tape for which retrieval was canceled.
--
-- /Note:/ Consider using 'tapeARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crrsTapeARN :: Lens.Lens' CancelRetrievalResponse (Lude.Maybe Lude.Text)
crrsTapeARN = Lens.lens (tapeARN :: CancelRetrievalResponse -> Lude.Maybe Lude.Text) (\s a -> s {tapeARN = a} :: CancelRetrievalResponse)
{-# DEPRECATED crrsTapeARN "Use generic-lens or generic-optics with 'tapeARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crrsResponseStatus :: Lens.Lens' CancelRetrievalResponse Lude.Int
crrsResponseStatus = Lens.lens (responseStatus :: CancelRetrievalResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CancelRetrievalResponse)
{-# DEPRECATED crrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
