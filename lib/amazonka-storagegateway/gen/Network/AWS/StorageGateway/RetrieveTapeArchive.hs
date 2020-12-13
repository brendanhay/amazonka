{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.RetrieveTapeArchive
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves an archived virtual tape from the virtual tape shelf (VTS) to a tape gateway. Virtual tapes archived in the VTS are not associated with any gateway. However after a tape is retrieved, it is associated with a gateway, even though it is also listed in the VTS, that is, archive. This operation is only supported in the tape gateway type.
--
-- Once a tape is successfully retrieved to a gateway, it cannot be retrieved again to another gateway. You must archive the tape again before you can retrieve it to another gateway. This operation is only supported in the tape gateway type.
module Network.AWS.StorageGateway.RetrieveTapeArchive
  ( -- * Creating a request
    RetrieveTapeArchive (..),
    mkRetrieveTapeArchive,

    -- ** Request lenses
    rtaTapeARN,
    rtaGatewayARN,

    -- * Destructuring the response
    RetrieveTapeArchiveResponse (..),
    mkRetrieveTapeArchiveResponse,

    -- ** Response lenses
    rtarsTapeARN,
    rtarsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.StorageGateway.Types

-- | RetrieveTapeArchiveInput
--
-- /See:/ 'mkRetrieveTapeArchive' smart constructor.
data RetrieveTapeArchive = RetrieveTapeArchive'
  { -- | The Amazon Resource Name (ARN) of the virtual tape you want to retrieve from the virtual tape shelf (VTS).
    tapeARN :: Lude.Text,
    -- | The Amazon Resource Name (ARN) of the gateway you want to retrieve the virtual tape to. Use the 'ListGateways' operation to return a list of gateways for your account and AWS Region.
    --
    -- You retrieve archived virtual tapes to only one gateway and the gateway must be a tape gateway.
    gatewayARN :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RetrieveTapeArchive' with the minimum fields required to make a request.
--
-- * 'tapeARN' - The Amazon Resource Name (ARN) of the virtual tape you want to retrieve from the virtual tape shelf (VTS).
-- * 'gatewayARN' - The Amazon Resource Name (ARN) of the gateway you want to retrieve the virtual tape to. Use the 'ListGateways' operation to return a list of gateways for your account and AWS Region.
--
-- You retrieve archived virtual tapes to only one gateway and the gateway must be a tape gateway.
mkRetrieveTapeArchive ::
  -- | 'tapeARN'
  Lude.Text ->
  -- | 'gatewayARN'
  Lude.Text ->
  RetrieveTapeArchive
mkRetrieveTapeArchive pTapeARN_ pGatewayARN_ =
  RetrieveTapeArchive'
    { tapeARN = pTapeARN_,
      gatewayARN = pGatewayARN_
    }

-- | The Amazon Resource Name (ARN) of the virtual tape you want to retrieve from the virtual tape shelf (VTS).
--
-- /Note:/ Consider using 'tapeARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtaTapeARN :: Lens.Lens' RetrieveTapeArchive Lude.Text
rtaTapeARN = Lens.lens (tapeARN :: RetrieveTapeArchive -> Lude.Text) (\s a -> s {tapeARN = a} :: RetrieveTapeArchive)
{-# DEPRECATED rtaTapeARN "Use generic-lens or generic-optics with 'tapeARN' instead." #-}

-- | The Amazon Resource Name (ARN) of the gateway you want to retrieve the virtual tape to. Use the 'ListGateways' operation to return a list of gateways for your account and AWS Region.
--
-- You retrieve archived virtual tapes to only one gateway and the gateway must be a tape gateway.
--
-- /Note:/ Consider using 'gatewayARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtaGatewayARN :: Lens.Lens' RetrieveTapeArchive Lude.Text
rtaGatewayARN = Lens.lens (gatewayARN :: RetrieveTapeArchive -> Lude.Text) (\s a -> s {gatewayARN = a} :: RetrieveTapeArchive)
{-# DEPRECATED rtaGatewayARN "Use generic-lens or generic-optics with 'gatewayARN' instead." #-}

instance Lude.AWSRequest RetrieveTapeArchive where
  type Rs RetrieveTapeArchive = RetrieveTapeArchiveResponse
  request = Req.postJSON storageGatewayService
  response =
    Res.receiveJSON
      ( \s h x ->
          RetrieveTapeArchiveResponse'
            Lude.<$> (x Lude..?> "TapeARN") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders RetrieveTapeArchive where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("StorageGateway_20130630.RetrieveTapeArchive" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON RetrieveTapeArchive where
  toJSON RetrieveTapeArchive' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("TapeARN" Lude..= tapeARN),
            Lude.Just ("GatewayARN" Lude..= gatewayARN)
          ]
      )

instance Lude.ToPath RetrieveTapeArchive where
  toPath = Lude.const "/"

instance Lude.ToQuery RetrieveTapeArchive where
  toQuery = Lude.const Lude.mempty

-- | RetrieveTapeArchiveOutput
--
-- /See:/ 'mkRetrieveTapeArchiveResponse' smart constructor.
data RetrieveTapeArchiveResponse = RetrieveTapeArchiveResponse'
  { -- | The Amazon Resource Name (ARN) of the retrieved virtual tape.
    tapeARN :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RetrieveTapeArchiveResponse' with the minimum fields required to make a request.
--
-- * 'tapeARN' - The Amazon Resource Name (ARN) of the retrieved virtual tape.
-- * 'responseStatus' - The response status code.
mkRetrieveTapeArchiveResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  RetrieveTapeArchiveResponse
mkRetrieveTapeArchiveResponse pResponseStatus_ =
  RetrieveTapeArchiveResponse'
    { tapeARN = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The Amazon Resource Name (ARN) of the retrieved virtual tape.
--
-- /Note:/ Consider using 'tapeARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtarsTapeARN :: Lens.Lens' RetrieveTapeArchiveResponse (Lude.Maybe Lude.Text)
rtarsTapeARN = Lens.lens (tapeARN :: RetrieveTapeArchiveResponse -> Lude.Maybe Lude.Text) (\s a -> s {tapeARN = a} :: RetrieveTapeArchiveResponse)
{-# DEPRECATED rtarsTapeARN "Use generic-lens or generic-optics with 'tapeARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtarsResponseStatus :: Lens.Lens' RetrieveTapeArchiveResponse Lude.Int
rtarsResponseStatus = Lens.lens (responseStatus :: RetrieveTapeArchiveResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: RetrieveTapeArchiveResponse)
{-# DEPRECATED rtarsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
