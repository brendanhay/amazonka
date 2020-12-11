{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.RetrieveTapeRecoveryPoint
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the recovery point for the specified virtual tape. This operation is only supported in the tape gateway type.
--
-- A recovery point is a point in time view of a virtual tape at which all the data on the tape is consistent. If your gateway crashes, virtual tapes that have recovery points can be recovered to a new gateway.
module Network.AWS.StorageGateway.RetrieveTapeRecoveryPoint
  ( -- * Creating a request
    RetrieveTapeRecoveryPoint (..),
    mkRetrieveTapeRecoveryPoint,

    -- ** Request lenses
    rtrpTapeARN,
    rtrpGatewayARN,

    -- * Destructuring the response
    RetrieveTapeRecoveryPointResponse (..),
    mkRetrieveTapeRecoveryPointResponse,

    -- ** Response lenses
    rtrprsTapeARN,
    rtrprsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.StorageGateway.Types

-- | RetrieveTapeRecoveryPointInput
--
-- /See:/ 'mkRetrieveTapeRecoveryPoint' smart constructor.
data RetrieveTapeRecoveryPoint = RetrieveTapeRecoveryPoint'
  { tapeARN ::
      Lude.Text,
    gatewayARN :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RetrieveTapeRecoveryPoint' with the minimum fields required to make a request.
--
-- * 'gatewayARN' - Undocumented field.
-- * 'tapeARN' - The Amazon Resource Name (ARN) of the virtual tape for which you want to retrieve the recovery point.
mkRetrieveTapeRecoveryPoint ::
  -- | 'tapeARN'
  Lude.Text ->
  -- | 'gatewayARN'
  Lude.Text ->
  RetrieveTapeRecoveryPoint
mkRetrieveTapeRecoveryPoint pTapeARN_ pGatewayARN_ =
  RetrieveTapeRecoveryPoint'
    { tapeARN = pTapeARN_,
      gatewayARN = pGatewayARN_
    }

-- | The Amazon Resource Name (ARN) of the virtual tape for which you want to retrieve the recovery point.
--
-- /Note:/ Consider using 'tapeARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtrpTapeARN :: Lens.Lens' RetrieveTapeRecoveryPoint Lude.Text
rtrpTapeARN = Lens.lens (tapeARN :: RetrieveTapeRecoveryPoint -> Lude.Text) (\s a -> s {tapeARN = a} :: RetrieveTapeRecoveryPoint)
{-# DEPRECATED rtrpTapeARN "Use generic-lens or generic-optics with 'tapeARN' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'gatewayARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtrpGatewayARN :: Lens.Lens' RetrieveTapeRecoveryPoint Lude.Text
rtrpGatewayARN = Lens.lens (gatewayARN :: RetrieveTapeRecoveryPoint -> Lude.Text) (\s a -> s {gatewayARN = a} :: RetrieveTapeRecoveryPoint)
{-# DEPRECATED rtrpGatewayARN "Use generic-lens or generic-optics with 'gatewayARN' instead." #-}

instance Lude.AWSRequest RetrieveTapeRecoveryPoint where
  type
    Rs RetrieveTapeRecoveryPoint =
      RetrieveTapeRecoveryPointResponse
  request = Req.postJSON storageGatewayService
  response =
    Res.receiveJSON
      ( \s h x ->
          RetrieveTapeRecoveryPointResponse'
            Lude.<$> (x Lude..?> "TapeARN") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders RetrieveTapeRecoveryPoint where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "StorageGateway_20130630.RetrieveTapeRecoveryPoint" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON RetrieveTapeRecoveryPoint where
  toJSON RetrieveTapeRecoveryPoint' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("TapeARN" Lude..= tapeARN),
            Lude.Just ("GatewayARN" Lude..= gatewayARN)
          ]
      )

instance Lude.ToPath RetrieveTapeRecoveryPoint where
  toPath = Lude.const "/"

instance Lude.ToQuery RetrieveTapeRecoveryPoint where
  toQuery = Lude.const Lude.mempty

-- | RetrieveTapeRecoveryPointOutput
--
-- /See:/ 'mkRetrieveTapeRecoveryPointResponse' smart constructor.
data RetrieveTapeRecoveryPointResponse = RetrieveTapeRecoveryPointResponse'
  { tapeARN ::
      Lude.Maybe Lude.Text,
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RetrieveTapeRecoveryPointResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'tapeARN' - The Amazon Resource Name (ARN) of the virtual tape for which the recovery point was retrieved.
mkRetrieveTapeRecoveryPointResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  RetrieveTapeRecoveryPointResponse
mkRetrieveTapeRecoveryPointResponse pResponseStatus_ =
  RetrieveTapeRecoveryPointResponse'
    { tapeARN = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The Amazon Resource Name (ARN) of the virtual tape for which the recovery point was retrieved.
--
-- /Note:/ Consider using 'tapeARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtrprsTapeARN :: Lens.Lens' RetrieveTapeRecoveryPointResponse (Lude.Maybe Lude.Text)
rtrprsTapeARN = Lens.lens (tapeARN :: RetrieveTapeRecoveryPointResponse -> Lude.Maybe Lude.Text) (\s a -> s {tapeARN = a} :: RetrieveTapeRecoveryPointResponse)
{-# DEPRECATED rtrprsTapeARN "Use generic-lens or generic-optics with 'tapeARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtrprsResponseStatus :: Lens.Lens' RetrieveTapeRecoveryPointResponse Lude.Int
rtrprsResponseStatus = Lens.lens (responseStatus :: RetrieveTapeRecoveryPointResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: RetrieveTapeRecoveryPointResponse)
{-# DEPRECATED rtrprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
