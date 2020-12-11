{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.DeleteTape
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified virtual tape. This operation is only supported in the tape gateway type.
module Network.AWS.StorageGateway.DeleteTape
  ( -- * Creating a request
    DeleteTape (..),
    mkDeleteTape,

    -- ** Request lenses
    delBypassGovernanceRetention,
    delGatewayARN,
    delTapeARN,

    -- * Destructuring the response
    DeleteTapeResponse (..),
    mkDeleteTapeResponse,

    -- ** Response lenses
    dtrsTapeARN,
    dtrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.StorageGateway.Types

-- | DeleteTapeInput
--
-- /See:/ 'mkDeleteTape' smart constructor.
data DeleteTape = DeleteTape'
  { bypassGovernanceRetention ::
      Lude.Maybe Lude.Bool,
    gatewayARN :: Lude.Text,
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

-- | Creates a value of 'DeleteTape' with the minimum fields required to make a request.
--
-- * 'bypassGovernanceRetention' - Set to @TRUE@ to delete an archived tape that belongs to a custom pool with tape retention lock. Only archived tapes with tape retention lock set to @governance@ can be deleted. Archived tapes with tape retention lock set to @compliance@ can't be deleted.
-- * 'gatewayARN' - The unique Amazon Resource Name (ARN) of the gateway that the virtual tape to delete is associated with. Use the 'ListGateways' operation to return a list of gateways for your account and AWS Region.
-- * 'tapeARN' - The Amazon Resource Name (ARN) of the virtual tape to delete.
mkDeleteTape ::
  -- | 'gatewayARN'
  Lude.Text ->
  -- | 'tapeARN'
  Lude.Text ->
  DeleteTape
mkDeleteTape pGatewayARN_ pTapeARN_ =
  DeleteTape'
    { bypassGovernanceRetention = Lude.Nothing,
      gatewayARN = pGatewayARN_,
      tapeARN = pTapeARN_
    }

-- | Set to @TRUE@ to delete an archived tape that belongs to a custom pool with tape retention lock. Only archived tapes with tape retention lock set to @governance@ can be deleted. Archived tapes with tape retention lock set to @compliance@ can't be deleted.
--
-- /Note:/ Consider using 'bypassGovernanceRetention' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
delBypassGovernanceRetention :: Lens.Lens' DeleteTape (Lude.Maybe Lude.Bool)
delBypassGovernanceRetention = Lens.lens (bypassGovernanceRetention :: DeleteTape -> Lude.Maybe Lude.Bool) (\s a -> s {bypassGovernanceRetention = a} :: DeleteTape)
{-# DEPRECATED delBypassGovernanceRetention "Use generic-lens or generic-optics with 'bypassGovernanceRetention' instead." #-}

-- | The unique Amazon Resource Name (ARN) of the gateway that the virtual tape to delete is associated with. Use the 'ListGateways' operation to return a list of gateways for your account and AWS Region.
--
-- /Note:/ Consider using 'gatewayARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
delGatewayARN :: Lens.Lens' DeleteTape Lude.Text
delGatewayARN = Lens.lens (gatewayARN :: DeleteTape -> Lude.Text) (\s a -> s {gatewayARN = a} :: DeleteTape)
{-# DEPRECATED delGatewayARN "Use generic-lens or generic-optics with 'gatewayARN' instead." #-}

-- | The Amazon Resource Name (ARN) of the virtual tape to delete.
--
-- /Note:/ Consider using 'tapeARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
delTapeARN :: Lens.Lens' DeleteTape Lude.Text
delTapeARN = Lens.lens (tapeARN :: DeleteTape -> Lude.Text) (\s a -> s {tapeARN = a} :: DeleteTape)
{-# DEPRECATED delTapeARN "Use generic-lens or generic-optics with 'tapeARN' instead." #-}

instance Lude.AWSRequest DeleteTape where
  type Rs DeleteTape = DeleteTapeResponse
  request = Req.postJSON storageGatewayService
  response =
    Res.receiveJSON
      ( \s h x ->
          DeleteTapeResponse'
            Lude.<$> (x Lude..?> "TapeARN") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteTape where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("StorageGateway_20130630.DeleteTape" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteTape where
  toJSON DeleteTape' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("BypassGovernanceRetention" Lude..=)
              Lude.<$> bypassGovernanceRetention,
            Lude.Just ("GatewayARN" Lude..= gatewayARN),
            Lude.Just ("TapeARN" Lude..= tapeARN)
          ]
      )

instance Lude.ToPath DeleteTape where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteTape where
  toQuery = Lude.const Lude.mempty

-- | DeleteTapeOutput
--
-- /See:/ 'mkDeleteTapeResponse' smart constructor.
data DeleteTapeResponse = DeleteTapeResponse'
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

-- | Creates a value of 'DeleteTapeResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'tapeARN' - The Amazon Resource Name (ARN) of the deleted virtual tape.
mkDeleteTapeResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteTapeResponse
mkDeleteTapeResponse pResponseStatus_ =
  DeleteTapeResponse'
    { tapeARN = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The Amazon Resource Name (ARN) of the deleted virtual tape.
--
-- /Note:/ Consider using 'tapeARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtrsTapeARN :: Lens.Lens' DeleteTapeResponse (Lude.Maybe Lude.Text)
dtrsTapeARN = Lens.lens (tapeARN :: DeleteTapeResponse -> Lude.Maybe Lude.Text) (\s a -> s {tapeARN = a} :: DeleteTapeResponse)
{-# DEPRECATED dtrsTapeARN "Use generic-lens or generic-optics with 'tapeARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtrsResponseStatus :: Lens.Lens' DeleteTapeResponse Lude.Int
dtrsResponseStatus = Lens.lens (responseStatus :: DeleteTapeResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteTapeResponse)
{-# DEPRECATED dtrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
