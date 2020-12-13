{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
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
    dtfTapeARN,
    dtfGatewayARN,
    dtfBypassGovernanceRetention,

    -- * Destructuring the response
    DeleteTapeResponse (..),
    mkDeleteTapeResponse,

    -- ** Response lenses
    dtfrsTapeARN,
    dtfrsResponseStatus,
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
  { -- | The Amazon Resource Name (ARN) of the virtual tape to delete.
    tapeARN :: Lude.Text,
    -- | The unique Amazon Resource Name (ARN) of the gateway that the virtual tape to delete is associated with. Use the 'ListGateways' operation to return a list of gateways for your account and AWS Region.
    gatewayARN :: Lude.Text,
    -- | Set to @TRUE@ to delete an archived tape that belongs to a custom pool with tape retention lock. Only archived tapes with tape retention lock set to @governance@ can be deleted. Archived tapes with tape retention lock set to @compliance@ can't be deleted.
    bypassGovernanceRetention :: Lude.Maybe Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteTape' with the minimum fields required to make a request.
--
-- * 'tapeARN' - The Amazon Resource Name (ARN) of the virtual tape to delete.
-- * 'gatewayARN' - The unique Amazon Resource Name (ARN) of the gateway that the virtual tape to delete is associated with. Use the 'ListGateways' operation to return a list of gateways for your account and AWS Region.
-- * 'bypassGovernanceRetention' - Set to @TRUE@ to delete an archived tape that belongs to a custom pool with tape retention lock. Only archived tapes with tape retention lock set to @governance@ can be deleted. Archived tapes with tape retention lock set to @compliance@ can't be deleted.
mkDeleteTape ::
  -- | 'tapeARN'
  Lude.Text ->
  -- | 'gatewayARN'
  Lude.Text ->
  DeleteTape
mkDeleteTape pTapeARN_ pGatewayARN_ =
  DeleteTape'
    { tapeARN = pTapeARN_,
      gatewayARN = pGatewayARN_,
      bypassGovernanceRetention = Lude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the virtual tape to delete.
--
-- /Note:/ Consider using 'tapeARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtfTapeARN :: Lens.Lens' DeleteTape Lude.Text
dtfTapeARN = Lens.lens (tapeARN :: DeleteTape -> Lude.Text) (\s a -> s {tapeARN = a} :: DeleteTape)
{-# DEPRECATED dtfTapeARN "Use generic-lens or generic-optics with 'tapeARN' instead." #-}

-- | The unique Amazon Resource Name (ARN) of the gateway that the virtual tape to delete is associated with. Use the 'ListGateways' operation to return a list of gateways for your account and AWS Region.
--
-- /Note:/ Consider using 'gatewayARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtfGatewayARN :: Lens.Lens' DeleteTape Lude.Text
dtfGatewayARN = Lens.lens (gatewayARN :: DeleteTape -> Lude.Text) (\s a -> s {gatewayARN = a} :: DeleteTape)
{-# DEPRECATED dtfGatewayARN "Use generic-lens or generic-optics with 'gatewayARN' instead." #-}

-- | Set to @TRUE@ to delete an archived tape that belongs to a custom pool with tape retention lock. Only archived tapes with tape retention lock set to @governance@ can be deleted. Archived tapes with tape retention lock set to @compliance@ can't be deleted.
--
-- /Note:/ Consider using 'bypassGovernanceRetention' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtfBypassGovernanceRetention :: Lens.Lens' DeleteTape (Lude.Maybe Lude.Bool)
dtfBypassGovernanceRetention = Lens.lens (bypassGovernanceRetention :: DeleteTape -> Lude.Maybe Lude.Bool) (\s a -> s {bypassGovernanceRetention = a} :: DeleteTape)
{-# DEPRECATED dtfBypassGovernanceRetention "Use generic-lens or generic-optics with 'bypassGovernanceRetention' instead." #-}

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
          [ Lude.Just ("TapeARN" Lude..= tapeARN),
            Lude.Just ("GatewayARN" Lude..= gatewayARN),
            ("BypassGovernanceRetention" Lude..=)
              Lude.<$> bypassGovernanceRetention
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
  { -- | The Amazon Resource Name (ARN) of the deleted virtual tape.
    tapeARN :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteTapeResponse' with the minimum fields required to make a request.
--
-- * 'tapeARN' - The Amazon Resource Name (ARN) of the deleted virtual tape.
-- * 'responseStatus' - The response status code.
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
dtfrsTapeARN :: Lens.Lens' DeleteTapeResponse (Lude.Maybe Lude.Text)
dtfrsTapeARN = Lens.lens (tapeARN :: DeleteTapeResponse -> Lude.Maybe Lude.Text) (\s a -> s {tapeARN = a} :: DeleteTapeResponse)
{-# DEPRECATED dtfrsTapeARN "Use generic-lens or generic-optics with 'tapeARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtfrsResponseStatus :: Lens.Lens' DeleteTapeResponse Lude.Int
dtfrsResponseStatus = Lens.lens (responseStatus :: DeleteTapeResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteTapeResponse)
{-# DEPRECATED dtfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
