{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.PeerVPC
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Tries to peer the Lightsail VPC with the user's default VPC.
module Network.AWS.Lightsail.PeerVPC
  ( -- * Creating a request
    PeerVPC (..),
    mkPeerVPC,

    -- * Destructuring the response
    PeerVPCResponse (..),
    mkPeerVPCResponse,

    -- ** Response lenses
    pvrsOperation,
    pvrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkPeerVPC' smart constructor.
data PeerVPC = PeerVPC'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PeerVPC' with the minimum fields required to make a request.
mkPeerVPC ::
  PeerVPC
mkPeerVPC = PeerVPC'

instance Lude.AWSRequest PeerVPC where
  type Rs PeerVPC = PeerVPCResponse
  request = Req.postJSON lightsailService
  response =
    Res.receiveJSON
      ( \s h x ->
          PeerVPCResponse'
            Lude.<$> (x Lude..?> "operation") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders PeerVPC where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Lightsail_20161128.PeerVpc" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON PeerVPC where
  toJSON = Lude.const (Lude.Object Lude.mempty)

instance Lude.ToPath PeerVPC where
  toPath = Lude.const "/"

instance Lude.ToQuery PeerVPC where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkPeerVPCResponse' smart constructor.
data PeerVPCResponse = PeerVPCResponse'
  { -- | An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
    operation :: Lude.Maybe Operation,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PeerVPCResponse' with the minimum fields required to make a request.
--
-- * 'operation' - An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
-- * 'responseStatus' - The response status code.
mkPeerVPCResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  PeerVPCResponse
mkPeerVPCResponse pResponseStatus_ =
  PeerVPCResponse'
    { operation = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
--
-- /Note:/ Consider using 'operation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pvrsOperation :: Lens.Lens' PeerVPCResponse (Lude.Maybe Operation)
pvrsOperation = Lens.lens (operation :: PeerVPCResponse -> Lude.Maybe Operation) (\s a -> s {operation = a} :: PeerVPCResponse)
{-# DEPRECATED pvrsOperation "Use generic-lens or generic-optics with 'operation' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pvrsResponseStatus :: Lens.Lens' PeerVPCResponse Lude.Int
pvrsResponseStatus = Lens.lens (responseStatus :: PeerVPCResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: PeerVPCResponse)
{-# DEPRECATED pvrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
