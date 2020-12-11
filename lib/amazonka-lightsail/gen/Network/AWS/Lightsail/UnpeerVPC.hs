{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.UnpeerVPC
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Attempts to unpeer the Lightsail VPC from the user's default VPC.
module Network.AWS.Lightsail.UnpeerVPC
  ( -- * Creating a request
    UnpeerVPC (..),
    mkUnpeerVPC,

    -- * Destructuring the response
    UnpeerVPCResponse (..),
    mkUnpeerVPCResponse,

    -- ** Response lenses
    uvrsOperation,
    uvrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUnpeerVPC' smart constructor.
data UnpeerVPC = UnpeerVPC'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UnpeerVPC' with the minimum fields required to make a request.
mkUnpeerVPC ::
  UnpeerVPC
mkUnpeerVPC = UnpeerVPC'

instance Lude.AWSRequest UnpeerVPC where
  type Rs UnpeerVPC = UnpeerVPCResponse
  request = Req.postJSON lightsailService
  response =
    Res.receiveJSON
      ( \s h x ->
          UnpeerVPCResponse'
            Lude.<$> (x Lude..?> "operation") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UnpeerVPC where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Lightsail_20161128.UnpeerVpc" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UnpeerVPC where
  toJSON = Lude.const (Lude.Object Lude.mempty)

instance Lude.ToPath UnpeerVPC where
  toPath = Lude.const "/"

instance Lude.ToQuery UnpeerVPC where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUnpeerVPCResponse' smart constructor.
data UnpeerVPCResponse = UnpeerVPCResponse'
  { operation ::
      Lude.Maybe Operation,
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

-- | Creates a value of 'UnpeerVPCResponse' with the minimum fields required to make a request.
--
-- * 'operation' - An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
-- * 'responseStatus' - The response status code.
mkUnpeerVPCResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UnpeerVPCResponse
mkUnpeerVPCResponse pResponseStatus_ =
  UnpeerVPCResponse'
    { operation = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
--
-- /Note:/ Consider using 'operation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uvrsOperation :: Lens.Lens' UnpeerVPCResponse (Lude.Maybe Operation)
uvrsOperation = Lens.lens (operation :: UnpeerVPCResponse -> Lude.Maybe Operation) (\s a -> s {operation = a} :: UnpeerVPCResponse)
{-# DEPRECATED uvrsOperation "Use generic-lens or generic-optics with 'operation' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uvrsResponseStatus :: Lens.Lens' UnpeerVPCResponse Lude.Int
uvrsResponseStatus = Lens.lens (responseStatus :: UnpeerVPCResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UnpeerVPCResponse)
{-# DEPRECATED uvrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
