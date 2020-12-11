{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DeleteEgressOnlyInternetGateway
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an egress-only internet gateway.
module Network.AWS.EC2.DeleteEgressOnlyInternetGateway
  ( -- * Creating a request
    DeleteEgressOnlyInternetGateway (..),
    mkDeleteEgressOnlyInternetGateway,

    -- ** Request lenses
    deoigeDryRun,
    deoigeEgressOnlyInternetGatewayId,

    -- * Destructuring the response
    DeleteEgressOnlyInternetGatewayResponse (..),
    mkDeleteEgressOnlyInternetGatewayResponse,

    -- ** Response lenses
    deoigersReturnCode,
    deoigersResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteEgressOnlyInternetGateway' smart constructor.
data DeleteEgressOnlyInternetGateway = DeleteEgressOnlyInternetGateway'
  { dryRun ::
      Lude.Maybe Lude.Bool,
    egressOnlyInternetGatewayId ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteEgressOnlyInternetGateway' with the minimum fields required to make a request.
--
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'egressOnlyInternetGatewayId' - The ID of the egress-only internet gateway.
mkDeleteEgressOnlyInternetGateway ::
  -- | 'egressOnlyInternetGatewayId'
  Lude.Text ->
  DeleteEgressOnlyInternetGateway
mkDeleteEgressOnlyInternetGateway pEgressOnlyInternetGatewayId_ =
  DeleteEgressOnlyInternetGateway'
    { dryRun = Lude.Nothing,
      egressOnlyInternetGatewayId = pEgressOnlyInternetGatewayId_
    }

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deoigeDryRun :: Lens.Lens' DeleteEgressOnlyInternetGateway (Lude.Maybe Lude.Bool)
deoigeDryRun = Lens.lens (dryRun :: DeleteEgressOnlyInternetGateway -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: DeleteEgressOnlyInternetGateway)
{-# DEPRECATED deoigeDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The ID of the egress-only internet gateway.
--
-- /Note:/ Consider using 'egressOnlyInternetGatewayId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deoigeEgressOnlyInternetGatewayId :: Lens.Lens' DeleteEgressOnlyInternetGateway Lude.Text
deoigeEgressOnlyInternetGatewayId = Lens.lens (egressOnlyInternetGatewayId :: DeleteEgressOnlyInternetGateway -> Lude.Text) (\s a -> s {egressOnlyInternetGatewayId = a} :: DeleteEgressOnlyInternetGateway)
{-# DEPRECATED deoigeEgressOnlyInternetGatewayId "Use generic-lens or generic-optics with 'egressOnlyInternetGatewayId' instead." #-}

instance Lude.AWSRequest DeleteEgressOnlyInternetGateway where
  type
    Rs DeleteEgressOnlyInternetGateway =
      DeleteEgressOnlyInternetGatewayResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          DeleteEgressOnlyInternetGatewayResponse'
            Lude.<$> (x Lude..@? "returnCode") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteEgressOnlyInternetGateway where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DeleteEgressOnlyInternetGateway where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteEgressOnlyInternetGateway where
  toQuery DeleteEgressOnlyInternetGateway' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("DeleteEgressOnlyInternetGateway" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "DryRun" Lude.=: dryRun,
        "EgressOnlyInternetGatewayId" Lude.=: egressOnlyInternetGatewayId
      ]

-- | /See:/ 'mkDeleteEgressOnlyInternetGatewayResponse' smart constructor.
data DeleteEgressOnlyInternetGatewayResponse = DeleteEgressOnlyInternetGatewayResponse'
  { returnCode ::
      Lude.Maybe
        Lude.Bool,
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

-- | Creates a value of 'DeleteEgressOnlyInternetGatewayResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'returnCode' - Returns @true@ if the request succeeds; otherwise, it returns an error.
mkDeleteEgressOnlyInternetGatewayResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteEgressOnlyInternetGatewayResponse
mkDeleteEgressOnlyInternetGatewayResponse pResponseStatus_ =
  DeleteEgressOnlyInternetGatewayResponse'
    { returnCode =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Returns @true@ if the request succeeds; otherwise, it returns an error.
--
-- /Note:/ Consider using 'returnCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deoigersReturnCode :: Lens.Lens' DeleteEgressOnlyInternetGatewayResponse (Lude.Maybe Lude.Bool)
deoigersReturnCode = Lens.lens (returnCode :: DeleteEgressOnlyInternetGatewayResponse -> Lude.Maybe Lude.Bool) (\s a -> s {returnCode = a} :: DeleteEgressOnlyInternetGatewayResponse)
{-# DEPRECATED deoigersReturnCode "Use generic-lens or generic-optics with 'returnCode' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deoigersResponseStatus :: Lens.Lens' DeleteEgressOnlyInternetGatewayResponse Lude.Int
deoigersResponseStatus = Lens.lens (responseStatus :: DeleteEgressOnlyInternetGatewayResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteEgressOnlyInternetGatewayResponse)
{-# DEPRECATED deoigersResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
