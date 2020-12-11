{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DeleteCustomerGateway
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified customer gateway. You must delete the VPN connection before you can delete the customer gateway.
module Network.AWS.EC2.DeleteCustomerGateway
  ( -- * Creating a request
    DeleteCustomerGateway (..),
    mkDeleteCustomerGateway,

    -- ** Request lenses
    deletecustomergatewayeDryRun,
    deletecustomergatewayeCustomerGatewayId,

    -- * Destructuring the response
    DeleteCustomerGatewayResponse (..),
    mkDeleteCustomerGatewayResponse,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Contains the parameters for DeleteCustomerGateway.
--
-- /See:/ 'mkDeleteCustomerGateway' smart constructor.
data DeleteCustomerGateway = DeleteCustomerGateway'
  { dryRun ::
      Lude.Maybe Lude.Bool,
    customerGatewayId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteCustomerGateway' with the minimum fields required to make a request.
--
-- * 'customerGatewayId' - The ID of the customer gateway.
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
mkDeleteCustomerGateway ::
  -- | 'customerGatewayId'
  Lude.Text ->
  DeleteCustomerGateway
mkDeleteCustomerGateway pCustomerGatewayId_ =
  DeleteCustomerGateway'
    { dryRun = Lude.Nothing,
      customerGatewayId = pCustomerGatewayId_
    }

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deletecustomergatewayeDryRun :: Lens.Lens' DeleteCustomerGateway (Lude.Maybe Lude.Bool)
deletecustomergatewayeDryRun = Lens.lens (dryRun :: DeleteCustomerGateway -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: DeleteCustomerGateway)
{-# DEPRECATED deletecustomergatewayeDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The ID of the customer gateway.
--
-- /Note:/ Consider using 'customerGatewayId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deletecustomergatewayeCustomerGatewayId :: Lens.Lens' DeleteCustomerGateway Lude.Text
deletecustomergatewayeCustomerGatewayId = Lens.lens (customerGatewayId :: DeleteCustomerGateway -> Lude.Text) (\s a -> s {customerGatewayId = a} :: DeleteCustomerGateway)
{-# DEPRECATED deletecustomergatewayeCustomerGatewayId "Use generic-lens or generic-optics with 'customerGatewayId' instead." #-}

instance Lude.AWSRequest DeleteCustomerGateway where
  type Rs DeleteCustomerGateway = DeleteCustomerGatewayResponse
  request = Req.postQuery ec2Service
  response = Res.receiveNull DeleteCustomerGatewayResponse'

instance Lude.ToHeaders DeleteCustomerGateway where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DeleteCustomerGateway where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteCustomerGateway where
  toQuery DeleteCustomerGateway' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DeleteCustomerGateway" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "DryRun" Lude.=: dryRun,
        "CustomerGatewayId" Lude.=: customerGatewayId
      ]

-- | /See:/ 'mkDeleteCustomerGatewayResponse' smart constructor.
data DeleteCustomerGatewayResponse = DeleteCustomerGatewayResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteCustomerGatewayResponse' with the minimum fields required to make a request.
mkDeleteCustomerGatewayResponse ::
  DeleteCustomerGatewayResponse
mkDeleteCustomerGatewayResponse = DeleteCustomerGatewayResponse'
