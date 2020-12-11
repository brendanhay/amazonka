{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DeleteTransitGatewayRouteTable
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified transit gateway route table. You must disassociate the route table from any transit gateway route tables before you can delete it.
module Network.AWS.EC2.DeleteTransitGatewayRouteTable
  ( -- * Creating a request
    DeleteTransitGatewayRouteTable (..),
    mkDeleteTransitGatewayRouteTable,

    -- ** Request lenses
    dtgrtDryRun,
    dtgrtTransitGatewayRouteTableId,

    -- * Destructuring the response
    DeleteTransitGatewayRouteTableResponse (..),
    mkDeleteTransitGatewayRouteTableResponse,

    -- ** Response lenses
    dtgrtrsTransitGatewayRouteTable,
    dtgrtrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteTransitGatewayRouteTable' smart constructor.
data DeleteTransitGatewayRouteTable = DeleteTransitGatewayRouteTable'
  { dryRun ::
      Lude.Maybe Lude.Bool,
    transitGatewayRouteTableId ::
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

-- | Creates a value of 'DeleteTransitGatewayRouteTable' with the minimum fields required to make a request.
--
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'transitGatewayRouteTableId' - The ID of the transit gateway route table.
mkDeleteTransitGatewayRouteTable ::
  -- | 'transitGatewayRouteTableId'
  Lude.Text ->
  DeleteTransitGatewayRouteTable
mkDeleteTransitGatewayRouteTable pTransitGatewayRouteTableId_ =
  DeleteTransitGatewayRouteTable'
    { dryRun = Lude.Nothing,
      transitGatewayRouteTableId = pTransitGatewayRouteTableId_
    }

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgrtDryRun :: Lens.Lens' DeleteTransitGatewayRouteTable (Lude.Maybe Lude.Bool)
dtgrtDryRun = Lens.lens (dryRun :: DeleteTransitGatewayRouteTable -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: DeleteTransitGatewayRouteTable)
{-# DEPRECATED dtgrtDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The ID of the transit gateway route table.
--
-- /Note:/ Consider using 'transitGatewayRouteTableId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgrtTransitGatewayRouteTableId :: Lens.Lens' DeleteTransitGatewayRouteTable Lude.Text
dtgrtTransitGatewayRouteTableId = Lens.lens (transitGatewayRouteTableId :: DeleteTransitGatewayRouteTable -> Lude.Text) (\s a -> s {transitGatewayRouteTableId = a} :: DeleteTransitGatewayRouteTable)
{-# DEPRECATED dtgrtTransitGatewayRouteTableId "Use generic-lens or generic-optics with 'transitGatewayRouteTableId' instead." #-}

instance Lude.AWSRequest DeleteTransitGatewayRouteTable where
  type
    Rs DeleteTransitGatewayRouteTable =
      DeleteTransitGatewayRouteTableResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          DeleteTransitGatewayRouteTableResponse'
            Lude.<$> (x Lude..@? "transitGatewayRouteTable")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteTransitGatewayRouteTable where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DeleteTransitGatewayRouteTable where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteTransitGatewayRouteTable where
  toQuery DeleteTransitGatewayRouteTable' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("DeleteTransitGatewayRouteTable" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "DryRun" Lude.=: dryRun,
        "TransitGatewayRouteTableId" Lude.=: transitGatewayRouteTableId
      ]

-- | /See:/ 'mkDeleteTransitGatewayRouteTableResponse' smart constructor.
data DeleteTransitGatewayRouteTableResponse = DeleteTransitGatewayRouteTableResponse'
  { transitGatewayRouteTable ::
      Lude.Maybe
        TransitGatewayRouteTable,
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

-- | Creates a value of 'DeleteTransitGatewayRouteTableResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'transitGatewayRouteTable' - Information about the deleted transit gateway route table.
mkDeleteTransitGatewayRouteTableResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteTransitGatewayRouteTableResponse
mkDeleteTransitGatewayRouteTableResponse pResponseStatus_ =
  DeleteTransitGatewayRouteTableResponse'
    { transitGatewayRouteTable =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the deleted transit gateway route table.
--
-- /Note:/ Consider using 'transitGatewayRouteTable' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgrtrsTransitGatewayRouteTable :: Lens.Lens' DeleteTransitGatewayRouteTableResponse (Lude.Maybe TransitGatewayRouteTable)
dtgrtrsTransitGatewayRouteTable = Lens.lens (transitGatewayRouteTable :: DeleteTransitGatewayRouteTableResponse -> Lude.Maybe TransitGatewayRouteTable) (\s a -> s {transitGatewayRouteTable = a} :: DeleteTransitGatewayRouteTableResponse)
{-# DEPRECATED dtgrtrsTransitGatewayRouteTable "Use generic-lens or generic-optics with 'transitGatewayRouteTable' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgrtrsResponseStatus :: Lens.Lens' DeleteTransitGatewayRouteTableResponse Lude.Int
dtgrtrsResponseStatus = Lens.lens (responseStatus :: DeleteTransitGatewayRouteTableResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteTransitGatewayRouteTableResponse)
{-# DEPRECATED dtgrtrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
