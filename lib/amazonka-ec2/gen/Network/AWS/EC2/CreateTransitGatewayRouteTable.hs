{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.CreateTransitGatewayRouteTable
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a route table for the specified transit gateway.
module Network.AWS.EC2.CreateTransitGatewayRouteTable
  ( -- * Creating a request
    CreateTransitGatewayRouteTable (..),
    mkCreateTransitGatewayRouteTable,

    -- ** Request lenses
    ctgrtTagSpecifications,
    ctgrtDryRun,
    ctgrtTransitGatewayId,

    -- * Destructuring the response
    CreateTransitGatewayRouteTableResponse (..),
    mkCreateTransitGatewayRouteTableResponse,

    -- ** Response lenses
    ctgrtrsTransitGatewayRouteTable,
    ctgrtrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateTransitGatewayRouteTable' smart constructor.
data CreateTransitGatewayRouteTable = CreateTransitGatewayRouteTable'
  { tagSpecifications ::
      Lude.Maybe [TagSpecification],
    dryRun ::
      Lude.Maybe Lude.Bool,
    transitGatewayId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateTransitGatewayRouteTable' with the minimum fields required to make a request.
--
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'tagSpecifications' - The tags to apply to the transit gateway route table.
-- * 'transitGatewayId' - The ID of the transit gateway.
mkCreateTransitGatewayRouteTable ::
  -- | 'transitGatewayId'
  Lude.Text ->
  CreateTransitGatewayRouteTable
mkCreateTransitGatewayRouteTable pTransitGatewayId_ =
  CreateTransitGatewayRouteTable'
    { tagSpecifications = Lude.Nothing,
      dryRun = Lude.Nothing,
      transitGatewayId = pTransitGatewayId_
    }

-- | The tags to apply to the transit gateway route table.
--
-- /Note:/ Consider using 'tagSpecifications' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctgrtTagSpecifications :: Lens.Lens' CreateTransitGatewayRouteTable (Lude.Maybe [TagSpecification])
ctgrtTagSpecifications = Lens.lens (tagSpecifications :: CreateTransitGatewayRouteTable -> Lude.Maybe [TagSpecification]) (\s a -> s {tagSpecifications = a} :: CreateTransitGatewayRouteTable)
{-# DEPRECATED ctgrtTagSpecifications "Use generic-lens or generic-optics with 'tagSpecifications' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctgrtDryRun :: Lens.Lens' CreateTransitGatewayRouteTable (Lude.Maybe Lude.Bool)
ctgrtDryRun = Lens.lens (dryRun :: CreateTransitGatewayRouteTable -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: CreateTransitGatewayRouteTable)
{-# DEPRECATED ctgrtDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The ID of the transit gateway.
--
-- /Note:/ Consider using 'transitGatewayId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctgrtTransitGatewayId :: Lens.Lens' CreateTransitGatewayRouteTable Lude.Text
ctgrtTransitGatewayId = Lens.lens (transitGatewayId :: CreateTransitGatewayRouteTable -> Lude.Text) (\s a -> s {transitGatewayId = a} :: CreateTransitGatewayRouteTable)
{-# DEPRECATED ctgrtTransitGatewayId "Use generic-lens or generic-optics with 'transitGatewayId' instead." #-}

instance Lude.AWSRequest CreateTransitGatewayRouteTable where
  type
    Rs CreateTransitGatewayRouteTable =
      CreateTransitGatewayRouteTableResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          CreateTransitGatewayRouteTableResponse'
            Lude.<$> (x Lude..@? "transitGatewayRouteTable")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateTransitGatewayRouteTable where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath CreateTransitGatewayRouteTable where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateTransitGatewayRouteTable where
  toQuery CreateTransitGatewayRouteTable' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("CreateTransitGatewayRouteTable" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        Lude.toQuery
          (Lude.toQueryList "TagSpecifications" Lude.<$> tagSpecifications),
        "DryRun" Lude.=: dryRun,
        "TransitGatewayId" Lude.=: transitGatewayId
      ]

-- | /See:/ 'mkCreateTransitGatewayRouteTableResponse' smart constructor.
data CreateTransitGatewayRouteTableResponse = CreateTransitGatewayRouteTableResponse'
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

-- | Creates a value of 'CreateTransitGatewayRouteTableResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'transitGatewayRouteTable' - Information about the transit gateway route table.
mkCreateTransitGatewayRouteTableResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateTransitGatewayRouteTableResponse
mkCreateTransitGatewayRouteTableResponse pResponseStatus_ =
  CreateTransitGatewayRouteTableResponse'
    { transitGatewayRouteTable =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the transit gateway route table.
--
-- /Note:/ Consider using 'transitGatewayRouteTable' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctgrtrsTransitGatewayRouteTable :: Lens.Lens' CreateTransitGatewayRouteTableResponse (Lude.Maybe TransitGatewayRouteTable)
ctgrtrsTransitGatewayRouteTable = Lens.lens (transitGatewayRouteTable :: CreateTransitGatewayRouteTableResponse -> Lude.Maybe TransitGatewayRouteTable) (\s a -> s {transitGatewayRouteTable = a} :: CreateTransitGatewayRouteTableResponse)
{-# DEPRECATED ctgrtrsTransitGatewayRouteTable "Use generic-lens or generic-optics with 'transitGatewayRouteTable' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctgrtrsResponseStatus :: Lens.Lens' CreateTransitGatewayRouteTableResponse Lude.Int
ctgrtrsResponseStatus = Lens.lens (responseStatus :: CreateTransitGatewayRouteTableResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateTransitGatewayRouteTableResponse)
{-# DEPRECATED ctgrtrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
