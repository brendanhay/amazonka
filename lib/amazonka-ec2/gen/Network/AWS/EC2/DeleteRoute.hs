{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DeleteRoute
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified route from the specified route table.
module Network.AWS.EC2.DeleteRoute
  ( -- * Creating a request
    DeleteRoute (..),
    mkDeleteRoute,

    -- ** Request lenses
    drfRouteTableId,
    drfDestinationIPv6CidrBlock,
    drfDestinationPrefixListId,
    drfDryRun,
    drfDestinationCidrBlock,

    -- * Destructuring the response
    DeleteRouteResponse (..),
    mkDeleteRouteResponse,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteRoute' smart constructor.
data DeleteRoute = DeleteRoute'
  { -- | The ID of the route table.
    routeTableId :: Lude.Text,
    -- | The IPv6 CIDR range for the route. The value you specify must match the CIDR for the route exactly.
    destinationIPv6CidrBlock :: Lude.Maybe Lude.Text,
    -- | The ID of the prefix list for the route.
    destinationPrefixListId :: Lude.Maybe Lude.Text,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Lude.Maybe Lude.Bool,
    -- | The IPv4 CIDR range for the route. The value you specify must match the CIDR for the route exactly.
    destinationCidrBlock :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteRoute' with the minimum fields required to make a request.
--
-- * 'routeTableId' - The ID of the route table.
-- * 'destinationIPv6CidrBlock' - The IPv6 CIDR range for the route. The value you specify must match the CIDR for the route exactly.
-- * 'destinationPrefixListId' - The ID of the prefix list for the route.
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'destinationCidrBlock' - The IPv4 CIDR range for the route. The value you specify must match the CIDR for the route exactly.
mkDeleteRoute ::
  -- | 'routeTableId'
  Lude.Text ->
  DeleteRoute
mkDeleteRoute pRouteTableId_ =
  DeleteRoute'
    { routeTableId = pRouteTableId_,
      destinationIPv6CidrBlock = Lude.Nothing,
      destinationPrefixListId = Lude.Nothing,
      dryRun = Lude.Nothing,
      destinationCidrBlock = Lude.Nothing
    }

-- | The ID of the route table.
--
-- /Note:/ Consider using 'routeTableId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drfRouteTableId :: Lens.Lens' DeleteRoute Lude.Text
drfRouteTableId = Lens.lens (routeTableId :: DeleteRoute -> Lude.Text) (\s a -> s {routeTableId = a} :: DeleteRoute)
{-# DEPRECATED drfRouteTableId "Use generic-lens or generic-optics with 'routeTableId' instead." #-}

-- | The IPv6 CIDR range for the route. The value you specify must match the CIDR for the route exactly.
--
-- /Note:/ Consider using 'destinationIPv6CidrBlock' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drfDestinationIPv6CidrBlock :: Lens.Lens' DeleteRoute (Lude.Maybe Lude.Text)
drfDestinationIPv6CidrBlock = Lens.lens (destinationIPv6CidrBlock :: DeleteRoute -> Lude.Maybe Lude.Text) (\s a -> s {destinationIPv6CidrBlock = a} :: DeleteRoute)
{-# DEPRECATED drfDestinationIPv6CidrBlock "Use generic-lens or generic-optics with 'destinationIPv6CidrBlock' instead." #-}

-- | The ID of the prefix list for the route.
--
-- /Note:/ Consider using 'destinationPrefixListId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drfDestinationPrefixListId :: Lens.Lens' DeleteRoute (Lude.Maybe Lude.Text)
drfDestinationPrefixListId = Lens.lens (destinationPrefixListId :: DeleteRoute -> Lude.Maybe Lude.Text) (\s a -> s {destinationPrefixListId = a} :: DeleteRoute)
{-# DEPRECATED drfDestinationPrefixListId "Use generic-lens or generic-optics with 'destinationPrefixListId' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drfDryRun :: Lens.Lens' DeleteRoute (Lude.Maybe Lude.Bool)
drfDryRun = Lens.lens (dryRun :: DeleteRoute -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: DeleteRoute)
{-# DEPRECATED drfDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The IPv4 CIDR range for the route. The value you specify must match the CIDR for the route exactly.
--
-- /Note:/ Consider using 'destinationCidrBlock' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drfDestinationCidrBlock :: Lens.Lens' DeleteRoute (Lude.Maybe Lude.Text)
drfDestinationCidrBlock = Lens.lens (destinationCidrBlock :: DeleteRoute -> Lude.Maybe Lude.Text) (\s a -> s {destinationCidrBlock = a} :: DeleteRoute)
{-# DEPRECATED drfDestinationCidrBlock "Use generic-lens or generic-optics with 'destinationCidrBlock' instead." #-}

instance Lude.AWSRequest DeleteRoute where
  type Rs DeleteRoute = DeleteRouteResponse
  request = Req.postQuery ec2Service
  response = Res.receiveNull DeleteRouteResponse'

instance Lude.ToHeaders DeleteRoute where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DeleteRoute where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteRoute where
  toQuery DeleteRoute' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DeleteRoute" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "RouteTableId" Lude.=: routeTableId,
        "DestinationIpv6CidrBlock" Lude.=: destinationIPv6CidrBlock,
        "DestinationPrefixListId" Lude.=: destinationPrefixListId,
        "DryRun" Lude.=: dryRun,
        "DestinationCidrBlock" Lude.=: destinationCidrBlock
      ]

-- | /See:/ 'mkDeleteRouteResponse' smart constructor.
data DeleteRouteResponse = DeleteRouteResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteRouteResponse' with the minimum fields required to make a request.
mkDeleteRouteResponse ::
  DeleteRouteResponse
mkDeleteRouteResponse = DeleteRouteResponse'
