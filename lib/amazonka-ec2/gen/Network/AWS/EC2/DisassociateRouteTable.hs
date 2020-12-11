{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DisassociateRouteTable
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociates a subnet or gateway from a route table.
--
-- After you perform this action, the subnet no longer uses the routes in the route table. Instead, it uses the routes in the VPC's main route table. For more information about route tables, see <https://docs.aws.amazon.com/vpc/latest/userguide/VPC_Route_Tables.html Route Tables> in the /Amazon Virtual Private Cloud User Guide/ .
module Network.AWS.EC2.DisassociateRouteTable
  ( -- * Creating a request
    DisassociateRouteTable (..),
    mkDisassociateRouteTable,

    -- ** Request lenses
    drtDryRun,
    drtAssociationId,

    -- * Destructuring the response
    DisassociateRouteTableResponse (..),
    mkDisassociateRouteTableResponse,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDisassociateRouteTable' smart constructor.
data DisassociateRouteTable = DisassociateRouteTable'
  { dryRun ::
      Lude.Maybe Lude.Bool,
    associationId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DisassociateRouteTable' with the minimum fields required to make a request.
--
-- * 'associationId' - The association ID representing the current association between the route table and subnet or gateway.
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
mkDisassociateRouteTable ::
  -- | 'associationId'
  Lude.Text ->
  DisassociateRouteTable
mkDisassociateRouteTable pAssociationId_ =
  DisassociateRouteTable'
    { dryRun = Lude.Nothing,
      associationId = pAssociationId_
    }

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drtDryRun :: Lens.Lens' DisassociateRouteTable (Lude.Maybe Lude.Bool)
drtDryRun = Lens.lens (dryRun :: DisassociateRouteTable -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: DisassociateRouteTable)
{-# DEPRECATED drtDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The association ID representing the current association between the route table and subnet or gateway.
--
-- /Note:/ Consider using 'associationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drtAssociationId :: Lens.Lens' DisassociateRouteTable Lude.Text
drtAssociationId = Lens.lens (associationId :: DisassociateRouteTable -> Lude.Text) (\s a -> s {associationId = a} :: DisassociateRouteTable)
{-# DEPRECATED drtAssociationId "Use generic-lens or generic-optics with 'associationId' instead." #-}

instance Lude.AWSRequest DisassociateRouteTable where
  type Rs DisassociateRouteTable = DisassociateRouteTableResponse
  request = Req.postQuery ec2Service
  response = Res.receiveNull DisassociateRouteTableResponse'

instance Lude.ToHeaders DisassociateRouteTable where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DisassociateRouteTable where
  toPath = Lude.const "/"

instance Lude.ToQuery DisassociateRouteTable where
  toQuery DisassociateRouteTable' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DisassociateRouteTable" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "DryRun" Lude.=: dryRun,
        "AssociationId" Lude.=: associationId
      ]

-- | /See:/ 'mkDisassociateRouteTableResponse' smart constructor.
data DisassociateRouteTableResponse = DisassociateRouteTableResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DisassociateRouteTableResponse' with the minimum fields required to make a request.
mkDisassociateRouteTableResponse ::
  DisassociateRouteTableResponse
mkDisassociateRouteTableResponse = DisassociateRouteTableResponse'
