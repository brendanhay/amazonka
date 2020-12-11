{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DeleteRouteTable
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified route table. You must disassociate the route table from any subnets before you can delete it. You can't delete the main route table.
module Network.AWS.EC2.DeleteRouteTable
  ( -- * Creating a request
    DeleteRouteTable (..),
    mkDeleteRouteTable,

    -- ** Request lenses
    drtrDryRun,
    drtrRouteTableId,

    -- * Destructuring the response
    DeleteRouteTableResponse (..),
    mkDeleteRouteTableResponse,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteRouteTable' smart constructor.
data DeleteRouteTable = DeleteRouteTable'
  { dryRun ::
      Lude.Maybe Lude.Bool,
    routeTableId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteRouteTable' with the minimum fields required to make a request.
--
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'routeTableId' - The ID of the route table.
mkDeleteRouteTable ::
  -- | 'routeTableId'
  Lude.Text ->
  DeleteRouteTable
mkDeleteRouteTable pRouteTableId_ =
  DeleteRouteTable'
    { dryRun = Lude.Nothing,
      routeTableId = pRouteTableId_
    }

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drtrDryRun :: Lens.Lens' DeleteRouteTable (Lude.Maybe Lude.Bool)
drtrDryRun = Lens.lens (dryRun :: DeleteRouteTable -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: DeleteRouteTable)
{-# DEPRECATED drtrDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The ID of the route table.
--
-- /Note:/ Consider using 'routeTableId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drtrRouteTableId :: Lens.Lens' DeleteRouteTable Lude.Text
drtrRouteTableId = Lens.lens (routeTableId :: DeleteRouteTable -> Lude.Text) (\s a -> s {routeTableId = a} :: DeleteRouteTable)
{-# DEPRECATED drtrRouteTableId "Use generic-lens or generic-optics with 'routeTableId' instead." #-}

instance Lude.AWSRequest DeleteRouteTable where
  type Rs DeleteRouteTable = DeleteRouteTableResponse
  request = Req.postQuery ec2Service
  response = Res.receiveNull DeleteRouteTableResponse'

instance Lude.ToHeaders DeleteRouteTable where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DeleteRouteTable where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteRouteTable where
  toQuery DeleteRouteTable' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DeleteRouteTable" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "DryRun" Lude.=: dryRun,
        "RouteTableId" Lude.=: routeTableId
      ]

-- | /See:/ 'mkDeleteRouteTableResponse' smart constructor.
data DeleteRouteTableResponse = DeleteRouteTableResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteRouteTableResponse' with the minimum fields required to make a request.
mkDeleteRouteTableResponse ::
  DeleteRouteTableResponse
mkDeleteRouteTableResponse = DeleteRouteTableResponse'
