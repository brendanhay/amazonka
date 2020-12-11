{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.ReplaceRouteTableAssociation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Changes the route table associated with a given subnet, internet gateway, or virtual private gateway in a VPC. After the operation completes, the subnet or gateway uses the routes in the new route table. For more information about route tables, see <https://docs.aws.amazon.com/vpc/latest/userguide/VPC_Route_Tables.html Route Tables> in the /Amazon Virtual Private Cloud User Guide/ .
--
-- You can also use this operation to change which table is the main route table in the VPC. Specify the main route table's association ID and the route table ID of the new main route table.
module Network.AWS.EC2.ReplaceRouteTableAssociation
  ( -- * Creating a request
    ReplaceRouteTableAssociation (..),
    mkReplaceRouteTableAssociation,

    -- ** Request lenses
    rrtaDryRun,
    rrtaAssociationId,
    rrtaRouteTableId,

    -- * Destructuring the response
    ReplaceRouteTableAssociationResponse (..),
    mkReplaceRouteTableAssociationResponse,

    -- ** Response lenses
    rrtarsNewAssociationId,
    rrtarsAssociationState,
    rrtarsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkReplaceRouteTableAssociation' smart constructor.
data ReplaceRouteTableAssociation = ReplaceRouteTableAssociation'
  { dryRun ::
      Lude.Maybe Lude.Bool,
    associationId :: Lude.Text,
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

-- | Creates a value of 'ReplaceRouteTableAssociation' with the minimum fields required to make a request.
--
-- * 'associationId' - The association ID.
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'routeTableId' - The ID of the new route table to associate with the subnet.
mkReplaceRouteTableAssociation ::
  -- | 'associationId'
  Lude.Text ->
  -- | 'routeTableId'
  Lude.Text ->
  ReplaceRouteTableAssociation
mkReplaceRouteTableAssociation pAssociationId_ pRouteTableId_ =
  ReplaceRouteTableAssociation'
    { dryRun = Lude.Nothing,
      associationId = pAssociationId_,
      routeTableId = pRouteTableId_
    }

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrtaDryRun :: Lens.Lens' ReplaceRouteTableAssociation (Lude.Maybe Lude.Bool)
rrtaDryRun = Lens.lens (dryRun :: ReplaceRouteTableAssociation -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: ReplaceRouteTableAssociation)
{-# DEPRECATED rrtaDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The association ID.
--
-- /Note:/ Consider using 'associationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrtaAssociationId :: Lens.Lens' ReplaceRouteTableAssociation Lude.Text
rrtaAssociationId = Lens.lens (associationId :: ReplaceRouteTableAssociation -> Lude.Text) (\s a -> s {associationId = a} :: ReplaceRouteTableAssociation)
{-# DEPRECATED rrtaAssociationId "Use generic-lens or generic-optics with 'associationId' instead." #-}

-- | The ID of the new route table to associate with the subnet.
--
-- /Note:/ Consider using 'routeTableId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrtaRouteTableId :: Lens.Lens' ReplaceRouteTableAssociation Lude.Text
rrtaRouteTableId = Lens.lens (routeTableId :: ReplaceRouteTableAssociation -> Lude.Text) (\s a -> s {routeTableId = a} :: ReplaceRouteTableAssociation)
{-# DEPRECATED rrtaRouteTableId "Use generic-lens or generic-optics with 'routeTableId' instead." #-}

instance Lude.AWSRequest ReplaceRouteTableAssociation where
  type
    Rs ReplaceRouteTableAssociation =
      ReplaceRouteTableAssociationResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          ReplaceRouteTableAssociationResponse'
            Lude.<$> (x Lude..@? "newAssociationId")
            Lude.<*> (x Lude..@? "associationState")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ReplaceRouteTableAssociation where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ReplaceRouteTableAssociation where
  toPath = Lude.const "/"

instance Lude.ToQuery ReplaceRouteTableAssociation where
  toQuery ReplaceRouteTableAssociation' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("ReplaceRouteTableAssociation" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "DryRun" Lude.=: dryRun,
        "AssociationId" Lude.=: associationId,
        "RouteTableId" Lude.=: routeTableId
      ]

-- | /See:/ 'mkReplaceRouteTableAssociationResponse' smart constructor.
data ReplaceRouteTableAssociationResponse = ReplaceRouteTableAssociationResponse'
  { newAssociationId ::
      Lude.Maybe
        Lude.Text,
    associationState ::
      Lude.Maybe
        RouteTableAssociationState,
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

-- | Creates a value of 'ReplaceRouteTableAssociationResponse' with the minimum fields required to make a request.
--
-- * 'associationState' - The state of the association.
-- * 'newAssociationId' - The ID of the new association.
-- * 'responseStatus' - The response status code.
mkReplaceRouteTableAssociationResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ReplaceRouteTableAssociationResponse
mkReplaceRouteTableAssociationResponse pResponseStatus_ =
  ReplaceRouteTableAssociationResponse'
    { newAssociationId =
        Lude.Nothing,
      associationState = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The ID of the new association.
--
-- /Note:/ Consider using 'newAssociationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrtarsNewAssociationId :: Lens.Lens' ReplaceRouteTableAssociationResponse (Lude.Maybe Lude.Text)
rrtarsNewAssociationId = Lens.lens (newAssociationId :: ReplaceRouteTableAssociationResponse -> Lude.Maybe Lude.Text) (\s a -> s {newAssociationId = a} :: ReplaceRouteTableAssociationResponse)
{-# DEPRECATED rrtarsNewAssociationId "Use generic-lens or generic-optics with 'newAssociationId' instead." #-}

-- | The state of the association.
--
-- /Note:/ Consider using 'associationState' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrtarsAssociationState :: Lens.Lens' ReplaceRouteTableAssociationResponse (Lude.Maybe RouteTableAssociationState)
rrtarsAssociationState = Lens.lens (associationState :: ReplaceRouteTableAssociationResponse -> Lude.Maybe RouteTableAssociationState) (\s a -> s {associationState = a} :: ReplaceRouteTableAssociationResponse)
{-# DEPRECATED rrtarsAssociationState "Use generic-lens or generic-optics with 'associationState' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrtarsResponseStatus :: Lens.Lens' ReplaceRouteTableAssociationResponse Lude.Int
rrtarsResponseStatus = Lens.lens (responseStatus :: ReplaceRouteTableAssociationResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ReplaceRouteTableAssociationResponse)
{-# DEPRECATED rrtarsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
