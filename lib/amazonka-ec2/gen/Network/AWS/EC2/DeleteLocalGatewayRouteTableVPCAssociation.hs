{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DeleteLocalGatewayRouteTableVPCAssociation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified association between a VPC and local gateway route table.
module Network.AWS.EC2.DeleteLocalGatewayRouteTableVPCAssociation
  ( -- * Creating a request
    DeleteLocalGatewayRouteTableVPCAssociation (..),
    mkDeleteLocalGatewayRouteTableVPCAssociation,

    -- ** Request lenses
    dlgrtvaDryRun,
    dlgrtvaLocalGatewayRouteTableVPCAssociationId,

    -- * Destructuring the response
    DeleteLocalGatewayRouteTableVPCAssociationResponse (..),
    mkDeleteLocalGatewayRouteTableVPCAssociationResponse,

    -- ** Response lenses
    dlgrtvarsLocalGatewayRouteTableVPCAssociation,
    dlgrtvarsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteLocalGatewayRouteTableVPCAssociation' smart constructor.
data DeleteLocalGatewayRouteTableVPCAssociation = DeleteLocalGatewayRouteTableVPCAssociation'
  { dryRun ::
      Lude.Maybe
        Lude.Bool,
    localGatewayRouteTableVPCAssociationId ::
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

-- | Creates a value of 'DeleteLocalGatewayRouteTableVPCAssociation' with the minimum fields required to make a request.
--
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'localGatewayRouteTableVPCAssociationId' - The ID of the association.
mkDeleteLocalGatewayRouteTableVPCAssociation ::
  -- | 'localGatewayRouteTableVPCAssociationId'
  Lude.Text ->
  DeleteLocalGatewayRouteTableVPCAssociation
mkDeleteLocalGatewayRouteTableVPCAssociation
  pLocalGatewayRouteTableVPCAssociationId_ =
    DeleteLocalGatewayRouteTableVPCAssociation'
      { dryRun =
          Lude.Nothing,
        localGatewayRouteTableVPCAssociationId =
          pLocalGatewayRouteTableVPCAssociationId_
      }

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlgrtvaDryRun :: Lens.Lens' DeleteLocalGatewayRouteTableVPCAssociation (Lude.Maybe Lude.Bool)
dlgrtvaDryRun = Lens.lens (dryRun :: DeleteLocalGatewayRouteTableVPCAssociation -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: DeleteLocalGatewayRouteTableVPCAssociation)
{-# DEPRECATED dlgrtvaDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The ID of the association.
--
-- /Note:/ Consider using 'localGatewayRouteTableVPCAssociationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlgrtvaLocalGatewayRouteTableVPCAssociationId :: Lens.Lens' DeleteLocalGatewayRouteTableVPCAssociation Lude.Text
dlgrtvaLocalGatewayRouteTableVPCAssociationId = Lens.lens (localGatewayRouteTableVPCAssociationId :: DeleteLocalGatewayRouteTableVPCAssociation -> Lude.Text) (\s a -> s {localGatewayRouteTableVPCAssociationId = a} :: DeleteLocalGatewayRouteTableVPCAssociation)
{-# DEPRECATED dlgrtvaLocalGatewayRouteTableVPCAssociationId "Use generic-lens or generic-optics with 'localGatewayRouteTableVPCAssociationId' instead." #-}

instance Lude.AWSRequest DeleteLocalGatewayRouteTableVPCAssociation where
  type
    Rs DeleteLocalGatewayRouteTableVPCAssociation =
      DeleteLocalGatewayRouteTableVPCAssociationResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          DeleteLocalGatewayRouteTableVPCAssociationResponse'
            Lude.<$> (x Lude..@? "localGatewayRouteTableVpcAssociation")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteLocalGatewayRouteTableVPCAssociation where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DeleteLocalGatewayRouteTableVPCAssociation where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteLocalGatewayRouteTableVPCAssociation where
  toQuery DeleteLocalGatewayRouteTableVPCAssociation' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("DeleteLocalGatewayRouteTableVpcAssociation" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "DryRun" Lude.=: dryRun,
        "LocalGatewayRouteTableVpcAssociationId"
          Lude.=: localGatewayRouteTableVPCAssociationId
      ]

-- | /See:/ 'mkDeleteLocalGatewayRouteTableVPCAssociationResponse' smart constructor.
data DeleteLocalGatewayRouteTableVPCAssociationResponse = DeleteLocalGatewayRouteTableVPCAssociationResponse'
  { localGatewayRouteTableVPCAssociation ::
      Lude.Maybe
        LocalGatewayRouteTableVPCAssociation,
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
  deriving anyclass
    ( Lude.Hashable,
      Lude.NFData
    )

-- | Creates a value of 'DeleteLocalGatewayRouteTableVPCAssociationResponse' with the minimum fields required to make a request.
--
-- * 'localGatewayRouteTableVPCAssociation' - Information about the association.
-- * 'responseStatus' - The response status code.
mkDeleteLocalGatewayRouteTableVPCAssociationResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteLocalGatewayRouteTableVPCAssociationResponse
mkDeleteLocalGatewayRouteTableVPCAssociationResponse
  pResponseStatus_ =
    DeleteLocalGatewayRouteTableVPCAssociationResponse'
      { localGatewayRouteTableVPCAssociation =
          Lude.Nothing,
        responseStatus = pResponseStatus_
      }

-- | Information about the association.
--
-- /Note:/ Consider using 'localGatewayRouteTableVPCAssociation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlgrtvarsLocalGatewayRouteTableVPCAssociation :: Lens.Lens' DeleteLocalGatewayRouteTableVPCAssociationResponse (Lude.Maybe LocalGatewayRouteTableVPCAssociation)
dlgrtvarsLocalGatewayRouteTableVPCAssociation = Lens.lens (localGatewayRouteTableVPCAssociation :: DeleteLocalGatewayRouteTableVPCAssociationResponse -> Lude.Maybe LocalGatewayRouteTableVPCAssociation) (\s a -> s {localGatewayRouteTableVPCAssociation = a} :: DeleteLocalGatewayRouteTableVPCAssociationResponse)
{-# DEPRECATED dlgrtvarsLocalGatewayRouteTableVPCAssociation "Use generic-lens or generic-optics with 'localGatewayRouteTableVPCAssociation' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlgrtvarsResponseStatus :: Lens.Lens' DeleteLocalGatewayRouteTableVPCAssociationResponse Lude.Int
dlgrtvarsResponseStatus = Lens.lens (responseStatus :: DeleteLocalGatewayRouteTableVPCAssociationResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteLocalGatewayRouteTableVPCAssociationResponse)
{-# DEPRECATED dlgrtvarsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
