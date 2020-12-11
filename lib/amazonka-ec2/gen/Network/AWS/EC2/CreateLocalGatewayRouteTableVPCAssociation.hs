{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.CreateLocalGatewayRouteTableVPCAssociation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates the specified VPC with the specified local gateway route table.
module Network.AWS.EC2.CreateLocalGatewayRouteTableVPCAssociation
  ( -- * Creating a request
    CreateLocalGatewayRouteTableVPCAssociation (..),
    mkCreateLocalGatewayRouteTableVPCAssociation,

    -- ** Request lenses
    clgrtvaTagSpecifications,
    clgrtvaDryRun,
    clgrtvaLocalGatewayRouteTableId,
    clgrtvaVPCId,

    -- * Destructuring the response
    CreateLocalGatewayRouteTableVPCAssociationResponse (..),
    mkCreateLocalGatewayRouteTableVPCAssociationResponse,

    -- ** Response lenses
    clgrtvarsLocalGatewayRouteTableVPCAssociation,
    clgrtvarsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateLocalGatewayRouteTableVPCAssociation' smart constructor.
data CreateLocalGatewayRouteTableVPCAssociation = CreateLocalGatewayRouteTableVPCAssociation'
  { tagSpecifications ::
      Lude.Maybe
        [TagSpecification],
    dryRun ::
      Lude.Maybe
        Lude.Bool,
    localGatewayRouteTableId ::
      Lude.Text,
    vpcId ::
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

-- | Creates a value of 'CreateLocalGatewayRouteTableVPCAssociation' with the minimum fields required to make a request.
--
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'localGatewayRouteTableId' - The ID of the local gateway route table.
-- * 'tagSpecifications' - The tags to assign to the local gateway route table VPC association.
-- * 'vpcId' - The ID of the VPC.
mkCreateLocalGatewayRouteTableVPCAssociation ::
  -- | 'localGatewayRouteTableId'
  Lude.Text ->
  -- | 'vpcId'
  Lude.Text ->
  CreateLocalGatewayRouteTableVPCAssociation
mkCreateLocalGatewayRouteTableVPCAssociation
  pLocalGatewayRouteTableId_
  pVPCId_ =
    CreateLocalGatewayRouteTableVPCAssociation'
      { tagSpecifications =
          Lude.Nothing,
        dryRun = Lude.Nothing,
        localGatewayRouteTableId =
          pLocalGatewayRouteTableId_,
        vpcId = pVPCId_
      }

-- | The tags to assign to the local gateway route table VPC association.
--
-- /Note:/ Consider using 'tagSpecifications' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clgrtvaTagSpecifications :: Lens.Lens' CreateLocalGatewayRouteTableVPCAssociation (Lude.Maybe [TagSpecification])
clgrtvaTagSpecifications = Lens.lens (tagSpecifications :: CreateLocalGatewayRouteTableVPCAssociation -> Lude.Maybe [TagSpecification]) (\s a -> s {tagSpecifications = a} :: CreateLocalGatewayRouteTableVPCAssociation)
{-# DEPRECATED clgrtvaTagSpecifications "Use generic-lens or generic-optics with 'tagSpecifications' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clgrtvaDryRun :: Lens.Lens' CreateLocalGatewayRouteTableVPCAssociation (Lude.Maybe Lude.Bool)
clgrtvaDryRun = Lens.lens (dryRun :: CreateLocalGatewayRouteTableVPCAssociation -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: CreateLocalGatewayRouteTableVPCAssociation)
{-# DEPRECATED clgrtvaDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The ID of the local gateway route table.
--
-- /Note:/ Consider using 'localGatewayRouteTableId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clgrtvaLocalGatewayRouteTableId :: Lens.Lens' CreateLocalGatewayRouteTableVPCAssociation Lude.Text
clgrtvaLocalGatewayRouteTableId = Lens.lens (localGatewayRouteTableId :: CreateLocalGatewayRouteTableVPCAssociation -> Lude.Text) (\s a -> s {localGatewayRouteTableId = a} :: CreateLocalGatewayRouteTableVPCAssociation)
{-# DEPRECATED clgrtvaLocalGatewayRouteTableId "Use generic-lens or generic-optics with 'localGatewayRouteTableId' instead." #-}

-- | The ID of the VPC.
--
-- /Note:/ Consider using 'vpcId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clgrtvaVPCId :: Lens.Lens' CreateLocalGatewayRouteTableVPCAssociation Lude.Text
clgrtvaVPCId = Lens.lens (vpcId :: CreateLocalGatewayRouteTableVPCAssociation -> Lude.Text) (\s a -> s {vpcId = a} :: CreateLocalGatewayRouteTableVPCAssociation)
{-# DEPRECATED clgrtvaVPCId "Use generic-lens or generic-optics with 'vpcId' instead." #-}

instance Lude.AWSRequest CreateLocalGatewayRouteTableVPCAssociation where
  type
    Rs CreateLocalGatewayRouteTableVPCAssociation =
      CreateLocalGatewayRouteTableVPCAssociationResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          CreateLocalGatewayRouteTableVPCAssociationResponse'
            Lude.<$> (x Lude..@? "localGatewayRouteTableVpcAssociation")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateLocalGatewayRouteTableVPCAssociation where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath CreateLocalGatewayRouteTableVPCAssociation where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateLocalGatewayRouteTableVPCAssociation where
  toQuery CreateLocalGatewayRouteTableVPCAssociation' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("CreateLocalGatewayRouteTableVpcAssociation" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        Lude.toQuery
          (Lude.toQueryList "TagSpecification" Lude.<$> tagSpecifications),
        "DryRun" Lude.=: dryRun,
        "LocalGatewayRouteTableId" Lude.=: localGatewayRouteTableId,
        "VpcId" Lude.=: vpcId
      ]

-- | /See:/ 'mkCreateLocalGatewayRouteTableVPCAssociationResponse' smart constructor.
data CreateLocalGatewayRouteTableVPCAssociationResponse = CreateLocalGatewayRouteTableVPCAssociationResponse'
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

-- | Creates a value of 'CreateLocalGatewayRouteTableVPCAssociationResponse' with the minimum fields required to make a request.
--
-- * 'localGatewayRouteTableVPCAssociation' - Information about the association.
-- * 'responseStatus' - The response status code.
mkCreateLocalGatewayRouteTableVPCAssociationResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateLocalGatewayRouteTableVPCAssociationResponse
mkCreateLocalGatewayRouteTableVPCAssociationResponse
  pResponseStatus_ =
    CreateLocalGatewayRouteTableVPCAssociationResponse'
      { localGatewayRouteTableVPCAssociation =
          Lude.Nothing,
        responseStatus = pResponseStatus_
      }

-- | Information about the association.
--
-- /Note:/ Consider using 'localGatewayRouteTableVPCAssociation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clgrtvarsLocalGatewayRouteTableVPCAssociation :: Lens.Lens' CreateLocalGatewayRouteTableVPCAssociationResponse (Lude.Maybe LocalGatewayRouteTableVPCAssociation)
clgrtvarsLocalGatewayRouteTableVPCAssociation = Lens.lens (localGatewayRouteTableVPCAssociation :: CreateLocalGatewayRouteTableVPCAssociationResponse -> Lude.Maybe LocalGatewayRouteTableVPCAssociation) (\s a -> s {localGatewayRouteTableVPCAssociation = a} :: CreateLocalGatewayRouteTableVPCAssociationResponse)
{-# DEPRECATED clgrtvarsLocalGatewayRouteTableVPCAssociation "Use generic-lens or generic-optics with 'localGatewayRouteTableVPCAssociation' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clgrtvarsResponseStatus :: Lens.Lens' CreateLocalGatewayRouteTableVPCAssociationResponse Lude.Int
clgrtvarsResponseStatus = Lens.lens (responseStatus :: CreateLocalGatewayRouteTableVPCAssociationResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateLocalGatewayRouteTableVPCAssociationResponse)
{-# DEPRECATED clgrtvarsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
