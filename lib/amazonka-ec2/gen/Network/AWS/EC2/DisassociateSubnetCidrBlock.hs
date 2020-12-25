{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DisassociateSubnetCidrBlock
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociates a CIDR block from a subnet. Currently, you can disassociate an IPv6 CIDR block only. You must detach or delete all gateways and resources that are associated with the CIDR block before you can disassociate it.
module Network.AWS.EC2.DisassociateSubnetCidrBlock
  ( -- * Creating a request
    DisassociateSubnetCidrBlock (..),
    mkDisassociateSubnetCidrBlock,

    -- ** Request lenses
    dscbAssociationId,

    -- * Destructuring the response
    DisassociateSubnetCidrBlockResponse (..),
    mkDisassociateSubnetCidrBlockResponse,

    -- ** Response lenses
    dscbrrsIpv6CidrBlockAssociation,
    dscbrrsSubnetId,
    dscbrrsResponseStatus,
  )
where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDisassociateSubnetCidrBlock' smart constructor.
newtype DisassociateSubnetCidrBlock = DisassociateSubnetCidrBlock'
  { -- | The association ID for the CIDR block.
    associationId :: Types.SubnetCidrAssociationId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DisassociateSubnetCidrBlock' value with any optional fields omitted.
mkDisassociateSubnetCidrBlock ::
  -- | 'associationId'
  Types.SubnetCidrAssociationId ->
  DisassociateSubnetCidrBlock
mkDisassociateSubnetCidrBlock associationId =
  DisassociateSubnetCidrBlock' {associationId}

-- | The association ID for the CIDR block.
--
-- /Note:/ Consider using 'associationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dscbAssociationId :: Lens.Lens' DisassociateSubnetCidrBlock Types.SubnetCidrAssociationId
dscbAssociationId = Lens.field @"associationId"
{-# DEPRECATED dscbAssociationId "Use generic-lens or generic-optics with 'associationId' instead." #-}

instance Core.AWSRequest DisassociateSubnetCidrBlock where
  type
    Rs DisassociateSubnetCidrBlock =
      DisassociateSubnetCidrBlockResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "Content-Type",
              "application/x-www-form-urlencoded; charset=utf-8"
            ),
        Core._rqBody =
          Core.toFormBody
            ( Core.pure ("Action", "DisassociateSubnetCidrBlock")
                Core.<> (Core.pure ("Version", "2016-11-15"))
                Core.<> (Core.toQueryValue "AssociationId" associationId)
            )
      }
  response =
    Response.receiveXML
      ( \s h x ->
          DisassociateSubnetCidrBlockResponse'
            Core.<$> (x Core..@? "ipv6CidrBlockAssociation")
            Core.<*> (x Core..@? "subnetId")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDisassociateSubnetCidrBlockResponse' smart constructor.
data DisassociateSubnetCidrBlockResponse = DisassociateSubnetCidrBlockResponse'
  { -- | Information about the IPv6 CIDR block association.
    ipv6CidrBlockAssociation :: Core.Maybe Types.SubnetIpv6CidrBlockAssociation,
    -- | The ID of the subnet.
    subnetId :: Core.Maybe Types.SubnetId,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DisassociateSubnetCidrBlockResponse' value with any optional fields omitted.
mkDisassociateSubnetCidrBlockResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DisassociateSubnetCidrBlockResponse
mkDisassociateSubnetCidrBlockResponse responseStatus =
  DisassociateSubnetCidrBlockResponse'
    { ipv6CidrBlockAssociation =
        Core.Nothing,
      subnetId = Core.Nothing,
      responseStatus
    }

-- | Information about the IPv6 CIDR block association.
--
-- /Note:/ Consider using 'ipv6CidrBlockAssociation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dscbrrsIpv6CidrBlockAssociation :: Lens.Lens' DisassociateSubnetCidrBlockResponse (Core.Maybe Types.SubnetIpv6CidrBlockAssociation)
dscbrrsIpv6CidrBlockAssociation = Lens.field @"ipv6CidrBlockAssociation"
{-# DEPRECATED dscbrrsIpv6CidrBlockAssociation "Use generic-lens or generic-optics with 'ipv6CidrBlockAssociation' instead." #-}

-- | The ID of the subnet.
--
-- /Note:/ Consider using 'subnetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dscbrrsSubnetId :: Lens.Lens' DisassociateSubnetCidrBlockResponse (Core.Maybe Types.SubnetId)
dscbrrsSubnetId = Lens.field @"subnetId"
{-# DEPRECATED dscbrrsSubnetId "Use generic-lens or generic-optics with 'subnetId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dscbrrsResponseStatus :: Lens.Lens' DisassociateSubnetCidrBlockResponse Core.Int
dscbrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dscbrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
