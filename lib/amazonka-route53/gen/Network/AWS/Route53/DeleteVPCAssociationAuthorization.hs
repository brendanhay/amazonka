{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.DeleteVPCAssociationAuthorization
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes authorization to submit an @AssociateVPCWithHostedZone@ request to associate a specified VPC with a hosted zone that was created by a different account. You must use the account that created the hosted zone to submit a @DeleteVPCAssociationAuthorization@ request.
--
-- /Important:/ Sending this request only prevents the AWS account that created the VPC from associating the VPC with the Amazon Route 53 hosted zone in the future. If the VPC is already associated with the hosted zone, @DeleteVPCAssociationAuthorization@ won't disassociate the VPC from the hosted zone. If you want to delete an existing association, use @DisassociateVPCFromHostedZone@ .
module Network.AWS.Route53.DeleteVPCAssociationAuthorization
  ( -- * Creating a request
    DeleteVPCAssociationAuthorization (..),
    mkDeleteVPCAssociationAuthorization,

    -- ** Request lenses
    dvpcaaHostedZoneId,
    dvpcaaVPC,

    -- * Destructuring the response
    DeleteVPCAssociationAuthorizationResponse (..),
    mkDeleteVPCAssociationAuthorizationResponse,

    -- ** Response lenses
    dvpcaarrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.Route53.Types as Types

-- | A complex type that contains information about the request to remove authorization to associate a VPC that was created by one AWS account with a hosted zone that was created with a different AWS account.
--
-- /See:/ 'mkDeleteVPCAssociationAuthorization' smart constructor.
data DeleteVPCAssociationAuthorization = DeleteVPCAssociationAuthorization'
  { -- | When removing authorization to associate a VPC that was created by one AWS account with a hosted zone that was created with a different AWS account, the ID of the hosted zone.
    hostedZoneId :: Types.ResourceId,
    -- | When removing authorization to associate a VPC that was created by one AWS account with a hosted zone that was created with a different AWS account, a complex type that includes the ID and region of the VPC.
    vpc :: Types.VPC
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteVPCAssociationAuthorization' value with any optional fields omitted.
mkDeleteVPCAssociationAuthorization ::
  -- | 'hostedZoneId'
  Types.ResourceId ->
  -- | 'vpc'
  Types.VPC ->
  DeleteVPCAssociationAuthorization
mkDeleteVPCAssociationAuthorization hostedZoneId vpc =
  DeleteVPCAssociationAuthorization' {hostedZoneId, vpc}

-- | When removing authorization to associate a VPC that was created by one AWS account with a hosted zone that was created with a different AWS account, the ID of the hosted zone.
--
-- /Note:/ Consider using 'hostedZoneId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvpcaaHostedZoneId :: Lens.Lens' DeleteVPCAssociationAuthorization Types.ResourceId
dvpcaaHostedZoneId = Lens.field @"hostedZoneId"
{-# DEPRECATED dvpcaaHostedZoneId "Use generic-lens or generic-optics with 'hostedZoneId' instead." #-}

-- | When removing authorization to associate a VPC that was created by one AWS account with a hosted zone that was created with a different AWS account, a complex type that includes the ID and region of the VPC.
--
-- /Note:/ Consider using 'vpc' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvpcaaVPC :: Lens.Lens' DeleteVPCAssociationAuthorization Types.VPC
dvpcaaVPC = Lens.field @"vpc"
{-# DEPRECATED dvpcaaVPC "Use generic-lens or generic-optics with 'vpc' instead." #-}

instance Core.ToXML DeleteVPCAssociationAuthorization where
  toXML DeleteVPCAssociationAuthorization {..} =
    Core.toXMLNode "VPC" vpc
  toXMLDocument =
    Core.mkXMLElement
      "{https://route53.amazonaws.com/doc/2013-04-01/}DeleteVPCAssociationAuthorizationRequest"

instance Core.AWSRequest DeleteVPCAssociationAuthorization where
  type
    Rs DeleteVPCAssociationAuthorization =
      DeleteVPCAssociationAuthorizationResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath =
          Core.rawPath
            ( "/2013-04-01/hostedzone/" Core.<> (Core.toText hostedZoneId)
                Core.<> ("/deauthorizevpcassociation")
            ),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = Core.toXMLBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteVPCAssociationAuthorizationResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | Empty response for the request.
--
-- /See:/ 'mkDeleteVPCAssociationAuthorizationResponse' smart constructor.
newtype DeleteVPCAssociationAuthorizationResponse = DeleteVPCAssociationAuthorizationResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteVPCAssociationAuthorizationResponse' value with any optional fields omitted.
mkDeleteVPCAssociationAuthorizationResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeleteVPCAssociationAuthorizationResponse
mkDeleteVPCAssociationAuthorizationResponse responseStatus =
  DeleteVPCAssociationAuthorizationResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvpcaarrsResponseStatus :: Lens.Lens' DeleteVPCAssociationAuthorizationResponse Core.Int
dvpcaarrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dvpcaarrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
