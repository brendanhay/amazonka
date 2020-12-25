{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.CreateVPCAssociationAuthorization
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Authorizes the AWS account that created a specified VPC to submit an @AssociateVPCWithHostedZone@ request to associate the VPC with a specified hosted zone that was created by a different account. To submit a @CreateVPCAssociationAuthorization@ request, you must use the account that created the hosted zone. After you authorize the association, use the account that created the VPC to submit an @AssociateVPCWithHostedZone@ request.
module Network.AWS.Route53.CreateVPCAssociationAuthorization
  ( -- * Creating a request
    CreateVPCAssociationAuthorization (..),
    mkCreateVPCAssociationAuthorization,

    -- ** Request lenses
    cvpcaaHostedZoneId,
    cvpcaaVPC,

    -- * Destructuring the response
    CreateVPCAssociationAuthorizationResponse (..),
    mkCreateVPCAssociationAuthorizationResponse,

    -- ** Response lenses
    cvpcaarrsHostedZoneId,
    cvpcaarrsVPC,
    cvpcaarrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.Route53.Types as Types

-- | A complex type that contains information about the request to authorize associating a VPC with your private hosted zone. Authorization is only required when a private hosted zone and a VPC were created by using different accounts.
--
-- /See:/ 'mkCreateVPCAssociationAuthorization' smart constructor.
data CreateVPCAssociationAuthorization = CreateVPCAssociationAuthorization'
  { -- | The ID of the private hosted zone that you want to authorize associating a VPC with.
    hostedZoneId :: Types.ResourceId,
    -- | A complex type that contains the VPC ID and region for the VPC that you want to authorize associating with your hosted zone.
    vpc :: Types.VPC
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateVPCAssociationAuthorization' value with any optional fields omitted.
mkCreateVPCAssociationAuthorization ::
  -- | 'hostedZoneId'
  Types.ResourceId ->
  -- | 'vpc'
  Types.VPC ->
  CreateVPCAssociationAuthorization
mkCreateVPCAssociationAuthorization hostedZoneId vpc =
  CreateVPCAssociationAuthorization' {hostedZoneId, vpc}

-- | The ID of the private hosted zone that you want to authorize associating a VPC with.
--
-- /Note:/ Consider using 'hostedZoneId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvpcaaHostedZoneId :: Lens.Lens' CreateVPCAssociationAuthorization Types.ResourceId
cvpcaaHostedZoneId = Lens.field @"hostedZoneId"
{-# DEPRECATED cvpcaaHostedZoneId "Use generic-lens or generic-optics with 'hostedZoneId' instead." #-}

-- | A complex type that contains the VPC ID and region for the VPC that you want to authorize associating with your hosted zone.
--
-- /Note:/ Consider using 'vpc' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvpcaaVPC :: Lens.Lens' CreateVPCAssociationAuthorization Types.VPC
cvpcaaVPC = Lens.field @"vpc"
{-# DEPRECATED cvpcaaVPC "Use generic-lens or generic-optics with 'vpc' instead." #-}

instance Core.ToXML CreateVPCAssociationAuthorization where
  toXML CreateVPCAssociationAuthorization {..} =
    Core.toXMLNode "VPC" vpc
  toXMLDocument =
    Core.mkXMLElement
      "{https://route53.amazonaws.com/doc/2013-04-01/}CreateVPCAssociationAuthorizationRequest"

instance Core.AWSRequest CreateVPCAssociationAuthorization where
  type
    Rs CreateVPCAssociationAuthorization =
      CreateVPCAssociationAuthorizationResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath =
          Core.rawPath
            ( "/2013-04-01/hostedzone/" Core.<> (Core.toText hostedZoneId)
                Core.<> ("/authorizevpcassociation")
            ),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = Core.toXMLBody x
      }
  response =
    Response.receiveXML
      ( \s h x ->
          CreateVPCAssociationAuthorizationResponse'
            Core.<$> (x Core..@ "HostedZoneId")
            Core.<*> (x Core..@ "VPC")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | A complex type that contains the response information from a @CreateVPCAssociationAuthorization@ request.
--
-- /See:/ 'mkCreateVPCAssociationAuthorizationResponse' smart constructor.
data CreateVPCAssociationAuthorizationResponse = CreateVPCAssociationAuthorizationResponse'
  { -- | The ID of the hosted zone that you authorized associating a VPC with.
    hostedZoneId :: Types.ResourceId,
    -- | The VPC that you authorized associating with a hosted zone.
    vpc :: Types.VPC,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateVPCAssociationAuthorizationResponse' value with any optional fields omitted.
mkCreateVPCAssociationAuthorizationResponse ::
  -- | 'hostedZoneId'
  Types.ResourceId ->
  -- | 'vpc'
  Types.VPC ->
  -- | 'responseStatus'
  Core.Int ->
  CreateVPCAssociationAuthorizationResponse
mkCreateVPCAssociationAuthorizationResponse
  hostedZoneId
  vpc
  responseStatus =
    CreateVPCAssociationAuthorizationResponse'
      { hostedZoneId,
        vpc,
        responseStatus
      }

-- | The ID of the hosted zone that you authorized associating a VPC with.
--
-- /Note:/ Consider using 'hostedZoneId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvpcaarrsHostedZoneId :: Lens.Lens' CreateVPCAssociationAuthorizationResponse Types.ResourceId
cvpcaarrsHostedZoneId = Lens.field @"hostedZoneId"
{-# DEPRECATED cvpcaarrsHostedZoneId "Use generic-lens or generic-optics with 'hostedZoneId' instead." #-}

-- | The VPC that you authorized associating with a hosted zone.
--
-- /Note:/ Consider using 'vpc' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvpcaarrsVPC :: Lens.Lens' CreateVPCAssociationAuthorizationResponse Types.VPC
cvpcaarrsVPC = Lens.field @"vpc"
{-# DEPRECATED cvpcaarrsVPC "Use generic-lens or generic-optics with 'vpc' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvpcaarrsResponseStatus :: Lens.Lens' CreateVPCAssociationAuthorizationResponse Core.Int
cvpcaarrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED cvpcaarrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
