{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.AssociateVPCWithHostedZone
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates an Amazon VPC with a private hosted zone.
--
-- /Important:/ To perform the association, the VPC and the private hosted zone must already exist. You can't convert a public hosted zone into a private hosted zone.
module Network.AWS.Route53.AssociateVPCWithHostedZone
  ( -- * Creating a request
    AssociateVPCWithHostedZone (..),
    mkAssociateVPCWithHostedZone,

    -- ** Request lenses
    avpcwhzHostedZoneId,
    avpcwhzVPC,
    avpcwhzComment,

    -- * Destructuring the response
    AssociateVPCWithHostedZoneResponse (..),
    mkAssociateVPCWithHostedZoneResponse,

    -- ** Response lenses
    avpcwhzrrsChangeInfo,
    avpcwhzrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.Route53.Types as Types

-- | A complex type that contains information about the request to associate a VPC with a private hosted zone.
--
-- /See:/ 'mkAssociateVPCWithHostedZone' smart constructor.
data AssociateVPCWithHostedZone = AssociateVPCWithHostedZone'
  { -- | The ID of the private hosted zone that you want to associate an Amazon VPC with.
    --
    -- Note that you can't associate a VPC with a hosted zone that doesn't have an existing VPC association.
    hostedZoneId :: Types.ResourceId,
    -- | A complex type that contains information about the VPC that you want to associate with a private hosted zone.
    vpc :: Types.VPC,
    -- | /Optional:/ A comment about the association request.
    comment :: Core.Maybe Types.AssociateVPCComment
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AssociateVPCWithHostedZone' value with any optional fields omitted.
mkAssociateVPCWithHostedZone ::
  -- | 'hostedZoneId'
  Types.ResourceId ->
  -- | 'vpc'
  Types.VPC ->
  AssociateVPCWithHostedZone
mkAssociateVPCWithHostedZone hostedZoneId vpc =
  AssociateVPCWithHostedZone'
    { hostedZoneId,
      vpc,
      comment = Core.Nothing
    }

-- | The ID of the private hosted zone that you want to associate an Amazon VPC with.
--
-- Note that you can't associate a VPC with a hosted zone that doesn't have an existing VPC association.
--
-- /Note:/ Consider using 'hostedZoneId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
avpcwhzHostedZoneId :: Lens.Lens' AssociateVPCWithHostedZone Types.ResourceId
avpcwhzHostedZoneId = Lens.field @"hostedZoneId"
{-# DEPRECATED avpcwhzHostedZoneId "Use generic-lens or generic-optics with 'hostedZoneId' instead." #-}

-- | A complex type that contains information about the VPC that you want to associate with a private hosted zone.
--
-- /Note:/ Consider using 'vpc' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
avpcwhzVPC :: Lens.Lens' AssociateVPCWithHostedZone Types.VPC
avpcwhzVPC = Lens.field @"vpc"
{-# DEPRECATED avpcwhzVPC "Use generic-lens or generic-optics with 'vpc' instead." #-}

-- | /Optional:/ A comment about the association request.
--
-- /Note:/ Consider using 'comment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
avpcwhzComment :: Lens.Lens' AssociateVPCWithHostedZone (Core.Maybe Types.AssociateVPCComment)
avpcwhzComment = Lens.field @"comment"
{-# DEPRECATED avpcwhzComment "Use generic-lens or generic-optics with 'comment' instead." #-}

instance Core.ToXML AssociateVPCWithHostedZone where
  toXML AssociateVPCWithHostedZone {..} =
    Core.toXMLNode "VPC" vpc
      Core.<> Core.toXMLNode "Comment" Core.<$> comment
  toXMLDocument =
    Core.mkXMLElement
      "{https://route53.amazonaws.com/doc/2013-04-01/}AssociateVPCWithHostedZoneRequest"

instance Core.AWSRequest AssociateVPCWithHostedZone where
  type
    Rs AssociateVPCWithHostedZone =
      AssociateVPCWithHostedZoneResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath =
          Core.rawPath
            ( "/2013-04-01/hostedzone/" Core.<> (Core.toText hostedZoneId)
                Core.<> ("/associatevpc")
            ),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = Core.toXMLBody x
      }
  response =
    Response.receiveXML
      ( \s h x ->
          AssociateVPCWithHostedZoneResponse'
            Core.<$> (x Core..@ "ChangeInfo") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | A complex type that contains the response information for the @AssociateVPCWithHostedZone@ request.
--
-- /See:/ 'mkAssociateVPCWithHostedZoneResponse' smart constructor.
data AssociateVPCWithHostedZoneResponse = AssociateVPCWithHostedZoneResponse'
  { -- | A complex type that describes the changes made to your hosted zone.
    changeInfo :: Types.ChangeInfo,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'AssociateVPCWithHostedZoneResponse' value with any optional fields omitted.
mkAssociateVPCWithHostedZoneResponse ::
  -- | 'changeInfo'
  Types.ChangeInfo ->
  -- | 'responseStatus'
  Core.Int ->
  AssociateVPCWithHostedZoneResponse
mkAssociateVPCWithHostedZoneResponse changeInfo responseStatus =
  AssociateVPCWithHostedZoneResponse' {changeInfo, responseStatus}

-- | A complex type that describes the changes made to your hosted zone.
--
-- /Note:/ Consider using 'changeInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
avpcwhzrrsChangeInfo :: Lens.Lens' AssociateVPCWithHostedZoneResponse Types.ChangeInfo
avpcwhzrrsChangeInfo = Lens.field @"changeInfo"
{-# DEPRECATED avpcwhzrrsChangeInfo "Use generic-lens or generic-optics with 'changeInfo' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
avpcwhzrrsResponseStatus :: Lens.Lens' AssociateVPCWithHostedZoneResponse Core.Int
avpcwhzrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED avpcwhzrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
