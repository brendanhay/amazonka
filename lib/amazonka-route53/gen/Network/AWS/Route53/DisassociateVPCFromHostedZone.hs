{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.DisassociateVPCFromHostedZone
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociates an Amazon Virtual Private Cloud (Amazon VPC) from an Amazon Route 53 private hosted zone. Note the following:
--
--
--     * You can't disassociate the last Amazon VPC from a private hosted zone.
--
--
--     * You can't convert a private hosted zone into a public hosted zone.
--
--
--     * You can submit a @DisassociateVPCFromHostedZone@ request using either the account that created the hosted zone or the account that created the Amazon VPC.
--
--
--     * Some services, such as AWS Cloud Map and Amazon Elastic File System (Amazon EFS) automatically create hosted zones and associate VPCs with the hosted zones. A service can create a hosted zone using your account or using its own account. You can disassociate a VPC from a hosted zone only if the service created the hosted zone using your account.
-- When you run <https://docs.aws.amazon.com/Route53/latest/APIReference/API_ListHostedZonesByVPC.html DisassociateVPCFromHostedZone> , if the hosted zone has a value for @OwningAccount@ , you can use @DisassociateVPCFromHostedZone@ . If the hosted zone has a value for @OwningService@ , you can't use @DisassociateVPCFromHostedZone@ .
--
--
module Network.AWS.Route53.DisassociateVPCFromHostedZone
    (
    -- * Creating a request
      DisassociateVPCFromHostedZone (..)
    , mkDisassociateVPCFromHostedZone
    -- ** Request lenses
    , dvpcfhzHostedZoneId
    , dvpcfhzVPC
    , dvpcfhzComment

    -- * Destructuring the response
    , DisassociateVPCFromHostedZoneResponse (..)
    , mkDisassociateVPCFromHostedZoneResponse
    -- ** Response lenses
    , dvpcfhzrrsChangeInfo
    , dvpcfhzrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.Route53.Types as Types

-- | A complex type that contains information about the VPC that you want to disassociate from a specified private hosted zone.
--
-- /See:/ 'mkDisassociateVPCFromHostedZone' smart constructor.
data DisassociateVPCFromHostedZone = DisassociateVPCFromHostedZone'
  { hostedZoneId :: Types.HostedZoneId
    -- ^ The ID of the private hosted zone that you want to disassociate a VPC from.
  , vpc :: Types.VPC
    -- ^ A complex type that contains information about the VPC that you're disassociating from the specified hosted zone.
  , comment :: Core.Maybe Types.Comment
    -- ^ /Optional:/ A comment about the disassociation request.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DisassociateVPCFromHostedZone' value with any optional fields omitted.
mkDisassociateVPCFromHostedZone
    :: Types.HostedZoneId -- ^ 'hostedZoneId'
    -> Types.VPC -- ^ 'vpc'
    -> DisassociateVPCFromHostedZone
mkDisassociateVPCFromHostedZone hostedZoneId vpc
  = DisassociateVPCFromHostedZone'{hostedZoneId, vpc,
                                   comment = Core.Nothing}

-- | The ID of the private hosted zone that you want to disassociate a VPC from.
--
-- /Note:/ Consider using 'hostedZoneId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvpcfhzHostedZoneId :: Lens.Lens' DisassociateVPCFromHostedZone Types.HostedZoneId
dvpcfhzHostedZoneId = Lens.field @"hostedZoneId"
{-# INLINEABLE dvpcfhzHostedZoneId #-}
{-# DEPRECATED hostedZoneId "Use generic-lens or generic-optics with 'hostedZoneId' instead"  #-}

-- | A complex type that contains information about the VPC that you're disassociating from the specified hosted zone.
--
-- /Note:/ Consider using 'vpc' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvpcfhzVPC :: Lens.Lens' DisassociateVPCFromHostedZone Types.VPC
dvpcfhzVPC = Lens.field @"vpc"
{-# INLINEABLE dvpcfhzVPC #-}
{-# DEPRECATED vpc "Use generic-lens or generic-optics with 'vpc' instead"  #-}

-- | /Optional:/ A comment about the disassociation request.
--
-- /Note:/ Consider using 'comment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvpcfhzComment :: Lens.Lens' DisassociateVPCFromHostedZone (Core.Maybe Types.Comment)
dvpcfhzComment = Lens.field @"comment"
{-# INLINEABLE dvpcfhzComment #-}
{-# DEPRECATED comment "Use generic-lens or generic-optics with 'comment' instead"  #-}

instance Core.ToQuery DisassociateVPCFromHostedZone where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DisassociateVPCFromHostedZone where
        toHeaders _ = Core.pure Core.mempty

instance Core.ToXML DisassociateVPCFromHostedZone where
        toXML DisassociateVPCFromHostedZone{..}
          = Core.toXMLElement "VPC" vpc Core.<>
              Core.maybe Core.mempty (Core.toXMLElement "Comment") comment
        toXMLDocument
          = Core.newXMLDocument
              "{https://route53.amazonaws.com/doc/2013-04-01/}DisassociateVPCFromHostedZoneRequest"

instance Core.AWSRequest DisassociateVPCFromHostedZone where
        type Rs DisassociateVPCFromHostedZone =
             DisassociateVPCFromHostedZoneResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST,
                         Core._rqPath =
                           "/2013-04-01/hostedzone/" Core.<> Core.toText hostedZoneId Core.<>
                             "/disassociatevpc",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toXMLBody (Core.toXMLDocument x)}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXML
              (\ s h x ->
                 DisassociateVPCFromHostedZoneResponse' Core.<$>
                   (x Core..@ "ChangeInfo") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | A complex type that contains the response information for the disassociate request.
--
-- /See:/ 'mkDisassociateVPCFromHostedZoneResponse' smart constructor.
data DisassociateVPCFromHostedZoneResponse = DisassociateVPCFromHostedZoneResponse'
  { changeInfo :: Types.ChangeInfo
    -- ^ A complex type that describes the changes made to the specified private hosted zone.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DisassociateVPCFromHostedZoneResponse' value with any optional fields omitted.
mkDisassociateVPCFromHostedZoneResponse
    :: Types.ChangeInfo -- ^ 'changeInfo'
    -> Core.Int -- ^ 'responseStatus'
    -> DisassociateVPCFromHostedZoneResponse
mkDisassociateVPCFromHostedZoneResponse changeInfo responseStatus
  = DisassociateVPCFromHostedZoneResponse'{changeInfo,
                                           responseStatus}

-- | A complex type that describes the changes made to the specified private hosted zone.
--
-- /Note:/ Consider using 'changeInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvpcfhzrrsChangeInfo :: Lens.Lens' DisassociateVPCFromHostedZoneResponse Types.ChangeInfo
dvpcfhzrrsChangeInfo = Lens.field @"changeInfo"
{-# INLINEABLE dvpcfhzrrsChangeInfo #-}
{-# DEPRECATED changeInfo "Use generic-lens or generic-optics with 'changeInfo' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvpcfhzrrsResponseStatus :: Lens.Lens' DisassociateVPCFromHostedZoneResponse Core.Int
dvpcfhzrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dvpcfhzrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
