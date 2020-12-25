{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.CreateHostedZone
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new public or private hosted zone. You create records in a public hosted zone to define how you want to route traffic on the internet for a domain, such as example.com, and its subdomains (apex.example.com, acme.example.com). You create records in a private hosted zone to define how you want to route traffic for a domain and its subdomains within one or more Amazon Virtual Private Clouds (Amazon VPCs).
--
-- /Important:/ You can't convert a public hosted zone to a private hosted zone or vice versa. Instead, you must create a new hosted zone with the same name and create new resource record sets.
-- For more information about charges for hosted zones, see <http://aws.amazon.com/route53/pricing/ Amazon Route 53 Pricing> .
-- Note the following:
--
--     * You can't create a hosted zone for a top-level domain (TLD) such as .com.
--
--
--     * For public hosted zones, Route 53 automatically creates a default SOA record and four NS records for the zone. For more information about SOA and NS records, see <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/SOA-NSrecords.html NS and SOA Records that Route 53 Creates for a Hosted Zone> in the /Amazon Route 53 Developer Guide/ .
-- If you want to use the same name servers for multiple public hosted zones, you can optionally associate a reusable delegation set with the hosted zone. See the @DelegationSetId@ element.
--
--
--     * If your domain is registered with a registrar other than Route 53, you must update the name servers with your registrar to make Route 53 the DNS service for the domain. For more information, see <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/MigratingDNS.html Migrating DNS Service for an Existing Domain to Amazon Route 53> in the /Amazon Route 53 Developer Guide/ .
--
--
-- When you submit a @CreateHostedZone@ request, the initial status of the hosted zone is @PENDING@ . For public hosted zones, this means that the NS and SOA records are not yet available on all Route 53 DNS servers. When the NS and SOA records are available, the status of the zone changes to @INSYNC@ .
module Network.AWS.Route53.CreateHostedZone
  ( -- * Creating a request
    CreateHostedZone (..),
    mkCreateHostedZone,

    -- ** Request lenses
    chzName,
    chzCallerReference,
    chzDelegationSetId,
    chzHostedZoneConfig,
    chzVPC,

    -- * Destructuring the response
    CreateHostedZoneResponse (..),
    mkCreateHostedZoneResponse,

    -- ** Response lenses
    chzrrsHostedZone,
    chzrrsChangeInfo,
    chzrrsDelegationSet,
    chzrrsLocation,
    chzrrsVPC,
    chzrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.Route53.Types as Types

-- | A complex type that contains information about the request to create a public or private hosted zone.
--
-- /See:/ 'mkCreateHostedZone' smart constructor.
data CreateHostedZone = CreateHostedZone'
  { -- | The name of the domain. Specify a fully qualified domain name, for example, /www.example.com/ . The trailing dot is optional; Amazon Route 53 assumes that the domain name is fully qualified. This means that Route 53 treats /www.example.com/ (without a trailing dot) and /www.example.com./ (with a trailing dot) as identical.
    --
    -- If you're creating a public hosted zone, this is the name you have registered with your DNS registrar. If your domain name is registered with a registrar other than Route 53, change the name servers for your domain to the set of @NameServers@ that @CreateHostedZone@ returns in @DelegationSet@ .
    name :: Types.Name,
    -- | A unique string that identifies the request and that allows failed @CreateHostedZone@ requests to be retried without the risk of executing the operation twice. You must use a unique @CallerReference@ string every time you submit a @CreateHostedZone@ request. @CallerReference@ can be any unique string, for example, a date/time stamp.
    callerReference :: Types.CallerReference,
    -- | If you want to associate a reusable delegation set with this hosted zone, the ID that Amazon Route 53 assigned to the reusable delegation set when you created it. For more information about reusable delegation sets, see <https://docs.aws.amazon.com/Route53/latest/APIReference/API_CreateReusableDelegationSet.html CreateReusableDelegationSet> .
    delegationSetId :: Core.Maybe Types.ResourceId,
    -- | (Optional) A complex type that contains the following optional values:
    --
    --
    --     * For public and private hosted zones, an optional comment
    --
    --
    --     * For private hosted zones, an optional @PrivateZone@ element
    --
    --
    -- If you don't specify a comment or the @PrivateZone@ element, omit @HostedZoneConfig@ and the other elements.
    hostedZoneConfig :: Core.Maybe Types.HostedZoneConfig,
    -- | (Private hosted zones only) A complex type that contains information about the Amazon VPC that you're associating with this hosted zone.
    --
    -- You can specify only one Amazon VPC when you create a private hosted zone. To associate additional Amazon VPCs with the hosted zone, use <https://docs.aws.amazon.com/Route53/latest/APIReference/API_AssociateVPCWithHostedZone.html AssociateVPCWithHostedZone> after you create a hosted zone.
    vpc :: Core.Maybe Types.VPC
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateHostedZone' value with any optional fields omitted.
mkCreateHostedZone ::
  -- | 'name'
  Types.Name ->
  -- | 'callerReference'
  Types.CallerReference ->
  CreateHostedZone
mkCreateHostedZone name callerReference =
  CreateHostedZone'
    { name,
      callerReference,
      delegationSetId = Core.Nothing,
      hostedZoneConfig = Core.Nothing,
      vpc = Core.Nothing
    }

-- | The name of the domain. Specify a fully qualified domain name, for example, /www.example.com/ . The trailing dot is optional; Amazon Route 53 assumes that the domain name is fully qualified. This means that Route 53 treats /www.example.com/ (without a trailing dot) and /www.example.com./ (with a trailing dot) as identical.
--
-- If you're creating a public hosted zone, this is the name you have registered with your DNS registrar. If your domain name is registered with a registrar other than Route 53, change the name servers for your domain to the set of @NameServers@ that @CreateHostedZone@ returns in @DelegationSet@ .
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
chzName :: Lens.Lens' CreateHostedZone Types.Name
chzName = Lens.field @"name"
{-# DEPRECATED chzName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | A unique string that identifies the request and that allows failed @CreateHostedZone@ requests to be retried without the risk of executing the operation twice. You must use a unique @CallerReference@ string every time you submit a @CreateHostedZone@ request. @CallerReference@ can be any unique string, for example, a date/time stamp.
--
-- /Note:/ Consider using 'callerReference' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
chzCallerReference :: Lens.Lens' CreateHostedZone Types.CallerReference
chzCallerReference = Lens.field @"callerReference"
{-# DEPRECATED chzCallerReference "Use generic-lens or generic-optics with 'callerReference' instead." #-}

-- | If you want to associate a reusable delegation set with this hosted zone, the ID that Amazon Route 53 assigned to the reusable delegation set when you created it. For more information about reusable delegation sets, see <https://docs.aws.amazon.com/Route53/latest/APIReference/API_CreateReusableDelegationSet.html CreateReusableDelegationSet> .
--
-- /Note:/ Consider using 'delegationSetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
chzDelegationSetId :: Lens.Lens' CreateHostedZone (Core.Maybe Types.ResourceId)
chzDelegationSetId = Lens.field @"delegationSetId"
{-# DEPRECATED chzDelegationSetId "Use generic-lens or generic-optics with 'delegationSetId' instead." #-}

-- | (Optional) A complex type that contains the following optional values:
--
--
--     * For public and private hosted zones, an optional comment
--
--
--     * For private hosted zones, an optional @PrivateZone@ element
--
--
-- If you don't specify a comment or the @PrivateZone@ element, omit @HostedZoneConfig@ and the other elements.
--
-- /Note:/ Consider using 'hostedZoneConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
chzHostedZoneConfig :: Lens.Lens' CreateHostedZone (Core.Maybe Types.HostedZoneConfig)
chzHostedZoneConfig = Lens.field @"hostedZoneConfig"
{-# DEPRECATED chzHostedZoneConfig "Use generic-lens or generic-optics with 'hostedZoneConfig' instead." #-}

-- | (Private hosted zones only) A complex type that contains information about the Amazon VPC that you're associating with this hosted zone.
--
-- You can specify only one Amazon VPC when you create a private hosted zone. To associate additional Amazon VPCs with the hosted zone, use <https://docs.aws.amazon.com/Route53/latest/APIReference/API_AssociateVPCWithHostedZone.html AssociateVPCWithHostedZone> after you create a hosted zone.
--
-- /Note:/ Consider using 'vpc' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
chzVPC :: Lens.Lens' CreateHostedZone (Core.Maybe Types.VPC)
chzVPC = Lens.field @"vpc"
{-# DEPRECATED chzVPC "Use generic-lens or generic-optics with 'vpc' instead." #-}

instance Core.ToXML CreateHostedZone where
  toXML CreateHostedZone {..} =
    Core.toXMLNode "Name" name
      Core.<> Core.toXMLNode "CallerReference" callerReference
      Core.<> Core.toXMLNode "DelegationSetId" Core.<$> delegationSetId
      Core.<> Core.toXMLNode "HostedZoneConfig" Core.<$> hostedZoneConfig
      Core.<> Core.toXMLNode "VPC" Core.<$> vpc
  toXMLDocument =
    Core.mkXMLElement
      "{https://route53.amazonaws.com/doc/2013-04-01/}CreateHostedZoneRequest"

instance Core.AWSRequest CreateHostedZone where
  type Rs CreateHostedZone = CreateHostedZoneResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/2013-04-01/hostedzone",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = Core.toXMLBody x
      }
  response =
    Response.receiveXML
      ( \s h x ->
          CreateHostedZoneResponse'
            Core.<$> (x Core..@ "HostedZone")
            Core.<*> (x Core..@ "ChangeInfo")
            Core.<*> (x Core..@ "DelegationSet")
            Core.<*> (Core.parseHeader "Location" h)
            Core.<*> (x Core..@? "VPC")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | A complex type containing the response information for the hosted zone.
--
-- /See:/ 'mkCreateHostedZoneResponse' smart constructor.
data CreateHostedZoneResponse = CreateHostedZoneResponse'
  { -- | A complex type that contains general information about the hosted zone.
    hostedZone :: Types.HostedZone,
    -- | A complex type that contains information about the @CreateHostedZone@ request.
    changeInfo :: Types.ChangeInfo,
    -- | A complex type that describes the name servers for this hosted zone.
    delegationSet :: Types.DelegationSet,
    -- | The unique URL representing the new hosted zone.
    location :: Types.ResourceURI,
    -- | A complex type that contains information about an Amazon VPC that you associated with this hosted zone.
    vpc :: Core.Maybe Types.VPC,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'CreateHostedZoneResponse' value with any optional fields omitted.
mkCreateHostedZoneResponse ::
  -- | 'hostedZone'
  Types.HostedZone ->
  -- | 'changeInfo'
  Types.ChangeInfo ->
  -- | 'delegationSet'
  Types.DelegationSet ->
  -- | 'location'
  Types.ResourceURI ->
  -- | 'responseStatus'
  Core.Int ->
  CreateHostedZoneResponse
mkCreateHostedZoneResponse
  hostedZone
  changeInfo
  delegationSet
  location
  responseStatus =
    CreateHostedZoneResponse'
      { hostedZone,
        changeInfo,
        delegationSet,
        location,
        vpc = Core.Nothing,
        responseStatus
      }

-- | A complex type that contains general information about the hosted zone.
--
-- /Note:/ Consider using 'hostedZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
chzrrsHostedZone :: Lens.Lens' CreateHostedZoneResponse Types.HostedZone
chzrrsHostedZone = Lens.field @"hostedZone"
{-# DEPRECATED chzrrsHostedZone "Use generic-lens or generic-optics with 'hostedZone' instead." #-}

-- | A complex type that contains information about the @CreateHostedZone@ request.
--
-- /Note:/ Consider using 'changeInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
chzrrsChangeInfo :: Lens.Lens' CreateHostedZoneResponse Types.ChangeInfo
chzrrsChangeInfo = Lens.field @"changeInfo"
{-# DEPRECATED chzrrsChangeInfo "Use generic-lens or generic-optics with 'changeInfo' instead." #-}

-- | A complex type that describes the name servers for this hosted zone.
--
-- /Note:/ Consider using 'delegationSet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
chzrrsDelegationSet :: Lens.Lens' CreateHostedZoneResponse Types.DelegationSet
chzrrsDelegationSet = Lens.field @"delegationSet"
{-# DEPRECATED chzrrsDelegationSet "Use generic-lens or generic-optics with 'delegationSet' instead." #-}

-- | The unique URL representing the new hosted zone.
--
-- /Note:/ Consider using 'location' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
chzrrsLocation :: Lens.Lens' CreateHostedZoneResponse Types.ResourceURI
chzrrsLocation = Lens.field @"location"
{-# DEPRECATED chzrrsLocation "Use generic-lens or generic-optics with 'location' instead." #-}

-- | A complex type that contains information about an Amazon VPC that you associated with this hosted zone.
--
-- /Note:/ Consider using 'vpc' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
chzrrsVPC :: Lens.Lens' CreateHostedZoneResponse (Core.Maybe Types.VPC)
chzrrsVPC = Lens.field @"vpc"
{-# DEPRECATED chzrrsVPC "Use generic-lens or generic-optics with 'vpc' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
chzrrsResponseStatus :: Lens.Lens' CreateHostedZoneResponse Core.Int
chzrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED chzrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
