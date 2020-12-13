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
    chzDelegationSetId,
    chzName,
    chzVPC,
    chzHostedZoneConfig,
    chzCallerReference,

    -- * Destructuring the response
    CreateHostedZoneResponse (..),
    mkCreateHostedZoneResponse,

    -- ** Response lenses
    chzrsLocation,
    chzrsHostedZone,
    chzrsVPC,
    chzrsDelegationSet,
    chzrsChangeInfo,
    chzrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.Route53.Types

-- | A complex type that contains information about the request to create a public or private hosted zone.
--
-- /See:/ 'mkCreateHostedZone' smart constructor.
data CreateHostedZone = CreateHostedZone'
  { -- | If you want to associate a reusable delegation set with this hosted zone, the ID that Amazon Route 53 assigned to the reusable delegation set when you created it. For more information about reusable delegation sets, see <https://docs.aws.amazon.com/Route53/latest/APIReference/API_CreateReusableDelegationSet.html CreateReusableDelegationSet> .
    delegationSetId :: Lude.Maybe ResourceId,
    -- | The name of the domain. Specify a fully qualified domain name, for example, /www.example.com/ . The trailing dot is optional; Amazon Route 53 assumes that the domain name is fully qualified. This means that Route 53 treats /www.example.com/ (without a trailing dot) and /www.example.com./ (with a trailing dot) as identical.
    --
    -- If you're creating a public hosted zone, this is the name you have registered with your DNS registrar. If your domain name is registered with a registrar other than Route 53, change the name servers for your domain to the set of @NameServers@ that @CreateHostedZone@ returns in @DelegationSet@ .
    name :: Lude.Text,
    -- | (Private hosted zones only) A complex type that contains information about the Amazon VPC that you're associating with this hosted zone.
    --
    -- You can specify only one Amazon VPC when you create a private hosted zone. To associate additional Amazon VPCs with the hosted zone, use <https://docs.aws.amazon.com/Route53/latest/APIReference/API_AssociateVPCWithHostedZone.html AssociateVPCWithHostedZone> after you create a hosted zone.
    vpc :: Lude.Maybe VPC,
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
    hostedZoneConfig :: Lude.Maybe HostedZoneConfig,
    -- | A unique string that identifies the request and that allows failed @CreateHostedZone@ requests to be retried without the risk of executing the operation twice. You must use a unique @CallerReference@ string every time you submit a @CreateHostedZone@ request. @CallerReference@ can be any unique string, for example, a date/time stamp.
    callerReference :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateHostedZone' with the minimum fields required to make a request.
--
-- * 'delegationSetId' - If you want to associate a reusable delegation set with this hosted zone, the ID that Amazon Route 53 assigned to the reusable delegation set when you created it. For more information about reusable delegation sets, see <https://docs.aws.amazon.com/Route53/latest/APIReference/API_CreateReusableDelegationSet.html CreateReusableDelegationSet> .
-- * 'name' - The name of the domain. Specify a fully qualified domain name, for example, /www.example.com/ . The trailing dot is optional; Amazon Route 53 assumes that the domain name is fully qualified. This means that Route 53 treats /www.example.com/ (without a trailing dot) and /www.example.com./ (with a trailing dot) as identical.
--
-- If you're creating a public hosted zone, this is the name you have registered with your DNS registrar. If your domain name is registered with a registrar other than Route 53, change the name servers for your domain to the set of @NameServers@ that @CreateHostedZone@ returns in @DelegationSet@ .
-- * 'vpc' - (Private hosted zones only) A complex type that contains information about the Amazon VPC that you're associating with this hosted zone.
--
-- You can specify only one Amazon VPC when you create a private hosted zone. To associate additional Amazon VPCs with the hosted zone, use <https://docs.aws.amazon.com/Route53/latest/APIReference/API_AssociateVPCWithHostedZone.html AssociateVPCWithHostedZone> after you create a hosted zone.
-- * 'hostedZoneConfig' - (Optional) A complex type that contains the following optional values:
--
--
--     * For public and private hosted zones, an optional comment
--
--
--     * For private hosted zones, an optional @PrivateZone@ element
--
--
-- If you don't specify a comment or the @PrivateZone@ element, omit @HostedZoneConfig@ and the other elements.
-- * 'callerReference' - A unique string that identifies the request and that allows failed @CreateHostedZone@ requests to be retried without the risk of executing the operation twice. You must use a unique @CallerReference@ string every time you submit a @CreateHostedZone@ request. @CallerReference@ can be any unique string, for example, a date/time stamp.
mkCreateHostedZone ::
  -- | 'name'
  Lude.Text ->
  -- | 'callerReference'
  Lude.Text ->
  CreateHostedZone
mkCreateHostedZone pName_ pCallerReference_ =
  CreateHostedZone'
    { delegationSetId = Lude.Nothing,
      name = pName_,
      vpc = Lude.Nothing,
      hostedZoneConfig = Lude.Nothing,
      callerReference = pCallerReference_
    }

-- | If you want to associate a reusable delegation set with this hosted zone, the ID that Amazon Route 53 assigned to the reusable delegation set when you created it. For more information about reusable delegation sets, see <https://docs.aws.amazon.com/Route53/latest/APIReference/API_CreateReusableDelegationSet.html CreateReusableDelegationSet> .
--
-- /Note:/ Consider using 'delegationSetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
chzDelegationSetId :: Lens.Lens' CreateHostedZone (Lude.Maybe ResourceId)
chzDelegationSetId = Lens.lens (delegationSetId :: CreateHostedZone -> Lude.Maybe ResourceId) (\s a -> s {delegationSetId = a} :: CreateHostedZone)
{-# DEPRECATED chzDelegationSetId "Use generic-lens or generic-optics with 'delegationSetId' instead." #-}

-- | The name of the domain. Specify a fully qualified domain name, for example, /www.example.com/ . The trailing dot is optional; Amazon Route 53 assumes that the domain name is fully qualified. This means that Route 53 treats /www.example.com/ (without a trailing dot) and /www.example.com./ (with a trailing dot) as identical.
--
-- If you're creating a public hosted zone, this is the name you have registered with your DNS registrar. If your domain name is registered with a registrar other than Route 53, change the name servers for your domain to the set of @NameServers@ that @CreateHostedZone@ returns in @DelegationSet@ .
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
chzName :: Lens.Lens' CreateHostedZone Lude.Text
chzName = Lens.lens (name :: CreateHostedZone -> Lude.Text) (\s a -> s {name = a} :: CreateHostedZone)
{-# DEPRECATED chzName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | (Private hosted zones only) A complex type that contains information about the Amazon VPC that you're associating with this hosted zone.
--
-- You can specify only one Amazon VPC when you create a private hosted zone. To associate additional Amazon VPCs with the hosted zone, use <https://docs.aws.amazon.com/Route53/latest/APIReference/API_AssociateVPCWithHostedZone.html AssociateVPCWithHostedZone> after you create a hosted zone.
--
-- /Note:/ Consider using 'vpc' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
chzVPC :: Lens.Lens' CreateHostedZone (Lude.Maybe VPC)
chzVPC = Lens.lens (vpc :: CreateHostedZone -> Lude.Maybe VPC) (\s a -> s {vpc = a} :: CreateHostedZone)
{-# DEPRECATED chzVPC "Use generic-lens or generic-optics with 'vpc' instead." #-}

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
chzHostedZoneConfig :: Lens.Lens' CreateHostedZone (Lude.Maybe HostedZoneConfig)
chzHostedZoneConfig = Lens.lens (hostedZoneConfig :: CreateHostedZone -> Lude.Maybe HostedZoneConfig) (\s a -> s {hostedZoneConfig = a} :: CreateHostedZone)
{-# DEPRECATED chzHostedZoneConfig "Use generic-lens or generic-optics with 'hostedZoneConfig' instead." #-}

-- | A unique string that identifies the request and that allows failed @CreateHostedZone@ requests to be retried without the risk of executing the operation twice. You must use a unique @CallerReference@ string every time you submit a @CreateHostedZone@ request. @CallerReference@ can be any unique string, for example, a date/time stamp.
--
-- /Note:/ Consider using 'callerReference' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
chzCallerReference :: Lens.Lens' CreateHostedZone Lude.Text
chzCallerReference = Lens.lens (callerReference :: CreateHostedZone -> Lude.Text) (\s a -> s {callerReference = a} :: CreateHostedZone)
{-# DEPRECATED chzCallerReference "Use generic-lens or generic-optics with 'callerReference' instead." #-}

instance Lude.AWSRequest CreateHostedZone where
  type Rs CreateHostedZone = CreateHostedZoneResponse
  request = Req.postXML route53Service
  response =
    Res.receiveXML
      ( \s h x ->
          CreateHostedZoneResponse'
            Lude.<$> (h Lude..# "Location")
            Lude.<*> (x Lude..@ "HostedZone")
            Lude.<*> (x Lude..@? "VPC")
            Lude.<*> (x Lude..@ "DelegationSet")
            Lude.<*> (x Lude..@ "ChangeInfo")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToElement CreateHostedZone where
  toElement =
    Lude.mkElement
      "{https://route53.amazonaws.com/doc/2013-04-01/}CreateHostedZoneRequest"

instance Lude.ToHeaders CreateHostedZone where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath CreateHostedZone where
  toPath = Lude.const "/2013-04-01/hostedzone"

instance Lude.ToQuery CreateHostedZone where
  toQuery = Lude.const Lude.mempty

instance Lude.ToXML CreateHostedZone where
  toXML CreateHostedZone' {..} =
    Lude.mconcat
      [ "DelegationSetId" Lude.@= delegationSetId,
        "Name" Lude.@= name,
        "VPC" Lude.@= vpc,
        "HostedZoneConfig" Lude.@= hostedZoneConfig,
        "CallerReference" Lude.@= callerReference
      ]

-- | A complex type containing the response information for the hosted zone.
--
-- /See:/ 'mkCreateHostedZoneResponse' smart constructor.
data CreateHostedZoneResponse = CreateHostedZoneResponse'
  { -- | The unique URL representing the new hosted zone.
    location :: Lude.Text,
    -- | A complex type that contains general information about the hosted zone.
    hostedZone :: HostedZone,
    -- | A complex type that contains information about an Amazon VPC that you associated with this hosted zone.
    vpc :: Lude.Maybe VPC,
    -- | A complex type that describes the name servers for this hosted zone.
    delegationSet :: DelegationSet,
    -- | A complex type that contains information about the @CreateHostedZone@ request.
    changeInfo :: ChangeInfo,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateHostedZoneResponse' with the minimum fields required to make a request.
--
-- * 'location' - The unique URL representing the new hosted zone.
-- * 'hostedZone' - A complex type that contains general information about the hosted zone.
-- * 'vpc' - A complex type that contains information about an Amazon VPC that you associated with this hosted zone.
-- * 'delegationSet' - A complex type that describes the name servers for this hosted zone.
-- * 'changeInfo' - A complex type that contains information about the @CreateHostedZone@ request.
-- * 'responseStatus' - The response status code.
mkCreateHostedZoneResponse ::
  -- | 'location'
  Lude.Text ->
  -- | 'hostedZone'
  HostedZone ->
  -- | 'delegationSet'
  DelegationSet ->
  -- | 'changeInfo'
  ChangeInfo ->
  -- | 'responseStatus'
  Lude.Int ->
  CreateHostedZoneResponse
mkCreateHostedZoneResponse
  pLocation_
  pHostedZone_
  pDelegationSet_
  pChangeInfo_
  pResponseStatus_ =
    CreateHostedZoneResponse'
      { location = pLocation_,
        hostedZone = pHostedZone_,
        vpc = Lude.Nothing,
        delegationSet = pDelegationSet_,
        changeInfo = pChangeInfo_,
        responseStatus = pResponseStatus_
      }

-- | The unique URL representing the new hosted zone.
--
-- /Note:/ Consider using 'location' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
chzrsLocation :: Lens.Lens' CreateHostedZoneResponse Lude.Text
chzrsLocation = Lens.lens (location :: CreateHostedZoneResponse -> Lude.Text) (\s a -> s {location = a} :: CreateHostedZoneResponse)
{-# DEPRECATED chzrsLocation "Use generic-lens or generic-optics with 'location' instead." #-}

-- | A complex type that contains general information about the hosted zone.
--
-- /Note:/ Consider using 'hostedZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
chzrsHostedZone :: Lens.Lens' CreateHostedZoneResponse HostedZone
chzrsHostedZone = Lens.lens (hostedZone :: CreateHostedZoneResponse -> HostedZone) (\s a -> s {hostedZone = a} :: CreateHostedZoneResponse)
{-# DEPRECATED chzrsHostedZone "Use generic-lens or generic-optics with 'hostedZone' instead." #-}

-- | A complex type that contains information about an Amazon VPC that you associated with this hosted zone.
--
-- /Note:/ Consider using 'vpc' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
chzrsVPC :: Lens.Lens' CreateHostedZoneResponse (Lude.Maybe VPC)
chzrsVPC = Lens.lens (vpc :: CreateHostedZoneResponse -> Lude.Maybe VPC) (\s a -> s {vpc = a} :: CreateHostedZoneResponse)
{-# DEPRECATED chzrsVPC "Use generic-lens or generic-optics with 'vpc' instead." #-}

-- | A complex type that describes the name servers for this hosted zone.
--
-- /Note:/ Consider using 'delegationSet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
chzrsDelegationSet :: Lens.Lens' CreateHostedZoneResponse DelegationSet
chzrsDelegationSet = Lens.lens (delegationSet :: CreateHostedZoneResponse -> DelegationSet) (\s a -> s {delegationSet = a} :: CreateHostedZoneResponse)
{-# DEPRECATED chzrsDelegationSet "Use generic-lens or generic-optics with 'delegationSet' instead." #-}

-- | A complex type that contains information about the @CreateHostedZone@ request.
--
-- /Note:/ Consider using 'changeInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
chzrsChangeInfo :: Lens.Lens' CreateHostedZoneResponse ChangeInfo
chzrsChangeInfo = Lens.lens (changeInfo :: CreateHostedZoneResponse -> ChangeInfo) (\s a -> s {changeInfo = a} :: CreateHostedZoneResponse)
{-# DEPRECATED chzrsChangeInfo "Use generic-lens or generic-optics with 'changeInfo' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
chzrsResponseStatus :: Lens.Lens' CreateHostedZoneResponse Lude.Int
chzrsResponseStatus = Lens.lens (responseStatus :: CreateHostedZoneResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateHostedZoneResponse)
{-# DEPRECATED chzrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
