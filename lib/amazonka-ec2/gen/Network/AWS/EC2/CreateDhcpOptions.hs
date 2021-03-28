{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.CreateDhcpOptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a set of DHCP options for your VPC. After creating the set, you must associate it with the VPC, causing all existing and new instances that you launch in the VPC to use this set of DHCP options. The following are the individual DHCP options you can specify. For more information about the options, see <http://www.ietf.org/rfc/rfc2132.txt RFC 2132> .
--
--
--     * @domain-name-servers@ - The IP addresses of up to four domain name servers, or AmazonProvidedDNS. The default DHCP option set specifies AmazonProvidedDNS. If specifying more than one domain name server, specify the IP addresses in a single parameter, separated by commas. To have your instance receive a custom DNS hostname as specified in @domain-name@ , you must set @domain-name-servers@ to a custom DNS server.
--
--
--     * @domain-name@ - If you're using AmazonProvidedDNS in @us-east-1@ , specify @ec2.internal@ . If you're using AmazonProvidedDNS in another Region, specify @region.compute.internal@ (for example, @ap-northeast-1.compute.internal@ ). Otherwise, specify a domain name (for example, @ExampleCompany.com@ ). This value is used to complete unqualified DNS hostnames. __Important__ : Some Linux operating systems accept multiple domain names separated by spaces. However, Windows and other Linux operating systems treat the value as a single domain, which results in unexpected behavior. If your DHCP options set is associated with a VPC that has instances with multiple operating systems, specify only one domain name.
--
--
--     * @ntp-servers@ - The IP addresses of up to four Network Time Protocol (NTP) servers.
--
--
--     * @netbios-name-servers@ - The IP addresses of up to four NetBIOS name servers.
--
--
--     * @netbios-node-type@ - The NetBIOS node type (1, 2, 4, or 8). We recommend that you specify 2 (broadcast and multicast are not currently supported). For more information about these node types, see <http://www.ietf.org/rfc/rfc2132.txt RFC 2132> .
--
--
-- Your VPC automatically starts out with a set of DHCP options that includes only a DNS server that we provide (AmazonProvidedDNS). If you create a set of options, and if your VPC has an internet gateway, make sure to set the @domain-name-servers@ option either to @AmazonProvidedDNS@ or to a domain name server of your choice. For more information, see <https://docs.aws.amazon.com/vpc/latest/userguide/VPC_DHCP_Options.html DHCP Options Sets> in the /Amazon Virtual Private Cloud User Guide/ .
module Network.AWS.EC2.CreateDhcpOptions
    (
    -- * Creating a request
      CreateDhcpOptions (..)
    , mkCreateDhcpOptions
    -- ** Request lenses
    , cdoDhcpConfigurations
    , cdoDryRun
    , cdoTagSpecifications

    -- * Destructuring the response
    , CreateDhcpOptionsResponse (..)
    , mkCreateDhcpOptionsResponse
    -- ** Response lenses
    , cdorrsDhcpOptions
    , cdorrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateDhcpOptions' smart constructor.
data CreateDhcpOptions = CreateDhcpOptions'
  { dhcpConfigurations :: [Types.NewDhcpConfiguration]
    -- ^ A DHCP configuration option.
  , dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  , tagSpecifications :: Core.Maybe [Types.TagSpecification]
    -- ^ The tags to assign to the DHCP option.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateDhcpOptions' value with any optional fields omitted.
mkCreateDhcpOptions
    :: CreateDhcpOptions
mkCreateDhcpOptions
  = CreateDhcpOptions'{dhcpConfigurations = Core.mempty,
                       dryRun = Core.Nothing, tagSpecifications = Core.Nothing}

-- | A DHCP configuration option.
--
-- /Note:/ Consider using 'dhcpConfigurations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdoDhcpConfigurations :: Lens.Lens' CreateDhcpOptions [Types.NewDhcpConfiguration]
cdoDhcpConfigurations = Lens.field @"dhcpConfigurations"
{-# INLINEABLE cdoDhcpConfigurations #-}
{-# DEPRECATED dhcpConfigurations "Use generic-lens or generic-optics with 'dhcpConfigurations' instead"  #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdoDryRun :: Lens.Lens' CreateDhcpOptions (Core.Maybe Core.Bool)
cdoDryRun = Lens.field @"dryRun"
{-# INLINEABLE cdoDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

-- | The tags to assign to the DHCP option.
--
-- /Note:/ Consider using 'tagSpecifications' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdoTagSpecifications :: Lens.Lens' CreateDhcpOptions (Core.Maybe [Types.TagSpecification])
cdoTagSpecifications = Lens.field @"tagSpecifications"
{-# INLINEABLE cdoTagSpecifications #-}
{-# DEPRECATED tagSpecifications "Use generic-lens or generic-optics with 'tagSpecifications' instead"  #-}

instance Core.ToQuery CreateDhcpOptions where
        toQuery CreateDhcpOptions{..}
          = Core.toQueryPair "Action" ("CreateDhcpOptions" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<> Core.toQueryList "DhcpConfiguration" dhcpConfigurations
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun
              Core.<>
              Core.maybe Core.mempty (Core.toQueryList "TagSpecification")
                tagSpecifications

instance Core.ToHeaders CreateDhcpOptions where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest CreateDhcpOptions where
        type Rs CreateDhcpOptions = CreateDhcpOptionsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.mempty,
                         Core._rqHeaders =
                           Core.pure
                             ("Content-Type",
                              "application/x-www-form-urlencoded; charset=utf-8")
                             Core.<> Core.toHeaders x,
                         Core._rqBody = Core.toFormBody (Core.toQuery x)}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXML
              (\ s h x ->
                 CreateDhcpOptionsResponse' Core.<$>
                   (x Core..@? "dhcpOptions") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateDhcpOptionsResponse' smart constructor.
data CreateDhcpOptionsResponse = CreateDhcpOptionsResponse'
  { dhcpOptions :: Core.Maybe Types.DhcpOptions
    -- ^ A set of DHCP options.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateDhcpOptionsResponse' value with any optional fields omitted.
mkCreateDhcpOptionsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateDhcpOptionsResponse
mkCreateDhcpOptionsResponse responseStatus
  = CreateDhcpOptionsResponse'{dhcpOptions = Core.Nothing,
                               responseStatus}

-- | A set of DHCP options.
--
-- /Note:/ Consider using 'dhcpOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdorrsDhcpOptions :: Lens.Lens' CreateDhcpOptionsResponse (Core.Maybe Types.DhcpOptions)
cdorrsDhcpOptions = Lens.field @"dhcpOptions"
{-# INLINEABLE cdorrsDhcpOptions #-}
{-# DEPRECATED dhcpOptions "Use generic-lens or generic-optics with 'dhcpOptions' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdorrsResponseStatus :: Lens.Lens' CreateDhcpOptionsResponse Core.Int
cdorrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE cdorrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
