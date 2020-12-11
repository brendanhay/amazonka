{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.CreateDHCPOptions
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
module Network.AWS.EC2.CreateDHCPOptions
  ( -- * Creating a request
    CreateDHCPOptions (..),
    mkCreateDHCPOptions,

    -- ** Request lenses
    cdoTagSpecifications,
    cdoDryRun,
    cdoDHCPConfigurations,

    -- * Destructuring the response
    CreateDHCPOptionsResponse (..),
    mkCreateDHCPOptionsResponse,

    -- ** Response lenses
    cdorsDHCPOptions,
    cdorsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateDHCPOptions' smart constructor.
data CreateDHCPOptions = CreateDHCPOptions'
  { tagSpecifications ::
      Lude.Maybe [TagSpecification],
    dryRun :: Lude.Maybe Lude.Bool,
    dhcpConfigurations :: [NewDHCPConfiguration]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateDHCPOptions' with the minimum fields required to make a request.
--
-- * 'dhcpConfigurations' - A DHCP configuration option.
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'tagSpecifications' - The tags to assign to the DHCP option.
mkCreateDHCPOptions ::
  CreateDHCPOptions
mkCreateDHCPOptions =
  CreateDHCPOptions'
    { tagSpecifications = Lude.Nothing,
      dryRun = Lude.Nothing,
      dhcpConfigurations = Lude.mempty
    }

-- | The tags to assign to the DHCP option.
--
-- /Note:/ Consider using 'tagSpecifications' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdoTagSpecifications :: Lens.Lens' CreateDHCPOptions (Lude.Maybe [TagSpecification])
cdoTagSpecifications = Lens.lens (tagSpecifications :: CreateDHCPOptions -> Lude.Maybe [TagSpecification]) (\s a -> s {tagSpecifications = a} :: CreateDHCPOptions)
{-# DEPRECATED cdoTagSpecifications "Use generic-lens or generic-optics with 'tagSpecifications' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdoDryRun :: Lens.Lens' CreateDHCPOptions (Lude.Maybe Lude.Bool)
cdoDryRun = Lens.lens (dryRun :: CreateDHCPOptions -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: CreateDHCPOptions)
{-# DEPRECATED cdoDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | A DHCP configuration option.
--
-- /Note:/ Consider using 'dhcpConfigurations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdoDHCPConfigurations :: Lens.Lens' CreateDHCPOptions [NewDHCPConfiguration]
cdoDHCPConfigurations = Lens.lens (dhcpConfigurations :: CreateDHCPOptions -> [NewDHCPConfiguration]) (\s a -> s {dhcpConfigurations = a} :: CreateDHCPOptions)
{-# DEPRECATED cdoDHCPConfigurations "Use generic-lens or generic-optics with 'dhcpConfigurations' instead." #-}

instance Lude.AWSRequest CreateDHCPOptions where
  type Rs CreateDHCPOptions = CreateDHCPOptionsResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          CreateDHCPOptionsResponse'
            Lude.<$> (x Lude..@? "dhcpOptions") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateDHCPOptions where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath CreateDHCPOptions where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateDHCPOptions where
  toQuery CreateDHCPOptions' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("CreateDhcpOptions" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        Lude.toQuery
          (Lude.toQueryList "TagSpecification" Lude.<$> tagSpecifications),
        "DryRun" Lude.=: dryRun,
        Lude.toQueryList "DhcpConfiguration" dhcpConfigurations
      ]

-- | /See:/ 'mkCreateDHCPOptionsResponse' smart constructor.
data CreateDHCPOptionsResponse = CreateDHCPOptionsResponse'
  { dhcpOptions ::
      Lude.Maybe DHCPOptions,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateDHCPOptionsResponse' with the minimum fields required to make a request.
--
-- * 'dhcpOptions' - A set of DHCP options.
-- * 'responseStatus' - The response status code.
mkCreateDHCPOptionsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateDHCPOptionsResponse
mkCreateDHCPOptionsResponse pResponseStatus_ =
  CreateDHCPOptionsResponse'
    { dhcpOptions = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A set of DHCP options.
--
-- /Note:/ Consider using 'dhcpOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdorsDHCPOptions :: Lens.Lens' CreateDHCPOptionsResponse (Lude.Maybe DHCPOptions)
cdorsDHCPOptions = Lens.lens (dhcpOptions :: CreateDHCPOptionsResponse -> Lude.Maybe DHCPOptions) (\s a -> s {dhcpOptions = a} :: CreateDHCPOptionsResponse)
{-# DEPRECATED cdorsDHCPOptions "Use generic-lens or generic-optics with 'dhcpOptions' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdorsResponseStatus :: Lens.Lens' CreateDHCPOptionsResponse Lude.Int
cdorsResponseStatus = Lens.lens (responseStatus :: CreateDHCPOptionsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateDHCPOptionsResponse)
{-# DEPRECATED cdorsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
