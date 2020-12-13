{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.IPPermission
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.IPPermission
  ( IPPermission (..),

    -- * Smart constructor
    mkIPPermission,

    -- * Lenses
    ipFromPort,
    ipUserIdGroupPairs,
    ipPrefixListIds,
    ipIPProtocol,
    ipToPort,
    ipIPv6Ranges,
    ipIPRanges,
  )
where

import Network.AWS.EC2.Types.IPRange
import Network.AWS.EC2.Types.IPv6Range
import Network.AWS.EC2.Types.PrefixListId
import Network.AWS.EC2.Types.UserIdGroupPair
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes a set of permissions for a security group rule.
--
-- /See:/ 'mkIPPermission' smart constructor.
data IPPermission = IPPermission'
  { -- | The start of port range for the TCP and UDP protocols, or an ICMP/ICMPv6 type number. A value of @-1@ indicates all ICMP/ICMPv6 types. If you specify all ICMP/ICMPv6 types, you must specify all codes.
    fromPort :: Lude.Maybe Lude.Int,
    -- | The security group and AWS account ID pairs.
    userIdGroupPairs :: Lude.Maybe [UserIdGroupPair],
    -- | [VPC only] The prefix list IDs.
    prefixListIds :: Lude.Maybe [PrefixListId],
    -- | The IP protocol name (@tcp@ , @udp@ , @icmp@ , @icmpv6@ ) or number (see <http://www.iana.org/assignments/protocol-numbers/protocol-numbers.xhtml Protocol Numbers> ).
    --
    -- [VPC only] Use @-1@ to specify all protocols. When authorizing security group rules, specifying @-1@ or a protocol number other than @tcp@ , @udp@ , @icmp@ , or @icmpv6@ allows traffic on all ports, regardless of any port range you specify. For @tcp@ , @udp@ , and @icmp@ , you must specify a port range. For @icmpv6@ , the port range is optional; if you omit the port range, traffic for all types and codes is allowed.
    ipProtocol :: Lude.Text,
    -- | The end of port range for the TCP and UDP protocols, or an ICMP/ICMPv6 code. A value of @-1@ indicates all ICMP/ICMPv6 codes. If you specify all ICMP/ICMPv6 types, you must specify all codes.
    toPort :: Lude.Maybe Lude.Int,
    -- | [VPC only] The IPv6 ranges.
    ipv6Ranges :: Lude.Maybe [IPv6Range],
    -- | The IPv4 ranges.
    ipRanges :: Lude.Maybe [IPRange]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'IPPermission' with the minimum fields required to make a request.
--
-- * 'fromPort' - The start of port range for the TCP and UDP protocols, or an ICMP/ICMPv6 type number. A value of @-1@ indicates all ICMP/ICMPv6 types. If you specify all ICMP/ICMPv6 types, you must specify all codes.
-- * 'userIdGroupPairs' - The security group and AWS account ID pairs.
-- * 'prefixListIds' - [VPC only] The prefix list IDs.
-- * 'ipProtocol' - The IP protocol name (@tcp@ , @udp@ , @icmp@ , @icmpv6@ ) or number (see <http://www.iana.org/assignments/protocol-numbers/protocol-numbers.xhtml Protocol Numbers> ).
--
-- [VPC only] Use @-1@ to specify all protocols. When authorizing security group rules, specifying @-1@ or a protocol number other than @tcp@ , @udp@ , @icmp@ , or @icmpv6@ allows traffic on all ports, regardless of any port range you specify. For @tcp@ , @udp@ , and @icmp@ , you must specify a port range. For @icmpv6@ , the port range is optional; if you omit the port range, traffic for all types and codes is allowed.
-- * 'toPort' - The end of port range for the TCP and UDP protocols, or an ICMP/ICMPv6 code. A value of @-1@ indicates all ICMP/ICMPv6 codes. If you specify all ICMP/ICMPv6 types, you must specify all codes.
-- * 'ipv6Ranges' - [VPC only] The IPv6 ranges.
-- * 'ipRanges' - The IPv4 ranges.
mkIPPermission ::
  -- | 'ipProtocol'
  Lude.Text ->
  IPPermission
mkIPPermission pIPProtocol_ =
  IPPermission'
    { fromPort = Lude.Nothing,
      userIdGroupPairs = Lude.Nothing,
      prefixListIds = Lude.Nothing,
      ipProtocol = pIPProtocol_,
      toPort = Lude.Nothing,
      ipv6Ranges = Lude.Nothing,
      ipRanges = Lude.Nothing
    }

-- | The start of port range for the TCP and UDP protocols, or an ICMP/ICMPv6 type number. A value of @-1@ indicates all ICMP/ICMPv6 types. If you specify all ICMP/ICMPv6 types, you must specify all codes.
--
-- /Note:/ Consider using 'fromPort' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ipFromPort :: Lens.Lens' IPPermission (Lude.Maybe Lude.Int)
ipFromPort = Lens.lens (fromPort :: IPPermission -> Lude.Maybe Lude.Int) (\s a -> s {fromPort = a} :: IPPermission)
{-# DEPRECATED ipFromPort "Use generic-lens or generic-optics with 'fromPort' instead." #-}

-- | The security group and AWS account ID pairs.
--
-- /Note:/ Consider using 'userIdGroupPairs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ipUserIdGroupPairs :: Lens.Lens' IPPermission (Lude.Maybe [UserIdGroupPair])
ipUserIdGroupPairs = Lens.lens (userIdGroupPairs :: IPPermission -> Lude.Maybe [UserIdGroupPair]) (\s a -> s {userIdGroupPairs = a} :: IPPermission)
{-# DEPRECATED ipUserIdGroupPairs "Use generic-lens or generic-optics with 'userIdGroupPairs' instead." #-}

-- | [VPC only] The prefix list IDs.
--
-- /Note:/ Consider using 'prefixListIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ipPrefixListIds :: Lens.Lens' IPPermission (Lude.Maybe [PrefixListId])
ipPrefixListIds = Lens.lens (prefixListIds :: IPPermission -> Lude.Maybe [PrefixListId]) (\s a -> s {prefixListIds = a} :: IPPermission)
{-# DEPRECATED ipPrefixListIds "Use generic-lens or generic-optics with 'prefixListIds' instead." #-}

-- | The IP protocol name (@tcp@ , @udp@ , @icmp@ , @icmpv6@ ) or number (see <http://www.iana.org/assignments/protocol-numbers/protocol-numbers.xhtml Protocol Numbers> ).
--
-- [VPC only] Use @-1@ to specify all protocols. When authorizing security group rules, specifying @-1@ or a protocol number other than @tcp@ , @udp@ , @icmp@ , or @icmpv6@ allows traffic on all ports, regardless of any port range you specify. For @tcp@ , @udp@ , and @icmp@ , you must specify a port range. For @icmpv6@ , the port range is optional; if you omit the port range, traffic for all types and codes is allowed.
--
-- /Note:/ Consider using 'ipProtocol' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ipIPProtocol :: Lens.Lens' IPPermission Lude.Text
ipIPProtocol = Lens.lens (ipProtocol :: IPPermission -> Lude.Text) (\s a -> s {ipProtocol = a} :: IPPermission)
{-# DEPRECATED ipIPProtocol "Use generic-lens or generic-optics with 'ipProtocol' instead." #-}

-- | The end of port range for the TCP and UDP protocols, or an ICMP/ICMPv6 code. A value of @-1@ indicates all ICMP/ICMPv6 codes. If you specify all ICMP/ICMPv6 types, you must specify all codes.
--
-- /Note:/ Consider using 'toPort' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ipToPort :: Lens.Lens' IPPermission (Lude.Maybe Lude.Int)
ipToPort = Lens.lens (toPort :: IPPermission -> Lude.Maybe Lude.Int) (\s a -> s {toPort = a} :: IPPermission)
{-# DEPRECATED ipToPort "Use generic-lens or generic-optics with 'toPort' instead." #-}

-- | [VPC only] The IPv6 ranges.
--
-- /Note:/ Consider using 'ipv6Ranges' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ipIPv6Ranges :: Lens.Lens' IPPermission (Lude.Maybe [IPv6Range])
ipIPv6Ranges = Lens.lens (ipv6Ranges :: IPPermission -> Lude.Maybe [IPv6Range]) (\s a -> s {ipv6Ranges = a} :: IPPermission)
{-# DEPRECATED ipIPv6Ranges "Use generic-lens or generic-optics with 'ipv6Ranges' instead." #-}

-- | The IPv4 ranges.
--
-- /Note:/ Consider using 'ipRanges' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ipIPRanges :: Lens.Lens' IPPermission (Lude.Maybe [IPRange])
ipIPRanges = Lens.lens (ipRanges :: IPPermission -> Lude.Maybe [IPRange]) (\s a -> s {ipRanges = a} :: IPPermission)
{-# DEPRECATED ipIPRanges "Use generic-lens or generic-optics with 'ipRanges' instead." #-}

instance Lude.FromXML IPPermission where
  parseXML x =
    IPPermission'
      Lude.<$> (x Lude..@? "fromPort")
      Lude.<*> ( x Lude..@? "groups" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "item")
               )
      Lude.<*> ( x Lude..@? "prefixListIds" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "item")
               )
      Lude.<*> (x Lude..@ "ipProtocol")
      Lude.<*> (x Lude..@? "toPort")
      Lude.<*> ( x Lude..@? "ipv6Ranges" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "item")
               )
      Lude.<*> ( x Lude..@? "ipRanges" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "item")
               )

instance Lude.ToQuery IPPermission where
  toQuery IPPermission' {..} =
    Lude.mconcat
      [ "FromPort" Lude.=: fromPort,
        Lude.toQuery (Lude.toQueryList "Groups" Lude.<$> userIdGroupPairs),
        Lude.toQuery
          (Lude.toQueryList "PrefixListIds" Lude.<$> prefixListIds),
        "IpProtocol" Lude.=: ipProtocol,
        "ToPort" Lude.=: toPort,
        Lude.toQuery (Lude.toQueryList "Ipv6Ranges" Lude.<$> ipv6Ranges),
        Lude.toQuery (Lude.toQueryList "IpRanges" Lude.<$> ipRanges)
      ]
