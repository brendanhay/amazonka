-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.FMS.Types.SecurityGroupRuleDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.FMS.Types.SecurityGroupRuleDescription
  ( SecurityGroupRuleDescription (..),

    -- * Smart constructor
    mkSecurityGroupRuleDescription,

    -- * Lenses
    sgrdFromPort,
    sgrdProtocol,
    sgrdIPV4Range,
    sgrdPrefixListId,
    sgrdToPort,
    sgrdIPV6Range,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes a set of permissions for a security group rule.
--
-- /See:/ 'mkSecurityGroupRuleDescription' smart constructor.
data SecurityGroupRuleDescription = SecurityGroupRuleDescription'
  { fromPort ::
      Lude.Maybe Lude.Natural,
    protocol :: Lude.Maybe Lude.Text,
    ipV4Range :: Lude.Maybe Lude.Text,
    prefixListId ::
      Lude.Maybe Lude.Text,
    toPort :: Lude.Maybe Lude.Natural,
    ipV6Range :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SecurityGroupRuleDescription' with the minimum fields required to make a request.
--
-- * 'fromPort' - The start of the port range for the TCP and UDP protocols, or an ICMP/ICMPv6 type number. A value of @-1@ indicates all ICMP/ICMPv6 types.
-- * 'ipV4Range' - The IPv4 ranges for the security group rule.
-- * 'ipV6Range' - The IPv6 ranges for the security group rule.
-- * 'prefixListId' - The ID of the prefix list for the security group rule.
-- * 'protocol' - The IP protocol name (@tcp@ , @udp@ , @icmp@ , @icmpv6@ ) or number.
-- * 'toPort' - The end of the port range for the TCP and UDP protocols, or an ICMP/ICMPv6 code. A value of @-1@ indicates all ICMP/ICMPv6 codes.
mkSecurityGroupRuleDescription ::
  SecurityGroupRuleDescription
mkSecurityGroupRuleDescription =
  SecurityGroupRuleDescription'
    { fromPort = Lude.Nothing,
      protocol = Lude.Nothing,
      ipV4Range = Lude.Nothing,
      prefixListId = Lude.Nothing,
      toPort = Lude.Nothing,
      ipV6Range = Lude.Nothing
    }

-- | The start of the port range for the TCP and UDP protocols, or an ICMP/ICMPv6 type number. A value of @-1@ indicates all ICMP/ICMPv6 types.
--
-- /Note:/ Consider using 'fromPort' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sgrdFromPort :: Lens.Lens' SecurityGroupRuleDescription (Lude.Maybe Lude.Natural)
sgrdFromPort = Lens.lens (fromPort :: SecurityGroupRuleDescription -> Lude.Maybe Lude.Natural) (\s a -> s {fromPort = a} :: SecurityGroupRuleDescription)
{-# DEPRECATED sgrdFromPort "Use generic-lens or generic-optics with 'fromPort' instead." #-}

-- | The IP protocol name (@tcp@ , @udp@ , @icmp@ , @icmpv6@ ) or number.
--
-- /Note:/ Consider using 'protocol' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sgrdProtocol :: Lens.Lens' SecurityGroupRuleDescription (Lude.Maybe Lude.Text)
sgrdProtocol = Lens.lens (protocol :: SecurityGroupRuleDescription -> Lude.Maybe Lude.Text) (\s a -> s {protocol = a} :: SecurityGroupRuleDescription)
{-# DEPRECATED sgrdProtocol "Use generic-lens or generic-optics with 'protocol' instead." #-}

-- | The IPv4 ranges for the security group rule.
--
-- /Note:/ Consider using 'ipV4Range' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sgrdIPV4Range :: Lens.Lens' SecurityGroupRuleDescription (Lude.Maybe Lude.Text)
sgrdIPV4Range = Lens.lens (ipV4Range :: SecurityGroupRuleDescription -> Lude.Maybe Lude.Text) (\s a -> s {ipV4Range = a} :: SecurityGroupRuleDescription)
{-# DEPRECATED sgrdIPV4Range "Use generic-lens or generic-optics with 'ipV4Range' instead." #-}

-- | The ID of the prefix list for the security group rule.
--
-- /Note:/ Consider using 'prefixListId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sgrdPrefixListId :: Lens.Lens' SecurityGroupRuleDescription (Lude.Maybe Lude.Text)
sgrdPrefixListId = Lens.lens (prefixListId :: SecurityGroupRuleDescription -> Lude.Maybe Lude.Text) (\s a -> s {prefixListId = a} :: SecurityGroupRuleDescription)
{-# DEPRECATED sgrdPrefixListId "Use generic-lens or generic-optics with 'prefixListId' instead." #-}

-- | The end of the port range for the TCP and UDP protocols, or an ICMP/ICMPv6 code. A value of @-1@ indicates all ICMP/ICMPv6 codes.
--
-- /Note:/ Consider using 'toPort' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sgrdToPort :: Lens.Lens' SecurityGroupRuleDescription (Lude.Maybe Lude.Natural)
sgrdToPort = Lens.lens (toPort :: SecurityGroupRuleDescription -> Lude.Maybe Lude.Natural) (\s a -> s {toPort = a} :: SecurityGroupRuleDescription)
{-# DEPRECATED sgrdToPort "Use generic-lens or generic-optics with 'toPort' instead." #-}

-- | The IPv6 ranges for the security group rule.
--
-- /Note:/ Consider using 'ipV6Range' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sgrdIPV6Range :: Lens.Lens' SecurityGroupRuleDescription (Lude.Maybe Lude.Text)
sgrdIPV6Range = Lens.lens (ipV6Range :: SecurityGroupRuleDescription -> Lude.Maybe Lude.Text) (\s a -> s {ipV6Range = a} :: SecurityGroupRuleDescription)
{-# DEPRECATED sgrdIPV6Range "Use generic-lens or generic-optics with 'ipV6Range' instead." #-}

instance Lude.FromJSON SecurityGroupRuleDescription where
  parseJSON =
    Lude.withObject
      "SecurityGroupRuleDescription"
      ( \x ->
          SecurityGroupRuleDescription'
            Lude.<$> (x Lude..:? "FromPort")
            Lude.<*> (x Lude..:? "Protocol")
            Lude.<*> (x Lude..:? "IPV4Range")
            Lude.<*> (x Lude..:? "PrefixListId")
            Lude.<*> (x Lude..:? "ToPort")
            Lude.<*> (x Lude..:? "IPV6Range")
      )
