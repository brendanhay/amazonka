{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.FMS.Types.SecurityGroupRuleDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.FMS.Types.SecurityGroupRuleDescription
  ( SecurityGroupRuleDescription (..)
  -- * Smart constructor
  , mkSecurityGroupRuleDescription
  -- * Lenses
  , sgrdFromPort
  , sgrdIPV4Range
  , sgrdIPV6Range
  , sgrdPrefixListId
  , sgrdProtocol
  , sgrdToPort
  ) where

import qualified Network.AWS.FMS.Types.IPV4Range as Types
import qualified Network.AWS.FMS.Types.IPV6Range as Types
import qualified Network.AWS.FMS.Types.Protocol as Types
import qualified Network.AWS.FMS.Types.ResourceId as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes a set of permissions for a security group rule.
--
-- /See:/ 'mkSecurityGroupRuleDescription' smart constructor.
data SecurityGroupRuleDescription = SecurityGroupRuleDescription'
  { fromPort :: Core.Maybe Core.Natural
    -- ^ The start of the port range for the TCP and UDP protocols, or an ICMP/ICMPv6 type number. A value of @-1@ indicates all ICMP/ICMPv6 types.
  , iPV4Range :: Core.Maybe Types.IPV4Range
    -- ^ The IPv4 ranges for the security group rule.
  , iPV6Range :: Core.Maybe Types.IPV6Range
    -- ^ The IPv6 ranges for the security group rule.
  , prefixListId :: Core.Maybe Types.ResourceId
    -- ^ The ID of the prefix list for the security group rule.
  , protocol :: Core.Maybe Types.Protocol
    -- ^ The IP protocol name (@tcp@ , @udp@ , @icmp@ , @icmpv6@ ) or number.
  , toPort :: Core.Maybe Core.Natural
    -- ^ The end of the port range for the TCP and UDP protocols, or an ICMP/ICMPv6 code. A value of @-1@ indicates all ICMP/ICMPv6 codes.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SecurityGroupRuleDescription' value with any optional fields omitted.
mkSecurityGroupRuleDescription
    :: SecurityGroupRuleDescription
mkSecurityGroupRuleDescription
  = SecurityGroupRuleDescription'{fromPort = Core.Nothing,
                                  iPV4Range = Core.Nothing, iPV6Range = Core.Nothing,
                                  prefixListId = Core.Nothing, protocol = Core.Nothing,
                                  toPort = Core.Nothing}

-- | The start of the port range for the TCP and UDP protocols, or an ICMP/ICMPv6 type number. A value of @-1@ indicates all ICMP/ICMPv6 types.
--
-- /Note:/ Consider using 'fromPort' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sgrdFromPort :: Lens.Lens' SecurityGroupRuleDescription (Core.Maybe Core.Natural)
sgrdFromPort = Lens.field @"fromPort"
{-# INLINEABLE sgrdFromPort #-}
{-# DEPRECATED fromPort "Use generic-lens or generic-optics with 'fromPort' instead"  #-}

-- | The IPv4 ranges for the security group rule.
--
-- /Note:/ Consider using 'iPV4Range' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sgrdIPV4Range :: Lens.Lens' SecurityGroupRuleDescription (Core.Maybe Types.IPV4Range)
sgrdIPV4Range = Lens.field @"iPV4Range"
{-# INLINEABLE sgrdIPV4Range #-}
{-# DEPRECATED iPV4Range "Use generic-lens or generic-optics with 'iPV4Range' instead"  #-}

-- | The IPv6 ranges for the security group rule.
--
-- /Note:/ Consider using 'iPV6Range' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sgrdIPV6Range :: Lens.Lens' SecurityGroupRuleDescription (Core.Maybe Types.IPV6Range)
sgrdIPV6Range = Lens.field @"iPV6Range"
{-# INLINEABLE sgrdIPV6Range #-}
{-# DEPRECATED iPV6Range "Use generic-lens or generic-optics with 'iPV6Range' instead"  #-}

-- | The ID of the prefix list for the security group rule.
--
-- /Note:/ Consider using 'prefixListId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sgrdPrefixListId :: Lens.Lens' SecurityGroupRuleDescription (Core.Maybe Types.ResourceId)
sgrdPrefixListId = Lens.field @"prefixListId"
{-# INLINEABLE sgrdPrefixListId #-}
{-# DEPRECATED prefixListId "Use generic-lens or generic-optics with 'prefixListId' instead"  #-}

-- | The IP protocol name (@tcp@ , @udp@ , @icmp@ , @icmpv6@ ) or number.
--
-- /Note:/ Consider using 'protocol' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sgrdProtocol :: Lens.Lens' SecurityGroupRuleDescription (Core.Maybe Types.Protocol)
sgrdProtocol = Lens.field @"protocol"
{-# INLINEABLE sgrdProtocol #-}
{-# DEPRECATED protocol "Use generic-lens or generic-optics with 'protocol' instead"  #-}

-- | The end of the port range for the TCP and UDP protocols, or an ICMP/ICMPv6 code. A value of @-1@ indicates all ICMP/ICMPv6 codes.
--
-- /Note:/ Consider using 'toPort' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sgrdToPort :: Lens.Lens' SecurityGroupRuleDescription (Core.Maybe Core.Natural)
sgrdToPort = Lens.field @"toPort"
{-# INLINEABLE sgrdToPort #-}
{-# DEPRECATED toPort "Use generic-lens or generic-optics with 'toPort' instead"  #-}

instance Core.FromJSON SecurityGroupRuleDescription where
        parseJSON
          = Core.withObject "SecurityGroupRuleDescription" Core.$
              \ x ->
                SecurityGroupRuleDescription' Core.<$>
                  (x Core..:? "FromPort") Core.<*> x Core..:? "IPV4Range" Core.<*>
                    x Core..:? "IPV6Range"
                    Core.<*> x Core..:? "PrefixListId"
                    Core.<*> x Core..:? "Protocol"
                    Core.<*> x Core..:? "ToPort"
