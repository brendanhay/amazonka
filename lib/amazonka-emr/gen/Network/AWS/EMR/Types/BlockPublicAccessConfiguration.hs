{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.BlockPublicAccessConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.BlockPublicAccessConfiguration
  ( BlockPublicAccessConfiguration (..),

    -- * Smart constructor
    mkBlockPublicAccessConfiguration,

    -- * Lenses
    bpacBlockPublicSecurityGroupRules,
    bpacPermittedPublicSecurityGroupRuleRanges,
  )
where

import qualified Network.AWS.EMR.Types.PortRange as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A configuration for Amazon EMR block public access. When @BlockPublicSecurityGroupRules@ is set to @true@ , Amazon EMR prevents cluster creation if one of the cluster's security groups has a rule that allows inbound traffic from 0.0.0.0/0 or ::/0 on a port, unless the port is specified as an exception using @PermittedPublicSecurityGroupRuleRanges@ .
--
-- /See:/ 'mkBlockPublicAccessConfiguration' smart constructor.
data BlockPublicAccessConfiguration = BlockPublicAccessConfiguration'
  { -- | Indicates whether Amazon EMR block public access is enabled (@true@ ) or disabled (@false@ ). By default, the value is @false@ for accounts that have created EMR clusters before July 2019. For accounts created after this, the default is @true@ .
    blockPublicSecurityGroupRules :: Core.Bool,
    -- | Specifies ports and port ranges that are permitted to have security group rules that allow inbound traffic from all public sources. For example, if Port 23 (Telnet) is specified for @PermittedPublicSecurityGroupRuleRanges@ , Amazon EMR allows cluster creation if a security group associated with the cluster has a rule that allows inbound traffic on Port 23 from IPv4 0.0.0.0/0 or IPv6 port ::/0 as the source.
    --
    -- By default, Port 22, which is used for SSH access to the cluster EC2 instances, is in the list of @PermittedPublicSecurityGroupRuleRanges@ .
    permittedPublicSecurityGroupRuleRanges :: Core.Maybe [Types.PortRange]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'BlockPublicAccessConfiguration' value with any optional fields omitted.
mkBlockPublicAccessConfiguration ::
  -- | 'blockPublicSecurityGroupRules'
  Core.Bool ->
  BlockPublicAccessConfiguration
mkBlockPublicAccessConfiguration blockPublicSecurityGroupRules =
  BlockPublicAccessConfiguration'
    { blockPublicSecurityGroupRules,
      permittedPublicSecurityGroupRuleRanges = Core.Nothing
    }

-- | Indicates whether Amazon EMR block public access is enabled (@true@ ) or disabled (@false@ ). By default, the value is @false@ for accounts that have created EMR clusters before July 2019. For accounts created after this, the default is @true@ .
--
-- /Note:/ Consider using 'blockPublicSecurityGroupRules' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bpacBlockPublicSecurityGroupRules :: Lens.Lens' BlockPublicAccessConfiguration Core.Bool
bpacBlockPublicSecurityGroupRules = Lens.field @"blockPublicSecurityGroupRules"
{-# DEPRECATED bpacBlockPublicSecurityGroupRules "Use generic-lens or generic-optics with 'blockPublicSecurityGroupRules' instead." #-}

-- | Specifies ports and port ranges that are permitted to have security group rules that allow inbound traffic from all public sources. For example, if Port 23 (Telnet) is specified for @PermittedPublicSecurityGroupRuleRanges@ , Amazon EMR allows cluster creation if a security group associated with the cluster has a rule that allows inbound traffic on Port 23 from IPv4 0.0.0.0/0 or IPv6 port ::/0 as the source.
--
-- By default, Port 22, which is used for SSH access to the cluster EC2 instances, is in the list of @PermittedPublicSecurityGroupRuleRanges@ .
--
-- /Note:/ Consider using 'permittedPublicSecurityGroupRuleRanges' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bpacPermittedPublicSecurityGroupRuleRanges :: Lens.Lens' BlockPublicAccessConfiguration (Core.Maybe [Types.PortRange])
bpacPermittedPublicSecurityGroupRuleRanges = Lens.field @"permittedPublicSecurityGroupRuleRanges"
{-# DEPRECATED bpacPermittedPublicSecurityGroupRuleRanges "Use generic-lens or generic-optics with 'permittedPublicSecurityGroupRuleRanges' instead." #-}

instance Core.FromJSON BlockPublicAccessConfiguration where
  toJSON BlockPublicAccessConfiguration {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ( "BlockPublicSecurityGroupRules"
                  Core..= blockPublicSecurityGroupRules
              ),
            ("PermittedPublicSecurityGroupRuleRanges" Core..=)
              Core.<$> permittedPublicSecurityGroupRuleRanges
          ]
      )

instance Core.FromJSON BlockPublicAccessConfiguration where
  parseJSON =
    Core.withObject "BlockPublicAccessConfiguration" Core.$
      \x ->
        BlockPublicAccessConfiguration'
          Core.<$> (x Core..: "BlockPublicSecurityGroupRules")
          Core.<*> (x Core..:? "PermittedPublicSecurityGroupRuleRanges")
