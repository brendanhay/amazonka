{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.FMS.Types.NetworkFirewallPolicyDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.FMS.Types.NetworkFirewallPolicyDescription
  ( NetworkFirewallPolicyDescription (..)
  -- * Smart constructor
  , mkNetworkFirewallPolicyDescription
  -- * Lenses
  , nfpdStatefulRuleGroups
  , nfpdStatelessCustomActions
  , nfpdStatelessDefaultActions
  , nfpdStatelessFragmentDefaultActions
  , nfpdStatelessRuleGroups
  ) where

import qualified Network.AWS.FMS.Types.NetworkFirewallAction as Types
import qualified Network.AWS.FMS.Types.StatefulRuleGroup as Types
import qualified Network.AWS.FMS.Types.StatelessRuleGroup as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The definition of the AWS Network Firewall firewall policy.
--
-- /See:/ 'mkNetworkFirewallPolicyDescription' smart constructor.
data NetworkFirewallPolicyDescription = NetworkFirewallPolicyDescription'
  { statefulRuleGroups :: Core.Maybe [Types.StatefulRuleGroup]
    -- ^ The stateful rule groups that are used in the Network Firewall firewall policy. 
  , statelessCustomActions :: Core.Maybe [Types.NetworkFirewallAction]
    -- ^ Names of custom actions that are available for use in the stateless default actions settings.
  , statelessDefaultActions :: Core.Maybe [Types.NetworkFirewallAction]
    -- ^ The actions to take on packets that don't match any of the stateless rule groups. 
  , statelessFragmentDefaultActions :: Core.Maybe [Types.NetworkFirewallAction]
    -- ^ The actions to take on packet fragments that don't match any of the stateless rule groups. 
  , statelessRuleGroups :: Core.Maybe [Types.StatelessRuleGroup]
    -- ^ The stateless rule groups that are used in the Network Firewall firewall policy. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'NetworkFirewallPolicyDescription' value with any optional fields omitted.
mkNetworkFirewallPolicyDescription
    :: NetworkFirewallPolicyDescription
mkNetworkFirewallPolicyDescription
  = NetworkFirewallPolicyDescription'{statefulRuleGroups =
                                        Core.Nothing,
                                      statelessCustomActions = Core.Nothing,
                                      statelessDefaultActions = Core.Nothing,
                                      statelessFragmentDefaultActions = Core.Nothing,
                                      statelessRuleGroups = Core.Nothing}

-- | The stateful rule groups that are used in the Network Firewall firewall policy. 
--
-- /Note:/ Consider using 'statefulRuleGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nfpdStatefulRuleGroups :: Lens.Lens' NetworkFirewallPolicyDescription (Core.Maybe [Types.StatefulRuleGroup])
nfpdStatefulRuleGroups = Lens.field @"statefulRuleGroups"
{-# INLINEABLE nfpdStatefulRuleGroups #-}
{-# DEPRECATED statefulRuleGroups "Use generic-lens or generic-optics with 'statefulRuleGroups' instead"  #-}

-- | Names of custom actions that are available for use in the stateless default actions settings.
--
-- /Note:/ Consider using 'statelessCustomActions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nfpdStatelessCustomActions :: Lens.Lens' NetworkFirewallPolicyDescription (Core.Maybe [Types.NetworkFirewallAction])
nfpdStatelessCustomActions = Lens.field @"statelessCustomActions"
{-# INLINEABLE nfpdStatelessCustomActions #-}
{-# DEPRECATED statelessCustomActions "Use generic-lens or generic-optics with 'statelessCustomActions' instead"  #-}

-- | The actions to take on packets that don't match any of the stateless rule groups. 
--
-- /Note:/ Consider using 'statelessDefaultActions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nfpdStatelessDefaultActions :: Lens.Lens' NetworkFirewallPolicyDescription (Core.Maybe [Types.NetworkFirewallAction])
nfpdStatelessDefaultActions = Lens.field @"statelessDefaultActions"
{-# INLINEABLE nfpdStatelessDefaultActions #-}
{-# DEPRECATED statelessDefaultActions "Use generic-lens or generic-optics with 'statelessDefaultActions' instead"  #-}

-- | The actions to take on packet fragments that don't match any of the stateless rule groups. 
--
-- /Note:/ Consider using 'statelessFragmentDefaultActions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nfpdStatelessFragmentDefaultActions :: Lens.Lens' NetworkFirewallPolicyDescription (Core.Maybe [Types.NetworkFirewallAction])
nfpdStatelessFragmentDefaultActions = Lens.field @"statelessFragmentDefaultActions"
{-# INLINEABLE nfpdStatelessFragmentDefaultActions #-}
{-# DEPRECATED statelessFragmentDefaultActions "Use generic-lens or generic-optics with 'statelessFragmentDefaultActions' instead"  #-}

-- | The stateless rule groups that are used in the Network Firewall firewall policy. 
--
-- /Note:/ Consider using 'statelessRuleGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nfpdStatelessRuleGroups :: Lens.Lens' NetworkFirewallPolicyDescription (Core.Maybe [Types.StatelessRuleGroup])
nfpdStatelessRuleGroups = Lens.field @"statelessRuleGroups"
{-# INLINEABLE nfpdStatelessRuleGroups #-}
{-# DEPRECATED statelessRuleGroups "Use generic-lens or generic-optics with 'statelessRuleGroups' instead"  #-}

instance Core.FromJSON NetworkFirewallPolicyDescription where
        parseJSON
          = Core.withObject "NetworkFirewallPolicyDescription" Core.$
              \ x ->
                NetworkFirewallPolicyDescription' Core.<$>
                  (x Core..:? "StatefulRuleGroups") Core.<*>
                    x Core..:? "StatelessCustomActions"
                    Core.<*> x Core..:? "StatelessDefaultActions"
                    Core.<*> x Core..:? "StatelessFragmentDefaultActions"
                    Core.<*> x Core..:? "StatelessRuleGroups"
