-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.FMS.Types.NetworkFirewallPolicyDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.FMS.Types.NetworkFirewallPolicyDescription
  ( NetworkFirewallPolicyDescription (..),

    -- * Smart constructor
    mkNetworkFirewallPolicyDescription,

    -- * Lenses
    nfpdStatefulRuleGroups,
    nfpdStatelessRuleGroups,
    nfpdStatelessFragmentDefaultActions,
    nfpdStatelessCustomActions,
    nfpdStatelessDefaultActions,
  )
where

import Network.AWS.FMS.Types.StatefulRuleGroup
import Network.AWS.FMS.Types.StatelessRuleGroup
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The definition of the AWS Network Firewall firewall policy.
--
-- /See:/ 'mkNetworkFirewallPolicyDescription' smart constructor.
data NetworkFirewallPolicyDescription = NetworkFirewallPolicyDescription'
  { statefulRuleGroups ::
      Lude.Maybe
        [StatefulRuleGroup],
    statelessRuleGroups ::
      Lude.Maybe
        [StatelessRuleGroup],
    statelessFragmentDefaultActions ::
      Lude.Maybe [Lude.Text],
    statelessCustomActions ::
      Lude.Maybe [Lude.Text],
    statelessDefaultActions ::
      Lude.Maybe [Lude.Text]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'NetworkFirewallPolicyDescription' with the minimum fields required to make a request.
--
-- * 'statefulRuleGroups' - The stateful rule groups that are used in the Network Firewall firewall policy.
-- * 'statelessCustomActions' - Names of custom actions that are available for use in the stateless default actions settings.
-- * 'statelessDefaultActions' - The actions to take on packets that don't match any of the stateless rule groups.
-- * 'statelessFragmentDefaultActions' - The actions to take on packet fragments that don't match any of the stateless rule groups.
-- * 'statelessRuleGroups' - The stateless rule groups that are used in the Network Firewall firewall policy.
mkNetworkFirewallPolicyDescription ::
  NetworkFirewallPolicyDescription
mkNetworkFirewallPolicyDescription =
  NetworkFirewallPolicyDescription'
    { statefulRuleGroups =
        Lude.Nothing,
      statelessRuleGroups = Lude.Nothing,
      statelessFragmentDefaultActions = Lude.Nothing,
      statelessCustomActions = Lude.Nothing,
      statelessDefaultActions = Lude.Nothing
    }

-- | The stateful rule groups that are used in the Network Firewall firewall policy.
--
-- /Note:/ Consider using 'statefulRuleGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nfpdStatefulRuleGroups :: Lens.Lens' NetworkFirewallPolicyDescription (Lude.Maybe [StatefulRuleGroup])
nfpdStatefulRuleGroups = Lens.lens (statefulRuleGroups :: NetworkFirewallPolicyDescription -> Lude.Maybe [StatefulRuleGroup]) (\s a -> s {statefulRuleGroups = a} :: NetworkFirewallPolicyDescription)
{-# DEPRECATED nfpdStatefulRuleGroups "Use generic-lens or generic-optics with 'statefulRuleGroups' instead." #-}

-- | The stateless rule groups that are used in the Network Firewall firewall policy.
--
-- /Note:/ Consider using 'statelessRuleGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nfpdStatelessRuleGroups :: Lens.Lens' NetworkFirewallPolicyDescription (Lude.Maybe [StatelessRuleGroup])
nfpdStatelessRuleGroups = Lens.lens (statelessRuleGroups :: NetworkFirewallPolicyDescription -> Lude.Maybe [StatelessRuleGroup]) (\s a -> s {statelessRuleGroups = a} :: NetworkFirewallPolicyDescription)
{-# DEPRECATED nfpdStatelessRuleGroups "Use generic-lens or generic-optics with 'statelessRuleGroups' instead." #-}

-- | The actions to take on packet fragments that don't match any of the stateless rule groups.
--
-- /Note:/ Consider using 'statelessFragmentDefaultActions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nfpdStatelessFragmentDefaultActions :: Lens.Lens' NetworkFirewallPolicyDescription (Lude.Maybe [Lude.Text])
nfpdStatelessFragmentDefaultActions = Lens.lens (statelessFragmentDefaultActions :: NetworkFirewallPolicyDescription -> Lude.Maybe [Lude.Text]) (\s a -> s {statelessFragmentDefaultActions = a} :: NetworkFirewallPolicyDescription)
{-# DEPRECATED nfpdStatelessFragmentDefaultActions "Use generic-lens or generic-optics with 'statelessFragmentDefaultActions' instead." #-}

-- | Names of custom actions that are available for use in the stateless default actions settings.
--
-- /Note:/ Consider using 'statelessCustomActions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nfpdStatelessCustomActions :: Lens.Lens' NetworkFirewallPolicyDescription (Lude.Maybe [Lude.Text])
nfpdStatelessCustomActions = Lens.lens (statelessCustomActions :: NetworkFirewallPolicyDescription -> Lude.Maybe [Lude.Text]) (\s a -> s {statelessCustomActions = a} :: NetworkFirewallPolicyDescription)
{-# DEPRECATED nfpdStatelessCustomActions "Use generic-lens or generic-optics with 'statelessCustomActions' instead." #-}

-- | The actions to take on packets that don't match any of the stateless rule groups.
--
-- /Note:/ Consider using 'statelessDefaultActions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nfpdStatelessDefaultActions :: Lens.Lens' NetworkFirewallPolicyDescription (Lude.Maybe [Lude.Text])
nfpdStatelessDefaultActions = Lens.lens (statelessDefaultActions :: NetworkFirewallPolicyDescription -> Lude.Maybe [Lude.Text]) (\s a -> s {statelessDefaultActions = a} :: NetworkFirewallPolicyDescription)
{-# DEPRECATED nfpdStatelessDefaultActions "Use generic-lens or generic-optics with 'statelessDefaultActions' instead." #-}

instance Lude.FromJSON NetworkFirewallPolicyDescription where
  parseJSON =
    Lude.withObject
      "NetworkFirewallPolicyDescription"
      ( \x ->
          NetworkFirewallPolicyDescription'
            Lude.<$> (x Lude..:? "StatefulRuleGroups" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "StatelessRuleGroups" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "StatelessFragmentDefaultActions" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "StatelessCustomActions" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "StatelessDefaultActions" Lude..!= Lude.mempty)
      )
