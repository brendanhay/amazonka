{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.FMS.Types.NetworkFirewallPolicyModifiedViolation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.FMS.Types.NetworkFirewallPolicyModifiedViolation
  ( NetworkFirewallPolicyModifiedViolation (..),

    -- * Smart constructor
    mkNetworkFirewallPolicyModifiedViolation,

    -- * Lenses
    nfpmvCurrentPolicyDescription,
    nfpmvExpectedPolicyDescription,
    nfpmvViolationTarget,
  )
where

import qualified Network.AWS.FMS.Types.NetworkFirewallPolicyDescription as Types
import qualified Network.AWS.FMS.Types.ViolationTarget as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Violation details for AWS Network Firewall for a firewall policy that has a different 'NetworkFirewallPolicyDescription' than is required by the Firewall Manager policy.
--
-- /See:/ 'mkNetworkFirewallPolicyModifiedViolation' smart constructor.
data NetworkFirewallPolicyModifiedViolation = NetworkFirewallPolicyModifiedViolation'
  { -- | The policy that's currently in use in the individual account.
    currentPolicyDescription :: Core.Maybe Types.NetworkFirewallPolicyDescription,
    -- | The policy that should be in use in the individual account in order to be compliant.
    expectedPolicyDescription :: Core.Maybe Types.NetworkFirewallPolicyDescription,
    -- | The ID of the AWS Network Firewall or VPC resource that's in violation.
    violationTarget :: Core.Maybe Types.ViolationTarget
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'NetworkFirewallPolicyModifiedViolation' value with any optional fields omitted.
mkNetworkFirewallPolicyModifiedViolation ::
  NetworkFirewallPolicyModifiedViolation
mkNetworkFirewallPolicyModifiedViolation =
  NetworkFirewallPolicyModifiedViolation'
    { currentPolicyDescription =
        Core.Nothing,
      expectedPolicyDescription = Core.Nothing,
      violationTarget = Core.Nothing
    }

-- | The policy that's currently in use in the individual account.
--
-- /Note:/ Consider using 'currentPolicyDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nfpmvCurrentPolicyDescription :: Lens.Lens' NetworkFirewallPolicyModifiedViolation (Core.Maybe Types.NetworkFirewallPolicyDescription)
nfpmvCurrentPolicyDescription = Lens.field @"currentPolicyDescription"
{-# DEPRECATED nfpmvCurrentPolicyDescription "Use generic-lens or generic-optics with 'currentPolicyDescription' instead." #-}

-- | The policy that should be in use in the individual account in order to be compliant.
--
-- /Note:/ Consider using 'expectedPolicyDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nfpmvExpectedPolicyDescription :: Lens.Lens' NetworkFirewallPolicyModifiedViolation (Core.Maybe Types.NetworkFirewallPolicyDescription)
nfpmvExpectedPolicyDescription = Lens.field @"expectedPolicyDescription"
{-# DEPRECATED nfpmvExpectedPolicyDescription "Use generic-lens or generic-optics with 'expectedPolicyDescription' instead." #-}

-- | The ID of the AWS Network Firewall or VPC resource that's in violation.
--
-- /Note:/ Consider using 'violationTarget' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nfpmvViolationTarget :: Lens.Lens' NetworkFirewallPolicyModifiedViolation (Core.Maybe Types.ViolationTarget)
nfpmvViolationTarget = Lens.field @"violationTarget"
{-# DEPRECATED nfpmvViolationTarget "Use generic-lens or generic-optics with 'violationTarget' instead." #-}

instance Core.FromJSON NetworkFirewallPolicyModifiedViolation where
  parseJSON =
    Core.withObject "NetworkFirewallPolicyModifiedViolation" Core.$
      \x ->
        NetworkFirewallPolicyModifiedViolation'
          Core.<$> (x Core..:? "CurrentPolicyDescription")
          Core.<*> (x Core..:? "ExpectedPolicyDescription")
          Core.<*> (x Core..:? "ViolationTarget")
