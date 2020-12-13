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
    nfpmvViolationTarget,
    nfpmvExpectedPolicyDescription,
  )
where

import Network.AWS.FMS.Types.NetworkFirewallPolicyDescription
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Violation details for AWS Network Firewall for a firewall policy that has a different 'NetworkFirewallPolicyDescription' than is required by the Firewall Manager policy.
--
-- /See:/ 'mkNetworkFirewallPolicyModifiedViolation' smart constructor.
data NetworkFirewallPolicyModifiedViolation = NetworkFirewallPolicyModifiedViolation'
  { -- | The policy that's currently in use in the individual account.
    currentPolicyDescription :: Lude.Maybe NetworkFirewallPolicyDescription,
    -- | The ID of the AWS Network Firewall or VPC resource that's in violation.
    violationTarget :: Lude.Maybe Lude.Text,
    -- | The policy that should be in use in the individual account in order to be compliant.
    expectedPolicyDescription :: Lude.Maybe NetworkFirewallPolicyDescription
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'NetworkFirewallPolicyModifiedViolation' with the minimum fields required to make a request.
--
-- * 'currentPolicyDescription' - The policy that's currently in use in the individual account.
-- * 'violationTarget' - The ID of the AWS Network Firewall or VPC resource that's in violation.
-- * 'expectedPolicyDescription' - The policy that should be in use in the individual account in order to be compliant.
mkNetworkFirewallPolicyModifiedViolation ::
  NetworkFirewallPolicyModifiedViolation
mkNetworkFirewallPolicyModifiedViolation =
  NetworkFirewallPolicyModifiedViolation'
    { currentPolicyDescription =
        Lude.Nothing,
      violationTarget = Lude.Nothing,
      expectedPolicyDescription = Lude.Nothing
    }

-- | The policy that's currently in use in the individual account.
--
-- /Note:/ Consider using 'currentPolicyDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nfpmvCurrentPolicyDescription :: Lens.Lens' NetworkFirewallPolicyModifiedViolation (Lude.Maybe NetworkFirewallPolicyDescription)
nfpmvCurrentPolicyDescription = Lens.lens (currentPolicyDescription :: NetworkFirewallPolicyModifiedViolation -> Lude.Maybe NetworkFirewallPolicyDescription) (\s a -> s {currentPolicyDescription = a} :: NetworkFirewallPolicyModifiedViolation)
{-# DEPRECATED nfpmvCurrentPolicyDescription "Use generic-lens or generic-optics with 'currentPolicyDescription' instead." #-}

-- | The ID of the AWS Network Firewall or VPC resource that's in violation.
--
-- /Note:/ Consider using 'violationTarget' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nfpmvViolationTarget :: Lens.Lens' NetworkFirewallPolicyModifiedViolation (Lude.Maybe Lude.Text)
nfpmvViolationTarget = Lens.lens (violationTarget :: NetworkFirewallPolicyModifiedViolation -> Lude.Maybe Lude.Text) (\s a -> s {violationTarget = a} :: NetworkFirewallPolicyModifiedViolation)
{-# DEPRECATED nfpmvViolationTarget "Use generic-lens or generic-optics with 'violationTarget' instead." #-}

-- | The policy that should be in use in the individual account in order to be compliant.
--
-- /Note:/ Consider using 'expectedPolicyDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nfpmvExpectedPolicyDescription :: Lens.Lens' NetworkFirewallPolicyModifiedViolation (Lude.Maybe NetworkFirewallPolicyDescription)
nfpmvExpectedPolicyDescription = Lens.lens (expectedPolicyDescription :: NetworkFirewallPolicyModifiedViolation -> Lude.Maybe NetworkFirewallPolicyDescription) (\s a -> s {expectedPolicyDescription = a} :: NetworkFirewallPolicyModifiedViolation)
{-# DEPRECATED nfpmvExpectedPolicyDescription "Use generic-lens or generic-optics with 'expectedPolicyDescription' instead." #-}

instance Lude.FromJSON NetworkFirewallPolicyModifiedViolation where
  parseJSON =
    Lude.withObject
      "NetworkFirewallPolicyModifiedViolation"
      ( \x ->
          NetworkFirewallPolicyModifiedViolation'
            Lude.<$> (x Lude..:? "CurrentPolicyDescription")
            Lude.<*> (x Lude..:? "ViolationTarget")
            Lude.<*> (x Lude..:? "ExpectedPolicyDescription")
      )
