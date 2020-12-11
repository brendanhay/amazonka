-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.FMS.Types.NetworkFirewallMissingSubnetViolation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.FMS.Types.NetworkFirewallMissingSubnetViolation
  ( NetworkFirewallMissingSubnetViolation (..),

    -- * Smart constructor
    mkNetworkFirewallMissingSubnetViolation,

    -- * Lenses
    nfmsvTargetViolationReason,
    nfmsvAvailabilityZone,
    nfmsvVPC,
    nfmsvViolationTarget,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Violation details for AWS Network Firewall for an Availability Zone that's missing the expected Firewall Manager managed subnet.
--
-- /See:/ 'mkNetworkFirewallMissingSubnetViolation' smart constructor.
data NetworkFirewallMissingSubnetViolation = NetworkFirewallMissingSubnetViolation'
  { targetViolationReason ::
      Lude.Maybe
        Lude.Text,
    availabilityZone ::
      Lude.Maybe
        Lude.Text,
    vpc ::
      Lude.Maybe
        Lude.Text,
    violationTarget ::
      Lude.Maybe
        Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'NetworkFirewallMissingSubnetViolation' with the minimum fields required to make a request.
--
-- * 'availabilityZone' - The Availability Zone of a violating subnet.
-- * 'targetViolationReason' - The reason the resource has this violation, if one is available.
-- * 'violationTarget' - The ID of the AWS Network Firewall or VPC resource that's in violation.
-- * 'vpc' - The resource ID of the VPC associated with a violating subnet.
mkNetworkFirewallMissingSubnetViolation ::
  NetworkFirewallMissingSubnetViolation
mkNetworkFirewallMissingSubnetViolation =
  NetworkFirewallMissingSubnetViolation'
    { targetViolationReason =
        Lude.Nothing,
      availabilityZone = Lude.Nothing,
      vpc = Lude.Nothing,
      violationTarget = Lude.Nothing
    }

-- | The reason the resource has this violation, if one is available.
--
-- /Note:/ Consider using 'targetViolationReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nfmsvTargetViolationReason :: Lens.Lens' NetworkFirewallMissingSubnetViolation (Lude.Maybe Lude.Text)
nfmsvTargetViolationReason = Lens.lens (targetViolationReason :: NetworkFirewallMissingSubnetViolation -> Lude.Maybe Lude.Text) (\s a -> s {targetViolationReason = a} :: NetworkFirewallMissingSubnetViolation)
{-# DEPRECATED nfmsvTargetViolationReason "Use generic-lens or generic-optics with 'targetViolationReason' instead." #-}

-- | The Availability Zone of a violating subnet.
--
-- /Note:/ Consider using 'availabilityZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nfmsvAvailabilityZone :: Lens.Lens' NetworkFirewallMissingSubnetViolation (Lude.Maybe Lude.Text)
nfmsvAvailabilityZone = Lens.lens (availabilityZone :: NetworkFirewallMissingSubnetViolation -> Lude.Maybe Lude.Text) (\s a -> s {availabilityZone = a} :: NetworkFirewallMissingSubnetViolation)
{-# DEPRECATED nfmsvAvailabilityZone "Use generic-lens or generic-optics with 'availabilityZone' instead." #-}

-- | The resource ID of the VPC associated with a violating subnet.
--
-- /Note:/ Consider using 'vpc' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nfmsvVPC :: Lens.Lens' NetworkFirewallMissingSubnetViolation (Lude.Maybe Lude.Text)
nfmsvVPC = Lens.lens (vpc :: NetworkFirewallMissingSubnetViolation -> Lude.Maybe Lude.Text) (\s a -> s {vpc = a} :: NetworkFirewallMissingSubnetViolation)
{-# DEPRECATED nfmsvVPC "Use generic-lens or generic-optics with 'vpc' instead." #-}

-- | The ID of the AWS Network Firewall or VPC resource that's in violation.
--
-- /Note:/ Consider using 'violationTarget' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nfmsvViolationTarget :: Lens.Lens' NetworkFirewallMissingSubnetViolation (Lude.Maybe Lude.Text)
nfmsvViolationTarget = Lens.lens (violationTarget :: NetworkFirewallMissingSubnetViolation -> Lude.Maybe Lude.Text) (\s a -> s {violationTarget = a} :: NetworkFirewallMissingSubnetViolation)
{-# DEPRECATED nfmsvViolationTarget "Use generic-lens or generic-optics with 'violationTarget' instead." #-}

instance Lude.FromJSON NetworkFirewallMissingSubnetViolation where
  parseJSON =
    Lude.withObject
      "NetworkFirewallMissingSubnetViolation"
      ( \x ->
          NetworkFirewallMissingSubnetViolation'
            Lude.<$> (x Lude..:? "TargetViolationReason")
            Lude.<*> (x Lude..:? "AvailabilityZone")
            Lude.<*> (x Lude..:? "VPC")
            Lude.<*> (x Lude..:? "ViolationTarget")
      )
