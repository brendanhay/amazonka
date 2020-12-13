{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.FMS.Types.NetworkFirewallMissingFirewallViolation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.FMS.Types.NetworkFirewallMissingFirewallViolation
  ( NetworkFirewallMissingFirewallViolation (..),

    -- * Smart constructor
    mkNetworkFirewallMissingFirewallViolation,

    -- * Lenses
    nfmfvTargetViolationReason,
    nfmfvAvailabilityZone,
    nfmfvVPC,
    nfmfvViolationTarget,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Violation details for AWS Network Firewall for a subnet that doesn't have a Firewall Manager managed firewall in its VPC.
--
-- /See:/ 'mkNetworkFirewallMissingFirewallViolation' smart constructor.
data NetworkFirewallMissingFirewallViolation = NetworkFirewallMissingFirewallViolation'
  { -- | The reason the resource has this violation, if one is available.
    targetViolationReason :: Lude.Maybe Lude.Text,
    -- | The Availability Zone of a violating subnet.
    availabilityZone :: Lude.Maybe Lude.Text,
    -- | The resource ID of the VPC associated with a violating subnet.
    vpc :: Lude.Maybe Lude.Text,
    -- | The ID of the AWS Network Firewall or VPC resource that's in violation.
    violationTarget :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'NetworkFirewallMissingFirewallViolation' with the minimum fields required to make a request.
--
-- * 'targetViolationReason' - The reason the resource has this violation, if one is available.
-- * 'availabilityZone' - The Availability Zone of a violating subnet.
-- * 'vpc' - The resource ID of the VPC associated with a violating subnet.
-- * 'violationTarget' - The ID of the AWS Network Firewall or VPC resource that's in violation.
mkNetworkFirewallMissingFirewallViolation ::
  NetworkFirewallMissingFirewallViolation
mkNetworkFirewallMissingFirewallViolation =
  NetworkFirewallMissingFirewallViolation'
    { targetViolationReason =
        Lude.Nothing,
      availabilityZone = Lude.Nothing,
      vpc = Lude.Nothing,
      violationTarget = Lude.Nothing
    }

-- | The reason the resource has this violation, if one is available.
--
-- /Note:/ Consider using 'targetViolationReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nfmfvTargetViolationReason :: Lens.Lens' NetworkFirewallMissingFirewallViolation (Lude.Maybe Lude.Text)
nfmfvTargetViolationReason = Lens.lens (targetViolationReason :: NetworkFirewallMissingFirewallViolation -> Lude.Maybe Lude.Text) (\s a -> s {targetViolationReason = a} :: NetworkFirewallMissingFirewallViolation)
{-# DEPRECATED nfmfvTargetViolationReason "Use generic-lens or generic-optics with 'targetViolationReason' instead." #-}

-- | The Availability Zone of a violating subnet.
--
-- /Note:/ Consider using 'availabilityZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nfmfvAvailabilityZone :: Lens.Lens' NetworkFirewallMissingFirewallViolation (Lude.Maybe Lude.Text)
nfmfvAvailabilityZone = Lens.lens (availabilityZone :: NetworkFirewallMissingFirewallViolation -> Lude.Maybe Lude.Text) (\s a -> s {availabilityZone = a} :: NetworkFirewallMissingFirewallViolation)
{-# DEPRECATED nfmfvAvailabilityZone "Use generic-lens or generic-optics with 'availabilityZone' instead." #-}

-- | The resource ID of the VPC associated with a violating subnet.
--
-- /Note:/ Consider using 'vpc' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nfmfvVPC :: Lens.Lens' NetworkFirewallMissingFirewallViolation (Lude.Maybe Lude.Text)
nfmfvVPC = Lens.lens (vpc :: NetworkFirewallMissingFirewallViolation -> Lude.Maybe Lude.Text) (\s a -> s {vpc = a} :: NetworkFirewallMissingFirewallViolation)
{-# DEPRECATED nfmfvVPC "Use generic-lens or generic-optics with 'vpc' instead." #-}

-- | The ID of the AWS Network Firewall or VPC resource that's in violation.
--
-- /Note:/ Consider using 'violationTarget' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nfmfvViolationTarget :: Lens.Lens' NetworkFirewallMissingFirewallViolation (Lude.Maybe Lude.Text)
nfmfvViolationTarget = Lens.lens (violationTarget :: NetworkFirewallMissingFirewallViolation -> Lude.Maybe Lude.Text) (\s a -> s {violationTarget = a} :: NetworkFirewallMissingFirewallViolation)
{-# DEPRECATED nfmfvViolationTarget "Use generic-lens or generic-optics with 'violationTarget' instead." #-}

instance Lude.FromJSON NetworkFirewallMissingFirewallViolation where
  parseJSON =
    Lude.withObject
      "NetworkFirewallMissingFirewallViolation"
      ( \x ->
          NetworkFirewallMissingFirewallViolation'
            Lude.<$> (x Lude..:? "TargetViolationReason")
            Lude.<*> (x Lude..:? "AvailabilityZone")
            Lude.<*> (x Lude..:? "VPC")
            Lude.<*> (x Lude..:? "ViolationTarget")
      )
