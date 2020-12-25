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
    nfmfvAvailabilityZone,
    nfmfvTargetViolationReason,
    nfmfvVPC,
    nfmfvViolationTarget,
  )
where

import qualified Network.AWS.FMS.Types.AvailabilityZone as Types
import qualified Network.AWS.FMS.Types.ResourceId as Types
import qualified Network.AWS.FMS.Types.TargetViolationReason as Types
import qualified Network.AWS.FMS.Types.ViolationTarget as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Violation details for AWS Network Firewall for a subnet that doesn't have a Firewall Manager managed firewall in its VPC.
--
-- /See:/ 'mkNetworkFirewallMissingFirewallViolation' smart constructor.
data NetworkFirewallMissingFirewallViolation = NetworkFirewallMissingFirewallViolation'
  { -- | The Availability Zone of a violating subnet.
    availabilityZone :: Core.Maybe Types.AvailabilityZone,
    -- | The reason the resource has this violation, if one is available.
    targetViolationReason :: Core.Maybe Types.TargetViolationReason,
    -- | The resource ID of the VPC associated with a violating subnet.
    vpc :: Core.Maybe Types.ResourceId,
    -- | The ID of the AWS Network Firewall or VPC resource that's in violation.
    violationTarget :: Core.Maybe Types.ViolationTarget
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'NetworkFirewallMissingFirewallViolation' value with any optional fields omitted.
mkNetworkFirewallMissingFirewallViolation ::
  NetworkFirewallMissingFirewallViolation
mkNetworkFirewallMissingFirewallViolation =
  NetworkFirewallMissingFirewallViolation'
    { availabilityZone =
        Core.Nothing,
      targetViolationReason = Core.Nothing,
      vpc = Core.Nothing,
      violationTarget = Core.Nothing
    }

-- | The Availability Zone of a violating subnet.
--
-- /Note:/ Consider using 'availabilityZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nfmfvAvailabilityZone :: Lens.Lens' NetworkFirewallMissingFirewallViolation (Core.Maybe Types.AvailabilityZone)
nfmfvAvailabilityZone = Lens.field @"availabilityZone"
{-# DEPRECATED nfmfvAvailabilityZone "Use generic-lens or generic-optics with 'availabilityZone' instead." #-}

-- | The reason the resource has this violation, if one is available.
--
-- /Note:/ Consider using 'targetViolationReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nfmfvTargetViolationReason :: Lens.Lens' NetworkFirewallMissingFirewallViolation (Core.Maybe Types.TargetViolationReason)
nfmfvTargetViolationReason = Lens.field @"targetViolationReason"
{-# DEPRECATED nfmfvTargetViolationReason "Use generic-lens or generic-optics with 'targetViolationReason' instead." #-}

-- | The resource ID of the VPC associated with a violating subnet.
--
-- /Note:/ Consider using 'vpc' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nfmfvVPC :: Lens.Lens' NetworkFirewallMissingFirewallViolation (Core.Maybe Types.ResourceId)
nfmfvVPC = Lens.field @"vpc"
{-# DEPRECATED nfmfvVPC "Use generic-lens or generic-optics with 'vpc' instead." #-}

-- | The ID of the AWS Network Firewall or VPC resource that's in violation.
--
-- /Note:/ Consider using 'violationTarget' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nfmfvViolationTarget :: Lens.Lens' NetworkFirewallMissingFirewallViolation (Core.Maybe Types.ViolationTarget)
nfmfvViolationTarget = Lens.field @"violationTarget"
{-# DEPRECATED nfmfvViolationTarget "Use generic-lens or generic-optics with 'violationTarget' instead." #-}

instance Core.FromJSON NetworkFirewallMissingFirewallViolation where
  parseJSON =
    Core.withObject "NetworkFirewallMissingFirewallViolation" Core.$
      \x ->
        NetworkFirewallMissingFirewallViolation'
          Core.<$> (x Core..:? "AvailabilityZone")
          Core.<*> (x Core..:? "TargetViolationReason")
          Core.<*> (x Core..:? "VPC")
          Core.<*> (x Core..:? "ViolationTarget")
