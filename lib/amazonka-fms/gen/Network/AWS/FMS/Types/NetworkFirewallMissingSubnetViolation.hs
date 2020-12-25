{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
    nfmsvAvailabilityZone,
    nfmsvTargetViolationReason,
    nfmsvVPC,
    nfmsvViolationTarget,
  )
where

import qualified Network.AWS.FMS.Types.AvailabilityZone as Types
import qualified Network.AWS.FMS.Types.ResourceId as Types
import qualified Network.AWS.FMS.Types.TargetViolationReason as Types
import qualified Network.AWS.FMS.Types.ViolationTarget as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Violation details for AWS Network Firewall for an Availability Zone that's missing the expected Firewall Manager managed subnet.
--
-- /See:/ 'mkNetworkFirewallMissingSubnetViolation' smart constructor.
data NetworkFirewallMissingSubnetViolation = NetworkFirewallMissingSubnetViolation'
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

-- | Creates a 'NetworkFirewallMissingSubnetViolation' value with any optional fields omitted.
mkNetworkFirewallMissingSubnetViolation ::
  NetworkFirewallMissingSubnetViolation
mkNetworkFirewallMissingSubnetViolation =
  NetworkFirewallMissingSubnetViolation'
    { availabilityZone =
        Core.Nothing,
      targetViolationReason = Core.Nothing,
      vpc = Core.Nothing,
      violationTarget = Core.Nothing
    }

-- | The Availability Zone of a violating subnet.
--
-- /Note:/ Consider using 'availabilityZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nfmsvAvailabilityZone :: Lens.Lens' NetworkFirewallMissingSubnetViolation (Core.Maybe Types.AvailabilityZone)
nfmsvAvailabilityZone = Lens.field @"availabilityZone"
{-# DEPRECATED nfmsvAvailabilityZone "Use generic-lens or generic-optics with 'availabilityZone' instead." #-}

-- | The reason the resource has this violation, if one is available.
--
-- /Note:/ Consider using 'targetViolationReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nfmsvTargetViolationReason :: Lens.Lens' NetworkFirewallMissingSubnetViolation (Core.Maybe Types.TargetViolationReason)
nfmsvTargetViolationReason = Lens.field @"targetViolationReason"
{-# DEPRECATED nfmsvTargetViolationReason "Use generic-lens or generic-optics with 'targetViolationReason' instead." #-}

-- | The resource ID of the VPC associated with a violating subnet.
--
-- /Note:/ Consider using 'vpc' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nfmsvVPC :: Lens.Lens' NetworkFirewallMissingSubnetViolation (Core.Maybe Types.ResourceId)
nfmsvVPC = Lens.field @"vpc"
{-# DEPRECATED nfmsvVPC "Use generic-lens or generic-optics with 'vpc' instead." #-}

-- | The ID of the AWS Network Firewall or VPC resource that's in violation.
--
-- /Note:/ Consider using 'violationTarget' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nfmsvViolationTarget :: Lens.Lens' NetworkFirewallMissingSubnetViolation (Core.Maybe Types.ViolationTarget)
nfmsvViolationTarget = Lens.field @"violationTarget"
{-# DEPRECATED nfmsvViolationTarget "Use generic-lens or generic-optics with 'violationTarget' instead." #-}

instance Core.FromJSON NetworkFirewallMissingSubnetViolation where
  parseJSON =
    Core.withObject "NetworkFirewallMissingSubnetViolation" Core.$
      \x ->
        NetworkFirewallMissingSubnetViolation'
          Core.<$> (x Core..:? "AvailabilityZone")
          Core.<*> (x Core..:? "TargetViolationReason")
          Core.<*> (x Core..:? "VPC")
          Core.<*> (x Core..:? "ViolationTarget")
