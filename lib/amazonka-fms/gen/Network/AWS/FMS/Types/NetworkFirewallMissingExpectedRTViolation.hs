{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.FMS.Types.NetworkFirewallMissingExpectedRTViolation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.FMS.Types.NetworkFirewallMissingExpectedRTViolation
  ( NetworkFirewallMissingExpectedRTViolation (..)
  -- * Smart constructor
  , mkNetworkFirewallMissingExpectedRTViolation
  -- * Lenses
  , nfmertvAvailabilityZone
  , nfmertvCurrentRouteTable
  , nfmertvExpectedRouteTable
  , nfmertvVPC
  , nfmertvViolationTarget
  ) where

import qualified Network.AWS.FMS.Types.AvailabilityZone as Types
import qualified Network.AWS.FMS.Types.ResourceId as Types
import qualified Network.AWS.FMS.Types.ViolationTarget as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Violation details for AWS Network Firewall for a subnet that's not associated to the expected Firewall Manager managed route table.
--
-- /See:/ 'mkNetworkFirewallMissingExpectedRTViolation' smart constructor.
data NetworkFirewallMissingExpectedRTViolation = NetworkFirewallMissingExpectedRTViolation'
  { availabilityZone :: Core.Maybe Types.AvailabilityZone
    -- ^ The Availability Zone of a violating subnet. 
  , currentRouteTable :: Core.Maybe Types.ResourceId
    -- ^ The resource ID of the current route table that's associated with the subnet, if one is available.
  , expectedRouteTable :: Core.Maybe Types.ResourceId
    -- ^ The resource ID of the route table that should be associated with the subnet.
  , vpc :: Core.Maybe Types.ResourceId
    -- ^ The resource ID of the VPC associated with a violating subnet.
  , violationTarget :: Core.Maybe Types.ViolationTarget
    -- ^ The ID of the AWS Network Firewall or VPC resource that's in violation.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'NetworkFirewallMissingExpectedRTViolation' value with any optional fields omitted.
mkNetworkFirewallMissingExpectedRTViolation
    :: NetworkFirewallMissingExpectedRTViolation
mkNetworkFirewallMissingExpectedRTViolation
  = NetworkFirewallMissingExpectedRTViolation'{availabilityZone =
                                                 Core.Nothing,
                                               currentRouteTable = Core.Nothing,
                                               expectedRouteTable = Core.Nothing,
                                               vpc = Core.Nothing, violationTarget = Core.Nothing}

-- | The Availability Zone of a violating subnet. 
--
-- /Note:/ Consider using 'availabilityZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nfmertvAvailabilityZone :: Lens.Lens' NetworkFirewallMissingExpectedRTViolation (Core.Maybe Types.AvailabilityZone)
nfmertvAvailabilityZone = Lens.field @"availabilityZone"
{-# INLINEABLE nfmertvAvailabilityZone #-}
{-# DEPRECATED availabilityZone "Use generic-lens or generic-optics with 'availabilityZone' instead"  #-}

-- | The resource ID of the current route table that's associated with the subnet, if one is available.
--
-- /Note:/ Consider using 'currentRouteTable' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nfmertvCurrentRouteTable :: Lens.Lens' NetworkFirewallMissingExpectedRTViolation (Core.Maybe Types.ResourceId)
nfmertvCurrentRouteTable = Lens.field @"currentRouteTable"
{-# INLINEABLE nfmertvCurrentRouteTable #-}
{-# DEPRECATED currentRouteTable "Use generic-lens or generic-optics with 'currentRouteTable' instead"  #-}

-- | The resource ID of the route table that should be associated with the subnet.
--
-- /Note:/ Consider using 'expectedRouteTable' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nfmertvExpectedRouteTable :: Lens.Lens' NetworkFirewallMissingExpectedRTViolation (Core.Maybe Types.ResourceId)
nfmertvExpectedRouteTable = Lens.field @"expectedRouteTable"
{-# INLINEABLE nfmertvExpectedRouteTable #-}
{-# DEPRECATED expectedRouteTable "Use generic-lens or generic-optics with 'expectedRouteTable' instead"  #-}

-- | The resource ID of the VPC associated with a violating subnet.
--
-- /Note:/ Consider using 'vpc' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nfmertvVPC :: Lens.Lens' NetworkFirewallMissingExpectedRTViolation (Core.Maybe Types.ResourceId)
nfmertvVPC = Lens.field @"vpc"
{-# INLINEABLE nfmertvVPC #-}
{-# DEPRECATED vpc "Use generic-lens or generic-optics with 'vpc' instead"  #-}

-- | The ID of the AWS Network Firewall or VPC resource that's in violation.
--
-- /Note:/ Consider using 'violationTarget' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nfmertvViolationTarget :: Lens.Lens' NetworkFirewallMissingExpectedRTViolation (Core.Maybe Types.ViolationTarget)
nfmertvViolationTarget = Lens.field @"violationTarget"
{-# INLINEABLE nfmertvViolationTarget #-}
{-# DEPRECATED violationTarget "Use generic-lens or generic-optics with 'violationTarget' instead"  #-}

instance Core.FromJSON NetworkFirewallMissingExpectedRTViolation
         where
        parseJSON
          = Core.withObject "NetworkFirewallMissingExpectedRTViolation"
              Core.$
              \ x ->
                NetworkFirewallMissingExpectedRTViolation' Core.<$>
                  (x Core..:? "AvailabilityZone") Core.<*>
                    x Core..:? "CurrentRouteTable"
                    Core.<*> x Core..:? "ExpectedRouteTable"
                    Core.<*> x Core..:? "VPC"
                    Core.<*> x Core..:? "ViolationTarget"
