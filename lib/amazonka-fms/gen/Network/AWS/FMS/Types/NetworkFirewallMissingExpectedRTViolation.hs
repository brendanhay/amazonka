{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.FMS.Types.NetworkFirewallMissingExpectedRTViolation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.FMS.Types.NetworkFirewallMissingExpectedRTViolation
  ( NetworkFirewallMissingExpectedRTViolation (..),

    -- * Smart constructor
    mkNetworkFirewallMissingExpectedRTViolation,

    -- * Lenses
    nfmertvCurrentRouteTable,
    nfmertvAvailabilityZone,
    nfmertvVPC,
    nfmertvViolationTarget,
    nfmertvExpectedRouteTable,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Violation details for AWS Network Firewall for a subnet that's not associated to the expected Firewall Manager managed route table.
--
-- /See:/ 'mkNetworkFirewallMissingExpectedRTViolation' smart constructor.
data NetworkFirewallMissingExpectedRTViolation = NetworkFirewallMissingExpectedRTViolation'
  { currentRouteTable ::
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
        Lude.Text,
    expectedRouteTable ::
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

-- | Creates a value of 'NetworkFirewallMissingExpectedRTViolation' with the minimum fields required to make a request.
--
-- * 'availabilityZone' - The Availability Zone of a violating subnet.
-- * 'currentRouteTable' - The resource ID of the current route table that's associated with the subnet, if one is available.
-- * 'expectedRouteTable' - The resource ID of the route table that should be associated with the subnet.
-- * 'violationTarget' - The ID of the AWS Network Firewall or VPC resource that's in violation.
-- * 'vpc' - The resource ID of the VPC associated with a violating subnet.
mkNetworkFirewallMissingExpectedRTViolation ::
  NetworkFirewallMissingExpectedRTViolation
mkNetworkFirewallMissingExpectedRTViolation =
  NetworkFirewallMissingExpectedRTViolation'
    { currentRouteTable =
        Lude.Nothing,
      availabilityZone = Lude.Nothing,
      vpc = Lude.Nothing,
      violationTarget = Lude.Nothing,
      expectedRouteTable = Lude.Nothing
    }

-- | The resource ID of the current route table that's associated with the subnet, if one is available.
--
-- /Note:/ Consider using 'currentRouteTable' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nfmertvCurrentRouteTable :: Lens.Lens' NetworkFirewallMissingExpectedRTViolation (Lude.Maybe Lude.Text)
nfmertvCurrentRouteTable = Lens.lens (currentRouteTable :: NetworkFirewallMissingExpectedRTViolation -> Lude.Maybe Lude.Text) (\s a -> s {currentRouteTable = a} :: NetworkFirewallMissingExpectedRTViolation)
{-# DEPRECATED nfmertvCurrentRouteTable "Use generic-lens or generic-optics with 'currentRouteTable' instead." #-}

-- | The Availability Zone of a violating subnet.
--
-- /Note:/ Consider using 'availabilityZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nfmertvAvailabilityZone :: Lens.Lens' NetworkFirewallMissingExpectedRTViolation (Lude.Maybe Lude.Text)
nfmertvAvailabilityZone = Lens.lens (availabilityZone :: NetworkFirewallMissingExpectedRTViolation -> Lude.Maybe Lude.Text) (\s a -> s {availabilityZone = a} :: NetworkFirewallMissingExpectedRTViolation)
{-# DEPRECATED nfmertvAvailabilityZone "Use generic-lens or generic-optics with 'availabilityZone' instead." #-}

-- | The resource ID of the VPC associated with a violating subnet.
--
-- /Note:/ Consider using 'vpc' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nfmertvVPC :: Lens.Lens' NetworkFirewallMissingExpectedRTViolation (Lude.Maybe Lude.Text)
nfmertvVPC = Lens.lens (vpc :: NetworkFirewallMissingExpectedRTViolation -> Lude.Maybe Lude.Text) (\s a -> s {vpc = a} :: NetworkFirewallMissingExpectedRTViolation)
{-# DEPRECATED nfmertvVPC "Use generic-lens or generic-optics with 'vpc' instead." #-}

-- | The ID of the AWS Network Firewall or VPC resource that's in violation.
--
-- /Note:/ Consider using 'violationTarget' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nfmertvViolationTarget :: Lens.Lens' NetworkFirewallMissingExpectedRTViolation (Lude.Maybe Lude.Text)
nfmertvViolationTarget = Lens.lens (violationTarget :: NetworkFirewallMissingExpectedRTViolation -> Lude.Maybe Lude.Text) (\s a -> s {violationTarget = a} :: NetworkFirewallMissingExpectedRTViolation)
{-# DEPRECATED nfmertvViolationTarget "Use generic-lens or generic-optics with 'violationTarget' instead." #-}

-- | The resource ID of the route table that should be associated with the subnet.
--
-- /Note:/ Consider using 'expectedRouteTable' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nfmertvExpectedRouteTable :: Lens.Lens' NetworkFirewallMissingExpectedRTViolation (Lude.Maybe Lude.Text)
nfmertvExpectedRouteTable = Lens.lens (expectedRouteTable :: NetworkFirewallMissingExpectedRTViolation -> Lude.Maybe Lude.Text) (\s a -> s {expectedRouteTable = a} :: NetworkFirewallMissingExpectedRTViolation)
{-# DEPRECATED nfmertvExpectedRouteTable "Use generic-lens or generic-optics with 'expectedRouteTable' instead." #-}

instance Lude.FromJSON NetworkFirewallMissingExpectedRTViolation where
  parseJSON =
    Lude.withObject
      "NetworkFirewallMissingExpectedRTViolation"
      ( \x ->
          NetworkFirewallMissingExpectedRTViolation'
            Lude.<$> (x Lude..:? "CurrentRouteTable")
            Lude.<*> (x Lude..:? "AvailabilityZone")
            Lude.<*> (x Lude..:? "VPC")
            Lude.<*> (x Lude..:? "ViolationTarget")
            Lude.<*> (x Lude..:? "ExpectedRouteTable")
      )
