{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELBv2.Types.AvailabilityZone
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ELBv2.Types.AvailabilityZone where

import qualified Network.AWS.Core as Core
import Network.AWS.ELBv2.Types.LoadBalancerAddress
import qualified Network.AWS.Lens as Lens

-- | Information about an Availability Zone.
--
-- /See:/ 'newAvailabilityZone' smart constructor.
data AvailabilityZone = AvailabilityZone'
  { -- | The name of the Availability Zone.
    zoneName :: Core.Maybe Core.Text,
    -- | [Application Load Balancers on Outposts] The ID of the Outpost.
    outpostId :: Core.Maybe Core.Text,
    -- | [Network Load Balancers] If you need static IP addresses for your load
    -- balancer, you can specify one Elastic IP address per Availability Zone
    -- when you create an internal-facing load balancer. For internal load
    -- balancers, you can specify a private IP address from the IPv4 range of
    -- the subnet.
    loadBalancerAddresses :: Core.Maybe [LoadBalancerAddress],
    -- | The ID of the subnet. You can specify one subnet per Availability Zone.
    subnetId :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AvailabilityZone' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'zoneName', 'availabilityZone_zoneName' - The name of the Availability Zone.
--
-- 'outpostId', 'availabilityZone_outpostId' - [Application Load Balancers on Outposts] The ID of the Outpost.
--
-- 'loadBalancerAddresses', 'availabilityZone_loadBalancerAddresses' - [Network Load Balancers] If you need static IP addresses for your load
-- balancer, you can specify one Elastic IP address per Availability Zone
-- when you create an internal-facing load balancer. For internal load
-- balancers, you can specify a private IP address from the IPv4 range of
-- the subnet.
--
-- 'subnetId', 'availabilityZone_subnetId' - The ID of the subnet. You can specify one subnet per Availability Zone.
newAvailabilityZone ::
  AvailabilityZone
newAvailabilityZone =
  AvailabilityZone'
    { zoneName = Core.Nothing,
      outpostId = Core.Nothing,
      loadBalancerAddresses = Core.Nothing,
      subnetId = Core.Nothing
    }

-- | The name of the Availability Zone.
availabilityZone_zoneName :: Lens.Lens' AvailabilityZone (Core.Maybe Core.Text)
availabilityZone_zoneName = Lens.lens (\AvailabilityZone' {zoneName} -> zoneName) (\s@AvailabilityZone' {} a -> s {zoneName = a} :: AvailabilityZone)

-- | [Application Load Balancers on Outposts] The ID of the Outpost.
availabilityZone_outpostId :: Lens.Lens' AvailabilityZone (Core.Maybe Core.Text)
availabilityZone_outpostId = Lens.lens (\AvailabilityZone' {outpostId} -> outpostId) (\s@AvailabilityZone' {} a -> s {outpostId = a} :: AvailabilityZone)

-- | [Network Load Balancers] If you need static IP addresses for your load
-- balancer, you can specify one Elastic IP address per Availability Zone
-- when you create an internal-facing load balancer. For internal load
-- balancers, you can specify a private IP address from the IPv4 range of
-- the subnet.
availabilityZone_loadBalancerAddresses :: Lens.Lens' AvailabilityZone (Core.Maybe [LoadBalancerAddress])
availabilityZone_loadBalancerAddresses = Lens.lens (\AvailabilityZone' {loadBalancerAddresses} -> loadBalancerAddresses) (\s@AvailabilityZone' {} a -> s {loadBalancerAddresses = a} :: AvailabilityZone) Core.. Lens.mapping Lens._Coerce

-- | The ID of the subnet. You can specify one subnet per Availability Zone.
availabilityZone_subnetId :: Lens.Lens' AvailabilityZone (Core.Maybe Core.Text)
availabilityZone_subnetId = Lens.lens (\AvailabilityZone' {subnetId} -> subnetId) (\s@AvailabilityZone' {} a -> s {subnetId = a} :: AvailabilityZone)

instance Core.FromXML AvailabilityZone where
  parseXML x =
    AvailabilityZone'
      Core.<$> (x Core..@? "ZoneName")
      Core.<*> (x Core..@? "OutpostId")
      Core.<*> ( x Core..@? "LoadBalancerAddresses"
                   Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "member")
               )
      Core.<*> (x Core..@? "SubnetId")

instance Core.Hashable AvailabilityZone

instance Core.NFData AvailabilityZone
