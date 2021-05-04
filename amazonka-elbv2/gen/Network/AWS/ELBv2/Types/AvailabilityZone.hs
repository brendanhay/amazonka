{-# LANGUAGE DeriveDataTypeable #-}
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

import Network.AWS.ELBv2.Types.LoadBalancerAddress
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Information about an Availability Zone.
--
-- /See:/ 'newAvailabilityZone' smart constructor.
data AvailabilityZone = AvailabilityZone'
  { -- | The name of the Availability Zone.
    zoneName :: Prelude.Maybe Prelude.Text,
    -- | [Application Load Balancers on Outposts] The ID of the Outpost.
    outpostId :: Prelude.Maybe Prelude.Text,
    -- | [Network Load Balancers] If you need static IP addresses for your load
    -- balancer, you can specify one Elastic IP address per Availability Zone
    -- when you create an internal-facing load balancer. For internal load
    -- balancers, you can specify a private IP address from the IPv4 range of
    -- the subnet.
    loadBalancerAddresses :: Prelude.Maybe [LoadBalancerAddress],
    -- | The ID of the subnet. You can specify one subnet per Availability Zone.
    subnetId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { zoneName = Prelude.Nothing,
      outpostId = Prelude.Nothing,
      loadBalancerAddresses = Prelude.Nothing,
      subnetId = Prelude.Nothing
    }

-- | The name of the Availability Zone.
availabilityZone_zoneName :: Lens.Lens' AvailabilityZone (Prelude.Maybe Prelude.Text)
availabilityZone_zoneName = Lens.lens (\AvailabilityZone' {zoneName} -> zoneName) (\s@AvailabilityZone' {} a -> s {zoneName = a} :: AvailabilityZone)

-- | [Application Load Balancers on Outposts] The ID of the Outpost.
availabilityZone_outpostId :: Lens.Lens' AvailabilityZone (Prelude.Maybe Prelude.Text)
availabilityZone_outpostId = Lens.lens (\AvailabilityZone' {outpostId} -> outpostId) (\s@AvailabilityZone' {} a -> s {outpostId = a} :: AvailabilityZone)

-- | [Network Load Balancers] If you need static IP addresses for your load
-- balancer, you can specify one Elastic IP address per Availability Zone
-- when you create an internal-facing load balancer. For internal load
-- balancers, you can specify a private IP address from the IPv4 range of
-- the subnet.
availabilityZone_loadBalancerAddresses :: Lens.Lens' AvailabilityZone (Prelude.Maybe [LoadBalancerAddress])
availabilityZone_loadBalancerAddresses = Lens.lens (\AvailabilityZone' {loadBalancerAddresses} -> loadBalancerAddresses) (\s@AvailabilityZone' {} a -> s {loadBalancerAddresses = a} :: AvailabilityZone) Prelude.. Lens.mapping Prelude._Coerce

-- | The ID of the subnet. You can specify one subnet per Availability Zone.
availabilityZone_subnetId :: Lens.Lens' AvailabilityZone (Prelude.Maybe Prelude.Text)
availabilityZone_subnetId = Lens.lens (\AvailabilityZone' {subnetId} -> subnetId) (\s@AvailabilityZone' {} a -> s {subnetId = a} :: AvailabilityZone)

instance Prelude.FromXML AvailabilityZone where
  parseXML x =
    AvailabilityZone'
      Prelude.<$> (x Prelude..@? "ZoneName")
      Prelude.<*> (x Prelude..@? "OutpostId")
      Prelude.<*> ( x Prelude..@? "LoadBalancerAddresses"
                      Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may (Prelude.parseXMLList "member")
                  )
      Prelude.<*> (x Prelude..@? "SubnetId")

instance Prelude.Hashable AvailabilityZone

instance Prelude.NFData AvailabilityZone
