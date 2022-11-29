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
-- Module      : Amazonka.ELBV2.Types.AvailabilityZone
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ELBV2.Types.AvailabilityZone where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.ELBV2.Types.LoadBalancerAddress
import qualified Amazonka.Prelude as Prelude

-- | Information about an Availability Zone.
--
-- /See:/ 'newAvailabilityZone' smart constructor.
data AvailabilityZone = AvailabilityZone'
  { -- | [Application Load Balancers on Outposts] The ID of the Outpost.
    outpostId :: Prelude.Maybe Prelude.Text,
    -- | The name of the Availability Zone.
    zoneName :: Prelude.Maybe Prelude.Text,
    -- | The ID of the subnet. You can specify one subnet per Availability Zone.
    subnetId :: Prelude.Maybe Prelude.Text,
    -- | [Network Load Balancers] If you need static IP addresses for your load
    -- balancer, you can specify one Elastic IP address per Availability Zone
    -- when you create an internal-facing load balancer. For internal load
    -- balancers, you can specify a private IP address from the IPv4 range of
    -- the subnet.
    loadBalancerAddresses :: Prelude.Maybe [LoadBalancerAddress]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AvailabilityZone' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'outpostId', 'availabilityZone_outpostId' - [Application Load Balancers on Outposts] The ID of the Outpost.
--
-- 'zoneName', 'availabilityZone_zoneName' - The name of the Availability Zone.
--
-- 'subnetId', 'availabilityZone_subnetId' - The ID of the subnet. You can specify one subnet per Availability Zone.
--
-- 'loadBalancerAddresses', 'availabilityZone_loadBalancerAddresses' - [Network Load Balancers] If you need static IP addresses for your load
-- balancer, you can specify one Elastic IP address per Availability Zone
-- when you create an internal-facing load balancer. For internal load
-- balancers, you can specify a private IP address from the IPv4 range of
-- the subnet.
newAvailabilityZone ::
  AvailabilityZone
newAvailabilityZone =
  AvailabilityZone'
    { outpostId = Prelude.Nothing,
      zoneName = Prelude.Nothing,
      subnetId = Prelude.Nothing,
      loadBalancerAddresses = Prelude.Nothing
    }

-- | [Application Load Balancers on Outposts] The ID of the Outpost.
availabilityZone_outpostId :: Lens.Lens' AvailabilityZone (Prelude.Maybe Prelude.Text)
availabilityZone_outpostId = Lens.lens (\AvailabilityZone' {outpostId} -> outpostId) (\s@AvailabilityZone' {} a -> s {outpostId = a} :: AvailabilityZone)

-- | The name of the Availability Zone.
availabilityZone_zoneName :: Lens.Lens' AvailabilityZone (Prelude.Maybe Prelude.Text)
availabilityZone_zoneName = Lens.lens (\AvailabilityZone' {zoneName} -> zoneName) (\s@AvailabilityZone' {} a -> s {zoneName = a} :: AvailabilityZone)

-- | The ID of the subnet. You can specify one subnet per Availability Zone.
availabilityZone_subnetId :: Lens.Lens' AvailabilityZone (Prelude.Maybe Prelude.Text)
availabilityZone_subnetId = Lens.lens (\AvailabilityZone' {subnetId} -> subnetId) (\s@AvailabilityZone' {} a -> s {subnetId = a} :: AvailabilityZone)

-- | [Network Load Balancers] If you need static IP addresses for your load
-- balancer, you can specify one Elastic IP address per Availability Zone
-- when you create an internal-facing load balancer. For internal load
-- balancers, you can specify a private IP address from the IPv4 range of
-- the subnet.
availabilityZone_loadBalancerAddresses :: Lens.Lens' AvailabilityZone (Prelude.Maybe [LoadBalancerAddress])
availabilityZone_loadBalancerAddresses = Lens.lens (\AvailabilityZone' {loadBalancerAddresses} -> loadBalancerAddresses) (\s@AvailabilityZone' {} a -> s {loadBalancerAddresses = a} :: AvailabilityZone) Prelude.. Lens.mapping Lens.coerced

instance Core.FromXML AvailabilityZone where
  parseXML x =
    AvailabilityZone'
      Prelude.<$> (x Core..@? "OutpostId")
      Prelude.<*> (x Core..@? "ZoneName")
      Prelude.<*> (x Core..@? "SubnetId")
      Prelude.<*> ( x Core..@? "LoadBalancerAddresses"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "member")
                  )

instance Prelude.Hashable AvailabilityZone where
  hashWithSalt _salt AvailabilityZone' {..} =
    _salt `Prelude.hashWithSalt` outpostId
      `Prelude.hashWithSalt` zoneName
      `Prelude.hashWithSalt` subnetId
      `Prelude.hashWithSalt` loadBalancerAddresses

instance Prelude.NFData AvailabilityZone where
  rnf AvailabilityZone' {..} =
    Prelude.rnf outpostId
      `Prelude.seq` Prelude.rnf zoneName
      `Prelude.seq` Prelude.rnf subnetId
      `Prelude.seq` Prelude.rnf loadBalancerAddresses
