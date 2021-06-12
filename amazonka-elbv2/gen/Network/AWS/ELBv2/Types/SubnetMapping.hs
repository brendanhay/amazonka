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
-- Module      : Network.AWS.ELBv2.Types.SubnetMapping
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ELBv2.Types.SubnetMapping where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Information about a subnet mapping.
--
-- /See:/ 'newSubnetMapping' smart constructor.
data SubnetMapping = SubnetMapping'
  { -- | [Network Load Balancers] The private IPv4 address for an internal load
    -- balancer.
    privateIPv4Address :: Core.Maybe Core.Text,
    -- | [Network Load Balancers] The IPv6 address.
    iPv6Address :: Core.Maybe Core.Text,
    -- | The ID of the subnet.
    subnetId :: Core.Maybe Core.Text,
    -- | [Network Load Balancers] The allocation ID of the Elastic IP address for
    -- an internet-facing load balancer.
    allocationId :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'SubnetMapping' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'privateIPv4Address', 'subnetMapping_privateIPv4Address' - [Network Load Balancers] The private IPv4 address for an internal load
-- balancer.
--
-- 'iPv6Address', 'subnetMapping_iPv6Address' - [Network Load Balancers] The IPv6 address.
--
-- 'subnetId', 'subnetMapping_subnetId' - The ID of the subnet.
--
-- 'allocationId', 'subnetMapping_allocationId' - [Network Load Balancers] The allocation ID of the Elastic IP address for
-- an internet-facing load balancer.
newSubnetMapping ::
  SubnetMapping
newSubnetMapping =
  SubnetMapping'
    { privateIPv4Address = Core.Nothing,
      iPv6Address = Core.Nothing,
      subnetId = Core.Nothing,
      allocationId = Core.Nothing
    }

-- | [Network Load Balancers] The private IPv4 address for an internal load
-- balancer.
subnetMapping_privateIPv4Address :: Lens.Lens' SubnetMapping (Core.Maybe Core.Text)
subnetMapping_privateIPv4Address = Lens.lens (\SubnetMapping' {privateIPv4Address} -> privateIPv4Address) (\s@SubnetMapping' {} a -> s {privateIPv4Address = a} :: SubnetMapping)

-- | [Network Load Balancers] The IPv6 address.
subnetMapping_iPv6Address :: Lens.Lens' SubnetMapping (Core.Maybe Core.Text)
subnetMapping_iPv6Address = Lens.lens (\SubnetMapping' {iPv6Address} -> iPv6Address) (\s@SubnetMapping' {} a -> s {iPv6Address = a} :: SubnetMapping)

-- | The ID of the subnet.
subnetMapping_subnetId :: Lens.Lens' SubnetMapping (Core.Maybe Core.Text)
subnetMapping_subnetId = Lens.lens (\SubnetMapping' {subnetId} -> subnetId) (\s@SubnetMapping' {} a -> s {subnetId = a} :: SubnetMapping)

-- | [Network Load Balancers] The allocation ID of the Elastic IP address for
-- an internet-facing load balancer.
subnetMapping_allocationId :: Lens.Lens' SubnetMapping (Core.Maybe Core.Text)
subnetMapping_allocationId = Lens.lens (\SubnetMapping' {allocationId} -> allocationId) (\s@SubnetMapping' {} a -> s {allocationId = a} :: SubnetMapping)

instance Core.Hashable SubnetMapping

instance Core.NFData SubnetMapping

instance Core.ToQuery SubnetMapping where
  toQuery SubnetMapping' {..} =
    Core.mconcat
      [ "PrivateIPv4Address" Core.=: privateIPv4Address,
        "IPv6Address" Core.=: iPv6Address,
        "SubnetId" Core.=: subnetId,
        "AllocationId" Core.=: allocationId
      ]
