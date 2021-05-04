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
-- Module      : Network.AWS.ELBv2.Types.SubnetMapping
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ELBv2.Types.SubnetMapping where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Information about a subnet mapping.
--
-- /See:/ 'newSubnetMapping' smart constructor.
data SubnetMapping = SubnetMapping'
  { -- | [Network Load Balancers] The private IPv4 address for an internal load
    -- balancer.
    privateIPv4Address :: Prelude.Maybe Prelude.Text,
    -- | [Network Load Balancers] The IPv6 address.
    iPv6Address :: Prelude.Maybe Prelude.Text,
    -- | The ID of the subnet.
    subnetId :: Prelude.Maybe Prelude.Text,
    -- | [Network Load Balancers] The allocation ID of the Elastic IP address for
    -- an internet-facing load balancer.
    allocationId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { privateIPv4Address =
        Prelude.Nothing,
      iPv6Address = Prelude.Nothing,
      subnetId = Prelude.Nothing,
      allocationId = Prelude.Nothing
    }

-- | [Network Load Balancers] The private IPv4 address for an internal load
-- balancer.
subnetMapping_privateIPv4Address :: Lens.Lens' SubnetMapping (Prelude.Maybe Prelude.Text)
subnetMapping_privateIPv4Address = Lens.lens (\SubnetMapping' {privateIPv4Address} -> privateIPv4Address) (\s@SubnetMapping' {} a -> s {privateIPv4Address = a} :: SubnetMapping)

-- | [Network Load Balancers] The IPv6 address.
subnetMapping_iPv6Address :: Lens.Lens' SubnetMapping (Prelude.Maybe Prelude.Text)
subnetMapping_iPv6Address = Lens.lens (\SubnetMapping' {iPv6Address} -> iPv6Address) (\s@SubnetMapping' {} a -> s {iPv6Address = a} :: SubnetMapping)

-- | The ID of the subnet.
subnetMapping_subnetId :: Lens.Lens' SubnetMapping (Prelude.Maybe Prelude.Text)
subnetMapping_subnetId = Lens.lens (\SubnetMapping' {subnetId} -> subnetId) (\s@SubnetMapping' {} a -> s {subnetId = a} :: SubnetMapping)

-- | [Network Load Balancers] The allocation ID of the Elastic IP address for
-- an internet-facing load balancer.
subnetMapping_allocationId :: Lens.Lens' SubnetMapping (Prelude.Maybe Prelude.Text)
subnetMapping_allocationId = Lens.lens (\SubnetMapping' {allocationId} -> allocationId) (\s@SubnetMapping' {} a -> s {allocationId = a} :: SubnetMapping)

instance Prelude.Hashable SubnetMapping

instance Prelude.NFData SubnetMapping

instance Prelude.ToQuery SubnetMapping where
  toQuery SubnetMapping' {..} =
    Prelude.mconcat
      [ "PrivateIPv4Address" Prelude.=: privateIPv4Address,
        "IPv6Address" Prelude.=: iPv6Address,
        "SubnetId" Prelude.=: subnetId,
        "AllocationId" Prelude.=: allocationId
      ]
