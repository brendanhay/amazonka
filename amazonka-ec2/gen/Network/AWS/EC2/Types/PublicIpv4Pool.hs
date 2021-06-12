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
-- Module      : Network.AWS.EC2.Types.PublicIpv4Pool
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.PublicIpv4Pool where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.PublicIpv4PoolRange
import Network.AWS.EC2.Types.Tag
import qualified Network.AWS.Lens as Lens

-- | Describes an IPv4 address pool.
--
-- /See:/ 'newPublicIpv4Pool' smart constructor.
data PublicIpv4Pool = PublicIpv4Pool'
  { -- | The ID of the address pool.
    poolId :: Core.Maybe Core.Text,
    -- | The address ranges.
    poolAddressRanges :: Core.Maybe [PublicIpv4PoolRange],
    -- | The total number of addresses.
    totalAddressCount :: Core.Maybe Core.Int,
    -- | Any tags for the address pool.
    tags :: Core.Maybe [Tag],
    -- | The total number of available addresses.
    totalAvailableAddressCount :: Core.Maybe Core.Int,
    -- | A description of the address pool.
    description :: Core.Maybe Core.Text,
    -- | The name of the location from which the address pool is advertised. A
    -- network border group is a unique set of Availability Zones or Local
    -- Zones from where AWS advertises public IP addresses.
    networkBorderGroup :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'PublicIpv4Pool' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'poolId', 'publicIpv4Pool_poolId' - The ID of the address pool.
--
-- 'poolAddressRanges', 'publicIpv4Pool_poolAddressRanges' - The address ranges.
--
-- 'totalAddressCount', 'publicIpv4Pool_totalAddressCount' - The total number of addresses.
--
-- 'tags', 'publicIpv4Pool_tags' - Any tags for the address pool.
--
-- 'totalAvailableAddressCount', 'publicIpv4Pool_totalAvailableAddressCount' - The total number of available addresses.
--
-- 'description', 'publicIpv4Pool_description' - A description of the address pool.
--
-- 'networkBorderGroup', 'publicIpv4Pool_networkBorderGroup' - The name of the location from which the address pool is advertised. A
-- network border group is a unique set of Availability Zones or Local
-- Zones from where AWS advertises public IP addresses.
newPublicIpv4Pool ::
  PublicIpv4Pool
newPublicIpv4Pool =
  PublicIpv4Pool'
    { poolId = Core.Nothing,
      poolAddressRanges = Core.Nothing,
      totalAddressCount = Core.Nothing,
      tags = Core.Nothing,
      totalAvailableAddressCount = Core.Nothing,
      description = Core.Nothing,
      networkBorderGroup = Core.Nothing
    }

-- | The ID of the address pool.
publicIpv4Pool_poolId :: Lens.Lens' PublicIpv4Pool (Core.Maybe Core.Text)
publicIpv4Pool_poolId = Lens.lens (\PublicIpv4Pool' {poolId} -> poolId) (\s@PublicIpv4Pool' {} a -> s {poolId = a} :: PublicIpv4Pool)

-- | The address ranges.
publicIpv4Pool_poolAddressRanges :: Lens.Lens' PublicIpv4Pool (Core.Maybe [PublicIpv4PoolRange])
publicIpv4Pool_poolAddressRanges = Lens.lens (\PublicIpv4Pool' {poolAddressRanges} -> poolAddressRanges) (\s@PublicIpv4Pool' {} a -> s {poolAddressRanges = a} :: PublicIpv4Pool) Core.. Lens.mapping Lens._Coerce

-- | The total number of addresses.
publicIpv4Pool_totalAddressCount :: Lens.Lens' PublicIpv4Pool (Core.Maybe Core.Int)
publicIpv4Pool_totalAddressCount = Lens.lens (\PublicIpv4Pool' {totalAddressCount} -> totalAddressCount) (\s@PublicIpv4Pool' {} a -> s {totalAddressCount = a} :: PublicIpv4Pool)

-- | Any tags for the address pool.
publicIpv4Pool_tags :: Lens.Lens' PublicIpv4Pool (Core.Maybe [Tag])
publicIpv4Pool_tags = Lens.lens (\PublicIpv4Pool' {tags} -> tags) (\s@PublicIpv4Pool' {} a -> s {tags = a} :: PublicIpv4Pool) Core.. Lens.mapping Lens._Coerce

-- | The total number of available addresses.
publicIpv4Pool_totalAvailableAddressCount :: Lens.Lens' PublicIpv4Pool (Core.Maybe Core.Int)
publicIpv4Pool_totalAvailableAddressCount = Lens.lens (\PublicIpv4Pool' {totalAvailableAddressCount} -> totalAvailableAddressCount) (\s@PublicIpv4Pool' {} a -> s {totalAvailableAddressCount = a} :: PublicIpv4Pool)

-- | A description of the address pool.
publicIpv4Pool_description :: Lens.Lens' PublicIpv4Pool (Core.Maybe Core.Text)
publicIpv4Pool_description = Lens.lens (\PublicIpv4Pool' {description} -> description) (\s@PublicIpv4Pool' {} a -> s {description = a} :: PublicIpv4Pool)

-- | The name of the location from which the address pool is advertised. A
-- network border group is a unique set of Availability Zones or Local
-- Zones from where AWS advertises public IP addresses.
publicIpv4Pool_networkBorderGroup :: Lens.Lens' PublicIpv4Pool (Core.Maybe Core.Text)
publicIpv4Pool_networkBorderGroup = Lens.lens (\PublicIpv4Pool' {networkBorderGroup} -> networkBorderGroup) (\s@PublicIpv4Pool' {} a -> s {networkBorderGroup = a} :: PublicIpv4Pool)

instance Core.FromXML PublicIpv4Pool where
  parseXML x =
    PublicIpv4Pool'
      Core.<$> (x Core..@? "poolId")
      Core.<*> ( x Core..@? "poolAddressRangeSet"
                   Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "item")
               )
      Core.<*> (x Core..@? "totalAddressCount")
      Core.<*> ( x Core..@? "tagSet" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "item")
               )
      Core.<*> (x Core..@? "totalAvailableAddressCount")
      Core.<*> (x Core..@? "description")
      Core.<*> (x Core..@? "networkBorderGroup")

instance Core.Hashable PublicIpv4Pool

instance Core.NFData PublicIpv4Pool
