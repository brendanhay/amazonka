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
-- Module      : Network.AWS.EC2.Types.PublicIpv4Pool
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.PublicIpv4Pool where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.PublicIpv4PoolRange
import Network.AWS.EC2.Types.Tag
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes an IPv4 address pool.
--
-- /See:/ 'newPublicIpv4Pool' smart constructor.
data PublicIpv4Pool = PublicIpv4Pool'
  { -- | The ID of the address pool.
    poolId :: Prelude.Maybe Prelude.Text,
    -- | The address ranges.
    poolAddressRanges :: Prelude.Maybe [PublicIpv4PoolRange],
    -- | The total number of addresses.
    totalAddressCount :: Prelude.Maybe Prelude.Int,
    -- | Any tags for the address pool.
    tags :: Prelude.Maybe [Tag],
    -- | The total number of available addresses.
    totalAvailableAddressCount :: Prelude.Maybe Prelude.Int,
    -- | A description of the address pool.
    description :: Prelude.Maybe Prelude.Text,
    -- | The name of the location from which the address pool is advertised. A
    -- network border group is a unique set of Availability Zones or Local
    -- Zones from where AWS advertises public IP addresses.
    networkBorderGroup :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { poolId = Prelude.Nothing,
      poolAddressRanges = Prelude.Nothing,
      totalAddressCount = Prelude.Nothing,
      tags = Prelude.Nothing,
      totalAvailableAddressCount = Prelude.Nothing,
      description = Prelude.Nothing,
      networkBorderGroup = Prelude.Nothing
    }

-- | The ID of the address pool.
publicIpv4Pool_poolId :: Lens.Lens' PublicIpv4Pool (Prelude.Maybe Prelude.Text)
publicIpv4Pool_poolId = Lens.lens (\PublicIpv4Pool' {poolId} -> poolId) (\s@PublicIpv4Pool' {} a -> s {poolId = a} :: PublicIpv4Pool)

-- | The address ranges.
publicIpv4Pool_poolAddressRanges :: Lens.Lens' PublicIpv4Pool (Prelude.Maybe [PublicIpv4PoolRange])
publicIpv4Pool_poolAddressRanges = Lens.lens (\PublicIpv4Pool' {poolAddressRanges} -> poolAddressRanges) (\s@PublicIpv4Pool' {} a -> s {poolAddressRanges = a} :: PublicIpv4Pool) Prelude.. Lens.mapping Prelude._Coerce

-- | The total number of addresses.
publicIpv4Pool_totalAddressCount :: Lens.Lens' PublicIpv4Pool (Prelude.Maybe Prelude.Int)
publicIpv4Pool_totalAddressCount = Lens.lens (\PublicIpv4Pool' {totalAddressCount} -> totalAddressCount) (\s@PublicIpv4Pool' {} a -> s {totalAddressCount = a} :: PublicIpv4Pool)

-- | Any tags for the address pool.
publicIpv4Pool_tags :: Lens.Lens' PublicIpv4Pool (Prelude.Maybe [Tag])
publicIpv4Pool_tags = Lens.lens (\PublicIpv4Pool' {tags} -> tags) (\s@PublicIpv4Pool' {} a -> s {tags = a} :: PublicIpv4Pool) Prelude.. Lens.mapping Prelude._Coerce

-- | The total number of available addresses.
publicIpv4Pool_totalAvailableAddressCount :: Lens.Lens' PublicIpv4Pool (Prelude.Maybe Prelude.Int)
publicIpv4Pool_totalAvailableAddressCount = Lens.lens (\PublicIpv4Pool' {totalAvailableAddressCount} -> totalAvailableAddressCount) (\s@PublicIpv4Pool' {} a -> s {totalAvailableAddressCount = a} :: PublicIpv4Pool)

-- | A description of the address pool.
publicIpv4Pool_description :: Lens.Lens' PublicIpv4Pool (Prelude.Maybe Prelude.Text)
publicIpv4Pool_description = Lens.lens (\PublicIpv4Pool' {description} -> description) (\s@PublicIpv4Pool' {} a -> s {description = a} :: PublicIpv4Pool)

-- | The name of the location from which the address pool is advertised. A
-- network border group is a unique set of Availability Zones or Local
-- Zones from where AWS advertises public IP addresses.
publicIpv4Pool_networkBorderGroup :: Lens.Lens' PublicIpv4Pool (Prelude.Maybe Prelude.Text)
publicIpv4Pool_networkBorderGroup = Lens.lens (\PublicIpv4Pool' {networkBorderGroup} -> networkBorderGroup) (\s@PublicIpv4Pool' {} a -> s {networkBorderGroup = a} :: PublicIpv4Pool)

instance Prelude.FromXML PublicIpv4Pool where
  parseXML x =
    PublicIpv4Pool'
      Prelude.<$> (x Prelude..@? "poolId")
      Prelude.<*> ( x Prelude..@? "poolAddressRangeSet"
                      Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may (Prelude.parseXMLList "item")
                  )
      Prelude.<*> (x Prelude..@? "totalAddressCount")
      Prelude.<*> ( x Prelude..@? "tagSet" Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may (Prelude.parseXMLList "item")
                  )
      Prelude.<*> (x Prelude..@? "totalAvailableAddressCount")
      Prelude.<*> (x Prelude..@? "description")
      Prelude.<*> (x Prelude..@? "networkBorderGroup")

instance Prelude.Hashable PublicIpv4Pool

instance Prelude.NFData PublicIpv4Pool
