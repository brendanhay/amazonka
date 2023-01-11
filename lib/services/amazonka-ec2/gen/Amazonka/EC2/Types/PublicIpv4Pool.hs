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
-- Module      : Amazonka.EC2.Types.PublicIpv4Pool
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.PublicIpv4Pool where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.PublicIpv4PoolRange
import Amazonka.EC2.Types.Tag
import qualified Amazonka.Prelude as Prelude

-- | Describes an IPv4 address pool.
--
-- /See:/ 'newPublicIpv4Pool' smart constructor.
data PublicIpv4Pool = PublicIpv4Pool'
  { -- | A description of the address pool.
    description :: Prelude.Maybe Prelude.Text,
    -- | The name of the location from which the address pool is advertised. A
    -- network border group is a unique set of Availability Zones or Local
    -- Zones from where Amazon Web Services advertises public IP addresses.
    networkBorderGroup :: Prelude.Maybe Prelude.Text,
    -- | The address ranges.
    poolAddressRanges :: Prelude.Maybe [PublicIpv4PoolRange],
    -- | The ID of the address pool.
    poolId :: Prelude.Maybe Prelude.Text,
    -- | Any tags for the address pool.
    tags :: Prelude.Maybe [Tag],
    -- | The total number of addresses.
    totalAddressCount :: Prelude.Maybe Prelude.Int,
    -- | The total number of available addresses.
    totalAvailableAddressCount :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PublicIpv4Pool' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'publicIpv4Pool_description' - A description of the address pool.
--
-- 'networkBorderGroup', 'publicIpv4Pool_networkBorderGroup' - The name of the location from which the address pool is advertised. A
-- network border group is a unique set of Availability Zones or Local
-- Zones from where Amazon Web Services advertises public IP addresses.
--
-- 'poolAddressRanges', 'publicIpv4Pool_poolAddressRanges' - The address ranges.
--
-- 'poolId', 'publicIpv4Pool_poolId' - The ID of the address pool.
--
-- 'tags', 'publicIpv4Pool_tags' - Any tags for the address pool.
--
-- 'totalAddressCount', 'publicIpv4Pool_totalAddressCount' - The total number of addresses.
--
-- 'totalAvailableAddressCount', 'publicIpv4Pool_totalAvailableAddressCount' - The total number of available addresses.
newPublicIpv4Pool ::
  PublicIpv4Pool
newPublicIpv4Pool =
  PublicIpv4Pool'
    { description = Prelude.Nothing,
      networkBorderGroup = Prelude.Nothing,
      poolAddressRanges = Prelude.Nothing,
      poolId = Prelude.Nothing,
      tags = Prelude.Nothing,
      totalAddressCount = Prelude.Nothing,
      totalAvailableAddressCount = Prelude.Nothing
    }

-- | A description of the address pool.
publicIpv4Pool_description :: Lens.Lens' PublicIpv4Pool (Prelude.Maybe Prelude.Text)
publicIpv4Pool_description = Lens.lens (\PublicIpv4Pool' {description} -> description) (\s@PublicIpv4Pool' {} a -> s {description = a} :: PublicIpv4Pool)

-- | The name of the location from which the address pool is advertised. A
-- network border group is a unique set of Availability Zones or Local
-- Zones from where Amazon Web Services advertises public IP addresses.
publicIpv4Pool_networkBorderGroup :: Lens.Lens' PublicIpv4Pool (Prelude.Maybe Prelude.Text)
publicIpv4Pool_networkBorderGroup = Lens.lens (\PublicIpv4Pool' {networkBorderGroup} -> networkBorderGroup) (\s@PublicIpv4Pool' {} a -> s {networkBorderGroup = a} :: PublicIpv4Pool)

-- | The address ranges.
publicIpv4Pool_poolAddressRanges :: Lens.Lens' PublicIpv4Pool (Prelude.Maybe [PublicIpv4PoolRange])
publicIpv4Pool_poolAddressRanges = Lens.lens (\PublicIpv4Pool' {poolAddressRanges} -> poolAddressRanges) (\s@PublicIpv4Pool' {} a -> s {poolAddressRanges = a} :: PublicIpv4Pool) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the address pool.
publicIpv4Pool_poolId :: Lens.Lens' PublicIpv4Pool (Prelude.Maybe Prelude.Text)
publicIpv4Pool_poolId = Lens.lens (\PublicIpv4Pool' {poolId} -> poolId) (\s@PublicIpv4Pool' {} a -> s {poolId = a} :: PublicIpv4Pool)

-- | Any tags for the address pool.
publicIpv4Pool_tags :: Lens.Lens' PublicIpv4Pool (Prelude.Maybe [Tag])
publicIpv4Pool_tags = Lens.lens (\PublicIpv4Pool' {tags} -> tags) (\s@PublicIpv4Pool' {} a -> s {tags = a} :: PublicIpv4Pool) Prelude.. Lens.mapping Lens.coerced

-- | The total number of addresses.
publicIpv4Pool_totalAddressCount :: Lens.Lens' PublicIpv4Pool (Prelude.Maybe Prelude.Int)
publicIpv4Pool_totalAddressCount = Lens.lens (\PublicIpv4Pool' {totalAddressCount} -> totalAddressCount) (\s@PublicIpv4Pool' {} a -> s {totalAddressCount = a} :: PublicIpv4Pool)

-- | The total number of available addresses.
publicIpv4Pool_totalAvailableAddressCount :: Lens.Lens' PublicIpv4Pool (Prelude.Maybe Prelude.Int)
publicIpv4Pool_totalAvailableAddressCount = Lens.lens (\PublicIpv4Pool' {totalAvailableAddressCount} -> totalAvailableAddressCount) (\s@PublicIpv4Pool' {} a -> s {totalAvailableAddressCount = a} :: PublicIpv4Pool)

instance Data.FromXML PublicIpv4Pool where
  parseXML x =
    PublicIpv4Pool'
      Prelude.<$> (x Data..@? "description")
      Prelude.<*> (x Data..@? "networkBorderGroup")
      Prelude.<*> ( x Data..@? "poolAddressRangeSet"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "item")
                  )
      Prelude.<*> (x Data..@? "poolId")
      Prelude.<*> ( x Data..@? "tagSet" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "item")
                  )
      Prelude.<*> (x Data..@? "totalAddressCount")
      Prelude.<*> (x Data..@? "totalAvailableAddressCount")

instance Prelude.Hashable PublicIpv4Pool where
  hashWithSalt _salt PublicIpv4Pool' {..} =
    _salt `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` networkBorderGroup
      `Prelude.hashWithSalt` poolAddressRanges
      `Prelude.hashWithSalt` poolId
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` totalAddressCount
      `Prelude.hashWithSalt` totalAvailableAddressCount

instance Prelude.NFData PublicIpv4Pool where
  rnf PublicIpv4Pool' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf networkBorderGroup
      `Prelude.seq` Prelude.rnf poolAddressRanges
      `Prelude.seq` Prelude.rnf poolId
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf totalAddressCount
      `Prelude.seq` Prelude.rnf totalAvailableAddressCount
