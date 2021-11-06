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
-- Module      : Amazonka.FMS.Types.ExpectedRoute
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FMS.Types.ExpectedRoute where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Information about the expected route in the route table.
--
-- /See:/ 'newExpectedRoute' smart constructor.
data ExpectedRoute = ExpectedRoute'
  { -- | Information about the IPv4 CIDR block.
    ipV4Cidr :: Prelude.Maybe Prelude.Text,
    -- | Information about the route table ID.
    routeTableId :: Prelude.Maybe Prelude.Text,
    -- | Information about the allowed targets.
    allowedTargets :: Prelude.Maybe [Prelude.Text],
    -- | Information about the ID of the prefix list for the route.
    prefixListId :: Prelude.Maybe Prelude.Text,
    -- | Information about the IPv6 CIDR block.
    ipV6Cidr :: Prelude.Maybe Prelude.Text,
    -- | Information about the contributing subnets.
    contributingSubnets :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ExpectedRoute' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ipV4Cidr', 'expectedRoute_ipV4Cidr' - Information about the IPv4 CIDR block.
--
-- 'routeTableId', 'expectedRoute_routeTableId' - Information about the route table ID.
--
-- 'allowedTargets', 'expectedRoute_allowedTargets' - Information about the allowed targets.
--
-- 'prefixListId', 'expectedRoute_prefixListId' - Information about the ID of the prefix list for the route.
--
-- 'ipV6Cidr', 'expectedRoute_ipV6Cidr' - Information about the IPv6 CIDR block.
--
-- 'contributingSubnets', 'expectedRoute_contributingSubnets' - Information about the contributing subnets.
newExpectedRoute ::
  ExpectedRoute
newExpectedRoute =
  ExpectedRoute'
    { ipV4Cidr = Prelude.Nothing,
      routeTableId = Prelude.Nothing,
      allowedTargets = Prelude.Nothing,
      prefixListId = Prelude.Nothing,
      ipV6Cidr = Prelude.Nothing,
      contributingSubnets = Prelude.Nothing
    }

-- | Information about the IPv4 CIDR block.
expectedRoute_ipV4Cidr :: Lens.Lens' ExpectedRoute (Prelude.Maybe Prelude.Text)
expectedRoute_ipV4Cidr = Lens.lens (\ExpectedRoute' {ipV4Cidr} -> ipV4Cidr) (\s@ExpectedRoute' {} a -> s {ipV4Cidr = a} :: ExpectedRoute)

-- | Information about the route table ID.
expectedRoute_routeTableId :: Lens.Lens' ExpectedRoute (Prelude.Maybe Prelude.Text)
expectedRoute_routeTableId = Lens.lens (\ExpectedRoute' {routeTableId} -> routeTableId) (\s@ExpectedRoute' {} a -> s {routeTableId = a} :: ExpectedRoute)

-- | Information about the allowed targets.
expectedRoute_allowedTargets :: Lens.Lens' ExpectedRoute (Prelude.Maybe [Prelude.Text])
expectedRoute_allowedTargets = Lens.lens (\ExpectedRoute' {allowedTargets} -> allowedTargets) (\s@ExpectedRoute' {} a -> s {allowedTargets = a} :: ExpectedRoute) Prelude.. Lens.mapping Lens.coerced

-- | Information about the ID of the prefix list for the route.
expectedRoute_prefixListId :: Lens.Lens' ExpectedRoute (Prelude.Maybe Prelude.Text)
expectedRoute_prefixListId = Lens.lens (\ExpectedRoute' {prefixListId} -> prefixListId) (\s@ExpectedRoute' {} a -> s {prefixListId = a} :: ExpectedRoute)

-- | Information about the IPv6 CIDR block.
expectedRoute_ipV6Cidr :: Lens.Lens' ExpectedRoute (Prelude.Maybe Prelude.Text)
expectedRoute_ipV6Cidr = Lens.lens (\ExpectedRoute' {ipV6Cidr} -> ipV6Cidr) (\s@ExpectedRoute' {} a -> s {ipV6Cidr = a} :: ExpectedRoute)

-- | Information about the contributing subnets.
expectedRoute_contributingSubnets :: Lens.Lens' ExpectedRoute (Prelude.Maybe [Prelude.Text])
expectedRoute_contributingSubnets = Lens.lens (\ExpectedRoute' {contributingSubnets} -> contributingSubnets) (\s@ExpectedRoute' {} a -> s {contributingSubnets = a} :: ExpectedRoute) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON ExpectedRoute where
  parseJSON =
    Core.withObject
      "ExpectedRoute"
      ( \x ->
          ExpectedRoute'
            Prelude.<$> (x Core..:? "IpV4Cidr")
            Prelude.<*> (x Core..:? "RouteTableId")
            Prelude.<*> (x Core..:? "AllowedTargets" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "PrefixListId")
            Prelude.<*> (x Core..:? "IpV6Cidr")
            Prelude.<*> ( x Core..:? "ContributingSubnets"
                            Core..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable ExpectedRoute

instance Prelude.NFData ExpectedRoute
