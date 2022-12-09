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
-- Module      : Amazonka.ArcZonalShift.Types.ManagedResourceSummary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ArcZonalShift.Types.ManagedResourceSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A complex structure for a managed resource in an account.
--
-- A managed resource is a Network Load Balancer or Application Load
-- Balancer that has been registered with Route 53 ARC by Elastic Load
-- Balancing. You can start a zonal shift in Route 53 ARC for a managed
-- resource to temporarily move traffic for the resource away from an
-- Availability Zone in an AWS Region.
--
-- At this time, you can only start a zonal shift for Network Load
-- Balancers and Application Load Balancers with cross-zone load balancing
-- turned off.
--
-- /See:/ 'newManagedResourceSummary' smart constructor.
data ManagedResourceSummary = ManagedResourceSummary'
  { -- | The Amazon Resource Name (ARN) for the managed resource.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The name of the managed resource.
    name :: Prelude.Maybe Prelude.Text,
    -- | The Availability Zones that a resource is deployed in.
    availabilityZones :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ManagedResourceSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'managedResourceSummary_arn' - The Amazon Resource Name (ARN) for the managed resource.
--
-- 'name', 'managedResourceSummary_name' - The name of the managed resource.
--
-- 'availabilityZones', 'managedResourceSummary_availabilityZones' - The Availability Zones that a resource is deployed in.
newManagedResourceSummary ::
  ManagedResourceSummary
newManagedResourceSummary =
  ManagedResourceSummary'
    { arn = Prelude.Nothing,
      name = Prelude.Nothing,
      availabilityZones = Prelude.mempty
    }

-- | The Amazon Resource Name (ARN) for the managed resource.
managedResourceSummary_arn :: Lens.Lens' ManagedResourceSummary (Prelude.Maybe Prelude.Text)
managedResourceSummary_arn = Lens.lens (\ManagedResourceSummary' {arn} -> arn) (\s@ManagedResourceSummary' {} a -> s {arn = a} :: ManagedResourceSummary)

-- | The name of the managed resource.
managedResourceSummary_name :: Lens.Lens' ManagedResourceSummary (Prelude.Maybe Prelude.Text)
managedResourceSummary_name = Lens.lens (\ManagedResourceSummary' {name} -> name) (\s@ManagedResourceSummary' {} a -> s {name = a} :: ManagedResourceSummary)

-- | The Availability Zones that a resource is deployed in.
managedResourceSummary_availabilityZones :: Lens.Lens' ManagedResourceSummary [Prelude.Text]
managedResourceSummary_availabilityZones = Lens.lens (\ManagedResourceSummary' {availabilityZones} -> availabilityZones) (\s@ManagedResourceSummary' {} a -> s {availabilityZones = a} :: ManagedResourceSummary) Prelude.. Lens.coerced

instance Data.FromJSON ManagedResourceSummary where
  parseJSON =
    Data.withObject
      "ManagedResourceSummary"
      ( \x ->
          ManagedResourceSummary'
            Prelude.<$> (x Data..:? "arn")
            Prelude.<*> (x Data..:? "name")
            Prelude.<*> ( x Data..:? "availabilityZones"
                            Data..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable ManagedResourceSummary where
  hashWithSalt _salt ManagedResourceSummary' {..} =
    _salt `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` availabilityZones

instance Prelude.NFData ManagedResourceSummary where
  rnf ManagedResourceSummary' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf availabilityZones
