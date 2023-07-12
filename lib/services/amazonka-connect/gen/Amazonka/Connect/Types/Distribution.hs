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
-- Module      : Amazonka.Connect.Types.Distribution
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Connect.Types.Distribution where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about a traffic distribution.
--
-- /See:/ 'newDistribution' smart constructor.
data Distribution = Distribution'
  { -- | The Amazon Web Services Region where the traffic is distributed.
    region :: Prelude.Text,
    -- | The percentage of the traffic that is distributed, in increments of 10.
    percentage :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Distribution' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'region', 'distribution_region' - The Amazon Web Services Region where the traffic is distributed.
--
-- 'percentage', 'distribution_percentage' - The percentage of the traffic that is distributed, in increments of 10.
newDistribution ::
  -- | 'region'
  Prelude.Text ->
  -- | 'percentage'
  Prelude.Natural ->
  Distribution
newDistribution pRegion_ pPercentage_ =
  Distribution'
    { region = pRegion_,
      percentage = pPercentage_
    }

-- | The Amazon Web Services Region where the traffic is distributed.
distribution_region :: Lens.Lens' Distribution Prelude.Text
distribution_region = Lens.lens (\Distribution' {region} -> region) (\s@Distribution' {} a -> s {region = a} :: Distribution)

-- | The percentage of the traffic that is distributed, in increments of 10.
distribution_percentage :: Lens.Lens' Distribution Prelude.Natural
distribution_percentage = Lens.lens (\Distribution' {percentage} -> percentage) (\s@Distribution' {} a -> s {percentage = a} :: Distribution)

instance Data.FromJSON Distribution where
  parseJSON =
    Data.withObject
      "Distribution"
      ( \x ->
          Distribution'
            Prelude.<$> (x Data..: "Region")
            Prelude.<*> (x Data..: "Percentage")
      )

instance Prelude.Hashable Distribution where
  hashWithSalt _salt Distribution' {..} =
    _salt
      `Prelude.hashWithSalt` region
      `Prelude.hashWithSalt` percentage

instance Prelude.NFData Distribution where
  rnf Distribution' {..} =
    Prelude.rnf region
      `Prelude.seq` Prelude.rnf percentage

instance Data.ToJSON Distribution where
  toJSON Distribution' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Region" Data..= region),
            Prelude.Just ("Percentage" Data..= percentage)
          ]
      )
