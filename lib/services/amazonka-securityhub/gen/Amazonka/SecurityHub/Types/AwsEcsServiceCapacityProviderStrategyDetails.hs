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
-- Module      : Amazonka.SecurityHub.Types.AwsEcsServiceCapacityProviderStrategyDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsEcsServiceCapacityProviderStrategyDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Strategy item for the capacity provider strategy that the service uses.
--
-- /See:/ 'newAwsEcsServiceCapacityProviderStrategyDetails' smart constructor.
data AwsEcsServiceCapacityProviderStrategyDetails = AwsEcsServiceCapacityProviderStrategyDetails'
  { -- | The minimum number of tasks to run on the capacity provider. Only one
    -- strategy item can specify a value for @Base@.
    --
    -- The value must be between 0 and 100000.
    base :: Prelude.Maybe Prelude.Int,
    -- | The short name of the capacity provider.
    capacityProvider :: Prelude.Maybe Prelude.Text,
    -- | The relative percentage of the total number of tasks that should use the
    -- capacity provider.
    --
    -- If no weight is specified, the default value is 0. At least one capacity
    -- provider must have a weight greater than 0.
    --
    -- The value can be between 0 and 1000.
    weight :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsEcsServiceCapacityProviderStrategyDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'base', 'awsEcsServiceCapacityProviderStrategyDetails_base' - The minimum number of tasks to run on the capacity provider. Only one
-- strategy item can specify a value for @Base@.
--
-- The value must be between 0 and 100000.
--
-- 'capacityProvider', 'awsEcsServiceCapacityProviderStrategyDetails_capacityProvider' - The short name of the capacity provider.
--
-- 'weight', 'awsEcsServiceCapacityProviderStrategyDetails_weight' - The relative percentage of the total number of tasks that should use the
-- capacity provider.
--
-- If no weight is specified, the default value is 0. At least one capacity
-- provider must have a weight greater than 0.
--
-- The value can be between 0 and 1000.
newAwsEcsServiceCapacityProviderStrategyDetails ::
  AwsEcsServiceCapacityProviderStrategyDetails
newAwsEcsServiceCapacityProviderStrategyDetails =
  AwsEcsServiceCapacityProviderStrategyDetails'
    { base =
        Prelude.Nothing,
      capacityProvider =
        Prelude.Nothing,
      weight = Prelude.Nothing
    }

-- | The minimum number of tasks to run on the capacity provider. Only one
-- strategy item can specify a value for @Base@.
--
-- The value must be between 0 and 100000.
awsEcsServiceCapacityProviderStrategyDetails_base :: Lens.Lens' AwsEcsServiceCapacityProviderStrategyDetails (Prelude.Maybe Prelude.Int)
awsEcsServiceCapacityProviderStrategyDetails_base = Lens.lens (\AwsEcsServiceCapacityProviderStrategyDetails' {base} -> base) (\s@AwsEcsServiceCapacityProviderStrategyDetails' {} a -> s {base = a} :: AwsEcsServiceCapacityProviderStrategyDetails)

-- | The short name of the capacity provider.
awsEcsServiceCapacityProviderStrategyDetails_capacityProvider :: Lens.Lens' AwsEcsServiceCapacityProviderStrategyDetails (Prelude.Maybe Prelude.Text)
awsEcsServiceCapacityProviderStrategyDetails_capacityProvider = Lens.lens (\AwsEcsServiceCapacityProviderStrategyDetails' {capacityProvider} -> capacityProvider) (\s@AwsEcsServiceCapacityProviderStrategyDetails' {} a -> s {capacityProvider = a} :: AwsEcsServiceCapacityProviderStrategyDetails)

-- | The relative percentage of the total number of tasks that should use the
-- capacity provider.
--
-- If no weight is specified, the default value is 0. At least one capacity
-- provider must have a weight greater than 0.
--
-- The value can be between 0 and 1000.
awsEcsServiceCapacityProviderStrategyDetails_weight :: Lens.Lens' AwsEcsServiceCapacityProviderStrategyDetails (Prelude.Maybe Prelude.Int)
awsEcsServiceCapacityProviderStrategyDetails_weight = Lens.lens (\AwsEcsServiceCapacityProviderStrategyDetails' {weight} -> weight) (\s@AwsEcsServiceCapacityProviderStrategyDetails' {} a -> s {weight = a} :: AwsEcsServiceCapacityProviderStrategyDetails)

instance
  Data.FromJSON
    AwsEcsServiceCapacityProviderStrategyDetails
  where
  parseJSON =
    Data.withObject
      "AwsEcsServiceCapacityProviderStrategyDetails"
      ( \x ->
          AwsEcsServiceCapacityProviderStrategyDetails'
            Prelude.<$> (x Data..:? "Base")
            Prelude.<*> (x Data..:? "CapacityProvider")
            Prelude.<*> (x Data..:? "Weight")
      )

instance
  Prelude.Hashable
    AwsEcsServiceCapacityProviderStrategyDetails
  where
  hashWithSalt
    _salt
    AwsEcsServiceCapacityProviderStrategyDetails' {..} =
      _salt
        `Prelude.hashWithSalt` base
        `Prelude.hashWithSalt` capacityProvider
        `Prelude.hashWithSalt` weight

instance
  Prelude.NFData
    AwsEcsServiceCapacityProviderStrategyDetails
  where
  rnf AwsEcsServiceCapacityProviderStrategyDetails' {..} =
    Prelude.rnf base
      `Prelude.seq` Prelude.rnf capacityProvider
      `Prelude.seq` Prelude.rnf weight

instance
  Data.ToJSON
    AwsEcsServiceCapacityProviderStrategyDetails
  where
  toJSON
    AwsEcsServiceCapacityProviderStrategyDetails' {..} =
      Data.object
        ( Prelude.catMaybes
            [ ("Base" Data..=) Prelude.<$> base,
              ("CapacityProvider" Data..=)
                Prelude.<$> capacityProvider,
              ("Weight" Data..=) Prelude.<$> weight
            ]
        )
