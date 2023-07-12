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
-- Module      : Amazonka.ECS.Types.CapacityProviderStrategyItem
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ECS.Types.CapacityProviderStrategyItem where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The details of a capacity provider strategy. A capacity provider
-- strategy can be set when using the RunTask or CreateCluster APIs or as
-- the default capacity provider strategy for a cluster with the
-- CreateCluster API.
--
-- Only capacity providers that are already associated with a cluster and
-- have an @ACTIVE@ or @UPDATING@ status can be used in a capacity provider
-- strategy. The PutClusterCapacityProviders API is used to associate a
-- capacity provider with a cluster.
--
-- If specifying a capacity provider that uses an Auto Scaling group, the
-- capacity provider must already be created. New Auto Scaling group
-- capacity providers can be created with the CreateCapacityProvider API
-- operation.
--
-- To use a Fargate capacity provider, specify either the @FARGATE@ or
-- @FARGATE_SPOT@ capacity providers. The Fargate capacity providers are
-- available to all accounts and only need to be associated with a cluster
-- to be used in a capacity provider strategy.
--
-- A capacity provider strategy may contain a maximum of 6 capacity
-- providers.
--
-- /See:/ 'newCapacityProviderStrategyItem' smart constructor.
data CapacityProviderStrategyItem = CapacityProviderStrategyItem'
  { -- | The /base/ value designates how many tasks, at a minimum, to run on the
    -- specified capacity provider. Only one capacity provider in a capacity
    -- provider strategy can have a /base/ defined. If no value is specified,
    -- the default value of @0@ is used.
    base :: Prelude.Maybe Prelude.Natural,
    -- | The /weight/ value designates the relative percentage of the total
    -- number of tasks launched that should use the specified capacity
    -- provider. The @weight@ value is taken into consideration after the
    -- @base@ value, if defined, is satisfied.
    --
    -- If no @weight@ value is specified, the default value of @0@ is used.
    -- When multiple capacity providers are specified within a capacity
    -- provider strategy, at least one of the capacity providers must have a
    -- weight value greater than zero and any capacity providers with a weight
    -- of @0@ can\'t be used to place tasks. If you specify multiple capacity
    -- providers in a strategy that all have a weight of @0@, any @RunTask@ or
    -- @CreateService@ actions using the capacity provider strategy will fail.
    --
    -- An example scenario for using weights is defining a strategy that
    -- contains two capacity providers and both have a weight of @1@, then when
    -- the @base@ is satisfied, the tasks will be split evenly across the two
    -- capacity providers. Using that same logic, if you specify a weight of
    -- @1@ for /capacityProviderA/ and a weight of @4@ for /capacityProviderB/,
    -- then for every one task that\'s run using /capacityProviderA/, four
    -- tasks would use /capacityProviderB/.
    weight :: Prelude.Maybe Prelude.Natural,
    -- | The short name of the capacity provider.
    capacityProvider :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CapacityProviderStrategyItem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'base', 'capacityProviderStrategyItem_base' - The /base/ value designates how many tasks, at a minimum, to run on the
-- specified capacity provider. Only one capacity provider in a capacity
-- provider strategy can have a /base/ defined. If no value is specified,
-- the default value of @0@ is used.
--
-- 'weight', 'capacityProviderStrategyItem_weight' - The /weight/ value designates the relative percentage of the total
-- number of tasks launched that should use the specified capacity
-- provider. The @weight@ value is taken into consideration after the
-- @base@ value, if defined, is satisfied.
--
-- If no @weight@ value is specified, the default value of @0@ is used.
-- When multiple capacity providers are specified within a capacity
-- provider strategy, at least one of the capacity providers must have a
-- weight value greater than zero and any capacity providers with a weight
-- of @0@ can\'t be used to place tasks. If you specify multiple capacity
-- providers in a strategy that all have a weight of @0@, any @RunTask@ or
-- @CreateService@ actions using the capacity provider strategy will fail.
--
-- An example scenario for using weights is defining a strategy that
-- contains two capacity providers and both have a weight of @1@, then when
-- the @base@ is satisfied, the tasks will be split evenly across the two
-- capacity providers. Using that same logic, if you specify a weight of
-- @1@ for /capacityProviderA/ and a weight of @4@ for /capacityProviderB/,
-- then for every one task that\'s run using /capacityProviderA/, four
-- tasks would use /capacityProviderB/.
--
-- 'capacityProvider', 'capacityProviderStrategyItem_capacityProvider' - The short name of the capacity provider.
newCapacityProviderStrategyItem ::
  -- | 'capacityProvider'
  Prelude.Text ->
  CapacityProviderStrategyItem
newCapacityProviderStrategyItem pCapacityProvider_ =
  CapacityProviderStrategyItem'
    { base =
        Prelude.Nothing,
      weight = Prelude.Nothing,
      capacityProvider = pCapacityProvider_
    }

-- | The /base/ value designates how many tasks, at a minimum, to run on the
-- specified capacity provider. Only one capacity provider in a capacity
-- provider strategy can have a /base/ defined. If no value is specified,
-- the default value of @0@ is used.
capacityProviderStrategyItem_base :: Lens.Lens' CapacityProviderStrategyItem (Prelude.Maybe Prelude.Natural)
capacityProviderStrategyItem_base = Lens.lens (\CapacityProviderStrategyItem' {base} -> base) (\s@CapacityProviderStrategyItem' {} a -> s {base = a} :: CapacityProviderStrategyItem)

-- | The /weight/ value designates the relative percentage of the total
-- number of tasks launched that should use the specified capacity
-- provider. The @weight@ value is taken into consideration after the
-- @base@ value, if defined, is satisfied.
--
-- If no @weight@ value is specified, the default value of @0@ is used.
-- When multiple capacity providers are specified within a capacity
-- provider strategy, at least one of the capacity providers must have a
-- weight value greater than zero and any capacity providers with a weight
-- of @0@ can\'t be used to place tasks. If you specify multiple capacity
-- providers in a strategy that all have a weight of @0@, any @RunTask@ or
-- @CreateService@ actions using the capacity provider strategy will fail.
--
-- An example scenario for using weights is defining a strategy that
-- contains two capacity providers and both have a weight of @1@, then when
-- the @base@ is satisfied, the tasks will be split evenly across the two
-- capacity providers. Using that same logic, if you specify a weight of
-- @1@ for /capacityProviderA/ and a weight of @4@ for /capacityProviderB/,
-- then for every one task that\'s run using /capacityProviderA/, four
-- tasks would use /capacityProviderB/.
capacityProviderStrategyItem_weight :: Lens.Lens' CapacityProviderStrategyItem (Prelude.Maybe Prelude.Natural)
capacityProviderStrategyItem_weight = Lens.lens (\CapacityProviderStrategyItem' {weight} -> weight) (\s@CapacityProviderStrategyItem' {} a -> s {weight = a} :: CapacityProviderStrategyItem)

-- | The short name of the capacity provider.
capacityProviderStrategyItem_capacityProvider :: Lens.Lens' CapacityProviderStrategyItem Prelude.Text
capacityProviderStrategyItem_capacityProvider = Lens.lens (\CapacityProviderStrategyItem' {capacityProvider} -> capacityProvider) (\s@CapacityProviderStrategyItem' {} a -> s {capacityProvider = a} :: CapacityProviderStrategyItem)

instance Data.FromJSON CapacityProviderStrategyItem where
  parseJSON =
    Data.withObject
      "CapacityProviderStrategyItem"
      ( \x ->
          CapacityProviderStrategyItem'
            Prelude.<$> (x Data..:? "base")
            Prelude.<*> (x Data..:? "weight")
            Prelude.<*> (x Data..: "capacityProvider")
      )

instance
  Prelude.Hashable
    CapacityProviderStrategyItem
  where
  hashWithSalt _salt CapacityProviderStrategyItem' {..} =
    _salt
      `Prelude.hashWithSalt` base
      `Prelude.hashWithSalt` weight
      `Prelude.hashWithSalt` capacityProvider

instance Prelude.NFData CapacityProviderStrategyItem where
  rnf CapacityProviderStrategyItem' {..} =
    Prelude.rnf base
      `Prelude.seq` Prelude.rnf weight
      `Prelude.seq` Prelude.rnf capacityProvider

instance Data.ToJSON CapacityProviderStrategyItem where
  toJSON CapacityProviderStrategyItem' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("base" Data..=) Prelude.<$> base,
            ("weight" Data..=) Prelude.<$> weight,
            Prelude.Just
              ("capacityProvider" Data..= capacityProvider)
          ]
      )
