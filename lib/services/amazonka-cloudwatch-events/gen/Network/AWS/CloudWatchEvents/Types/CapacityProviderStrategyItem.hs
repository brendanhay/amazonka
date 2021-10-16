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
-- Module      : Network.AWS.CloudWatchEvents.Types.CapacityProviderStrategyItem
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatchEvents.Types.CapacityProviderStrategyItem where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The details of a capacity provider strategy. To learn more, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/APIReference/API_CapacityProviderStrategyItem.html CapacityProviderStrategyItem>
-- in the Amazon ECS API Reference.
--
-- /See:/ 'newCapacityProviderStrategyItem' smart constructor.
data CapacityProviderStrategyItem = CapacityProviderStrategyItem'
  { -- | The weight value designates the relative percentage of the total number
    -- of tasks launched that should use the specified capacity provider. The
    -- weight value is taken into consideration after the base value, if
    -- defined, is satisfied.
    weight :: Prelude.Maybe Prelude.Natural,
    -- | The base value designates how many tasks, at a minimum, to run on the
    -- specified capacity provider. Only one capacity provider in a capacity
    -- provider strategy can have a base defined. If no value is specified, the
    -- default value of 0 is used.
    base :: Prelude.Maybe Prelude.Natural,
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
-- 'weight', 'capacityProviderStrategyItem_weight' - The weight value designates the relative percentage of the total number
-- of tasks launched that should use the specified capacity provider. The
-- weight value is taken into consideration after the base value, if
-- defined, is satisfied.
--
-- 'base', 'capacityProviderStrategyItem_base' - The base value designates how many tasks, at a minimum, to run on the
-- specified capacity provider. Only one capacity provider in a capacity
-- provider strategy can have a base defined. If no value is specified, the
-- default value of 0 is used.
--
-- 'capacityProvider', 'capacityProviderStrategyItem_capacityProvider' - The short name of the capacity provider.
newCapacityProviderStrategyItem ::
  -- | 'capacityProvider'
  Prelude.Text ->
  CapacityProviderStrategyItem
newCapacityProviderStrategyItem pCapacityProvider_ =
  CapacityProviderStrategyItem'
    { weight =
        Prelude.Nothing,
      base = Prelude.Nothing,
      capacityProvider = pCapacityProvider_
    }

-- | The weight value designates the relative percentage of the total number
-- of tasks launched that should use the specified capacity provider. The
-- weight value is taken into consideration after the base value, if
-- defined, is satisfied.
capacityProviderStrategyItem_weight :: Lens.Lens' CapacityProviderStrategyItem (Prelude.Maybe Prelude.Natural)
capacityProviderStrategyItem_weight = Lens.lens (\CapacityProviderStrategyItem' {weight} -> weight) (\s@CapacityProviderStrategyItem' {} a -> s {weight = a} :: CapacityProviderStrategyItem)

-- | The base value designates how many tasks, at a minimum, to run on the
-- specified capacity provider. Only one capacity provider in a capacity
-- provider strategy can have a base defined. If no value is specified, the
-- default value of 0 is used.
capacityProviderStrategyItem_base :: Lens.Lens' CapacityProviderStrategyItem (Prelude.Maybe Prelude.Natural)
capacityProviderStrategyItem_base = Lens.lens (\CapacityProviderStrategyItem' {base} -> base) (\s@CapacityProviderStrategyItem' {} a -> s {base = a} :: CapacityProviderStrategyItem)

-- | The short name of the capacity provider.
capacityProviderStrategyItem_capacityProvider :: Lens.Lens' CapacityProviderStrategyItem Prelude.Text
capacityProviderStrategyItem_capacityProvider = Lens.lens (\CapacityProviderStrategyItem' {capacityProvider} -> capacityProvider) (\s@CapacityProviderStrategyItem' {} a -> s {capacityProvider = a} :: CapacityProviderStrategyItem)

instance Core.FromJSON CapacityProviderStrategyItem where
  parseJSON =
    Core.withObject
      "CapacityProviderStrategyItem"
      ( \x ->
          CapacityProviderStrategyItem'
            Prelude.<$> (x Core..:? "weight")
            Prelude.<*> (x Core..:? "base")
            Prelude.<*> (x Core..: "capacityProvider")
      )

instance
  Prelude.Hashable
    CapacityProviderStrategyItem

instance Prelude.NFData CapacityProviderStrategyItem

instance Core.ToJSON CapacityProviderStrategyItem where
  toJSON CapacityProviderStrategyItem' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("weight" Core..=) Prelude.<$> weight,
            ("base" Core..=) Prelude.<$> base,
            Prelude.Just
              ("capacityProvider" Core..= capacityProvider)
          ]
      )
