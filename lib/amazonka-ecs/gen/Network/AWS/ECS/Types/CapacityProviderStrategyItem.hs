{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.CapacityProviderStrategyItem
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.CapacityProviderStrategyItem
  ( CapacityProviderStrategyItem (..),

    -- * Smart constructor
    mkCapacityProviderStrategyItem,

    -- * Lenses
    cpsiCapacityProvider,
    cpsiBase,
    cpsiWeight,
  )
where

import qualified Network.AWS.ECS.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The details of a capacity provider strategy.
--
-- /See:/ 'mkCapacityProviderStrategyItem' smart constructor.
data CapacityProviderStrategyItem = CapacityProviderStrategyItem'
  { -- | The short name of the capacity provider.
    capacityProvider :: Types.String,
    -- | The /base/ value designates how many tasks, at a minimum, to run on the specified capacity provider. Only one capacity provider in a capacity provider strategy can have a /base/ defined.
    base :: Core.Maybe Core.Natural,
    -- | The /weight/ value designates the relative percentage of the total number of tasks launched that should use the specified capacity provider.
    --
    -- For example, if you have a strategy that contains two capacity providers and both have a weight of @1@ , then when the @base@ is satisfied, the tasks will be split evenly across the two capacity providers. Using that same logic, if you specify a weight of @1@ for /capacityProviderA/ and a weight of @4@ for /capacityProviderB/ , then for every one task that is run using /capacityProviderA/ , four tasks would use /capacityProviderB/ .
    weight :: Core.Maybe Core.Natural
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CapacityProviderStrategyItem' value with any optional fields omitted.
mkCapacityProviderStrategyItem ::
  -- | 'capacityProvider'
  Types.String ->
  CapacityProviderStrategyItem
mkCapacityProviderStrategyItem capacityProvider =
  CapacityProviderStrategyItem'
    { capacityProvider,
      base = Core.Nothing,
      weight = Core.Nothing
    }

-- | The short name of the capacity provider.
--
-- /Note:/ Consider using 'capacityProvider' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpsiCapacityProvider :: Lens.Lens' CapacityProviderStrategyItem Types.String
cpsiCapacityProvider = Lens.field @"capacityProvider"
{-# DEPRECATED cpsiCapacityProvider "Use generic-lens or generic-optics with 'capacityProvider' instead." #-}

-- | The /base/ value designates how many tasks, at a minimum, to run on the specified capacity provider. Only one capacity provider in a capacity provider strategy can have a /base/ defined.
--
-- /Note:/ Consider using 'base' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpsiBase :: Lens.Lens' CapacityProviderStrategyItem (Core.Maybe Core.Natural)
cpsiBase = Lens.field @"base"
{-# DEPRECATED cpsiBase "Use generic-lens or generic-optics with 'base' instead." #-}

-- | The /weight/ value designates the relative percentage of the total number of tasks launched that should use the specified capacity provider.
--
-- For example, if you have a strategy that contains two capacity providers and both have a weight of @1@ , then when the @base@ is satisfied, the tasks will be split evenly across the two capacity providers. Using that same logic, if you specify a weight of @1@ for /capacityProviderA/ and a weight of @4@ for /capacityProviderB/ , then for every one task that is run using /capacityProviderA/ , four tasks would use /capacityProviderB/ .
--
-- /Note:/ Consider using 'weight' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpsiWeight :: Lens.Lens' CapacityProviderStrategyItem (Core.Maybe Core.Natural)
cpsiWeight = Lens.field @"weight"
{-# DEPRECATED cpsiWeight "Use generic-lens or generic-optics with 'weight' instead." #-}

instance Core.FromJSON CapacityProviderStrategyItem where
  toJSON CapacityProviderStrategyItem {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("capacityProvider" Core..= capacityProvider),
            ("base" Core..=) Core.<$> base,
            ("weight" Core..=) Core.<$> weight
          ]
      )

instance Core.FromJSON CapacityProviderStrategyItem where
  parseJSON =
    Core.withObject "CapacityProviderStrategyItem" Core.$
      \x ->
        CapacityProviderStrategyItem'
          Core.<$> (x Core..: "capacityProvider")
          Core.<*> (x Core..:? "base")
          Core.<*> (x Core..:? "weight")
