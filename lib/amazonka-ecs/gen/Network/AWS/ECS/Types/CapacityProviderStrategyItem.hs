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
    cpsiBase,
    cpsiWeight,
    cpsiCapacityProvider,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The details of a capacity provider strategy.
--
-- /See:/ 'mkCapacityProviderStrategyItem' smart constructor.
data CapacityProviderStrategyItem = CapacityProviderStrategyItem'
  { base ::
      Lude.Maybe Lude.Natural,
    weight :: Lude.Maybe Lude.Natural,
    capacityProvider :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CapacityProviderStrategyItem' with the minimum fields required to make a request.
--
-- * 'base' - The /base/ value designates how many tasks, at a minimum, to run on the specified capacity provider. Only one capacity provider in a capacity provider strategy can have a /base/ defined.
-- * 'capacityProvider' - The short name of the capacity provider.
-- * 'weight' - The /weight/ value designates the relative percentage of the total number of tasks launched that should use the specified capacity provider.
--
-- For example, if you have a strategy that contains two capacity providers and both have a weight of @1@ , then when the @base@ is satisfied, the tasks will be split evenly across the two capacity providers. Using that same logic, if you specify a weight of @1@ for /capacityProviderA/ and a weight of @4@ for /capacityProviderB/ , then for every one task that is run using /capacityProviderA/ , four tasks would use /capacityProviderB/ .
mkCapacityProviderStrategyItem ::
  -- | 'capacityProvider'
  Lude.Text ->
  CapacityProviderStrategyItem
mkCapacityProviderStrategyItem pCapacityProvider_ =
  CapacityProviderStrategyItem'
    { base = Lude.Nothing,
      weight = Lude.Nothing,
      capacityProvider = pCapacityProvider_
    }

-- | The /base/ value designates how many tasks, at a minimum, to run on the specified capacity provider. Only one capacity provider in a capacity provider strategy can have a /base/ defined.
--
-- /Note:/ Consider using 'base' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpsiBase :: Lens.Lens' CapacityProviderStrategyItem (Lude.Maybe Lude.Natural)
cpsiBase = Lens.lens (base :: CapacityProviderStrategyItem -> Lude.Maybe Lude.Natural) (\s a -> s {base = a} :: CapacityProviderStrategyItem)
{-# DEPRECATED cpsiBase "Use generic-lens or generic-optics with 'base' instead." #-}

-- | The /weight/ value designates the relative percentage of the total number of tasks launched that should use the specified capacity provider.
--
-- For example, if you have a strategy that contains two capacity providers and both have a weight of @1@ , then when the @base@ is satisfied, the tasks will be split evenly across the two capacity providers. Using that same logic, if you specify a weight of @1@ for /capacityProviderA/ and a weight of @4@ for /capacityProviderB/ , then for every one task that is run using /capacityProviderA/ , four tasks would use /capacityProviderB/ .
--
-- /Note:/ Consider using 'weight' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpsiWeight :: Lens.Lens' CapacityProviderStrategyItem (Lude.Maybe Lude.Natural)
cpsiWeight = Lens.lens (weight :: CapacityProviderStrategyItem -> Lude.Maybe Lude.Natural) (\s a -> s {weight = a} :: CapacityProviderStrategyItem)
{-# DEPRECATED cpsiWeight "Use generic-lens or generic-optics with 'weight' instead." #-}

-- | The short name of the capacity provider.
--
-- /Note:/ Consider using 'capacityProvider' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpsiCapacityProvider :: Lens.Lens' CapacityProviderStrategyItem Lude.Text
cpsiCapacityProvider = Lens.lens (capacityProvider :: CapacityProviderStrategyItem -> Lude.Text) (\s a -> s {capacityProvider = a} :: CapacityProviderStrategyItem)
{-# DEPRECATED cpsiCapacityProvider "Use generic-lens or generic-optics with 'capacityProvider' instead." #-}

instance Lude.FromJSON CapacityProviderStrategyItem where
  parseJSON =
    Lude.withObject
      "CapacityProviderStrategyItem"
      ( \x ->
          CapacityProviderStrategyItem'
            Lude.<$> (x Lude..:? "base")
            Lude.<*> (x Lude..:? "weight")
            Lude.<*> (x Lude..: "capacityProvider")
      )

instance Lude.ToJSON CapacityProviderStrategyItem where
  toJSON CapacityProviderStrategyItem' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("base" Lude..=) Lude.<$> base,
            ("weight" Lude..=) Lude.<$> weight,
            Lude.Just ("capacityProvider" Lude..= capacityProvider)
          ]
      )
