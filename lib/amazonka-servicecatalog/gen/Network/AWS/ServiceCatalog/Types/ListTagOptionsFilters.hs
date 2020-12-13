{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.Types.ListTagOptionsFilters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServiceCatalog.Types.ListTagOptionsFilters
  ( ListTagOptionsFilters (..),

    -- * Smart constructor
    mkListTagOptionsFilters,

    -- * Lenses
    ltofValue,
    ltofActive,
    ltofKey,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Filters to use when listing TagOptions.
--
-- /See:/ 'mkListTagOptionsFilters' smart constructor.
data ListTagOptionsFilters = ListTagOptionsFilters'
  { -- | The TagOption value.
    value :: Lude.Maybe Lude.Text,
    -- | The active state.
    active :: Lude.Maybe Lude.Bool,
    -- | The TagOption key.
    key :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListTagOptionsFilters' with the minimum fields required to make a request.
--
-- * 'value' - The TagOption value.
-- * 'active' - The active state.
-- * 'key' - The TagOption key.
mkListTagOptionsFilters ::
  ListTagOptionsFilters
mkListTagOptionsFilters =
  ListTagOptionsFilters'
    { value = Lude.Nothing,
      active = Lude.Nothing,
      key = Lude.Nothing
    }

-- | The TagOption value.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltofValue :: Lens.Lens' ListTagOptionsFilters (Lude.Maybe Lude.Text)
ltofValue = Lens.lens (value :: ListTagOptionsFilters -> Lude.Maybe Lude.Text) (\s a -> s {value = a} :: ListTagOptionsFilters)
{-# DEPRECATED ltofValue "Use generic-lens or generic-optics with 'value' instead." #-}

-- | The active state.
--
-- /Note:/ Consider using 'active' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltofActive :: Lens.Lens' ListTagOptionsFilters (Lude.Maybe Lude.Bool)
ltofActive = Lens.lens (active :: ListTagOptionsFilters -> Lude.Maybe Lude.Bool) (\s a -> s {active = a} :: ListTagOptionsFilters)
{-# DEPRECATED ltofActive "Use generic-lens or generic-optics with 'active' instead." #-}

-- | The TagOption key.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltofKey :: Lens.Lens' ListTagOptionsFilters (Lude.Maybe Lude.Text)
ltofKey = Lens.lens (key :: ListTagOptionsFilters -> Lude.Maybe Lude.Text) (\s a -> s {key = a} :: ListTagOptionsFilters)
{-# DEPRECATED ltofKey "Use generic-lens or generic-optics with 'key' instead." #-}

instance Lude.ToJSON ListTagOptionsFilters where
  toJSON ListTagOptionsFilters' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Value" Lude..=) Lude.<$> value,
            ("Active" Lude..=) Lude.<$> active,
            ("Key" Lude..=) Lude.<$> key
          ]
      )
