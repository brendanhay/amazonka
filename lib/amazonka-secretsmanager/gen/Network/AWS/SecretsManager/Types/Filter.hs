-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SecretsManager.Types.Filter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SecretsManager.Types.Filter
  ( Filter (..),

    -- * Smart constructor
    mkFilter,

    -- * Lenses
    fValues,
    fKey,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SecretsManager.Types.FilterNameStringType

-- | Allows you to filter your list of secrets.
--
-- /See:/ 'mkFilter' smart constructor.
data Filter = Filter'
  { values ::
      Lude.Maybe (Lude.NonEmpty Lude.Text),
    key :: Lude.Maybe FilterNameStringType
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Filter' with the minimum fields required to make a request.
--
-- * 'key' - Filters your list of secrets by a specific key.
-- * 'values' - Filters your list of secrets by a specific value.
mkFilter ::
  Filter
mkFilter = Filter' {values = Lude.Nothing, key = Lude.Nothing}

-- | Filters your list of secrets by a specific value.
--
-- /Note:/ Consider using 'values' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fValues :: Lens.Lens' Filter (Lude.Maybe (Lude.NonEmpty Lude.Text))
fValues = Lens.lens (values :: Filter -> Lude.Maybe (Lude.NonEmpty Lude.Text)) (\s a -> s {values = a} :: Filter)
{-# DEPRECATED fValues "Use generic-lens or generic-optics with 'values' instead." #-}

-- | Filters your list of secrets by a specific key.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fKey :: Lens.Lens' Filter (Lude.Maybe FilterNameStringType)
fKey = Lens.lens (key :: Filter -> Lude.Maybe FilterNameStringType) (\s a -> s {key = a} :: Filter)
{-# DEPRECATED fKey "Use generic-lens or generic-optics with 'key' instead." #-}

instance Lude.ToJSON Filter where
  toJSON Filter' {..} =
    Lude.object
      ( Lude.catMaybes
          [("Values" Lude..=) Lude.<$> values, ("Key" Lude..=) Lude.<$> key]
      )
