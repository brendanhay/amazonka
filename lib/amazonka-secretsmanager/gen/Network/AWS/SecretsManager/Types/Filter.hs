{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
    fKey,
    fValues,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SecretsManager.Types.FilterNameStringType as Types
import qualified Network.AWS.SecretsManager.Types.FilterValueStringType as Types

-- | Allows you to filter your list of secrets.
--
-- /See:/ 'mkFilter' smart constructor.
data Filter = Filter'
  { -- | Filters your list of secrets by a specific key.
    key :: Core.Maybe Types.FilterNameStringType,
    -- | Filters your list of secrets by a specific value.
    values :: Core.Maybe (Core.NonEmpty Types.FilterValueStringType)
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Filter' value with any optional fields omitted.
mkFilter ::
  Filter
mkFilter = Filter' {key = Core.Nothing, values = Core.Nothing}

-- | Filters your list of secrets by a specific key.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fKey :: Lens.Lens' Filter (Core.Maybe Types.FilterNameStringType)
fKey = Lens.field @"key"
{-# DEPRECATED fKey "Use generic-lens or generic-optics with 'key' instead." #-}

-- | Filters your list of secrets by a specific value.
--
-- /Note:/ Consider using 'values' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fValues :: Lens.Lens' Filter (Core.Maybe (Core.NonEmpty Types.FilterValueStringType))
fValues = Lens.field @"values"
{-# DEPRECATED fValues "Use generic-lens or generic-optics with 'values' instead." #-}

instance Core.FromJSON Filter where
  toJSON Filter {..} =
    Core.object
      ( Core.catMaybes
          [("Key" Core..=) Core.<$> key, ("Values" Core..=) Core.<$> values]
      )
