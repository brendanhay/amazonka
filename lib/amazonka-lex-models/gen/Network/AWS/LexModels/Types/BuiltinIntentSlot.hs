{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexModels.Types.BuiltinIntentSlot
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.LexModels.Types.BuiltinIntentSlot
  ( BuiltinIntentSlot (..),

    -- * Smart constructor
    mkBuiltinIntentSlot,

    -- * Lenses
    bisName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.LexModels.Types.String as Types
import qualified Network.AWS.Prelude as Core

-- | Provides information about a slot used in a built-in intent.
--
-- /See:/ 'mkBuiltinIntentSlot' smart constructor.
newtype BuiltinIntentSlot = BuiltinIntentSlot'
  { -- | A list of the slots defined for the intent.
    name :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'BuiltinIntentSlot' value with any optional fields omitted.
mkBuiltinIntentSlot ::
  BuiltinIntentSlot
mkBuiltinIntentSlot = BuiltinIntentSlot' {name = Core.Nothing}

-- | A list of the slots defined for the intent.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bisName :: Lens.Lens' BuiltinIntentSlot (Core.Maybe Types.String)
bisName = Lens.field @"name"
{-# DEPRECATED bisName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Core.FromJSON BuiltinIntentSlot where
  parseJSON =
    Core.withObject "BuiltinIntentSlot" Core.$
      \x -> BuiltinIntentSlot' Core.<$> (x Core..:? "name")
