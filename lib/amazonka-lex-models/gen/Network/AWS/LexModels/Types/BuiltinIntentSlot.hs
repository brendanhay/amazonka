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
import qualified Network.AWS.Prelude as Lude

-- | Provides information about a slot used in a built-in intent.
--
-- /See:/ 'mkBuiltinIntentSlot' smart constructor.
newtype BuiltinIntentSlot = BuiltinIntentSlot'
  { name ::
      Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BuiltinIntentSlot' with the minimum fields required to make a request.
--
-- * 'name' - A list of the slots defined for the intent.
mkBuiltinIntentSlot ::
  BuiltinIntentSlot
mkBuiltinIntentSlot = BuiltinIntentSlot' {name = Lude.Nothing}

-- | A list of the slots defined for the intent.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bisName :: Lens.Lens' BuiltinIntentSlot (Lude.Maybe Lude.Text)
bisName = Lens.lens (name :: BuiltinIntentSlot -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: BuiltinIntentSlot)
{-# DEPRECATED bisName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.FromJSON BuiltinIntentSlot where
  parseJSON =
    Lude.withObject
      "BuiltinIntentSlot"
      (\x -> BuiltinIntentSlot' Lude.<$> (x Lude..:? "name"))
