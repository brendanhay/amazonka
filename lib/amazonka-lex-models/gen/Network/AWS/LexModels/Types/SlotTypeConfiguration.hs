-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexModels.Types.SlotTypeConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.LexModels.Types.SlotTypeConfiguration
  ( SlotTypeConfiguration (..),

    -- * Smart constructor
    mkSlotTypeConfiguration,

    -- * Lenses
    stcRegexConfiguration,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.LexModels.Types.SlotTypeRegexConfiguration
import qualified Network.AWS.Prelude as Lude

-- | Provides configuration information for a slot type.
--
-- /See:/ 'mkSlotTypeConfiguration' smart constructor.
newtype SlotTypeConfiguration = SlotTypeConfiguration'
  { regexConfiguration ::
      Lude.Maybe SlotTypeRegexConfiguration
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SlotTypeConfiguration' with the minimum fields required to make a request.
--
-- * 'regexConfiguration' - A regular expression used to validate the value of a slot.
mkSlotTypeConfiguration ::
  SlotTypeConfiguration
mkSlotTypeConfiguration =
  SlotTypeConfiguration' {regexConfiguration = Lude.Nothing}

-- | A regular expression used to validate the value of a slot.
--
-- /Note:/ Consider using 'regexConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stcRegexConfiguration :: Lens.Lens' SlotTypeConfiguration (Lude.Maybe SlotTypeRegexConfiguration)
stcRegexConfiguration = Lens.lens (regexConfiguration :: SlotTypeConfiguration -> Lude.Maybe SlotTypeRegexConfiguration) (\s a -> s {regexConfiguration = a} :: SlotTypeConfiguration)
{-# DEPRECATED stcRegexConfiguration "Use generic-lens or generic-optics with 'regexConfiguration' instead." #-}

instance Lude.FromJSON SlotTypeConfiguration where
  parseJSON =
    Lude.withObject
      "SlotTypeConfiguration"
      ( \x ->
          SlotTypeConfiguration' Lude.<$> (x Lude..:? "regexConfiguration")
      )

instance Lude.ToJSON SlotTypeConfiguration where
  toJSON SlotTypeConfiguration' {..} =
    Lude.object
      ( Lude.catMaybes
          [("regexConfiguration" Lude..=) Lude.<$> regexConfiguration]
      )
