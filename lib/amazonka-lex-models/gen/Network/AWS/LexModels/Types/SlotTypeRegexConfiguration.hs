-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexModels.Types.SlotTypeRegexConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.LexModels.Types.SlotTypeRegexConfiguration
  ( SlotTypeRegexConfiguration (..),

    -- * Smart constructor
    mkSlotTypeRegexConfiguration,

    -- * Lenses
    strcPattern,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Provides a regular expression used to validate the value of a slot.
--
-- /See:/ 'mkSlotTypeRegexConfiguration' smart constructor.
newtype SlotTypeRegexConfiguration = SlotTypeRegexConfiguration'
  { pattern' ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SlotTypeRegexConfiguration' with the minimum fields required to make a request.
--
-- * 'pattern'' - A regular expression used to validate the value of a slot.
--
-- Use a standard regular expression. Amazon Lex supports the following characters in the regular expression:
--
--     * A-Z, a-z
--
--
--     * 0-9
--
--
--     * Unicode characters ("\ u<Unicode>")
--
--
-- Represent Unicode characters with four digits, for example "\u0041" or "\u005A".
-- The following regular expression operators are not supported:
--
--     * Infinite repeaters: *, +, or {x,} with no upper bound.
--
--
--     * Wild card (.)
mkSlotTypeRegexConfiguration ::
  -- | 'pattern''
  Lude.Text ->
  SlotTypeRegexConfiguration
mkSlotTypeRegexConfiguration pPattern_ =
  SlotTypeRegexConfiguration' {pattern' = pPattern_}

-- | A regular expression used to validate the value of a slot.
--
-- Use a standard regular expression. Amazon Lex supports the following characters in the regular expression:
--
--     * A-Z, a-z
--
--
--     * 0-9
--
--
--     * Unicode characters ("\ u<Unicode>")
--
--
-- Represent Unicode characters with four digits, for example "\u0041" or "\u005A".
-- The following regular expression operators are not supported:
--
--     * Infinite repeaters: *, +, or {x,} with no upper bound.
--
--
--     * Wild card (.)
--
--
--
-- /Note:/ Consider using 'pattern'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
strcPattern :: Lens.Lens' SlotTypeRegexConfiguration Lude.Text
strcPattern = Lens.lens (pattern' :: SlotTypeRegexConfiguration -> Lude.Text) (\s a -> s {pattern' = a} :: SlotTypeRegexConfiguration)
{-# DEPRECATED strcPattern "Use generic-lens or generic-optics with 'pattern'' instead." #-}

instance Lude.FromJSON SlotTypeRegexConfiguration where
  parseJSON =
    Lude.withObject
      "SlotTypeRegexConfiguration"
      (\x -> SlotTypeRegexConfiguration' Lude.<$> (x Lude..: "pattern"))

instance Lude.ToJSON SlotTypeRegexConfiguration where
  toJSON SlotTypeRegexConfiguration' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("pattern" Lude..= pattern')])
