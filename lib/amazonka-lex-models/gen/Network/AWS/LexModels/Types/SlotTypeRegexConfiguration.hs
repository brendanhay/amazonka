{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexModels.Types.SlotTypeRegexConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.LexModels.Types.SlotTypeRegexConfiguration
  ( SlotTypeRegexConfiguration (..)
  -- * Smart constructor
  , mkSlotTypeRegexConfiguration
  -- * Lenses
  , strcPattern
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.LexModels.Types.Pattern as Types
import qualified Network.AWS.Prelude as Core

-- | Provides a regular expression used to validate the value of a slot.
--
-- /See:/ 'mkSlotTypeRegexConfiguration' smart constructor.
newtype SlotTypeRegexConfiguration = SlotTypeRegexConfiguration'
  { pattern' :: Types.Pattern
    -- ^ A regular expression used to validate the value of a slot. 
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
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'SlotTypeRegexConfiguration' value with any optional fields omitted.
mkSlotTypeRegexConfiguration
    :: Types.Pattern -- ^ 'pattern\''
    -> SlotTypeRegexConfiguration
mkSlotTypeRegexConfiguration pattern'
  = SlotTypeRegexConfiguration'{pattern'}

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
strcPattern :: Lens.Lens' SlotTypeRegexConfiguration Types.Pattern
strcPattern = Lens.field @"pattern'"
{-# INLINEABLE strcPattern #-}
{-# DEPRECATED pattern' "Use generic-lens or generic-optics with 'pattern'' instead"  #-}

instance Core.FromJSON SlotTypeRegexConfiguration where
        toJSON SlotTypeRegexConfiguration{..}
          = Core.object
              (Core.catMaybes [Core.Just ("pattern" Core..= pattern')])

instance Core.FromJSON SlotTypeRegexConfiguration where
        parseJSON
          = Core.withObject "SlotTypeRegexConfiguration" Core.$
              \ x -> SlotTypeRegexConfiguration' Core.<$> (x Core..: "pattern")
