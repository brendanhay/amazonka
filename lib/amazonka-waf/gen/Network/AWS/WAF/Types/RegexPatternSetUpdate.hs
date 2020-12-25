{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAF.Types.RegexPatternSetUpdate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WAF.Types.RegexPatternSetUpdate
  ( RegexPatternSetUpdate (..),

    -- * Smart constructor
    mkRegexPatternSetUpdate,

    -- * Lenses
    rpsuAction,
    rpsuRegexPatternString,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.WAF.Types.ChangeAction as Types
import qualified Network.AWS.WAF.Types.RegexPatternString as Types

-- | In an 'UpdateRegexPatternSet' request, @RegexPatternSetUpdate@ specifies whether to insert or delete a @RegexPatternString@ and includes the settings for the @RegexPatternString@ .
--
-- /See:/ 'mkRegexPatternSetUpdate' smart constructor.
data RegexPatternSetUpdate = RegexPatternSetUpdate'
  { -- | Specifies whether to insert or delete a @RegexPatternString@ .
    action :: Types.ChangeAction,
    -- | Specifies the regular expression (regex) pattern that you want AWS WAF to search for, such as @B[a@]dB[o0]t@ .
    regexPatternString :: Types.RegexPatternString
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RegexPatternSetUpdate' value with any optional fields omitted.
mkRegexPatternSetUpdate ::
  -- | 'action'
  Types.ChangeAction ->
  -- | 'regexPatternString'
  Types.RegexPatternString ->
  RegexPatternSetUpdate
mkRegexPatternSetUpdate action regexPatternString =
  RegexPatternSetUpdate' {action, regexPatternString}

-- | Specifies whether to insert or delete a @RegexPatternString@ .
--
-- /Note:/ Consider using 'action' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rpsuAction :: Lens.Lens' RegexPatternSetUpdate Types.ChangeAction
rpsuAction = Lens.field @"action"
{-# DEPRECATED rpsuAction "Use generic-lens or generic-optics with 'action' instead." #-}

-- | Specifies the regular expression (regex) pattern that you want AWS WAF to search for, such as @B[a@]dB[o0]t@ .
--
-- /Note:/ Consider using 'regexPatternString' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rpsuRegexPatternString :: Lens.Lens' RegexPatternSetUpdate Types.RegexPatternString
rpsuRegexPatternString = Lens.field @"regexPatternString"
{-# DEPRECATED rpsuRegexPatternString "Use generic-lens or generic-optics with 'regexPatternString' instead." #-}

instance Core.FromJSON RegexPatternSetUpdate where
  toJSON RegexPatternSetUpdate {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Action" Core..= action),
            Core.Just ("RegexPatternString" Core..= regexPatternString)
          ]
      )
