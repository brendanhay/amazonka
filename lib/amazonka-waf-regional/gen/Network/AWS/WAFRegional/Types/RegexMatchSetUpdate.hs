{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAFRegional.Types.RegexMatchSetUpdate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WAFRegional.Types.RegexMatchSetUpdate
  ( RegexMatchSetUpdate (..),

    -- * Smart constructor
    mkRegexMatchSetUpdate,

    -- * Lenses
    rmsuAction,
    rmsuRegexMatchTuple,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.WAFRegional.Types.ChangeAction as Types
import qualified Network.AWS.WAFRegional.Types.RegexMatchTuple as Types

-- | In an 'UpdateRegexMatchSet' request, @RegexMatchSetUpdate@ specifies whether to insert or delete a 'RegexMatchTuple' and includes the settings for the @RegexMatchTuple@ .
--
-- /See:/ 'mkRegexMatchSetUpdate' smart constructor.
data RegexMatchSetUpdate = RegexMatchSetUpdate'
  { -- | Specifies whether to insert or delete a 'RegexMatchTuple' .
    action :: Types.ChangeAction,
    -- | Information about the part of a web request that you want AWS WAF to inspect and the identifier of the regular expression (regex) pattern that you want AWS WAF to search for. If you specify @DELETE@ for the value of @Action@ , the @RegexMatchTuple@ values must exactly match the values in the @RegexMatchTuple@ that you want to delete from the @RegexMatchSet@ .
    regexMatchTuple :: Types.RegexMatchTuple
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RegexMatchSetUpdate' value with any optional fields omitted.
mkRegexMatchSetUpdate ::
  -- | 'action'
  Types.ChangeAction ->
  -- | 'regexMatchTuple'
  Types.RegexMatchTuple ->
  RegexMatchSetUpdate
mkRegexMatchSetUpdate action regexMatchTuple =
  RegexMatchSetUpdate' {action, regexMatchTuple}

-- | Specifies whether to insert or delete a 'RegexMatchTuple' .
--
-- /Note:/ Consider using 'action' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rmsuAction :: Lens.Lens' RegexMatchSetUpdate Types.ChangeAction
rmsuAction = Lens.field @"action"
{-# DEPRECATED rmsuAction "Use generic-lens or generic-optics with 'action' instead." #-}

-- | Information about the part of a web request that you want AWS WAF to inspect and the identifier of the regular expression (regex) pattern that you want AWS WAF to search for. If you specify @DELETE@ for the value of @Action@ , the @RegexMatchTuple@ values must exactly match the values in the @RegexMatchTuple@ that you want to delete from the @RegexMatchSet@ .
--
-- /Note:/ Consider using 'regexMatchTuple' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rmsuRegexMatchTuple :: Lens.Lens' RegexMatchSetUpdate Types.RegexMatchTuple
rmsuRegexMatchTuple = Lens.field @"regexMatchTuple"
{-# DEPRECATED rmsuRegexMatchTuple "Use generic-lens or generic-optics with 'regexMatchTuple' instead." #-}

instance Core.FromJSON RegexMatchSetUpdate where
  toJSON RegexMatchSetUpdate {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Action" Core..= action),
            Core.Just ("RegexMatchTuple" Core..= regexMatchTuple)
          ]
      )
