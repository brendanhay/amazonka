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
import qualified Network.AWS.Prelude as Lude
import Network.AWS.WAFRegional.Types.ChangeAction
import Network.AWS.WAFRegional.Types.RegexMatchTuple

-- | In an 'UpdateRegexMatchSet' request, @RegexMatchSetUpdate@ specifies whether to insert or delete a 'RegexMatchTuple' and includes the settings for the @RegexMatchTuple@ .
--
-- /See:/ 'mkRegexMatchSetUpdate' smart constructor.
data RegexMatchSetUpdate = RegexMatchSetUpdate'
  { action ::
      ChangeAction,
    regexMatchTuple :: RegexMatchTuple
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RegexMatchSetUpdate' with the minimum fields required to make a request.
--
-- * 'action' - Specifies whether to insert or delete a 'RegexMatchTuple' .
-- * 'regexMatchTuple' - Information about the part of a web request that you want AWS WAF to inspect and the identifier of the regular expression (regex) pattern that you want AWS WAF to search for. If you specify @DELETE@ for the value of @Action@ , the @RegexMatchTuple@ values must exactly match the values in the @RegexMatchTuple@ that you want to delete from the @RegexMatchSet@ .
mkRegexMatchSetUpdate ::
  -- | 'action'
  ChangeAction ->
  -- | 'regexMatchTuple'
  RegexMatchTuple ->
  RegexMatchSetUpdate
mkRegexMatchSetUpdate pAction_ pRegexMatchTuple_ =
  RegexMatchSetUpdate'
    { action = pAction_,
      regexMatchTuple = pRegexMatchTuple_
    }

-- | Specifies whether to insert or delete a 'RegexMatchTuple' .
--
-- /Note:/ Consider using 'action' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rmsuAction :: Lens.Lens' RegexMatchSetUpdate ChangeAction
rmsuAction = Lens.lens (action :: RegexMatchSetUpdate -> ChangeAction) (\s a -> s {action = a} :: RegexMatchSetUpdate)
{-# DEPRECATED rmsuAction "Use generic-lens or generic-optics with 'action' instead." #-}

-- | Information about the part of a web request that you want AWS WAF to inspect and the identifier of the regular expression (regex) pattern that you want AWS WAF to search for. If you specify @DELETE@ for the value of @Action@ , the @RegexMatchTuple@ values must exactly match the values in the @RegexMatchTuple@ that you want to delete from the @RegexMatchSet@ .
--
-- /Note:/ Consider using 'regexMatchTuple' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rmsuRegexMatchTuple :: Lens.Lens' RegexMatchSetUpdate RegexMatchTuple
rmsuRegexMatchTuple = Lens.lens (regexMatchTuple :: RegexMatchSetUpdate -> RegexMatchTuple) (\s a -> s {regexMatchTuple = a} :: RegexMatchSetUpdate)
{-# DEPRECATED rmsuRegexMatchTuple "Use generic-lens or generic-optics with 'regexMatchTuple' instead." #-}

instance Lude.ToJSON RegexMatchSetUpdate where
  toJSON RegexMatchSetUpdate' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("Action" Lude..= action),
            Lude.Just ("RegexMatchTuple" Lude..= regexMatchTuple)
          ]
      )
