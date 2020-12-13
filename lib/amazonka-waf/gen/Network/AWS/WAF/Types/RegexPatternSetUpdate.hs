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
import qualified Network.AWS.Prelude as Lude
import Network.AWS.WAF.Types.ChangeAction

-- | In an 'UpdateRegexPatternSet' request, @RegexPatternSetUpdate@ specifies whether to insert or delete a @RegexPatternString@ and includes the settings for the @RegexPatternString@ .
--
-- /See:/ 'mkRegexPatternSetUpdate' smart constructor.
data RegexPatternSetUpdate = RegexPatternSetUpdate'
  { -- | Specifies whether to insert or delete a @RegexPatternString@ .
    action :: ChangeAction,
    -- | Specifies the regular expression (regex) pattern that you want AWS WAF to search for, such as @B[a@]dB[o0]t@ .
    regexPatternString :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RegexPatternSetUpdate' with the minimum fields required to make a request.
--
-- * 'action' - Specifies whether to insert or delete a @RegexPatternString@ .
-- * 'regexPatternString' - Specifies the regular expression (regex) pattern that you want AWS WAF to search for, such as @B[a@]dB[o0]t@ .
mkRegexPatternSetUpdate ::
  -- | 'action'
  ChangeAction ->
  -- | 'regexPatternString'
  Lude.Text ->
  RegexPatternSetUpdate
mkRegexPatternSetUpdate pAction_ pRegexPatternString_ =
  RegexPatternSetUpdate'
    { action = pAction_,
      regexPatternString = pRegexPatternString_
    }

-- | Specifies whether to insert or delete a @RegexPatternString@ .
--
-- /Note:/ Consider using 'action' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rpsuAction :: Lens.Lens' RegexPatternSetUpdate ChangeAction
rpsuAction = Lens.lens (action :: RegexPatternSetUpdate -> ChangeAction) (\s a -> s {action = a} :: RegexPatternSetUpdate)
{-# DEPRECATED rpsuAction "Use generic-lens or generic-optics with 'action' instead." #-}

-- | Specifies the regular expression (regex) pattern that you want AWS WAF to search for, such as @B[a@]dB[o0]t@ .
--
-- /Note:/ Consider using 'regexPatternString' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rpsuRegexPatternString :: Lens.Lens' RegexPatternSetUpdate Lude.Text
rpsuRegexPatternString = Lens.lens (regexPatternString :: RegexPatternSetUpdate -> Lude.Text) (\s a -> s {regexPatternString = a} :: RegexPatternSetUpdate)
{-# DEPRECATED rpsuRegexPatternString "Use generic-lens or generic-optics with 'regexPatternString' instead." #-}

instance Lude.ToJSON RegexPatternSetUpdate where
  toJSON RegexPatternSetUpdate' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("Action" Lude..= action),
            Lude.Just ("RegexPatternString" Lude..= regexPatternString)
          ]
      )
