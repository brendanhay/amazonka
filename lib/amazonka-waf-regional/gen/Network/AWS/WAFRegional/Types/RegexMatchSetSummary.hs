{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAFRegional.Types.RegexMatchSetSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WAFRegional.Types.RegexMatchSetSummary
  ( RegexMatchSetSummary (..),

    -- * Smart constructor
    mkRegexMatchSetSummary,

    -- * Lenses
    rmssName,
    rmssRegexMatchSetId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Returned by 'ListRegexMatchSets' . Each @RegexMatchSetSummary@ object includes the @Name@ and @RegexMatchSetId@ for one 'RegexMatchSet' .
--
-- /See:/ 'mkRegexMatchSetSummary' smart constructor.
data RegexMatchSetSummary = RegexMatchSetSummary'
  { -- | A friendly name or description of the 'RegexMatchSet' . You can't change @Name@ after you create a @RegexMatchSet@ .
    name :: Lude.Text,
    -- | The @RegexMatchSetId@ for a @RegexMatchSet@ . You use @RegexMatchSetId@ to get information about a @RegexMatchSet@ , update a @RegexMatchSet@ , remove a @RegexMatchSet@ from a @Rule@ , and delete a @RegexMatchSet@ from AWS WAF.
    --
    -- @RegexMatchSetId@ is returned by 'CreateRegexMatchSet' and by 'ListRegexMatchSets' .
    regexMatchSetId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RegexMatchSetSummary' with the minimum fields required to make a request.
--
-- * 'name' - A friendly name or description of the 'RegexMatchSet' . You can't change @Name@ after you create a @RegexMatchSet@ .
-- * 'regexMatchSetId' - The @RegexMatchSetId@ for a @RegexMatchSet@ . You use @RegexMatchSetId@ to get information about a @RegexMatchSet@ , update a @RegexMatchSet@ , remove a @RegexMatchSet@ from a @Rule@ , and delete a @RegexMatchSet@ from AWS WAF.
--
-- @RegexMatchSetId@ is returned by 'CreateRegexMatchSet' and by 'ListRegexMatchSets' .
mkRegexMatchSetSummary ::
  -- | 'name'
  Lude.Text ->
  -- | 'regexMatchSetId'
  Lude.Text ->
  RegexMatchSetSummary
mkRegexMatchSetSummary pName_ pRegexMatchSetId_ =
  RegexMatchSetSummary'
    { name = pName_,
      regexMatchSetId = pRegexMatchSetId_
    }

-- | A friendly name or description of the 'RegexMatchSet' . You can't change @Name@ after you create a @RegexMatchSet@ .
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rmssName :: Lens.Lens' RegexMatchSetSummary Lude.Text
rmssName = Lens.lens (name :: RegexMatchSetSummary -> Lude.Text) (\s a -> s {name = a} :: RegexMatchSetSummary)
{-# DEPRECATED rmssName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The @RegexMatchSetId@ for a @RegexMatchSet@ . You use @RegexMatchSetId@ to get information about a @RegexMatchSet@ , update a @RegexMatchSet@ , remove a @RegexMatchSet@ from a @Rule@ , and delete a @RegexMatchSet@ from AWS WAF.
--
-- @RegexMatchSetId@ is returned by 'CreateRegexMatchSet' and by 'ListRegexMatchSets' .
--
-- /Note:/ Consider using 'regexMatchSetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rmssRegexMatchSetId :: Lens.Lens' RegexMatchSetSummary Lude.Text
rmssRegexMatchSetId = Lens.lens (regexMatchSetId :: RegexMatchSetSummary -> Lude.Text) (\s a -> s {regexMatchSetId = a} :: RegexMatchSetSummary)
{-# DEPRECATED rmssRegexMatchSetId "Use generic-lens or generic-optics with 'regexMatchSetId' instead." #-}

instance Lude.FromJSON RegexMatchSetSummary where
  parseJSON =
    Lude.withObject
      "RegexMatchSetSummary"
      ( \x ->
          RegexMatchSetSummary'
            Lude.<$> (x Lude..: "Name") Lude.<*> (x Lude..: "RegexMatchSetId")
      )
