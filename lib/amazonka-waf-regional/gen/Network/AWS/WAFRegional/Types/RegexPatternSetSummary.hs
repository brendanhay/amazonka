{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAFRegional.Types.RegexPatternSetSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WAFRegional.Types.RegexPatternSetSummary
  ( RegexPatternSetSummary (..),

    -- * Smart constructor
    mkRegexPatternSetSummary,

    -- * Lenses
    rpssName,
    rpssRegexPatternSetId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Returned by 'ListRegexPatternSets' . Each @RegexPatternSetSummary@ object includes the @Name@ and @RegexPatternSetId@ for one 'RegexPatternSet' .
--
-- /See:/ 'mkRegexPatternSetSummary' smart constructor.
data RegexPatternSetSummary = RegexPatternSetSummary'
  { -- | A friendly name or description of the 'RegexPatternSet' . You can't change @Name@ after you create a @RegexPatternSet@ .
    name :: Lude.Text,
    -- | The @RegexPatternSetId@ for a @RegexPatternSet@ . You use @RegexPatternSetId@ to get information about a @RegexPatternSet@ , update a @RegexPatternSet@ , remove a @RegexPatternSet@ from a @RegexMatchSet@ , and delete a @RegexPatternSet@ from AWS WAF.
    --
    -- @RegexPatternSetId@ is returned by 'CreateRegexPatternSet' and by 'ListRegexPatternSets' .
    regexPatternSetId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RegexPatternSetSummary' with the minimum fields required to make a request.
--
-- * 'name' - A friendly name or description of the 'RegexPatternSet' . You can't change @Name@ after you create a @RegexPatternSet@ .
-- * 'regexPatternSetId' - The @RegexPatternSetId@ for a @RegexPatternSet@ . You use @RegexPatternSetId@ to get information about a @RegexPatternSet@ , update a @RegexPatternSet@ , remove a @RegexPatternSet@ from a @RegexMatchSet@ , and delete a @RegexPatternSet@ from AWS WAF.
--
-- @RegexPatternSetId@ is returned by 'CreateRegexPatternSet' and by 'ListRegexPatternSets' .
mkRegexPatternSetSummary ::
  -- | 'name'
  Lude.Text ->
  -- | 'regexPatternSetId'
  Lude.Text ->
  RegexPatternSetSummary
mkRegexPatternSetSummary pName_ pRegexPatternSetId_ =
  RegexPatternSetSummary'
    { name = pName_,
      regexPatternSetId = pRegexPatternSetId_
    }

-- | A friendly name or description of the 'RegexPatternSet' . You can't change @Name@ after you create a @RegexPatternSet@ .
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rpssName :: Lens.Lens' RegexPatternSetSummary Lude.Text
rpssName = Lens.lens (name :: RegexPatternSetSummary -> Lude.Text) (\s a -> s {name = a} :: RegexPatternSetSummary)
{-# DEPRECATED rpssName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The @RegexPatternSetId@ for a @RegexPatternSet@ . You use @RegexPatternSetId@ to get information about a @RegexPatternSet@ , update a @RegexPatternSet@ , remove a @RegexPatternSet@ from a @RegexMatchSet@ , and delete a @RegexPatternSet@ from AWS WAF.
--
-- @RegexPatternSetId@ is returned by 'CreateRegexPatternSet' and by 'ListRegexPatternSets' .
--
-- /Note:/ Consider using 'regexPatternSetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rpssRegexPatternSetId :: Lens.Lens' RegexPatternSetSummary Lude.Text
rpssRegexPatternSetId = Lens.lens (regexPatternSetId :: RegexPatternSetSummary -> Lude.Text) (\s a -> s {regexPatternSetId = a} :: RegexPatternSetSummary)
{-# DEPRECATED rpssRegexPatternSetId "Use generic-lens or generic-optics with 'regexPatternSetId' instead." #-}

instance Lude.FromJSON RegexPatternSetSummary where
  parseJSON =
    Lude.withObject
      "RegexPatternSetSummary"
      ( \x ->
          RegexPatternSetSummary'
            Lude.<$> (x Lude..: "Name") Lude.<*> (x Lude..: "RegexPatternSetId")
      )
