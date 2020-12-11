-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAFRegional.Types.RegexMatchSet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WAFRegional.Types.RegexMatchSet
  ( RegexMatchSet (..),

    -- * Smart constructor
    mkRegexMatchSet,

    -- * Lenses
    rmsName,
    rmsRegexMatchTuples,
    rmsRegexMatchSetId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.WAFRegional.Types.RegexMatchTuple

-- | In a 'GetRegexMatchSet' request, @RegexMatchSet@ is a complex type that contains the @RegexMatchSetId@ and @Name@ of a @RegexMatchSet@ , and the values that you specified when you updated the @RegexMatchSet@ .
--
-- The values are contained in a @RegexMatchTuple@ object, which specify the parts of web requests that you want AWS WAF to inspect and the values that you want AWS WAF to search for. If a @RegexMatchSet@ contains more than one @RegexMatchTuple@ object, a request needs to match the settings in only one @ByteMatchTuple@ to be considered a match.
--
-- /See:/ 'mkRegexMatchSet' smart constructor.
data RegexMatchSet = RegexMatchSet'
  { name :: Lude.Maybe Lude.Text,
    regexMatchTuples :: Lude.Maybe [RegexMatchTuple],
    regexMatchSetId :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RegexMatchSet' with the minimum fields required to make a request.
--
-- * 'name' - A friendly name or description of the 'RegexMatchSet' . You can't change @Name@ after you create a @RegexMatchSet@ .
-- * 'regexMatchSetId' - The @RegexMatchSetId@ for a @RegexMatchSet@ . You use @RegexMatchSetId@ to get information about a @RegexMatchSet@ (see 'GetRegexMatchSet' ), update a @RegexMatchSet@ (see 'UpdateRegexMatchSet' ), insert a @RegexMatchSet@ into a @Rule@ or delete one from a @Rule@ (see 'UpdateRule' ), and delete a @RegexMatchSet@ from AWS WAF (see 'DeleteRegexMatchSet' ).
--
-- @RegexMatchSetId@ is returned by 'CreateRegexMatchSet' and by 'ListRegexMatchSets' .
-- * 'regexMatchTuples' - Contains an array of 'RegexMatchTuple' objects. Each @RegexMatchTuple@ object contains:
--
--
--     * The part of a web request that you want AWS WAF to inspect, such as a query string or the value of the @User-Agent@ header.
--
--
--     * The identifier of the pattern (a regular expression) that you want AWS WAF to look for. For more information, see 'RegexPatternSet' .
--
--
--     * Whether to perform any conversions on the request, such as converting it to lowercase, before inspecting it for the specified string.
mkRegexMatchSet ::
  RegexMatchSet
mkRegexMatchSet =
  RegexMatchSet'
    { name = Lude.Nothing,
      regexMatchTuples = Lude.Nothing,
      regexMatchSetId = Lude.Nothing
    }

-- | A friendly name or description of the 'RegexMatchSet' . You can't change @Name@ after you create a @RegexMatchSet@ .
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rmsName :: Lens.Lens' RegexMatchSet (Lude.Maybe Lude.Text)
rmsName = Lens.lens (name :: RegexMatchSet -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: RegexMatchSet)
{-# DEPRECATED rmsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | Contains an array of 'RegexMatchTuple' objects. Each @RegexMatchTuple@ object contains:
--
--
--     * The part of a web request that you want AWS WAF to inspect, such as a query string or the value of the @User-Agent@ header.
--
--
--     * The identifier of the pattern (a regular expression) that you want AWS WAF to look for. For more information, see 'RegexPatternSet' .
--
--
--     * Whether to perform any conversions on the request, such as converting it to lowercase, before inspecting it for the specified string.
--
--
--
-- /Note:/ Consider using 'regexMatchTuples' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rmsRegexMatchTuples :: Lens.Lens' RegexMatchSet (Lude.Maybe [RegexMatchTuple])
rmsRegexMatchTuples = Lens.lens (regexMatchTuples :: RegexMatchSet -> Lude.Maybe [RegexMatchTuple]) (\s a -> s {regexMatchTuples = a} :: RegexMatchSet)
{-# DEPRECATED rmsRegexMatchTuples "Use generic-lens or generic-optics with 'regexMatchTuples' instead." #-}

-- | The @RegexMatchSetId@ for a @RegexMatchSet@ . You use @RegexMatchSetId@ to get information about a @RegexMatchSet@ (see 'GetRegexMatchSet' ), update a @RegexMatchSet@ (see 'UpdateRegexMatchSet' ), insert a @RegexMatchSet@ into a @Rule@ or delete one from a @Rule@ (see 'UpdateRule' ), and delete a @RegexMatchSet@ from AWS WAF (see 'DeleteRegexMatchSet' ).
--
-- @RegexMatchSetId@ is returned by 'CreateRegexMatchSet' and by 'ListRegexMatchSets' .
--
-- /Note:/ Consider using 'regexMatchSetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rmsRegexMatchSetId :: Lens.Lens' RegexMatchSet (Lude.Maybe Lude.Text)
rmsRegexMatchSetId = Lens.lens (regexMatchSetId :: RegexMatchSet -> Lude.Maybe Lude.Text) (\s a -> s {regexMatchSetId = a} :: RegexMatchSet)
{-# DEPRECATED rmsRegexMatchSetId "Use generic-lens or generic-optics with 'regexMatchSetId' instead." #-}

instance Lude.FromJSON RegexMatchSet where
  parseJSON =
    Lude.withObject
      "RegexMatchSet"
      ( \x ->
          RegexMatchSet'
            Lude.<$> (x Lude..:? "Name")
            Lude.<*> (x Lude..:? "RegexMatchTuples" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "RegexMatchSetId")
      )
