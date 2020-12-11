-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAF.Types.XSSMatchSet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WAF.Types.XSSMatchSet
  ( XSSMatchSet (..),

    -- * Smart constructor
    mkXSSMatchSet,

    -- * Lenses
    xmsName,
    xmsXSSMatchSetId,
    xmsXSSMatchTuples,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.WAF.Types.XSSMatchTuple

-- | A complex type that contains @XssMatchTuple@ objects, which specify the parts of web requests that you want AWS WAF to inspect for cross-site scripting attacks and, if you want AWS WAF to inspect a header, the name of the header. If a @XssMatchSet@ contains more than one @XssMatchTuple@ object, a request needs to include cross-site scripting attacks in only one of the specified parts of the request to be considered a match.
--
-- /See:/ 'mkXSSMatchSet' smart constructor.
data XSSMatchSet = XSSMatchSet'
  { name :: Lude.Maybe Lude.Text,
    xssMatchSetId :: Lude.Text,
    xssMatchTuples :: [XSSMatchTuple]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'XSSMatchSet' with the minimum fields required to make a request.
--
-- * 'name' - The name, if any, of the @XssMatchSet@ .
-- * 'xssMatchSetId' - A unique identifier for an @XssMatchSet@ . You use @XssMatchSetId@ to get information about an @XssMatchSet@ (see 'GetXssMatchSet' ), update an @XssMatchSet@ (see 'UpdateXssMatchSet' ), insert an @XssMatchSet@ into a @Rule@ or delete one from a @Rule@ (see 'UpdateRule' ), and delete an @XssMatchSet@ from AWS WAF (see 'DeleteXssMatchSet' ).
--
-- @XssMatchSetId@ is returned by 'CreateXssMatchSet' and by 'ListXssMatchSets' .
-- * 'xssMatchTuples' - Specifies the parts of web requests that you want to inspect for cross-site scripting attacks.
mkXSSMatchSet ::
  -- | 'xssMatchSetId'
  Lude.Text ->
  XSSMatchSet
mkXSSMatchSet pXSSMatchSetId_ =
  XSSMatchSet'
    { name = Lude.Nothing,
      xssMatchSetId = pXSSMatchSetId_,
      xssMatchTuples = Lude.mempty
    }

-- | The name, if any, of the @XssMatchSet@ .
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
xmsName :: Lens.Lens' XSSMatchSet (Lude.Maybe Lude.Text)
xmsName = Lens.lens (name :: XSSMatchSet -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: XSSMatchSet)
{-# DEPRECATED xmsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | A unique identifier for an @XssMatchSet@ . You use @XssMatchSetId@ to get information about an @XssMatchSet@ (see 'GetXssMatchSet' ), update an @XssMatchSet@ (see 'UpdateXssMatchSet' ), insert an @XssMatchSet@ into a @Rule@ or delete one from a @Rule@ (see 'UpdateRule' ), and delete an @XssMatchSet@ from AWS WAF (see 'DeleteXssMatchSet' ).
--
-- @XssMatchSetId@ is returned by 'CreateXssMatchSet' and by 'ListXssMatchSets' .
--
-- /Note:/ Consider using 'xssMatchSetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
xmsXSSMatchSetId :: Lens.Lens' XSSMatchSet Lude.Text
xmsXSSMatchSetId = Lens.lens (xssMatchSetId :: XSSMatchSet -> Lude.Text) (\s a -> s {xssMatchSetId = a} :: XSSMatchSet)
{-# DEPRECATED xmsXSSMatchSetId "Use generic-lens or generic-optics with 'xssMatchSetId' instead." #-}

-- | Specifies the parts of web requests that you want to inspect for cross-site scripting attacks.
--
-- /Note:/ Consider using 'xssMatchTuples' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
xmsXSSMatchTuples :: Lens.Lens' XSSMatchSet [XSSMatchTuple]
xmsXSSMatchTuples = Lens.lens (xssMatchTuples :: XSSMatchSet -> [XSSMatchTuple]) (\s a -> s {xssMatchTuples = a} :: XSSMatchSet)
{-# DEPRECATED xmsXSSMatchTuples "Use generic-lens or generic-optics with 'xssMatchTuples' instead." #-}

instance Lude.FromJSON XSSMatchSet where
  parseJSON =
    Lude.withObject
      "XSSMatchSet"
      ( \x ->
          XSSMatchSet'
            Lude.<$> (x Lude..:? "Name")
            Lude.<*> (x Lude..: "XssMatchSetId")
            Lude.<*> (x Lude..:? "XssMatchTuples" Lude..!= Lude.mempty)
      )
