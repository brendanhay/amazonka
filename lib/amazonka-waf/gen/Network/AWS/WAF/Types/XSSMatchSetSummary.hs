{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAF.Types.XSSMatchSetSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WAF.Types.XSSMatchSetSummary
  ( XSSMatchSetSummary (..),

    -- * Smart constructor
    mkXSSMatchSetSummary,

    -- * Lenses
    xmssXSSMatchSetId,
    xmssName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The @Id@ and @Name@ of an @XssMatchSet@ .
--
-- /See:/ 'mkXSSMatchSetSummary' smart constructor.
data XSSMatchSetSummary = XSSMatchSetSummary'
  { xssMatchSetId ::
      Lude.Text,
    name :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'XSSMatchSetSummary' with the minimum fields required to make a request.
--
-- * 'name' - The name of the @XssMatchSet@ , if any, specified by @Id@ .
-- * 'xssMatchSetId' - A unique identifier for an @XssMatchSet@ . You use @XssMatchSetId@ to get information about a @XssMatchSet@ (see 'GetXssMatchSet' ), update an @XssMatchSet@ (see 'UpdateXssMatchSet' ), insert an @XssMatchSet@ into a @Rule@ or delete one from a @Rule@ (see 'UpdateRule' ), and delete an @XssMatchSet@ from AWS WAF (see 'DeleteXssMatchSet' ).
--
-- @XssMatchSetId@ is returned by 'CreateXssMatchSet' and by 'ListXssMatchSets' .
mkXSSMatchSetSummary ::
  -- | 'xssMatchSetId'
  Lude.Text ->
  -- | 'name'
  Lude.Text ->
  XSSMatchSetSummary
mkXSSMatchSetSummary pXSSMatchSetId_ pName_ =
  XSSMatchSetSummary'
    { xssMatchSetId = pXSSMatchSetId_,
      name = pName_
    }

-- | A unique identifier for an @XssMatchSet@ . You use @XssMatchSetId@ to get information about a @XssMatchSet@ (see 'GetXssMatchSet' ), update an @XssMatchSet@ (see 'UpdateXssMatchSet' ), insert an @XssMatchSet@ into a @Rule@ or delete one from a @Rule@ (see 'UpdateRule' ), and delete an @XssMatchSet@ from AWS WAF (see 'DeleteXssMatchSet' ).
--
-- @XssMatchSetId@ is returned by 'CreateXssMatchSet' and by 'ListXssMatchSets' .
--
-- /Note:/ Consider using 'xssMatchSetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
xmssXSSMatchSetId :: Lens.Lens' XSSMatchSetSummary Lude.Text
xmssXSSMatchSetId = Lens.lens (xssMatchSetId :: XSSMatchSetSummary -> Lude.Text) (\s a -> s {xssMatchSetId = a} :: XSSMatchSetSummary)
{-# DEPRECATED xmssXSSMatchSetId "Use generic-lens or generic-optics with 'xssMatchSetId' instead." #-}

-- | The name of the @XssMatchSet@ , if any, specified by @Id@ .
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
xmssName :: Lens.Lens' XSSMatchSetSummary Lude.Text
xmssName = Lens.lens (name :: XSSMatchSetSummary -> Lude.Text) (\s a -> s {name = a} :: XSSMatchSetSummary)
{-# DEPRECATED xmssName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.FromJSON XSSMatchSetSummary where
  parseJSON =
    Lude.withObject
      "XSSMatchSetSummary"
      ( \x ->
          XSSMatchSetSummary'
            Lude.<$> (x Lude..: "XssMatchSetId") Lude.<*> (x Lude..: "Name")
      )
