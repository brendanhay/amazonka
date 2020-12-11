-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAFRegional.Types.ByteMatchSetSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WAFRegional.Types.ByteMatchSetSummary
  ( ByteMatchSetSummary (..),

    -- * Smart constructor
    mkByteMatchSetSummary,

    -- * Lenses
    bmssByteMatchSetId,
    bmssName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Returned by 'ListByteMatchSets' . Each @ByteMatchSetSummary@ object includes the @Name@ and @ByteMatchSetId@ for one 'ByteMatchSet' .
--
-- /See:/ 'mkByteMatchSetSummary' smart constructor.
data ByteMatchSetSummary = ByteMatchSetSummary'
  { byteMatchSetId ::
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

-- | Creates a value of 'ByteMatchSetSummary' with the minimum fields required to make a request.
--
-- * 'byteMatchSetId' - The @ByteMatchSetId@ for a @ByteMatchSet@ . You use @ByteMatchSetId@ to get information about a @ByteMatchSet@ , update a @ByteMatchSet@ , remove a @ByteMatchSet@ from a @Rule@ , and delete a @ByteMatchSet@ from AWS WAF.
--
-- @ByteMatchSetId@ is returned by 'CreateByteMatchSet' and by 'ListByteMatchSets' .
-- * 'name' - A friendly name or description of the 'ByteMatchSet' . You can't change @Name@ after you create a @ByteMatchSet@ .
mkByteMatchSetSummary ::
  -- | 'byteMatchSetId'
  Lude.Text ->
  -- | 'name'
  Lude.Text ->
  ByteMatchSetSummary
mkByteMatchSetSummary pByteMatchSetId_ pName_ =
  ByteMatchSetSummary'
    { byteMatchSetId = pByteMatchSetId_,
      name = pName_
    }

-- | The @ByteMatchSetId@ for a @ByteMatchSet@ . You use @ByteMatchSetId@ to get information about a @ByteMatchSet@ , update a @ByteMatchSet@ , remove a @ByteMatchSet@ from a @Rule@ , and delete a @ByteMatchSet@ from AWS WAF.
--
-- @ByteMatchSetId@ is returned by 'CreateByteMatchSet' and by 'ListByteMatchSets' .
--
-- /Note:/ Consider using 'byteMatchSetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bmssByteMatchSetId :: Lens.Lens' ByteMatchSetSummary Lude.Text
bmssByteMatchSetId = Lens.lens (byteMatchSetId :: ByteMatchSetSummary -> Lude.Text) (\s a -> s {byteMatchSetId = a} :: ByteMatchSetSummary)
{-# DEPRECATED bmssByteMatchSetId "Use generic-lens or generic-optics with 'byteMatchSetId' instead." #-}

-- | A friendly name or description of the 'ByteMatchSet' . You can't change @Name@ after you create a @ByteMatchSet@ .
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bmssName :: Lens.Lens' ByteMatchSetSummary Lude.Text
bmssName = Lens.lens (name :: ByteMatchSetSummary -> Lude.Text) (\s a -> s {name = a} :: ByteMatchSetSummary)
{-# DEPRECATED bmssName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.FromJSON ByteMatchSetSummary where
  parseJSON =
    Lude.withObject
      "ByteMatchSetSummary"
      ( \x ->
          ByteMatchSetSummary'
            Lude.<$> (x Lude..: "ByteMatchSetId") Lude.<*> (x Lude..: "Name")
      )
