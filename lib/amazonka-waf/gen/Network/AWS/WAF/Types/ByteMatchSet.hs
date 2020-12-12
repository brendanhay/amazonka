{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAF.Types.ByteMatchSet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WAF.Types.ByteMatchSet
  ( ByteMatchSet (..),

    -- * Smart constructor
    mkByteMatchSet,

    -- * Lenses
    bmsName,
    bmsByteMatchSetId,
    bmsByteMatchTuples,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.WAF.Types.ByteMatchTuple

-- | In a 'GetByteMatchSet' request, @ByteMatchSet@ is a complex type that contains the @ByteMatchSetId@ and @Name@ of a @ByteMatchSet@ , and the values that you specified when you updated the @ByteMatchSet@ .
--
-- A complex type that contains @ByteMatchTuple@ objects, which specify the parts of web requests that you want AWS WAF to inspect and the values that you want AWS WAF to search for. If a @ByteMatchSet@ contains more than one @ByteMatchTuple@ object, a request needs to match the settings in only one @ByteMatchTuple@ to be considered a match.
--
-- /See:/ 'mkByteMatchSet' smart constructor.
data ByteMatchSet = ByteMatchSet'
  { name :: Lude.Maybe Lude.Text,
    byteMatchSetId :: Lude.Text,
    byteMatchTuples :: [ByteMatchTuple]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ByteMatchSet' with the minimum fields required to make a request.
--
-- * 'byteMatchSetId' - The @ByteMatchSetId@ for a @ByteMatchSet@ . You use @ByteMatchSetId@ to get information about a @ByteMatchSet@ (see 'GetByteMatchSet' ), update a @ByteMatchSet@ (see 'UpdateByteMatchSet' ), insert a @ByteMatchSet@ into a @Rule@ or delete one from a @Rule@ (see 'UpdateRule' ), and delete a @ByteMatchSet@ from AWS WAF (see 'DeleteByteMatchSet' ).
--
-- @ByteMatchSetId@ is returned by 'CreateByteMatchSet' and by 'ListByteMatchSets' .
-- * 'byteMatchTuples' - Specifies the bytes (typically a string that corresponds with ASCII characters) that you want AWS WAF to search for in web requests, the location in requests that you want AWS WAF to search, and other settings.
-- * 'name' - A friendly name or description of the 'ByteMatchSet' . You can't change @Name@ after you create a @ByteMatchSet@ .
mkByteMatchSet ::
  -- | 'byteMatchSetId'
  Lude.Text ->
  ByteMatchSet
mkByteMatchSet pByteMatchSetId_ =
  ByteMatchSet'
    { name = Lude.Nothing,
      byteMatchSetId = pByteMatchSetId_,
      byteMatchTuples = Lude.mempty
    }

-- | A friendly name or description of the 'ByteMatchSet' . You can't change @Name@ after you create a @ByteMatchSet@ .
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bmsName :: Lens.Lens' ByteMatchSet (Lude.Maybe Lude.Text)
bmsName = Lens.lens (name :: ByteMatchSet -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: ByteMatchSet)
{-# DEPRECATED bmsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The @ByteMatchSetId@ for a @ByteMatchSet@ . You use @ByteMatchSetId@ to get information about a @ByteMatchSet@ (see 'GetByteMatchSet' ), update a @ByteMatchSet@ (see 'UpdateByteMatchSet' ), insert a @ByteMatchSet@ into a @Rule@ or delete one from a @Rule@ (see 'UpdateRule' ), and delete a @ByteMatchSet@ from AWS WAF (see 'DeleteByteMatchSet' ).
--
-- @ByteMatchSetId@ is returned by 'CreateByteMatchSet' and by 'ListByteMatchSets' .
--
-- /Note:/ Consider using 'byteMatchSetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bmsByteMatchSetId :: Lens.Lens' ByteMatchSet Lude.Text
bmsByteMatchSetId = Lens.lens (byteMatchSetId :: ByteMatchSet -> Lude.Text) (\s a -> s {byteMatchSetId = a} :: ByteMatchSet)
{-# DEPRECATED bmsByteMatchSetId "Use generic-lens or generic-optics with 'byteMatchSetId' instead." #-}

-- | Specifies the bytes (typically a string that corresponds with ASCII characters) that you want AWS WAF to search for in web requests, the location in requests that you want AWS WAF to search, and other settings.
--
-- /Note:/ Consider using 'byteMatchTuples' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bmsByteMatchTuples :: Lens.Lens' ByteMatchSet [ByteMatchTuple]
bmsByteMatchTuples = Lens.lens (byteMatchTuples :: ByteMatchSet -> [ByteMatchTuple]) (\s a -> s {byteMatchTuples = a} :: ByteMatchSet)
{-# DEPRECATED bmsByteMatchTuples "Use generic-lens or generic-optics with 'byteMatchTuples' instead." #-}

instance Lude.FromJSON ByteMatchSet where
  parseJSON =
    Lude.withObject
      "ByteMatchSet"
      ( \x ->
          ByteMatchSet'
            Lude.<$> (x Lude..:? "Name")
            Lude.<*> (x Lude..: "ByteMatchSetId")
            Lude.<*> (x Lude..:? "ByteMatchTuples" Lude..!= Lude.mempty)
      )
