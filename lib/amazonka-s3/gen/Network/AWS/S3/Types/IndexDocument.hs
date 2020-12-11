-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.IndexDocument
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.IndexDocument
  ( IndexDocument (..),

    -- * Smart constructor
    mkIndexDocument,

    -- * Lenses
    idSuffix,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.S3.Internal

-- | Container for the @Suffix@ element.
--
-- /See:/ 'mkIndexDocument' smart constructor.
newtype IndexDocument = IndexDocument' {suffix :: Lude.Text}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'IndexDocument' with the minimum fields required to make a request.
--
-- * 'suffix' - A suffix that is appended to a request that is for a directory on the website endpoint (for example,if the suffix is index.html and you make a request to samplebucket/images/ the data that is returned will be for the object with the key name images/index.html) The suffix must not be empty and must not include a slash character.
mkIndexDocument ::
  -- | 'suffix'
  Lude.Text ->
  IndexDocument
mkIndexDocument pSuffix_ = IndexDocument' {suffix = pSuffix_}

-- | A suffix that is appended to a request that is for a directory on the website endpoint (for example,if the suffix is index.html and you make a request to samplebucket/images/ the data that is returned will be for the object with the key name images/index.html) The suffix must not be empty and must not include a slash character.
--
-- /Note:/ Consider using 'suffix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idSuffix :: Lens.Lens' IndexDocument Lude.Text
idSuffix = Lens.lens (suffix :: IndexDocument -> Lude.Text) (\s a -> s {suffix = a} :: IndexDocument)
{-# DEPRECATED idSuffix "Use generic-lens or generic-optics with 'suffix' instead." #-}

instance Lude.FromXML IndexDocument where
  parseXML x = IndexDocument' Lude.<$> (x Lude..@ "Suffix")

instance Lude.ToXML IndexDocument where
  toXML IndexDocument' {..} = Lude.mconcat ["Suffix" Lude.@= suffix]
