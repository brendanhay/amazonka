-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.ErrorDocument
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.ErrorDocument
  ( ErrorDocument (..),

    -- * Smart constructor
    mkErrorDocument,

    -- * Lenses
    edKey,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.S3.Internal

-- | The error information.
--
-- /See:/ 'mkErrorDocument' smart constructor.
newtype ErrorDocument = ErrorDocument' {key :: ObjectKey}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ErrorDocument' with the minimum fields required to make a request.
--
-- * 'key' - The object key name to use when a 4XX class error occurs.
mkErrorDocument ::
  -- | 'key'
  ObjectKey ->
  ErrorDocument
mkErrorDocument pKey_ = ErrorDocument' {key = pKey_}

-- | The object key name to use when a 4XX class error occurs.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edKey :: Lens.Lens' ErrorDocument ObjectKey
edKey = Lens.lens (key :: ErrorDocument -> ObjectKey) (\s a -> s {key = a} :: ErrorDocument)
{-# DEPRECATED edKey "Use generic-lens or generic-optics with 'key' instead." #-}

instance Lude.FromXML ErrorDocument where
  parseXML x = ErrorDocument' Lude.<$> (x Lude..@ "Key")

instance Lude.ToXML ErrorDocument where
  toXML ErrorDocument' {..} = Lude.mconcat ["Key" Lude.@= key]
