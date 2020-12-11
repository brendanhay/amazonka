-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.CommonPrefix
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.CommonPrefix
  ( CommonPrefix (..),

    -- * Smart constructor
    mkCommonPrefix,

    -- * Lenses
    cpPrefix,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.S3.Internal

-- | Container for all (if there are any) keys between Prefix and the next occurrence of the string specified by a delimiter. CommonPrefixes lists keys that act like subdirectories in the directory specified by Prefix. For example, if the prefix is notes/ and the delimiter is a slash (/) as in notes/summer/july, the common prefix is notes/summer/.
--
-- /See:/ 'mkCommonPrefix' smart constructor.
newtype CommonPrefix = CommonPrefix'
  { prefix ::
      Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CommonPrefix' with the minimum fields required to make a request.
--
-- * 'prefix' - Container for the specified common prefix.
mkCommonPrefix ::
  CommonPrefix
mkCommonPrefix = CommonPrefix' {prefix = Lude.Nothing}

-- | Container for the specified common prefix.
--
-- /Note:/ Consider using 'prefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpPrefix :: Lens.Lens' CommonPrefix (Lude.Maybe Lude.Text)
cpPrefix = Lens.lens (prefix :: CommonPrefix -> Lude.Maybe Lude.Text) (\s a -> s {prefix = a} :: CommonPrefix)
{-# DEPRECATED cpPrefix "Use generic-lens or generic-optics with 'prefix' instead." #-}

instance Lude.FromXML CommonPrefix where
  parseXML x = CommonPrefix' Lude.<$> (x Lude..@? "Prefix")
