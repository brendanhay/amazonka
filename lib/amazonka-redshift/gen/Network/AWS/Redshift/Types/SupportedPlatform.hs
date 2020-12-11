-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.Types.SupportedPlatform
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Redshift.Types.SupportedPlatform
  ( SupportedPlatform (..),

    -- * Smart constructor
    mkSupportedPlatform,

    -- * Lenses
    spName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Redshift.Internal

-- | A list of supported platforms for orderable clusters.
--
-- /See:/ 'mkSupportedPlatform' smart constructor.
newtype SupportedPlatform = SupportedPlatform'
  { name ::
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

-- | Creates a value of 'SupportedPlatform' with the minimum fields required to make a request.
--
-- * 'name' -
mkSupportedPlatform ::
  SupportedPlatform
mkSupportedPlatform = SupportedPlatform' {name = Lude.Nothing}

-- |
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spName :: Lens.Lens' SupportedPlatform (Lude.Maybe Lude.Text)
spName = Lens.lens (name :: SupportedPlatform -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: SupportedPlatform)
{-# DEPRECATED spName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.FromXML SupportedPlatform where
  parseXML x = SupportedPlatform' Lude.<$> (x Lude..@? "Name")
