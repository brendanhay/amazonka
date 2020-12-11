-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.TagKeys
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.TagKeys
  ( TagKeys (..),

    -- * Smart constructor
    mkTagKeys,

    -- * Lenses
    tkItems,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A complex type that contains zero or more @Tag@ elements.
--
-- /See:/ 'mkTagKeys' smart constructor.
newtype TagKeys = TagKeys' {items :: Lude.Maybe [Lude.Text]}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TagKeys' with the minimum fields required to make a request.
--
-- * 'items' - A complex type that contains @Tag@ key elements.
mkTagKeys ::
  TagKeys
mkTagKeys = TagKeys' {items = Lude.Nothing}

-- | A complex type that contains @Tag@ key elements.
--
-- /Note:/ Consider using 'items' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tkItems :: Lens.Lens' TagKeys (Lude.Maybe [Lude.Text])
tkItems = Lens.lens (items :: TagKeys -> Lude.Maybe [Lude.Text]) (\s a -> s {items = a} :: TagKeys)
{-# DEPRECATED tkItems "Use generic-lens or generic-optics with 'items' instead." #-}

instance Lude.ToXML TagKeys where
  toXML TagKeys' {..} =
    Lude.mconcat
      ["Items" Lude.@= Lude.toXML (Lude.toXMLList "Key" Lude.<$> items)]
