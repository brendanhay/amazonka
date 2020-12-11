-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.Tags
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.Tags
  ( Tags (..),

    -- * Smart constructor
    mkTags,

    -- * Lenses
    tItems,
  )
where

import Network.AWS.CloudFront.Types.Tag
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A complex type that contains zero or more @Tag@ elements.
--
-- /See:/ 'mkTags' smart constructor.
newtype Tags = Tags' {items :: Lude.Maybe [Tag]}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Tags' with the minimum fields required to make a request.
--
-- * 'items' - A complex type that contains @Tag@ elements.
mkTags ::
  Tags
mkTags = Tags' {items = Lude.Nothing}

-- | A complex type that contains @Tag@ elements.
--
-- /Note:/ Consider using 'items' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tItems :: Lens.Lens' Tags (Lude.Maybe [Tag])
tItems = Lens.lens (items :: Tags -> Lude.Maybe [Tag]) (\s a -> s {items = a} :: Tags)
{-# DEPRECATED tItems "Use generic-lens or generic-optics with 'items' instead." #-}

instance Lude.FromXML Tags where
  parseXML x =
    Tags'
      Lude.<$> ( x Lude..@? "Items" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "Tag")
               )

instance Lude.ToXML Tags where
  toXML Tags' {..} =
    Lude.mconcat
      ["Items" Lude.@= Lude.toXML (Lude.toXMLList "Tag" Lude.<$> items)]
