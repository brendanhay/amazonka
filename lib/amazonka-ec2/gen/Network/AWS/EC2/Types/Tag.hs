-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.Tag
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.Tag
  ( Tag (..),

    -- * Smart constructor
    mkTag,

    -- * Lenses
    tKey,
    tValue,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes a tag.
--
-- /See:/ 'mkTag' smart constructor.
data Tag = Tag' {key :: Lude.Text, value :: Lude.Text}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Tag' with the minimum fields required to make a request.
--
-- * 'key' - The key of the tag.
--
-- Constraints: Tag keys are case-sensitive and accept a maximum of 127 Unicode characters. May not begin with @aws:@ .
-- * 'value' - The value of the tag.
--
-- Constraints: Tag values are case-sensitive and accept a maximum of 255 Unicode characters.
mkTag ::
  -- | 'key'
  Lude.Text ->
  -- | 'value'
  Lude.Text ->
  Tag
mkTag pKey_ pValue_ = Tag' {key = pKey_, value = pValue_}

-- | The key of the tag.
--
-- Constraints: Tag keys are case-sensitive and accept a maximum of 127 Unicode characters. May not begin with @aws:@ .
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tKey :: Lens.Lens' Tag Lude.Text
tKey = Lens.lens (key :: Tag -> Lude.Text) (\s a -> s {key = a} :: Tag)
{-# DEPRECATED tKey "Use generic-lens or generic-optics with 'key' instead." #-}

-- | The value of the tag.
--
-- Constraints: Tag values are case-sensitive and accept a maximum of 255 Unicode characters.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tValue :: Lens.Lens' Tag Lude.Text
tValue = Lens.lens (value :: Tag -> Lude.Text) (\s a -> s {value = a} :: Tag)
{-# DEPRECATED tValue "Use generic-lens or generic-optics with 'value' instead." #-}

instance Lude.FromXML Tag where
  parseXML x =
    Tag' Lude.<$> (x Lude..@ "key") Lude.<*> (x Lude..@ "value")

instance Lude.ToQuery Tag where
  toQuery Tag' {..} =
    Lude.mconcat ["Key" Lude.=: key, "Value" Lude.=: value]
