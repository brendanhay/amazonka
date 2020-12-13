{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudTrail.Types.Tag
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudTrail.Types.Tag
  ( Tag (..),

    -- * Smart constructor
    mkTag,

    -- * Lenses
    tValue,
    tKey,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A custom key-value pair associated with a resource such as a CloudTrail trail.
--
-- /See:/ 'mkTag' smart constructor.
data Tag = Tag'
  { -- | The value in a key-value pair of a tag. The value must be no longer than 256 Unicode characters.
    value :: Lude.Maybe Lude.Text,
    -- | The key in a key-value pair. The key must be must be no longer than 128 Unicode characters. The key must be unique for the resource to which it applies.
    key :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Tag' with the minimum fields required to make a request.
--
-- * 'value' - The value in a key-value pair of a tag. The value must be no longer than 256 Unicode characters.
-- * 'key' - The key in a key-value pair. The key must be must be no longer than 128 Unicode characters. The key must be unique for the resource to which it applies.
mkTag ::
  -- | 'key'
  Lude.Text ->
  Tag
mkTag pKey_ = Tag' {value = Lude.Nothing, key = pKey_}

-- | The value in a key-value pair of a tag. The value must be no longer than 256 Unicode characters.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tValue :: Lens.Lens' Tag (Lude.Maybe Lude.Text)
tValue = Lens.lens (value :: Tag -> Lude.Maybe Lude.Text) (\s a -> s {value = a} :: Tag)
{-# DEPRECATED tValue "Use generic-lens or generic-optics with 'value' instead." #-}

-- | The key in a key-value pair. The key must be must be no longer than 128 Unicode characters. The key must be unique for the resource to which it applies.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tKey :: Lens.Lens' Tag Lude.Text
tKey = Lens.lens (key :: Tag -> Lude.Text) (\s a -> s {key = a} :: Tag)
{-# DEPRECATED tKey "Use generic-lens or generic-optics with 'key' instead." #-}

instance Lude.FromJSON Tag where
  parseJSON =
    Lude.withObject
      "Tag"
      ( \x ->
          Tag' Lude.<$> (x Lude..:? "Value") Lude.<*> (x Lude..: "Key")
      )

instance Lude.ToJSON Tag where
  toJSON Tag' {..} =
    Lude.object
      ( Lude.catMaybes
          [("Value" Lude..=) Lude.<$> value, Lude.Just ("Key" Lude..= key)]
      )
