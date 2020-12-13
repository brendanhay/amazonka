{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.Types.Tag
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Comprehend.Types.Tag
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

-- | A key-value pair that adds as a metadata to a resource used by Amazon Comprehend. For example, a tag with the key-value pair ‘Department’:’Sales’ might be added to a resource to indicate its use by a particular department.
--
-- /See:/ 'mkTag' smart constructor.
data Tag = Tag'
  { -- | The second part of a key-value pair that forms a tag associated with a given resource. For instance, if you want to show which resources are used by which departments, you might use “Department” as the initial (key) portion of the pair, with a value of “sales” to indicate the sales department.
    value :: Lude.Maybe Lude.Text,
    -- | The initial part of a key-value pair that forms a tag associated with a given resource. For instance, if you want to show which resources are used by which departments, you might use “Department” as the key portion of the pair, with multiple possible values such as “sales,” “legal,” and “administration.”
    key :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Tag' with the minimum fields required to make a request.
--
-- * 'value' - The second part of a key-value pair that forms a tag associated with a given resource. For instance, if you want to show which resources are used by which departments, you might use “Department” as the initial (key) portion of the pair, with a value of “sales” to indicate the sales department.
-- * 'key' - The initial part of a key-value pair that forms a tag associated with a given resource. For instance, if you want to show which resources are used by which departments, you might use “Department” as the key portion of the pair, with multiple possible values such as “sales,” “legal,” and “administration.”
mkTag ::
  -- | 'key'
  Lude.Text ->
  Tag
mkTag pKey_ = Tag' {value = Lude.Nothing, key = pKey_}

-- | The second part of a key-value pair that forms a tag associated with a given resource. For instance, if you want to show which resources are used by which departments, you might use “Department” as the initial (key) portion of the pair, with a value of “sales” to indicate the sales department.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tValue :: Lens.Lens' Tag (Lude.Maybe Lude.Text)
tValue = Lens.lens (value :: Tag -> Lude.Maybe Lude.Text) (\s a -> s {value = a} :: Tag)
{-# DEPRECATED tValue "Use generic-lens or generic-optics with 'value' instead." #-}

-- | The initial part of a key-value pair that forms a tag associated with a given resource. For instance, if you want to show which resources are used by which departments, you might use “Department” as the key portion of the pair, with multiple possible values such as “sales,” “legal,” and “administration.”
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
