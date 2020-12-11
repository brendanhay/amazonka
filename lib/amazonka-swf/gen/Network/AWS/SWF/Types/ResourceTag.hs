-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.ResourceTag
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.ResourceTag
  ( ResourceTag (..),

    -- * Smart constructor
    mkResourceTag,

    -- * Lenses
    rtValue,
    rtKey,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Tags are key-value pairs that can be associated with Amazon SWF state machines and activities.
--
-- Tags may only contain unicode letters, digits, whitespace, or these symbols: @_ . : / = + - @@ .
--
-- /See:/ 'mkResourceTag' smart constructor.
data ResourceTag = ResourceTag'
  { value :: Lude.Maybe Lude.Text,
    key :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ResourceTag' with the minimum fields required to make a request.
--
-- * 'key' - The key of a tag.
-- * 'value' - The value of a tag.
mkResourceTag ::
  -- | 'key'
  Lude.Text ->
  ResourceTag
mkResourceTag pKey_ =
  ResourceTag' {value = Lude.Nothing, key = pKey_}

-- | The value of a tag.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtValue :: Lens.Lens' ResourceTag (Lude.Maybe Lude.Text)
rtValue = Lens.lens (value :: ResourceTag -> Lude.Maybe Lude.Text) (\s a -> s {value = a} :: ResourceTag)
{-# DEPRECATED rtValue "Use generic-lens or generic-optics with 'value' instead." #-}

-- | The key of a tag.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtKey :: Lens.Lens' ResourceTag Lude.Text
rtKey = Lens.lens (key :: ResourceTag -> Lude.Text) (\s a -> s {key = a} :: ResourceTag)
{-# DEPRECATED rtKey "Use generic-lens or generic-optics with 'key' instead." #-}

instance Lude.FromJSON ResourceTag where
  parseJSON =
    Lude.withObject
      "ResourceTag"
      ( \x ->
          ResourceTag'
            Lude.<$> (x Lude..:? "value") Lude.<*> (x Lude..: "key")
      )

instance Lude.ToJSON ResourceTag where
  toJSON ResourceTag' {..} =
    Lude.object
      ( Lude.catMaybes
          [("value" Lude..=) Lude.<$> value, Lude.Just ("key" Lude..= key)]
      )
