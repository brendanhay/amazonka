-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Inspector.Types.ResourceGroupTag
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Inspector.Types.ResourceGroupTag
  ( ResourceGroupTag (..),

    -- * Smart constructor
    mkResourceGroupTag,

    -- * Lenses
    rgtValue,
    rgtKey,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | This data type is used as one of the elements of the 'ResourceGroup' data type.
--
-- /See:/ 'mkResourceGroupTag' smart constructor.
data ResourceGroupTag = ResourceGroupTag'
  { value ::
      Lude.Maybe Lude.Text,
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

-- | Creates a value of 'ResourceGroupTag' with the minimum fields required to make a request.
--
-- * 'key' - A tag key.
-- * 'value' - The value assigned to a tag key.
mkResourceGroupTag ::
  -- | 'key'
  Lude.Text ->
  ResourceGroupTag
mkResourceGroupTag pKey_ =
  ResourceGroupTag' {value = Lude.Nothing, key = pKey_}

-- | The value assigned to a tag key.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rgtValue :: Lens.Lens' ResourceGroupTag (Lude.Maybe Lude.Text)
rgtValue = Lens.lens (value :: ResourceGroupTag -> Lude.Maybe Lude.Text) (\s a -> s {value = a} :: ResourceGroupTag)
{-# DEPRECATED rgtValue "Use generic-lens or generic-optics with 'value' instead." #-}

-- | A tag key.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rgtKey :: Lens.Lens' ResourceGroupTag Lude.Text
rgtKey = Lens.lens (key :: ResourceGroupTag -> Lude.Text) (\s a -> s {key = a} :: ResourceGroupTag)
{-# DEPRECATED rgtKey "Use generic-lens or generic-optics with 'key' instead." #-}

instance Lude.FromJSON ResourceGroupTag where
  parseJSON =
    Lude.withObject
      "ResourceGroupTag"
      ( \x ->
          ResourceGroupTag'
            Lude.<$> (x Lude..:? "value") Lude.<*> (x Lude..: "key")
      )

instance Lude.ToJSON ResourceGroupTag where
  toJSON ResourceGroupTag' {..} =
    Lude.object
      ( Lude.catMaybes
          [("value" Lude..=) Lude.<$> value, Lude.Just ("key" Lude..= key)]
      )
