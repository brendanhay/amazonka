{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.Types.TagFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeDeploy.Types.TagFilter
  ( TagFilter (..),

    -- * Smart constructor
    mkTagFilter,

    -- * Lenses
    tfValue,
    tfKey,
    tfType,
  )
where

import Network.AWS.CodeDeploy.Types.TagFilterType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about an on-premises instance tag filter.
--
-- /See:/ 'mkTagFilter' smart constructor.
data TagFilter = TagFilter'
  { value :: Lude.Maybe Lude.Text,
    key :: Lude.Maybe Lude.Text,
    type' :: Lude.Maybe TagFilterType
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TagFilter' with the minimum fields required to make a request.
--
-- * 'key' - The on-premises instance tag filter key.
-- * 'type'' - The on-premises instance tag filter type:
--
--
--     * KEY_ONLY: Key only.
--
--
--     * VALUE_ONLY: Value only.
--
--
--     * KEY_AND_VALUE: Key and value.
--
--
-- * 'value' - The on-premises instance tag filter value.
mkTagFilter ::
  TagFilter
mkTagFilter =
  TagFilter'
    { value = Lude.Nothing,
      key = Lude.Nothing,
      type' = Lude.Nothing
    }

-- | The on-premises instance tag filter value.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tfValue :: Lens.Lens' TagFilter (Lude.Maybe Lude.Text)
tfValue = Lens.lens (value :: TagFilter -> Lude.Maybe Lude.Text) (\s a -> s {value = a} :: TagFilter)
{-# DEPRECATED tfValue "Use generic-lens or generic-optics with 'value' instead." #-}

-- | The on-premises instance tag filter key.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tfKey :: Lens.Lens' TagFilter (Lude.Maybe Lude.Text)
tfKey = Lens.lens (key :: TagFilter -> Lude.Maybe Lude.Text) (\s a -> s {key = a} :: TagFilter)
{-# DEPRECATED tfKey "Use generic-lens or generic-optics with 'key' instead." #-}

-- | The on-premises instance tag filter type:
--
--
--     * KEY_ONLY: Key only.
--
--
--     * VALUE_ONLY: Value only.
--
--
--     * KEY_AND_VALUE: Key and value.
--
--
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tfType :: Lens.Lens' TagFilter (Lude.Maybe TagFilterType)
tfType = Lens.lens (type' :: TagFilter -> Lude.Maybe TagFilterType) (\s a -> s {type' = a} :: TagFilter)
{-# DEPRECATED tfType "Use generic-lens or generic-optics with 'type'' instead." #-}

instance Lude.FromJSON TagFilter where
  parseJSON =
    Lude.withObject
      "TagFilter"
      ( \x ->
          TagFilter'
            Lude.<$> (x Lude..:? "Value")
            Lude.<*> (x Lude..:? "Key")
            Lude.<*> (x Lude..:? "Type")
      )

instance Lude.ToJSON TagFilter where
  toJSON TagFilter' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Value" Lude..=) Lude.<$> value,
            ("Key" Lude..=) Lude.<$> key,
            ("Type" Lude..=) Lude.<$> type'
          ]
      )
