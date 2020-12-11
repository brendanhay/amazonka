-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ResourceGroupsTagging.Types.TagFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ResourceGroupsTagging.Types.TagFilter
  ( TagFilter (..),

    -- * Smart constructor
    mkTagFilter,

    -- * Lenses
    tfValues,
    tfKey,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A list of tags (keys and values) that are used to specify the associated resources.
--
-- /See:/ 'mkTagFilter' smart constructor.
data TagFilter = TagFilter'
  { values :: Lude.Maybe [Lude.Text],
    key :: Lude.Maybe Lude.Text
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
-- * 'key' - One part of a key-value pair that makes up a tag. A key is a general label that acts like a category for more specific tag values.
-- * 'values' - One part of a key-value pair that make up a tag. A value acts as a descriptor within a tag category (key). The value can be empty or null.
mkTagFilter ::
  TagFilter
mkTagFilter = TagFilter' {values = Lude.Nothing, key = Lude.Nothing}

-- | One part of a key-value pair that make up a tag. A value acts as a descriptor within a tag category (key). The value can be empty or null.
--
-- /Note:/ Consider using 'values' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tfValues :: Lens.Lens' TagFilter (Lude.Maybe [Lude.Text])
tfValues = Lens.lens (values :: TagFilter -> Lude.Maybe [Lude.Text]) (\s a -> s {values = a} :: TagFilter)
{-# DEPRECATED tfValues "Use generic-lens or generic-optics with 'values' instead." #-}

-- | One part of a key-value pair that makes up a tag. A key is a general label that acts like a category for more specific tag values.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tfKey :: Lens.Lens' TagFilter (Lude.Maybe Lude.Text)
tfKey = Lens.lens (key :: TagFilter -> Lude.Maybe Lude.Text) (\s a -> s {key = a} :: TagFilter)
{-# DEPRECATED tfKey "Use generic-lens or generic-optics with 'key' instead." #-}

instance Lude.ToJSON TagFilter where
  toJSON TagFilter' {..} =
    Lude.object
      ( Lude.catMaybes
          [("Values" Lude..=) Lude.<$> values, ("Key" Lude..=) Lude.<$> key]
      )
