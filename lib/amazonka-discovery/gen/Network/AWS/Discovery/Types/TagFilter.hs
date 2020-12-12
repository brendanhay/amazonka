{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Discovery.Types.TagFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Discovery.Types.TagFilter
  ( TagFilter (..),

    -- * Smart constructor
    mkTagFilter,

    -- * Lenses
    tfName,
    tfValues,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The tag filter. Valid names are: @tagKey@ , @tagValue@ , @configurationId@ .
--
-- /See:/ 'mkTagFilter' smart constructor.
data TagFilter = TagFilter'
  { name :: Lude.Text,
    values :: [Lude.Text]
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
-- * 'name' - A name of the tag filter.
-- * 'values' - Values for the tag filter.
mkTagFilter ::
  -- | 'name'
  Lude.Text ->
  TagFilter
mkTagFilter pName_ =
  TagFilter' {name = pName_, values = Lude.mempty}

-- | A name of the tag filter.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tfName :: Lens.Lens' TagFilter Lude.Text
tfName = Lens.lens (name :: TagFilter -> Lude.Text) (\s a -> s {name = a} :: TagFilter)
{-# DEPRECATED tfName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | Values for the tag filter.
--
-- /Note:/ Consider using 'values' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tfValues :: Lens.Lens' TagFilter [Lude.Text]
tfValues = Lens.lens (values :: TagFilter -> [Lude.Text]) (\s a -> s {values = a} :: TagFilter)
{-# DEPRECATED tfValues "Use generic-lens or generic-optics with 'values' instead." #-}

instance Lude.ToJSON TagFilter where
  toJSON TagFilter' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("name" Lude..= name),
            Lude.Just ("values" Lude..= values)
          ]
      )
