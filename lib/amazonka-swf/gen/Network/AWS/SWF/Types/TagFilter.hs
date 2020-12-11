-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.TagFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.TagFilter
  ( TagFilter (..),

    -- * Smart constructor
    mkTagFilter,

    -- * Lenses
    tfTag,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Used to filter the workflow executions in visibility APIs based on a tag.
--
-- /See:/ 'mkTagFilter' smart constructor.
newtype TagFilter = TagFilter' {tag :: Lude.Text}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TagFilter' with the minimum fields required to make a request.
--
-- * 'tag' - Specifies the tag that must be associated with the execution for it to meet the filter criteria.
--
-- Tags may only contain unicode letters, digits, whitespace, or these symbols: @_ . : / = + - @@ .
mkTagFilter ::
  -- | 'tag'
  Lude.Text ->
  TagFilter
mkTagFilter pTag_ = TagFilter' {tag = pTag_}

-- | Specifies the tag that must be associated with the execution for it to meet the filter criteria.
--
-- Tags may only contain unicode letters, digits, whitespace, or these symbols: @_ . : / = + - @@ .
--
-- /Note:/ Consider using 'tag' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tfTag :: Lens.Lens' TagFilter Lude.Text
tfTag = Lens.lens (tag :: TagFilter -> Lude.Text) (\s a -> s {tag = a} :: TagFilter)
{-# DEPRECATED tfTag "Use generic-lens or generic-optics with 'tag' instead." #-}

instance Lude.ToJSON TagFilter where
  toJSON TagFilter' {..} =
    Lude.object (Lude.catMaybes [Lude.Just ("tag" Lude..= tag)])
