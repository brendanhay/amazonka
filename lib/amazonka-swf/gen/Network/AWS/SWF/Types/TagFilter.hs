{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SWF.Types.Tag as Types

-- | Used to filter the workflow executions in visibility APIs based on a tag.
--
-- /See:/ 'mkTagFilter' smart constructor.
newtype TagFilter = TagFilter'
  { -- | Specifies the tag that must be associated with the execution for it to meet the filter criteria.
    --
    -- Tags may only contain unicode letters, digits, whitespace, or these symbols: @_ . : / = + - @@ .
    tag :: Types.Tag
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'TagFilter' value with any optional fields omitted.
mkTagFilter ::
  -- | 'tag'
  Types.Tag ->
  TagFilter
mkTagFilter tag = TagFilter' {tag}

-- | Specifies the tag that must be associated with the execution for it to meet the filter criteria.
--
-- Tags may only contain unicode letters, digits, whitespace, or these symbols: @_ . : / = + - @@ .
--
-- /Note:/ Consider using 'tag' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tfTag :: Lens.Lens' TagFilter Types.Tag
tfTag = Lens.field @"tag"
{-# DEPRECATED tfTag "Use generic-lens or generic-optics with 'tag' instead." #-}

instance Core.FromJSON TagFilter where
  toJSON TagFilter {..} =
    Core.object (Core.catMaybes [Core.Just ("tag" Core..= tag)])
