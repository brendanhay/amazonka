{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.MetricsFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.MetricsFilter
  ( MetricsFilter (..),

    -- * Smart constructor
    mkMetricsFilter,

    -- * Lenses
    mfAnd,
    mfPrefix,
    mfTag,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.S3.Internal as Types
import qualified Network.AWS.S3.Types.MetricsAndOperator as Types
import qualified Network.AWS.S3.Types.Prefix as Types
import qualified Network.AWS.S3.Types.Tag as Types

-- | Specifies a metrics configuration filter. The metrics configuration only includes objects that meet the filter's criteria. A filter must be a prefix, a tag, or a conjunction (MetricsAndOperator).
--
-- /See:/ 'mkMetricsFilter' smart constructor.
data MetricsFilter = MetricsFilter'
  { -- | A conjunction (logical AND) of predicates, which is used in evaluating a metrics filter. The operator must have at least two predicates, and an object must match all of the predicates in order for the filter to apply.
    and :: Core.Maybe Types.MetricsAndOperator,
    -- | The prefix used when evaluating a metrics filter.
    prefix :: Core.Maybe Types.Prefix,
    -- | The tag used when evaluating a metrics filter.
    tag :: Core.Maybe Types.Tag
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'MetricsFilter' value with any optional fields omitted.
mkMetricsFilter ::
  MetricsFilter
mkMetricsFilter =
  MetricsFilter'
    { and = Core.Nothing,
      prefix = Core.Nothing,
      tag = Core.Nothing
    }

-- | A conjunction (logical AND) of predicates, which is used in evaluating a metrics filter. The operator must have at least two predicates, and an object must match all of the predicates in order for the filter to apply.
--
-- /Note:/ Consider using 'and' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mfAnd :: Lens.Lens' MetricsFilter (Core.Maybe Types.MetricsAndOperator)
mfAnd = Lens.field @"and"
{-# DEPRECATED mfAnd "Use generic-lens or generic-optics with 'and' instead." #-}

-- | The prefix used when evaluating a metrics filter.
--
-- /Note:/ Consider using 'prefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mfPrefix :: Lens.Lens' MetricsFilter (Core.Maybe Types.Prefix)
mfPrefix = Lens.field @"prefix"
{-# DEPRECATED mfPrefix "Use generic-lens or generic-optics with 'prefix' instead." #-}

-- | The tag used when evaluating a metrics filter.
--
-- /Note:/ Consider using 'tag' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mfTag :: Lens.Lens' MetricsFilter (Core.Maybe Types.Tag)
mfTag = Lens.field @"tag"
{-# DEPRECATED mfTag "Use generic-lens or generic-optics with 'tag' instead." #-}

instance Core.ToXML MetricsFilter where
  toXML MetricsFilter {..} =
    Core.toXMLNode "And" Core.<$> and
      Core.<> Core.toXMLNode "Prefix" Core.<$> prefix
      Core.<> Core.toXMLNode "Tag" Core.<$> tag

instance Core.FromXML MetricsFilter where
  parseXML x =
    MetricsFilter'
      Core.<$> (x Core..@? "And")
      Core.<*> (x Core..@? "Prefix")
      Core.<*> (x Core..@? "Tag")
