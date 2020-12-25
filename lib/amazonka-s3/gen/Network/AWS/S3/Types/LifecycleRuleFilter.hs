{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.LifecycleRuleFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.LifecycleRuleFilter
  ( LifecycleRuleFilter (..),

    -- * Smart constructor
    mkLifecycleRuleFilter,

    -- * Lenses
    lrfAnd,
    lrfPrefix,
    lrfTag,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.S3.Internal as Types
import qualified Network.AWS.S3.Types.LifecycleRuleAndOperator as Types
import qualified Network.AWS.S3.Types.Prefix as Types
import qualified Network.AWS.S3.Types.Tag as Types

-- | The @Filter@ is used to identify objects that a Lifecycle Rule applies to. A @Filter@ must have exactly one of @Prefix@ , @Tag@ , or @And@ specified.
--
-- /See:/ 'mkLifecycleRuleFilter' smart constructor.
data LifecycleRuleFilter = LifecycleRuleFilter'
  { and :: Core.Maybe Types.LifecycleRuleAndOperator,
    -- | Prefix identifying one or more objects to which the rule applies.
    prefix :: Core.Maybe Types.Prefix,
    -- | This tag must exist in the object's tag set in order for the rule to apply.
    tag :: Core.Maybe Types.Tag
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'LifecycleRuleFilter' value with any optional fields omitted.
mkLifecycleRuleFilter ::
  LifecycleRuleFilter
mkLifecycleRuleFilter =
  LifecycleRuleFilter'
    { and = Core.Nothing,
      prefix = Core.Nothing,
      tag = Core.Nothing
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'and' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrfAnd :: Lens.Lens' LifecycleRuleFilter (Core.Maybe Types.LifecycleRuleAndOperator)
lrfAnd = Lens.field @"and"
{-# DEPRECATED lrfAnd "Use generic-lens or generic-optics with 'and' instead." #-}

-- | Prefix identifying one or more objects to which the rule applies.
--
-- /Note:/ Consider using 'prefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrfPrefix :: Lens.Lens' LifecycleRuleFilter (Core.Maybe Types.Prefix)
lrfPrefix = Lens.field @"prefix"
{-# DEPRECATED lrfPrefix "Use generic-lens or generic-optics with 'prefix' instead." #-}

-- | This tag must exist in the object's tag set in order for the rule to apply.
--
-- /Note:/ Consider using 'tag' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrfTag :: Lens.Lens' LifecycleRuleFilter (Core.Maybe Types.Tag)
lrfTag = Lens.field @"tag"
{-# DEPRECATED lrfTag "Use generic-lens or generic-optics with 'tag' instead." #-}

instance Core.ToXML LifecycleRuleFilter where
  toXML LifecycleRuleFilter {..} =
    Core.toXMLNode "And" Core.<$> and
      Core.<> Core.toXMLNode "Prefix" Core.<$> prefix
      Core.<> Core.toXMLNode "Tag" Core.<$> tag

instance Core.FromXML LifecycleRuleFilter where
  parseXML x =
    LifecycleRuleFilter'
      Core.<$> (x Core..@? "And")
      Core.<*> (x Core..@? "Prefix")
      Core.<*> (x Core..@? "Tag")
