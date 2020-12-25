{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.IntelligentTieringFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.IntelligentTieringFilter
  ( IntelligentTieringFilter (..),

    -- * Smart constructor
    mkIntelligentTieringFilter,

    -- * Lenses
    itfAnd,
    itfPrefix,
    itfTag,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.S3.Internal as Types
import qualified Network.AWS.S3.Types.IntelligentTieringAndOperator as Types
import qualified Network.AWS.S3.Types.Prefix as Types
import qualified Network.AWS.S3.Types.Tag as Types

-- | The @Filter@ is used to identify objects that the S3 Intelligent-Tiering configuration applies to.
--
-- /See:/ 'mkIntelligentTieringFilter' smart constructor.
data IntelligentTieringFilter = IntelligentTieringFilter'
  { -- | A conjunction (logical AND) of predicates, which is used in evaluating a metrics filter. The operator must have at least two predicates, and an object must match all of the predicates in order for the filter to apply.
    and :: Core.Maybe Types.IntelligentTieringAndOperator,
    -- | An object key name prefix that identifies the subset of objects to which the rule applies.
    prefix :: Core.Maybe Types.Prefix,
    tag :: Core.Maybe Types.Tag
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'IntelligentTieringFilter' value with any optional fields omitted.
mkIntelligentTieringFilter ::
  IntelligentTieringFilter
mkIntelligentTieringFilter =
  IntelligentTieringFilter'
    { and = Core.Nothing,
      prefix = Core.Nothing,
      tag = Core.Nothing
    }

-- | A conjunction (logical AND) of predicates, which is used in evaluating a metrics filter. The operator must have at least two predicates, and an object must match all of the predicates in order for the filter to apply.
--
-- /Note:/ Consider using 'and' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
itfAnd :: Lens.Lens' IntelligentTieringFilter (Core.Maybe Types.IntelligentTieringAndOperator)
itfAnd = Lens.field @"and"
{-# DEPRECATED itfAnd "Use generic-lens or generic-optics with 'and' instead." #-}

-- | An object key name prefix that identifies the subset of objects to which the rule applies.
--
-- /Note:/ Consider using 'prefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
itfPrefix :: Lens.Lens' IntelligentTieringFilter (Core.Maybe Types.Prefix)
itfPrefix = Lens.field @"prefix"
{-# DEPRECATED itfPrefix "Use generic-lens or generic-optics with 'prefix' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'tag' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
itfTag :: Lens.Lens' IntelligentTieringFilter (Core.Maybe Types.Tag)
itfTag = Lens.field @"tag"
{-# DEPRECATED itfTag "Use generic-lens or generic-optics with 'tag' instead." #-}

instance Core.ToXML IntelligentTieringFilter where
  toXML IntelligentTieringFilter {..} =
    Core.toXMLNode "And" Core.<$> and
      Core.<> Core.toXMLNode "Prefix" Core.<$> prefix
      Core.<> Core.toXMLNode "Tag" Core.<$> tag

instance Core.FromXML IntelligentTieringFilter where
  parseXML x =
    IntelligentTieringFilter'
      Core.<$> (x Core..@? "And")
      Core.<*> (x Core..@? "Prefix")
      Core.<*> (x Core..@? "Tag")
