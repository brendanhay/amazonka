{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.AnalyticsFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.S3.Types.AnalyticsFilter
  ( AnalyticsFilter (..)
  -- * Smart constructor
  , mkAnalyticsFilter
  -- * Lenses
  , afAnd
  , afPrefix
  , afTag
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.S3.Internal as Types
import qualified Network.AWS.S3.Types.AnalyticsAndOperator as Types
import qualified Network.AWS.S3.Types.Prefix as Types
import qualified Network.AWS.S3.Types.Tag as Types

-- | The filter used to describe a set of objects for analyses. A filter must have exactly one prefix, one tag, or one conjunction (AnalyticsAndOperator). If no filter is provided, all objects will be considered in any analysis.
--
-- /See:/ 'mkAnalyticsFilter' smart constructor.
data AnalyticsFilter = AnalyticsFilter'
  { and :: Core.Maybe Types.AnalyticsAndOperator
    -- ^ A conjunction (logical AND) of predicates, which is used in evaluating an analytics filter. The operator must have at least two predicates.
  , prefix :: Core.Maybe Types.Prefix
    -- ^ The prefix to use when evaluating an analytics filter.
  , tag :: Core.Maybe Types.Tag
    -- ^ The tag to use when evaluating an analytics filter.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AnalyticsFilter' value with any optional fields omitted.
mkAnalyticsFilter
    :: AnalyticsFilter
mkAnalyticsFilter
  = AnalyticsFilter'{and = Core.Nothing, prefix = Core.Nothing,
                     tag = Core.Nothing}

-- | A conjunction (logical AND) of predicates, which is used in evaluating an analytics filter. The operator must have at least two predicates.
--
-- /Note:/ Consider using 'and' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
afAnd :: Lens.Lens' AnalyticsFilter (Core.Maybe Types.AnalyticsAndOperator)
afAnd = Lens.field @"and"
{-# INLINEABLE afAnd #-}
{-# DEPRECATED and "Use generic-lens or generic-optics with 'and' instead"  #-}

-- | The prefix to use when evaluating an analytics filter.
--
-- /Note:/ Consider using 'prefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
afPrefix :: Lens.Lens' AnalyticsFilter (Core.Maybe Types.Prefix)
afPrefix = Lens.field @"prefix"
{-# INLINEABLE afPrefix #-}
{-# DEPRECATED prefix "Use generic-lens or generic-optics with 'prefix' instead"  #-}

-- | The tag to use when evaluating an analytics filter.
--
-- /Note:/ Consider using 'tag' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
afTag :: Lens.Lens' AnalyticsFilter (Core.Maybe Types.Tag)
afTag = Lens.field @"tag"
{-# INLINEABLE afTag #-}
{-# DEPRECATED tag "Use generic-lens or generic-optics with 'tag' instead"  #-}

instance Core.ToXML AnalyticsFilter where
        toXML AnalyticsFilter{..}
          = Core.maybe Core.mempty (Core.toXMLElement "And") and Core.<>
              Core.maybe Core.mempty (Core.toXMLElement "Prefix") prefix
              Core.<> Core.maybe Core.mempty (Core.toXMLElement "Tag") tag

instance Core.FromXML AnalyticsFilter where
        parseXML x
          = AnalyticsFilter' Core.<$>
              (x Core..@? "And") Core.<*> x Core..@? "Prefix" Core.<*>
                x Core..@? "Tag"
