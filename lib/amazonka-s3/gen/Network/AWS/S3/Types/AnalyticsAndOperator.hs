{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.AnalyticsAndOperator
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.S3.Types.AnalyticsAndOperator
  ( AnalyticsAndOperator (..)
  -- * Smart constructor
  , mkAnalyticsAndOperator
  -- * Lenses
  , aaoPrefix
  , aaoTags
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.S3.Internal as Types
import qualified Network.AWS.S3.Types.Prefix as Types
import qualified Network.AWS.S3.Types.Tag as Types

-- | A conjunction (logical AND) of predicates, which is used in evaluating a metrics filter. The operator must have at least two predicates in any combination, and an object must match all of the predicates for the filter to apply.
--
-- /See:/ 'mkAnalyticsAndOperator' smart constructor.
data AnalyticsAndOperator = AnalyticsAndOperator'
  { prefix :: Core.Maybe Types.Prefix
    -- ^ The prefix to use when evaluating an AND predicate: The prefix that an object must have to be included in the metrics results.
  , tags :: Core.Maybe [Types.Tag]
    -- ^ The list of tags to use when evaluating an AND predicate.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AnalyticsAndOperator' value with any optional fields omitted.
mkAnalyticsAndOperator
    :: AnalyticsAndOperator
mkAnalyticsAndOperator
  = AnalyticsAndOperator'{prefix = Core.Nothing, tags = Core.Nothing}

-- | The prefix to use when evaluating an AND predicate: The prefix that an object must have to be included in the metrics results.
--
-- /Note:/ Consider using 'prefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aaoPrefix :: Lens.Lens' AnalyticsAndOperator (Core.Maybe Types.Prefix)
aaoPrefix = Lens.field @"prefix"
{-# INLINEABLE aaoPrefix #-}
{-# DEPRECATED prefix "Use generic-lens or generic-optics with 'prefix' instead"  #-}

-- | The list of tags to use when evaluating an AND predicate.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aaoTags :: Lens.Lens' AnalyticsAndOperator (Core.Maybe [Types.Tag])
aaoTags = Lens.field @"tags"
{-# INLINEABLE aaoTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

instance Core.ToXML AnalyticsAndOperator where
        toXML AnalyticsAndOperator{..}
          = Core.maybe Core.mempty (Core.toXMLElement "Prefix") prefix
              Core.<>
              Core.toXMLElement "Tag"
                (Core.maybe Core.mempty (Core.toXMLList "Tag") tags)

instance Core.FromXML AnalyticsAndOperator where
        parseXML x
          = AnalyticsAndOperator' Core.<$>
              (x Core..@? "Prefix") Core.<*>
                x Core..@? "Tag" Core..<@> Core.parseXMLList "Tag"
