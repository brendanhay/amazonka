{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.MetricsAndOperator
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.S3.Types.MetricsAndOperator
  ( MetricsAndOperator (..)
  -- * Smart constructor
  , mkMetricsAndOperator
  -- * Lenses
  , maoPrefix
  , maoTags
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.S3.Internal as Types
import qualified Network.AWS.S3.Types.Prefix as Types
import qualified Network.AWS.S3.Types.Tag as Types

-- | A conjunction (logical AND) of predicates, which is used in evaluating a metrics filter. The operator must have at least two predicates, and an object must match all of the predicates in order for the filter to apply.
--
-- /See:/ 'mkMetricsAndOperator' smart constructor.
data MetricsAndOperator = MetricsAndOperator'
  { prefix :: Core.Maybe Types.Prefix
    -- ^ The prefix used when evaluating an AND predicate.
  , tags :: Core.Maybe [Types.Tag]
    -- ^ The list of tags used when evaluating an AND predicate.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'MetricsAndOperator' value with any optional fields omitted.
mkMetricsAndOperator
    :: MetricsAndOperator
mkMetricsAndOperator
  = MetricsAndOperator'{prefix = Core.Nothing, tags = Core.Nothing}

-- | The prefix used when evaluating an AND predicate.
--
-- /Note:/ Consider using 'prefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
maoPrefix :: Lens.Lens' MetricsAndOperator (Core.Maybe Types.Prefix)
maoPrefix = Lens.field @"prefix"
{-# INLINEABLE maoPrefix #-}
{-# DEPRECATED prefix "Use generic-lens or generic-optics with 'prefix' instead"  #-}

-- | The list of tags used when evaluating an AND predicate.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
maoTags :: Lens.Lens' MetricsAndOperator (Core.Maybe [Types.Tag])
maoTags = Lens.field @"tags"
{-# INLINEABLE maoTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

instance Core.ToXML MetricsAndOperator where
        toXML MetricsAndOperator{..}
          = Core.maybe Core.mempty (Core.toXMLElement "Prefix") prefix
              Core.<>
              Core.toXMLElement "Tag"
                (Core.maybe Core.mempty (Core.toXMLList "Tag") tags)

instance Core.FromXML MetricsAndOperator where
        parseXML x
          = MetricsAndOperator' Core.<$>
              (x Core..@? "Prefix") Core.<*>
                x Core..@? "Tag" Core..<@> Core.parseXMLList "Tag"
