{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.LifecycleRuleAndOperator
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.S3.Types.LifecycleRuleAndOperator
  ( LifecycleRuleAndOperator (..)
  -- * Smart constructor
  , mkLifecycleRuleAndOperator
  -- * Lenses
  , lraoPrefix
  , lraoTags
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.S3.Internal as Types
import qualified Network.AWS.S3.Types.Prefix as Types
import qualified Network.AWS.S3.Types.Tag as Types

-- | This is used in a Lifecycle Rule Filter to apply a logical AND to two or more predicates. The Lifecycle Rule will apply to any object matching all of the predicates configured inside the And operator.
--
-- /See:/ 'mkLifecycleRuleAndOperator' smart constructor.
data LifecycleRuleAndOperator = LifecycleRuleAndOperator'
  { prefix :: Core.Maybe Types.Prefix
    -- ^ Prefix identifying one or more objects to which the rule applies.
  , tags :: Core.Maybe [Types.Tag]
    -- ^ All of these tags must exist in the object's tag set in order for the rule to apply.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'LifecycleRuleAndOperator' value with any optional fields omitted.
mkLifecycleRuleAndOperator
    :: LifecycleRuleAndOperator
mkLifecycleRuleAndOperator
  = LifecycleRuleAndOperator'{prefix = Core.Nothing,
                              tags = Core.Nothing}

-- | Prefix identifying one or more objects to which the rule applies.
--
-- /Note:/ Consider using 'prefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lraoPrefix :: Lens.Lens' LifecycleRuleAndOperator (Core.Maybe Types.Prefix)
lraoPrefix = Lens.field @"prefix"
{-# INLINEABLE lraoPrefix #-}
{-# DEPRECATED prefix "Use generic-lens or generic-optics with 'prefix' instead"  #-}

-- | All of these tags must exist in the object's tag set in order for the rule to apply.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lraoTags :: Lens.Lens' LifecycleRuleAndOperator (Core.Maybe [Types.Tag])
lraoTags = Lens.field @"tags"
{-# INLINEABLE lraoTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

instance Core.ToXML LifecycleRuleAndOperator where
        toXML LifecycleRuleAndOperator{..}
          = Core.maybe Core.mempty (Core.toXMLElement "Prefix") prefix
              Core.<>
              Core.toXMLElement "Tag"
                (Core.maybe Core.mempty (Core.toXMLList "Tag") tags)

instance Core.FromXML LifecycleRuleAndOperator where
        parseXML x
          = LifecycleRuleAndOperator' Core.<$>
              (x Core..@? "Prefix") Core.<*>
                x Core..@? "Tag" Core..<@> Core.parseXMLList "Tag"
