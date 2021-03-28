{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.ReplicationRuleFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.S3.Types.ReplicationRuleFilter
  ( ReplicationRuleFilter (..)
  -- * Smart constructor
  , mkReplicationRuleFilter
  -- * Lenses
  , rrfAnd
  , rrfPrefix
  , rrfTag
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.S3.Internal as Types
import qualified Network.AWS.S3.Types.Prefix as Types
import qualified Network.AWS.S3.Types.ReplicationRuleAndOperator as Types
import qualified Network.AWS.S3.Types.Tag as Types

-- | A filter that identifies the subset of objects to which the replication rule applies. A @Filter@ must specify exactly one @Prefix@ , @Tag@ , or an @And@ child element.
--
-- /See:/ 'mkReplicationRuleFilter' smart constructor.
data ReplicationRuleFilter = ReplicationRuleFilter'
  { and :: Core.Maybe Types.ReplicationRuleAndOperator
    -- ^ A container for specifying rule filters. The filters determine the subset of objects to which the rule applies. This element is required only if you specify more than one filter. For example: 
--
--
--     * If you specify both a @Prefix@ and a @Tag@ filter, wrap these filters in an @And@ tag.
--
--
--     * If you specify a filter based on multiple tags, wrap the @Tag@ elements in an @And@ tag.
--
--
  , prefix :: Core.Maybe Types.Prefix
    -- ^ An object key name prefix that identifies the subset of objects to which the rule applies.
  , tag :: Core.Maybe Types.Tag
    -- ^ A container for specifying a tag key and value. 
--
-- The rule applies only to objects that have the tag in their tag set.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ReplicationRuleFilter' value with any optional fields omitted.
mkReplicationRuleFilter
    :: ReplicationRuleFilter
mkReplicationRuleFilter
  = ReplicationRuleFilter'{and = Core.Nothing, prefix = Core.Nothing,
                           tag = Core.Nothing}

-- | A container for specifying rule filters. The filters determine the subset of objects to which the rule applies. This element is required only if you specify more than one filter. For example: 
--
--
--     * If you specify both a @Prefix@ and a @Tag@ filter, wrap these filters in an @And@ tag.
--
--
--     * If you specify a filter based on multiple tags, wrap the @Tag@ elements in an @And@ tag.
--
--
--
-- /Note:/ Consider using 'and' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrfAnd :: Lens.Lens' ReplicationRuleFilter (Core.Maybe Types.ReplicationRuleAndOperator)
rrfAnd = Lens.field @"and"
{-# INLINEABLE rrfAnd #-}
{-# DEPRECATED and "Use generic-lens or generic-optics with 'and' instead"  #-}

-- | An object key name prefix that identifies the subset of objects to which the rule applies.
--
-- /Note:/ Consider using 'prefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrfPrefix :: Lens.Lens' ReplicationRuleFilter (Core.Maybe Types.Prefix)
rrfPrefix = Lens.field @"prefix"
{-# INLINEABLE rrfPrefix #-}
{-# DEPRECATED prefix "Use generic-lens or generic-optics with 'prefix' instead"  #-}

-- | A container for specifying a tag key and value. 
--
-- The rule applies only to objects that have the tag in their tag set.
--
-- /Note:/ Consider using 'tag' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrfTag :: Lens.Lens' ReplicationRuleFilter (Core.Maybe Types.Tag)
rrfTag = Lens.field @"tag"
{-# INLINEABLE rrfTag #-}
{-# DEPRECATED tag "Use generic-lens or generic-optics with 'tag' instead"  #-}

instance Core.ToXML ReplicationRuleFilter where
        toXML ReplicationRuleFilter{..}
          = Core.maybe Core.mempty (Core.toXMLElement "And") and Core.<>
              Core.maybe Core.mempty (Core.toXMLElement "Prefix") prefix
              Core.<> Core.maybe Core.mempty (Core.toXMLElement "Tag") tag

instance Core.FromXML ReplicationRuleFilter where
        parseXML x
          = ReplicationRuleFilter' Core.<$>
              (x Core..@? "And") Core.<*> x Core..@? "Prefix" Core.<*>
                x Core..@? "Tag"
