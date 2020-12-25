{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.ReplicationRuleAndOperator
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.ReplicationRuleAndOperator
  ( ReplicationRuleAndOperator (..),

    -- * Smart constructor
    mkReplicationRuleAndOperator,

    -- * Lenses
    rraoPrefix,
    rraoTags,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.S3.Internal as Types
import qualified Network.AWS.S3.Types.Prefix as Types
import qualified Network.AWS.S3.Types.Tag as Types

-- | A container for specifying rule filters. The filters determine the subset of objects to which the rule applies. This element is required only if you specify more than one filter.
--
-- For example:
--
--     * If you specify both a @Prefix@ and a @Tag@ filter, wrap these filters in an @And@ tag.
--
--
--     * If you specify a filter based on multiple tags, wrap the @Tag@ elements in an @And@ tag
--
--
--
-- /See:/ 'mkReplicationRuleAndOperator' smart constructor.
data ReplicationRuleAndOperator = ReplicationRuleAndOperator'
  { -- | An object key name prefix that identifies the subset of objects to which the rule applies.
    prefix :: Core.Maybe Types.Prefix,
    -- | An array of tags containing key and value pairs.
    tags :: Core.Maybe [Types.Tag]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ReplicationRuleAndOperator' value with any optional fields omitted.
mkReplicationRuleAndOperator ::
  ReplicationRuleAndOperator
mkReplicationRuleAndOperator =
  ReplicationRuleAndOperator'
    { prefix = Core.Nothing,
      tags = Core.Nothing
    }

-- | An object key name prefix that identifies the subset of objects to which the rule applies.
--
-- /Note:/ Consider using 'prefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rraoPrefix :: Lens.Lens' ReplicationRuleAndOperator (Core.Maybe Types.Prefix)
rraoPrefix = Lens.field @"prefix"
{-# DEPRECATED rraoPrefix "Use generic-lens or generic-optics with 'prefix' instead." #-}

-- | An array of tags containing key and value pairs.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rraoTags :: Lens.Lens' ReplicationRuleAndOperator (Core.Maybe [Types.Tag])
rraoTags = Lens.field @"tags"
{-# DEPRECATED rraoTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Core.ToXML ReplicationRuleAndOperator where
  toXML ReplicationRuleAndOperator {..} =
    Core.toXMLNode "Prefix" Core.<$> prefix
      Core.<> Core.toXMLNode "Tag" (Core.toXMLList "Tag" Core.<$> tags)

instance Core.FromXML ReplicationRuleAndOperator where
  parseXML x =
    ReplicationRuleAndOperator'
      Core.<$> (x Core..@? "Prefix")
      Core.<*> (x Core..@? "Tag" Core..<@> Core.parseXMLList "Tag")
