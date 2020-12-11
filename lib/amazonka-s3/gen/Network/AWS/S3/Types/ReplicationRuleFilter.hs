-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.ReplicationRuleFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.ReplicationRuleFilter
  ( ReplicationRuleFilter (..),

    -- * Smart constructor
    mkReplicationRuleFilter,

    -- * Lenses
    rrfTag,
    rrfPrefix,
    rrfAnd,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.S3.Internal
import Network.AWS.S3.Types.ReplicationRuleAndOperator
import Network.AWS.S3.Types.Tag

-- | A filter that identifies the subset of objects to which the replication rule applies. A @Filter@ must specify exactly one @Prefix@ , @Tag@ , or an @And@ child element.
--
-- /See:/ 'mkReplicationRuleFilter' smart constructor.
data ReplicationRuleFilter = ReplicationRuleFilter'
  { tag ::
      Lude.Maybe Tag,
    prefix :: Lude.Maybe Lude.Text,
    and :: Lude.Maybe ReplicationRuleAndOperator
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ReplicationRuleFilter' with the minimum fields required to make a request.
--
-- * 'and' - A container for specifying rule filters. The filters determine the subset of objects to which the rule applies. This element is required only if you specify more than one filter. For example:
--
--
--     * If you specify both a @Prefix@ and a @Tag@ filter, wrap these filters in an @And@ tag.
--
--
--     * If you specify a filter based on multiple tags, wrap the @Tag@ elements in an @And@ tag.
--
--
-- * 'prefix' - An object key name prefix that identifies the subset of objects to which the rule applies.
-- * 'tag' - A container for specifying a tag key and value.
--
-- The rule applies only to objects that have the tag in their tag set.
mkReplicationRuleFilter ::
  ReplicationRuleFilter
mkReplicationRuleFilter =
  ReplicationRuleFilter'
    { tag = Lude.Nothing,
      prefix = Lude.Nothing,
      and = Lude.Nothing
    }

-- | A container for specifying a tag key and value.
--
-- The rule applies only to objects that have the tag in their tag set.
--
-- /Note:/ Consider using 'tag' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrfTag :: Lens.Lens' ReplicationRuleFilter (Lude.Maybe Tag)
rrfTag = Lens.lens (tag :: ReplicationRuleFilter -> Lude.Maybe Tag) (\s a -> s {tag = a} :: ReplicationRuleFilter)
{-# DEPRECATED rrfTag "Use generic-lens or generic-optics with 'tag' instead." #-}

-- | An object key name prefix that identifies the subset of objects to which the rule applies.
--
-- /Note:/ Consider using 'prefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrfPrefix :: Lens.Lens' ReplicationRuleFilter (Lude.Maybe Lude.Text)
rrfPrefix = Lens.lens (prefix :: ReplicationRuleFilter -> Lude.Maybe Lude.Text) (\s a -> s {prefix = a} :: ReplicationRuleFilter)
{-# DEPRECATED rrfPrefix "Use generic-lens or generic-optics with 'prefix' instead." #-}

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
rrfAnd :: Lens.Lens' ReplicationRuleFilter (Lude.Maybe ReplicationRuleAndOperator)
rrfAnd = Lens.lens (and :: ReplicationRuleFilter -> Lude.Maybe ReplicationRuleAndOperator) (\s a -> s {and = a} :: ReplicationRuleFilter)
{-# DEPRECATED rrfAnd "Use generic-lens or generic-optics with 'and' instead." #-}

instance Lude.FromXML ReplicationRuleFilter where
  parseXML x =
    ReplicationRuleFilter'
      Lude.<$> (x Lude..@? "Tag")
      Lude.<*> (x Lude..@? "Prefix")
      Lude.<*> (x Lude..@? "And")

instance Lude.ToXML ReplicationRuleFilter where
  toXML ReplicationRuleFilter' {..} =
    Lude.mconcat
      ["Tag" Lude.@= tag, "Prefix" Lude.@= prefix, "And" Lude.@= and]
