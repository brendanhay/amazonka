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
import qualified Network.AWS.Prelude as Lude
import Network.AWS.S3.Internal
import Network.AWS.S3.Types.Tag

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
    prefix :: Lude.Maybe Lude.Text,
    -- | An array of tags containing key and value pairs.
    tags :: Lude.Maybe [Tag]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ReplicationRuleAndOperator' with the minimum fields required to make a request.
--
-- * 'prefix' - An object key name prefix that identifies the subset of objects to which the rule applies.
-- * 'tags' - An array of tags containing key and value pairs.
mkReplicationRuleAndOperator ::
  ReplicationRuleAndOperator
mkReplicationRuleAndOperator =
  ReplicationRuleAndOperator'
    { prefix = Lude.Nothing,
      tags = Lude.Nothing
    }

-- | An object key name prefix that identifies the subset of objects to which the rule applies.
--
-- /Note:/ Consider using 'prefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rraoPrefix :: Lens.Lens' ReplicationRuleAndOperator (Lude.Maybe Lude.Text)
rraoPrefix = Lens.lens (prefix :: ReplicationRuleAndOperator -> Lude.Maybe Lude.Text) (\s a -> s {prefix = a} :: ReplicationRuleAndOperator)
{-# DEPRECATED rraoPrefix "Use generic-lens or generic-optics with 'prefix' instead." #-}

-- | An array of tags containing key and value pairs.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rraoTags :: Lens.Lens' ReplicationRuleAndOperator (Lude.Maybe [Tag])
rraoTags = Lens.lens (tags :: ReplicationRuleAndOperator -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: ReplicationRuleAndOperator)
{-# DEPRECATED rraoTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.FromXML ReplicationRuleAndOperator where
  parseXML x =
    ReplicationRuleAndOperator'
      Lude.<$> (x Lude..@? "Prefix")
      Lude.<*> ( x Lude..@? "Tag" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "Tag")
               )

instance Lude.ToXML ReplicationRuleAndOperator where
  toXML ReplicationRuleAndOperator' {..} =
    Lude.mconcat
      [ "Prefix" Lude.@= prefix,
        "Tag" Lude.@= Lude.toXML (Lude.toXMLList "Tag" Lude.<$> tags)
      ]
