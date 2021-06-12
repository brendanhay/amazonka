{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.ReplicationRuleAndOperator
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.ReplicationRuleAndOperator where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.S3.Internal
import Network.AWS.S3.Types.Tag

-- | A container for specifying rule filters. The filters determine the
-- subset of objects to which the rule applies. This element is required
-- only if you specify more than one filter.
--
-- For example:
--
-- -   If you specify both a @Prefix@ and a @Tag@ filter, wrap these
--     filters in an @And@ tag.
--
-- -   If you specify a filter based on multiple tags, wrap the @Tag@
--     elements in an @And@ tag
--
-- /See:/ 'newReplicationRuleAndOperator' smart constructor.
data ReplicationRuleAndOperator = ReplicationRuleAndOperator'
  { -- | An object key name prefix that identifies the subset of objects to which
    -- the rule applies.
    prefix :: Core.Maybe Core.Text,
    -- | An array of tags containing key and value pairs.
    tags :: Core.Maybe [Tag]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ReplicationRuleAndOperator' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'prefix', 'replicationRuleAndOperator_prefix' - An object key name prefix that identifies the subset of objects to which
-- the rule applies.
--
-- 'tags', 'replicationRuleAndOperator_tags' - An array of tags containing key and value pairs.
newReplicationRuleAndOperator ::
  ReplicationRuleAndOperator
newReplicationRuleAndOperator =
  ReplicationRuleAndOperator'
    { prefix = Core.Nothing,
      tags = Core.Nothing
    }

-- | An object key name prefix that identifies the subset of objects to which
-- the rule applies.
replicationRuleAndOperator_prefix :: Lens.Lens' ReplicationRuleAndOperator (Core.Maybe Core.Text)
replicationRuleAndOperator_prefix = Lens.lens (\ReplicationRuleAndOperator' {prefix} -> prefix) (\s@ReplicationRuleAndOperator' {} a -> s {prefix = a} :: ReplicationRuleAndOperator)

-- | An array of tags containing key and value pairs.
replicationRuleAndOperator_tags :: Lens.Lens' ReplicationRuleAndOperator (Core.Maybe [Tag])
replicationRuleAndOperator_tags = Lens.lens (\ReplicationRuleAndOperator' {tags} -> tags) (\s@ReplicationRuleAndOperator' {} a -> s {tags = a} :: ReplicationRuleAndOperator) Core.. Lens.mapping Lens._Coerce

instance Core.FromXML ReplicationRuleAndOperator where
  parseXML x =
    ReplicationRuleAndOperator'
      Core.<$> (x Core..@? "Prefix")
      Core.<*> ( x Core..@? "Tag" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "Tag")
               )

instance Core.Hashable ReplicationRuleAndOperator

instance Core.NFData ReplicationRuleAndOperator

instance Core.ToXML ReplicationRuleAndOperator where
  toXML ReplicationRuleAndOperator' {..} =
    Core.mconcat
      [ "Prefix" Core.@= prefix,
        "Tag"
          Core.@= Core.toXML (Core.toXMLList "Tag" Core.<$> tags)
      ]
