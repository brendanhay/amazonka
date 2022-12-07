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
-- Module      : Amazonka.S3.Types.ReplicationRuleFilter
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.S3.Types.ReplicationRuleFilter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.S3.Internal
import Amazonka.S3.Types.ReplicationRuleAndOperator
import Amazonka.S3.Types.Tag

-- | A filter that identifies the subset of objects to which the replication
-- rule applies. A @Filter@ must specify exactly one @Prefix@, @Tag@, or an
-- @And@ child element.
--
-- /See:/ 'newReplicationRuleFilter' smart constructor.
data ReplicationRuleFilter = ReplicationRuleFilter'
  { -- | A container for specifying a tag key and value.
    --
    -- The rule applies only to objects that have the tag in their tag set.
    tag :: Prelude.Maybe Tag,
    -- | An object key name prefix that identifies the subset of objects to which
    -- the rule applies.
    --
    -- Replacement must be made for object keys containing special characters
    -- (such as carriage returns) when using XML requests. For more
    -- information, see
    -- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/object-keys.html#object-key-xml-related-constraints XML related object key constraints>.
    prefix :: Prelude.Maybe Prelude.Text,
    -- | A container for specifying rule filters. The filters determine the
    -- subset of objects to which the rule applies. This element is required
    -- only if you specify more than one filter. For example:
    --
    -- -   If you specify both a @Prefix@ and a @Tag@ filter, wrap these
    --     filters in an @And@ tag.
    --
    -- -   If you specify a filter based on multiple tags, wrap the @Tag@
    --     elements in an @And@ tag.
    and :: Prelude.Maybe ReplicationRuleAndOperator
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ReplicationRuleFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tag', 'replicationRuleFilter_tag' - A container for specifying a tag key and value.
--
-- The rule applies only to objects that have the tag in their tag set.
--
-- 'prefix', 'replicationRuleFilter_prefix' - An object key name prefix that identifies the subset of objects to which
-- the rule applies.
--
-- Replacement must be made for object keys containing special characters
-- (such as carriage returns) when using XML requests. For more
-- information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/object-keys.html#object-key-xml-related-constraints XML related object key constraints>.
--
-- 'and', 'replicationRuleFilter_and' - A container for specifying rule filters. The filters determine the
-- subset of objects to which the rule applies. This element is required
-- only if you specify more than one filter. For example:
--
-- -   If you specify both a @Prefix@ and a @Tag@ filter, wrap these
--     filters in an @And@ tag.
--
-- -   If you specify a filter based on multiple tags, wrap the @Tag@
--     elements in an @And@ tag.
newReplicationRuleFilter ::
  ReplicationRuleFilter
newReplicationRuleFilter =
  ReplicationRuleFilter'
    { tag = Prelude.Nothing,
      prefix = Prelude.Nothing,
      and = Prelude.Nothing
    }

-- | A container for specifying a tag key and value.
--
-- The rule applies only to objects that have the tag in their tag set.
replicationRuleFilter_tag :: Lens.Lens' ReplicationRuleFilter (Prelude.Maybe Tag)
replicationRuleFilter_tag = Lens.lens (\ReplicationRuleFilter' {tag} -> tag) (\s@ReplicationRuleFilter' {} a -> s {tag = a} :: ReplicationRuleFilter)

-- | An object key name prefix that identifies the subset of objects to which
-- the rule applies.
--
-- Replacement must be made for object keys containing special characters
-- (such as carriage returns) when using XML requests. For more
-- information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/object-keys.html#object-key-xml-related-constraints XML related object key constraints>.
replicationRuleFilter_prefix :: Lens.Lens' ReplicationRuleFilter (Prelude.Maybe Prelude.Text)
replicationRuleFilter_prefix = Lens.lens (\ReplicationRuleFilter' {prefix} -> prefix) (\s@ReplicationRuleFilter' {} a -> s {prefix = a} :: ReplicationRuleFilter)

-- | A container for specifying rule filters. The filters determine the
-- subset of objects to which the rule applies. This element is required
-- only if you specify more than one filter. For example:
--
-- -   If you specify both a @Prefix@ and a @Tag@ filter, wrap these
--     filters in an @And@ tag.
--
-- -   If you specify a filter based on multiple tags, wrap the @Tag@
--     elements in an @And@ tag.
replicationRuleFilter_and :: Lens.Lens' ReplicationRuleFilter (Prelude.Maybe ReplicationRuleAndOperator)
replicationRuleFilter_and = Lens.lens (\ReplicationRuleFilter' {and} -> and) (\s@ReplicationRuleFilter' {} a -> s {and = a} :: ReplicationRuleFilter)

instance Data.FromXML ReplicationRuleFilter where
  parseXML x =
    ReplicationRuleFilter'
      Prelude.<$> (x Data..@? "Tag")
      Prelude.<*> (x Data..@? "Prefix")
      Prelude.<*> (x Data..@? "And")

instance Prelude.Hashable ReplicationRuleFilter where
  hashWithSalt _salt ReplicationRuleFilter' {..} =
    _salt `Prelude.hashWithSalt` tag
      `Prelude.hashWithSalt` prefix
      `Prelude.hashWithSalt` and

instance Prelude.NFData ReplicationRuleFilter where
  rnf ReplicationRuleFilter' {..} =
    Prelude.rnf tag
      `Prelude.seq` Prelude.rnf prefix
      `Prelude.seq` Prelude.rnf and

instance Data.ToXML ReplicationRuleFilter where
  toXML ReplicationRuleFilter' {..} =
    Prelude.mconcat
      [ "Tag" Data.@= tag,
        "Prefix" Data.@= prefix,
        "And" Data.@= and
      ]
