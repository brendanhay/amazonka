{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
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
    prefix :: Prelude.Maybe Prelude.Text,
    -- | An array of tags containing key and value pairs.
    tags :: Prelude.Maybe [Tag]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { prefix =
        Prelude.Nothing,
      tags = Prelude.Nothing
    }

-- | An object key name prefix that identifies the subset of objects to which
-- the rule applies.
replicationRuleAndOperator_prefix :: Lens.Lens' ReplicationRuleAndOperator (Prelude.Maybe Prelude.Text)
replicationRuleAndOperator_prefix = Lens.lens (\ReplicationRuleAndOperator' {prefix} -> prefix) (\s@ReplicationRuleAndOperator' {} a -> s {prefix = a} :: ReplicationRuleAndOperator)

-- | An array of tags containing key and value pairs.
replicationRuleAndOperator_tags :: Lens.Lens' ReplicationRuleAndOperator (Prelude.Maybe [Tag])
replicationRuleAndOperator_tags = Lens.lens (\ReplicationRuleAndOperator' {tags} -> tags) (\s@ReplicationRuleAndOperator' {} a -> s {tags = a} :: ReplicationRuleAndOperator) Prelude.. Lens.mapping Prelude._Coerce

instance Prelude.FromXML ReplicationRuleAndOperator where
  parseXML x =
    ReplicationRuleAndOperator'
      Prelude.<$> (x Prelude..@? "Prefix")
      Prelude.<*> ( x Prelude..@? "Tag" Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may (Prelude.parseXMLList "Tag")
                  )

instance Prelude.Hashable ReplicationRuleAndOperator

instance Prelude.NFData ReplicationRuleAndOperator

instance Prelude.ToXML ReplicationRuleAndOperator where
  toXML ReplicationRuleAndOperator' {..} =
    Prelude.mconcat
      [ "Prefix" Prelude.@= prefix,
        "Tag"
          Prelude.@= Prelude.toXML
            (Prelude.toXMLList "Tag" Prelude.<$> tags)
      ]
