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
-- Module      : Network.AWS.S3.Types.ReplicationConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.ReplicationConfiguration where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.S3.Internal
import Network.AWS.S3.Types.ReplicationRule

-- | A container for replication rules. You can add up to 1,000 rules. The
-- maximum size of a replication configuration is 2 MB.
--
-- /See:/ 'newReplicationConfiguration' smart constructor.
data ReplicationConfiguration = ReplicationConfiguration'
  { -- | The Amazon Resource Name (ARN) of the AWS Identity and Access Management
    -- (IAM) role that Amazon S3 assumes when replicating objects. For more
    -- information, see
    -- <https://docs.aws.amazon.com/AmazonS3/latest/dev/replication-how-setup.html How to Set Up Replication>
    -- in the /Amazon Simple Storage Service Developer Guide/.
    role' :: Core.Text,
    -- | A container for one or more replication rules. A replication
    -- configuration must have at least one rule and can contain a maximum of
    -- 1,000 rules.
    rules :: [ReplicationRule]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ReplicationConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'role'', 'replicationConfiguration_role' - The Amazon Resource Name (ARN) of the AWS Identity and Access Management
-- (IAM) role that Amazon S3 assumes when replicating objects. For more
-- information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/replication-how-setup.html How to Set Up Replication>
-- in the /Amazon Simple Storage Service Developer Guide/.
--
-- 'rules', 'replicationConfiguration_rules' - A container for one or more replication rules. A replication
-- configuration must have at least one rule and can contain a maximum of
-- 1,000 rules.
newReplicationConfiguration ::
  -- | 'role''
  Core.Text ->
  ReplicationConfiguration
newReplicationConfiguration pRole_ =
  ReplicationConfiguration'
    { role' = pRole_,
      rules = Core.mempty
    }

-- | The Amazon Resource Name (ARN) of the AWS Identity and Access Management
-- (IAM) role that Amazon S3 assumes when replicating objects. For more
-- information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/replication-how-setup.html How to Set Up Replication>
-- in the /Amazon Simple Storage Service Developer Guide/.
replicationConfiguration_role :: Lens.Lens' ReplicationConfiguration Core.Text
replicationConfiguration_role = Lens.lens (\ReplicationConfiguration' {role'} -> role') (\s@ReplicationConfiguration' {} a -> s {role' = a} :: ReplicationConfiguration)

-- | A container for one or more replication rules. A replication
-- configuration must have at least one rule and can contain a maximum of
-- 1,000 rules.
replicationConfiguration_rules :: Lens.Lens' ReplicationConfiguration [ReplicationRule]
replicationConfiguration_rules = Lens.lens (\ReplicationConfiguration' {rules} -> rules) (\s@ReplicationConfiguration' {} a -> s {rules = a} :: ReplicationConfiguration) Core.. Lens._Coerce

instance Core.FromXML ReplicationConfiguration where
  parseXML x =
    ReplicationConfiguration'
      Core.<$> (x Core..@ "Role")
      Core.<*> (Core.parseXMLList "Rule" x)

instance Core.Hashable ReplicationConfiguration

instance Core.NFData ReplicationConfiguration

instance Core.ToXML ReplicationConfiguration where
  toXML ReplicationConfiguration' {..} =
    Core.mconcat
      ["Role" Core.@= role', Core.toXMLList "Rule" rules]
