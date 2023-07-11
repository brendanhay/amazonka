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
-- Module      : Amazonka.S3.Types.ReplicationConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.S3.Types.ReplicationConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.S3.Internal
import Amazonka.S3.Types.ReplicationRule

-- | A container for replication rules. You can add up to 1,000 rules. The
-- maximum size of a replication configuration is 2 MB.
--
-- /See:/ 'newReplicationConfiguration' smart constructor.
data ReplicationConfiguration = ReplicationConfiguration'
  { -- | The Amazon Resource Name (ARN) of the Identity and Access Management
    -- (IAM) role that Amazon S3 assumes when replicating objects. For more
    -- information, see
    -- <https://docs.aws.amazon.com/AmazonS3/latest/dev/replication-how-setup.html How to Set Up Replication>
    -- in the /Amazon S3 User Guide/.
    role' :: Prelude.Text,
    -- | A container for one or more replication rules. A replication
    -- configuration must have at least one rule and can contain a maximum of
    -- 1,000 rules.
    rules :: [ReplicationRule]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ReplicationConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'role'', 'replicationConfiguration_role' - The Amazon Resource Name (ARN) of the Identity and Access Management
-- (IAM) role that Amazon S3 assumes when replicating objects. For more
-- information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/replication-how-setup.html How to Set Up Replication>
-- in the /Amazon S3 User Guide/.
--
-- 'rules', 'replicationConfiguration_rules' - A container for one or more replication rules. A replication
-- configuration must have at least one rule and can contain a maximum of
-- 1,000 rules.
newReplicationConfiguration ::
  -- | 'role''
  Prelude.Text ->
  ReplicationConfiguration
newReplicationConfiguration pRole_ =
  ReplicationConfiguration'
    { role' = pRole_,
      rules = Prelude.mempty
    }

-- | The Amazon Resource Name (ARN) of the Identity and Access Management
-- (IAM) role that Amazon S3 assumes when replicating objects. For more
-- information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/replication-how-setup.html How to Set Up Replication>
-- in the /Amazon S3 User Guide/.
replicationConfiguration_role :: Lens.Lens' ReplicationConfiguration Prelude.Text
replicationConfiguration_role = Lens.lens (\ReplicationConfiguration' {role'} -> role') (\s@ReplicationConfiguration' {} a -> s {role' = a} :: ReplicationConfiguration)

-- | A container for one or more replication rules. A replication
-- configuration must have at least one rule and can contain a maximum of
-- 1,000 rules.
replicationConfiguration_rules :: Lens.Lens' ReplicationConfiguration [ReplicationRule]
replicationConfiguration_rules = Lens.lens (\ReplicationConfiguration' {rules} -> rules) (\s@ReplicationConfiguration' {} a -> s {rules = a} :: ReplicationConfiguration) Prelude.. Lens.coerced

instance Data.FromXML ReplicationConfiguration where
  parseXML x =
    ReplicationConfiguration'
      Prelude.<$> (x Data..@ "Role")
      Prelude.<*> (Data.parseXMLList "Rule" x)

instance Prelude.Hashable ReplicationConfiguration where
  hashWithSalt _salt ReplicationConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` role'
      `Prelude.hashWithSalt` rules

instance Prelude.NFData ReplicationConfiguration where
  rnf ReplicationConfiguration' {..} =
    Prelude.rnf role' `Prelude.seq` Prelude.rnf rules

instance Data.ToXML ReplicationConfiguration where
  toXML ReplicationConfiguration' {..} =
    Prelude.mconcat
      ["Role" Data.@= role', Data.toXMLList "Rule" rules]
