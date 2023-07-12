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
-- Module      : Amazonka.ECR.Types.ReplicationConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ECR.Types.ReplicationConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ECR.Types.ReplicationRule
import qualified Amazonka.Prelude as Prelude

-- | The replication configuration for a registry.
--
-- /See:/ 'newReplicationConfiguration' smart constructor.
data ReplicationConfiguration = ReplicationConfiguration'
  { -- | An array of objects representing the replication destinations and
    -- repository filters for a replication configuration.
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
-- 'rules', 'replicationConfiguration_rules' - An array of objects representing the replication destinations and
-- repository filters for a replication configuration.
newReplicationConfiguration ::
  ReplicationConfiguration
newReplicationConfiguration =
  ReplicationConfiguration' {rules = Prelude.mempty}

-- | An array of objects representing the replication destinations and
-- repository filters for a replication configuration.
replicationConfiguration_rules :: Lens.Lens' ReplicationConfiguration [ReplicationRule]
replicationConfiguration_rules = Lens.lens (\ReplicationConfiguration' {rules} -> rules) (\s@ReplicationConfiguration' {} a -> s {rules = a} :: ReplicationConfiguration) Prelude.. Lens.coerced

instance Data.FromJSON ReplicationConfiguration where
  parseJSON =
    Data.withObject
      "ReplicationConfiguration"
      ( \x ->
          ReplicationConfiguration'
            Prelude.<$> (x Data..:? "rules" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable ReplicationConfiguration where
  hashWithSalt _salt ReplicationConfiguration' {..} =
    _salt `Prelude.hashWithSalt` rules

instance Prelude.NFData ReplicationConfiguration where
  rnf ReplicationConfiguration' {..} = Prelude.rnf rules

instance Data.ToJSON ReplicationConfiguration where
  toJSON ReplicationConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("rules" Data..= rules)]
      )
