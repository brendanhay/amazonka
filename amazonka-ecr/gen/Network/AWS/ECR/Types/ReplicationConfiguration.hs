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
-- Module      : Network.AWS.ECR.Types.ReplicationConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECR.Types.ReplicationConfiguration where

import Network.AWS.ECR.Types.ReplicationRule
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The replication configuration for a registry.
--
-- /See:/ 'newReplicationConfiguration' smart constructor.
data ReplicationConfiguration = ReplicationConfiguration'
  { -- | An array of objects representing the replication rules for a replication
    -- configuration. A replication configuration may contain only one
    -- replication rule but the rule may contain one or more replication
    -- destinations.
    rules :: [ReplicationRule]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ReplicationConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'rules', 'replicationConfiguration_rules' - An array of objects representing the replication rules for a replication
-- configuration. A replication configuration may contain only one
-- replication rule but the rule may contain one or more replication
-- destinations.
newReplicationConfiguration ::
  ReplicationConfiguration
newReplicationConfiguration =
  ReplicationConfiguration' {rules = Prelude.mempty}

-- | An array of objects representing the replication rules for a replication
-- configuration. A replication configuration may contain only one
-- replication rule but the rule may contain one or more replication
-- destinations.
replicationConfiguration_rules :: Lens.Lens' ReplicationConfiguration [ReplicationRule]
replicationConfiguration_rules = Lens.lens (\ReplicationConfiguration' {rules} -> rules) (\s@ReplicationConfiguration' {} a -> s {rules = a} :: ReplicationConfiguration) Prelude.. Prelude._Coerce

instance Prelude.FromJSON ReplicationConfiguration where
  parseJSON =
    Prelude.withObject
      "ReplicationConfiguration"
      ( \x ->
          ReplicationConfiguration'
            Prelude.<$> (x Prelude..:? "rules" Prelude..!= Prelude.mempty)
      )

instance Prelude.Hashable ReplicationConfiguration

instance Prelude.NFData ReplicationConfiguration

instance Prelude.ToJSON ReplicationConfiguration where
  toJSON ReplicationConfiguration' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [Prelude.Just ("rules" Prelude..= rules)]
      )
