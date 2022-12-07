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
-- Module      : Amazonka.MemoryDb.Types.ReplicaConfigurationRequest
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MemoryDb.Types.ReplicaConfigurationRequest where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A request to configure the number of replicas in a shard
--
-- /See:/ 'newReplicaConfigurationRequest' smart constructor.
data ReplicaConfigurationRequest = ReplicaConfigurationRequest'
  { -- | The number of replicas to scale up or down to
    replicaCount :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ReplicaConfigurationRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'replicaCount', 'replicaConfigurationRequest_replicaCount' - The number of replicas to scale up or down to
newReplicaConfigurationRequest ::
  ReplicaConfigurationRequest
newReplicaConfigurationRequest =
  ReplicaConfigurationRequest'
    { replicaCount =
        Prelude.Nothing
    }

-- | The number of replicas to scale up or down to
replicaConfigurationRequest_replicaCount :: Lens.Lens' ReplicaConfigurationRequest (Prelude.Maybe Prelude.Int)
replicaConfigurationRequest_replicaCount = Lens.lens (\ReplicaConfigurationRequest' {replicaCount} -> replicaCount) (\s@ReplicaConfigurationRequest' {} a -> s {replicaCount = a} :: ReplicaConfigurationRequest)

instance Prelude.Hashable ReplicaConfigurationRequest where
  hashWithSalt _salt ReplicaConfigurationRequest' {..} =
    _salt `Prelude.hashWithSalt` replicaCount

instance Prelude.NFData ReplicaConfigurationRequest where
  rnf ReplicaConfigurationRequest' {..} =
    Prelude.rnf replicaCount

instance Data.ToJSON ReplicaConfigurationRequest where
  toJSON ReplicaConfigurationRequest' {..} =
    Data.object
      ( Prelude.catMaybes
          [("ReplicaCount" Data..=) Prelude.<$> replicaCount]
      )
