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
-- Module      : Network.AWS.MemoryDb.Types.ReplicaConfigurationRequest
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MemoryDb.Types.ReplicaConfigurationRequest where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

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

instance Prelude.Hashable ReplicaConfigurationRequest

instance Prelude.NFData ReplicaConfigurationRequest

instance Core.ToJSON ReplicaConfigurationRequest where
  toJSON ReplicaConfigurationRequest' {..} =
    Core.object
      ( Prelude.catMaybes
          [("ReplicaCount" Core..=) Prelude.<$> replicaCount]
      )
