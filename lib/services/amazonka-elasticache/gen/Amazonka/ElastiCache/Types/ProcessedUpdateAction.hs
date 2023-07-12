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
-- Module      : Amazonka.ElastiCache.Types.ProcessedUpdateAction
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ElastiCache.Types.ProcessedUpdateAction where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ElastiCache.Types.UpdateActionStatus
import qualified Amazonka.Prelude as Prelude

-- | Update action that has been processed for the corresponding apply\/stop
-- request
--
-- /See:/ 'newProcessedUpdateAction' smart constructor.
data ProcessedUpdateAction = ProcessedUpdateAction'
  { -- | The ID of the cache cluster
    cacheClusterId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the replication group
    replicationGroupId :: Prelude.Maybe Prelude.Text,
    -- | The unique ID of the service update
    serviceUpdateName :: Prelude.Maybe Prelude.Text,
    -- | The status of the update action on the Redis cluster
    updateActionStatus :: Prelude.Maybe UpdateActionStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ProcessedUpdateAction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cacheClusterId', 'processedUpdateAction_cacheClusterId' - The ID of the cache cluster
--
-- 'replicationGroupId', 'processedUpdateAction_replicationGroupId' - The ID of the replication group
--
-- 'serviceUpdateName', 'processedUpdateAction_serviceUpdateName' - The unique ID of the service update
--
-- 'updateActionStatus', 'processedUpdateAction_updateActionStatus' - The status of the update action on the Redis cluster
newProcessedUpdateAction ::
  ProcessedUpdateAction
newProcessedUpdateAction =
  ProcessedUpdateAction'
    { cacheClusterId =
        Prelude.Nothing,
      replicationGroupId = Prelude.Nothing,
      serviceUpdateName = Prelude.Nothing,
      updateActionStatus = Prelude.Nothing
    }

-- | The ID of the cache cluster
processedUpdateAction_cacheClusterId :: Lens.Lens' ProcessedUpdateAction (Prelude.Maybe Prelude.Text)
processedUpdateAction_cacheClusterId = Lens.lens (\ProcessedUpdateAction' {cacheClusterId} -> cacheClusterId) (\s@ProcessedUpdateAction' {} a -> s {cacheClusterId = a} :: ProcessedUpdateAction)

-- | The ID of the replication group
processedUpdateAction_replicationGroupId :: Lens.Lens' ProcessedUpdateAction (Prelude.Maybe Prelude.Text)
processedUpdateAction_replicationGroupId = Lens.lens (\ProcessedUpdateAction' {replicationGroupId} -> replicationGroupId) (\s@ProcessedUpdateAction' {} a -> s {replicationGroupId = a} :: ProcessedUpdateAction)

-- | The unique ID of the service update
processedUpdateAction_serviceUpdateName :: Lens.Lens' ProcessedUpdateAction (Prelude.Maybe Prelude.Text)
processedUpdateAction_serviceUpdateName = Lens.lens (\ProcessedUpdateAction' {serviceUpdateName} -> serviceUpdateName) (\s@ProcessedUpdateAction' {} a -> s {serviceUpdateName = a} :: ProcessedUpdateAction)

-- | The status of the update action on the Redis cluster
processedUpdateAction_updateActionStatus :: Lens.Lens' ProcessedUpdateAction (Prelude.Maybe UpdateActionStatus)
processedUpdateAction_updateActionStatus = Lens.lens (\ProcessedUpdateAction' {updateActionStatus} -> updateActionStatus) (\s@ProcessedUpdateAction' {} a -> s {updateActionStatus = a} :: ProcessedUpdateAction)

instance Data.FromXML ProcessedUpdateAction where
  parseXML x =
    ProcessedUpdateAction'
      Prelude.<$> (x Data..@? "CacheClusterId")
      Prelude.<*> (x Data..@? "ReplicationGroupId")
      Prelude.<*> (x Data..@? "ServiceUpdateName")
      Prelude.<*> (x Data..@? "UpdateActionStatus")

instance Prelude.Hashable ProcessedUpdateAction where
  hashWithSalt _salt ProcessedUpdateAction' {..} =
    _salt
      `Prelude.hashWithSalt` cacheClusterId
      `Prelude.hashWithSalt` replicationGroupId
      `Prelude.hashWithSalt` serviceUpdateName
      `Prelude.hashWithSalt` updateActionStatus

instance Prelude.NFData ProcessedUpdateAction where
  rnf ProcessedUpdateAction' {..} =
    Prelude.rnf cacheClusterId
      `Prelude.seq` Prelude.rnf replicationGroupId
      `Prelude.seq` Prelude.rnf serviceUpdateName
      `Prelude.seq` Prelude.rnf updateActionStatus
