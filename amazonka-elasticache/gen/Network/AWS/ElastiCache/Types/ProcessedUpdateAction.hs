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
-- Module      : Network.AWS.ElastiCache.Types.ProcessedUpdateAction
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElastiCache.Types.ProcessedUpdateAction where

import Network.AWS.ElastiCache.Types.UpdateActionStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Update action that has been processed for the corresponding apply\/stop
-- request
--
-- /See:/ 'newProcessedUpdateAction' smart constructor.
data ProcessedUpdateAction = ProcessedUpdateAction'
  { -- | The ID of the replication group
    replicationGroupId :: Prelude.Maybe Prelude.Text,
    -- | The status of the update action on the Redis cluster
    updateActionStatus :: Prelude.Maybe UpdateActionStatus,
    -- | The ID of the cache cluster
    cacheClusterId :: Prelude.Maybe Prelude.Text,
    -- | The unique ID of the service update
    serviceUpdateName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ProcessedUpdateAction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'replicationGroupId', 'processedUpdateAction_replicationGroupId' - The ID of the replication group
--
-- 'updateActionStatus', 'processedUpdateAction_updateActionStatus' - The status of the update action on the Redis cluster
--
-- 'cacheClusterId', 'processedUpdateAction_cacheClusterId' - The ID of the cache cluster
--
-- 'serviceUpdateName', 'processedUpdateAction_serviceUpdateName' - The unique ID of the service update
newProcessedUpdateAction ::
  ProcessedUpdateAction
newProcessedUpdateAction =
  ProcessedUpdateAction'
    { replicationGroupId =
        Prelude.Nothing,
      updateActionStatus = Prelude.Nothing,
      cacheClusterId = Prelude.Nothing,
      serviceUpdateName = Prelude.Nothing
    }

-- | The ID of the replication group
processedUpdateAction_replicationGroupId :: Lens.Lens' ProcessedUpdateAction (Prelude.Maybe Prelude.Text)
processedUpdateAction_replicationGroupId = Lens.lens (\ProcessedUpdateAction' {replicationGroupId} -> replicationGroupId) (\s@ProcessedUpdateAction' {} a -> s {replicationGroupId = a} :: ProcessedUpdateAction)

-- | The status of the update action on the Redis cluster
processedUpdateAction_updateActionStatus :: Lens.Lens' ProcessedUpdateAction (Prelude.Maybe UpdateActionStatus)
processedUpdateAction_updateActionStatus = Lens.lens (\ProcessedUpdateAction' {updateActionStatus} -> updateActionStatus) (\s@ProcessedUpdateAction' {} a -> s {updateActionStatus = a} :: ProcessedUpdateAction)

-- | The ID of the cache cluster
processedUpdateAction_cacheClusterId :: Lens.Lens' ProcessedUpdateAction (Prelude.Maybe Prelude.Text)
processedUpdateAction_cacheClusterId = Lens.lens (\ProcessedUpdateAction' {cacheClusterId} -> cacheClusterId) (\s@ProcessedUpdateAction' {} a -> s {cacheClusterId = a} :: ProcessedUpdateAction)

-- | The unique ID of the service update
processedUpdateAction_serviceUpdateName :: Lens.Lens' ProcessedUpdateAction (Prelude.Maybe Prelude.Text)
processedUpdateAction_serviceUpdateName = Lens.lens (\ProcessedUpdateAction' {serviceUpdateName} -> serviceUpdateName) (\s@ProcessedUpdateAction' {} a -> s {serviceUpdateName = a} :: ProcessedUpdateAction)

instance Prelude.FromXML ProcessedUpdateAction where
  parseXML x =
    ProcessedUpdateAction'
      Prelude.<$> (x Prelude..@? "ReplicationGroupId")
      Prelude.<*> (x Prelude..@? "UpdateActionStatus")
      Prelude.<*> (x Prelude..@? "CacheClusterId")
      Prelude.<*> (x Prelude..@? "ServiceUpdateName")

instance Prelude.Hashable ProcessedUpdateAction

instance Prelude.NFData ProcessedUpdateAction
