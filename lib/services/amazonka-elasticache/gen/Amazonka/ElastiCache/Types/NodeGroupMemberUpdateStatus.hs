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
-- Module      : Amazonka.ElastiCache.Types.NodeGroupMemberUpdateStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ElastiCache.Types.NodeGroupMemberUpdateStatus where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ElastiCache.Types.NodeUpdateInitiatedBy
import Amazonka.ElastiCache.Types.NodeUpdateStatus
import qualified Amazonka.Prelude as Prelude

-- | The status of the service update on the node group member
--
-- /See:/ 'newNodeGroupMemberUpdateStatus' smart constructor.
data NodeGroupMemberUpdateStatus = NodeGroupMemberUpdateStatus'
  { -- | The update status of the node
    nodeUpdateStatus :: Prelude.Maybe NodeUpdateStatus,
    -- | The end date of the update for a node
    nodeUpdateEndDate :: Prelude.Maybe Data.ISO8601,
    -- | The start date of the update for a node
    nodeUpdateStartDate :: Prelude.Maybe Data.ISO8601,
    -- | The deletion date of the node
    nodeDeletionDate :: Prelude.Maybe Data.ISO8601,
    -- | Reflects whether the update was initiated by the customer or
    -- automatically applied
    nodeUpdateInitiatedBy :: Prelude.Maybe NodeUpdateInitiatedBy,
    -- | The date when the update is triggered
    nodeUpdateInitiatedDate :: Prelude.Maybe Data.ISO8601,
    -- | The cache cluster ID
    cacheClusterId :: Prelude.Maybe Prelude.Text,
    -- | The date when the NodeUpdateStatus was last modified
    nodeUpdateStatusModifiedDate :: Prelude.Maybe Data.ISO8601,
    -- | The node ID of the cache cluster
    cacheNodeId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'NodeGroupMemberUpdateStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nodeUpdateStatus', 'nodeGroupMemberUpdateStatus_nodeUpdateStatus' - The update status of the node
--
-- 'nodeUpdateEndDate', 'nodeGroupMemberUpdateStatus_nodeUpdateEndDate' - The end date of the update for a node
--
-- 'nodeUpdateStartDate', 'nodeGroupMemberUpdateStatus_nodeUpdateStartDate' - The start date of the update for a node
--
-- 'nodeDeletionDate', 'nodeGroupMemberUpdateStatus_nodeDeletionDate' - The deletion date of the node
--
-- 'nodeUpdateInitiatedBy', 'nodeGroupMemberUpdateStatus_nodeUpdateInitiatedBy' - Reflects whether the update was initiated by the customer or
-- automatically applied
--
-- 'nodeUpdateInitiatedDate', 'nodeGroupMemberUpdateStatus_nodeUpdateInitiatedDate' - The date when the update is triggered
--
-- 'cacheClusterId', 'nodeGroupMemberUpdateStatus_cacheClusterId' - The cache cluster ID
--
-- 'nodeUpdateStatusModifiedDate', 'nodeGroupMemberUpdateStatus_nodeUpdateStatusModifiedDate' - The date when the NodeUpdateStatus was last modified
--
-- 'cacheNodeId', 'nodeGroupMemberUpdateStatus_cacheNodeId' - The node ID of the cache cluster
newNodeGroupMemberUpdateStatus ::
  NodeGroupMemberUpdateStatus
newNodeGroupMemberUpdateStatus =
  NodeGroupMemberUpdateStatus'
    { nodeUpdateStatus =
        Prelude.Nothing,
      nodeUpdateEndDate = Prelude.Nothing,
      nodeUpdateStartDate = Prelude.Nothing,
      nodeDeletionDate = Prelude.Nothing,
      nodeUpdateInitiatedBy = Prelude.Nothing,
      nodeUpdateInitiatedDate = Prelude.Nothing,
      cacheClusterId = Prelude.Nothing,
      nodeUpdateStatusModifiedDate = Prelude.Nothing,
      cacheNodeId = Prelude.Nothing
    }

-- | The update status of the node
nodeGroupMemberUpdateStatus_nodeUpdateStatus :: Lens.Lens' NodeGroupMemberUpdateStatus (Prelude.Maybe NodeUpdateStatus)
nodeGroupMemberUpdateStatus_nodeUpdateStatus = Lens.lens (\NodeGroupMemberUpdateStatus' {nodeUpdateStatus} -> nodeUpdateStatus) (\s@NodeGroupMemberUpdateStatus' {} a -> s {nodeUpdateStatus = a} :: NodeGroupMemberUpdateStatus)

-- | The end date of the update for a node
nodeGroupMemberUpdateStatus_nodeUpdateEndDate :: Lens.Lens' NodeGroupMemberUpdateStatus (Prelude.Maybe Prelude.UTCTime)
nodeGroupMemberUpdateStatus_nodeUpdateEndDate = Lens.lens (\NodeGroupMemberUpdateStatus' {nodeUpdateEndDate} -> nodeUpdateEndDate) (\s@NodeGroupMemberUpdateStatus' {} a -> s {nodeUpdateEndDate = a} :: NodeGroupMemberUpdateStatus) Prelude.. Lens.mapping Data._Time

-- | The start date of the update for a node
nodeGroupMemberUpdateStatus_nodeUpdateStartDate :: Lens.Lens' NodeGroupMemberUpdateStatus (Prelude.Maybe Prelude.UTCTime)
nodeGroupMemberUpdateStatus_nodeUpdateStartDate = Lens.lens (\NodeGroupMemberUpdateStatus' {nodeUpdateStartDate} -> nodeUpdateStartDate) (\s@NodeGroupMemberUpdateStatus' {} a -> s {nodeUpdateStartDate = a} :: NodeGroupMemberUpdateStatus) Prelude.. Lens.mapping Data._Time

-- | The deletion date of the node
nodeGroupMemberUpdateStatus_nodeDeletionDate :: Lens.Lens' NodeGroupMemberUpdateStatus (Prelude.Maybe Prelude.UTCTime)
nodeGroupMemberUpdateStatus_nodeDeletionDate = Lens.lens (\NodeGroupMemberUpdateStatus' {nodeDeletionDate} -> nodeDeletionDate) (\s@NodeGroupMemberUpdateStatus' {} a -> s {nodeDeletionDate = a} :: NodeGroupMemberUpdateStatus) Prelude.. Lens.mapping Data._Time

-- | Reflects whether the update was initiated by the customer or
-- automatically applied
nodeGroupMemberUpdateStatus_nodeUpdateInitiatedBy :: Lens.Lens' NodeGroupMemberUpdateStatus (Prelude.Maybe NodeUpdateInitiatedBy)
nodeGroupMemberUpdateStatus_nodeUpdateInitiatedBy = Lens.lens (\NodeGroupMemberUpdateStatus' {nodeUpdateInitiatedBy} -> nodeUpdateInitiatedBy) (\s@NodeGroupMemberUpdateStatus' {} a -> s {nodeUpdateInitiatedBy = a} :: NodeGroupMemberUpdateStatus)

-- | The date when the update is triggered
nodeGroupMemberUpdateStatus_nodeUpdateInitiatedDate :: Lens.Lens' NodeGroupMemberUpdateStatus (Prelude.Maybe Prelude.UTCTime)
nodeGroupMemberUpdateStatus_nodeUpdateInitiatedDate = Lens.lens (\NodeGroupMemberUpdateStatus' {nodeUpdateInitiatedDate} -> nodeUpdateInitiatedDate) (\s@NodeGroupMemberUpdateStatus' {} a -> s {nodeUpdateInitiatedDate = a} :: NodeGroupMemberUpdateStatus) Prelude.. Lens.mapping Data._Time

-- | The cache cluster ID
nodeGroupMemberUpdateStatus_cacheClusterId :: Lens.Lens' NodeGroupMemberUpdateStatus (Prelude.Maybe Prelude.Text)
nodeGroupMemberUpdateStatus_cacheClusterId = Lens.lens (\NodeGroupMemberUpdateStatus' {cacheClusterId} -> cacheClusterId) (\s@NodeGroupMemberUpdateStatus' {} a -> s {cacheClusterId = a} :: NodeGroupMemberUpdateStatus)

-- | The date when the NodeUpdateStatus was last modified
nodeGroupMemberUpdateStatus_nodeUpdateStatusModifiedDate :: Lens.Lens' NodeGroupMemberUpdateStatus (Prelude.Maybe Prelude.UTCTime)
nodeGroupMemberUpdateStatus_nodeUpdateStatusModifiedDate = Lens.lens (\NodeGroupMemberUpdateStatus' {nodeUpdateStatusModifiedDate} -> nodeUpdateStatusModifiedDate) (\s@NodeGroupMemberUpdateStatus' {} a -> s {nodeUpdateStatusModifiedDate = a} :: NodeGroupMemberUpdateStatus) Prelude.. Lens.mapping Data._Time

-- | The node ID of the cache cluster
nodeGroupMemberUpdateStatus_cacheNodeId :: Lens.Lens' NodeGroupMemberUpdateStatus (Prelude.Maybe Prelude.Text)
nodeGroupMemberUpdateStatus_cacheNodeId = Lens.lens (\NodeGroupMemberUpdateStatus' {cacheNodeId} -> cacheNodeId) (\s@NodeGroupMemberUpdateStatus' {} a -> s {cacheNodeId = a} :: NodeGroupMemberUpdateStatus)

instance Data.FromXML NodeGroupMemberUpdateStatus where
  parseXML x =
    NodeGroupMemberUpdateStatus'
      Prelude.<$> (x Data..@? "NodeUpdateStatus")
      Prelude.<*> (x Data..@? "NodeUpdateEndDate")
      Prelude.<*> (x Data..@? "NodeUpdateStartDate")
      Prelude.<*> (x Data..@? "NodeDeletionDate")
      Prelude.<*> (x Data..@? "NodeUpdateInitiatedBy")
      Prelude.<*> (x Data..@? "NodeUpdateInitiatedDate")
      Prelude.<*> (x Data..@? "CacheClusterId")
      Prelude.<*> (x Data..@? "NodeUpdateStatusModifiedDate")
      Prelude.<*> (x Data..@? "CacheNodeId")

instance Prelude.Hashable NodeGroupMemberUpdateStatus where
  hashWithSalt _salt NodeGroupMemberUpdateStatus' {..} =
    _salt `Prelude.hashWithSalt` nodeUpdateStatus
      `Prelude.hashWithSalt` nodeUpdateEndDate
      `Prelude.hashWithSalt` nodeUpdateStartDate
      `Prelude.hashWithSalt` nodeDeletionDate
      `Prelude.hashWithSalt` nodeUpdateInitiatedBy
      `Prelude.hashWithSalt` nodeUpdateInitiatedDate
      `Prelude.hashWithSalt` cacheClusterId
      `Prelude.hashWithSalt` nodeUpdateStatusModifiedDate
      `Prelude.hashWithSalt` cacheNodeId

instance Prelude.NFData NodeGroupMemberUpdateStatus where
  rnf NodeGroupMemberUpdateStatus' {..} =
    Prelude.rnf nodeUpdateStatus
      `Prelude.seq` Prelude.rnf nodeUpdateEndDate
      `Prelude.seq` Prelude.rnf nodeUpdateStartDate
      `Prelude.seq` Prelude.rnf nodeDeletionDate
      `Prelude.seq` Prelude.rnf nodeUpdateInitiatedBy
      `Prelude.seq` Prelude.rnf nodeUpdateInitiatedDate
      `Prelude.seq` Prelude.rnf cacheClusterId
      `Prelude.seq` Prelude.rnf nodeUpdateStatusModifiedDate
      `Prelude.seq` Prelude.rnf cacheNodeId
