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
-- Module      : Network.AWS.ElastiCache.Types.NodeGroupMemberUpdateStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElastiCache.Types.NodeGroupMemberUpdateStatus where

import qualified Network.AWS.Core as Core
import Network.AWS.ElastiCache.Types.NodeUpdateInitiatedBy
import Network.AWS.ElastiCache.Types.NodeUpdateStatus
import qualified Network.AWS.Lens as Lens

-- | The status of the service update on the node group member
--
-- /See:/ 'newNodeGroupMemberUpdateStatus' smart constructor.
data NodeGroupMemberUpdateStatus = NodeGroupMemberUpdateStatus'
  { -- | The cache cluster ID
    cacheClusterId :: Core.Maybe Core.Text,
    -- | The date when the NodeUpdateStatus was last modified
    nodeUpdateStatusModifiedDate :: Core.Maybe Core.ISO8601,
    -- | The update status of the node
    nodeUpdateStatus :: Core.Maybe NodeUpdateStatus,
    -- | Reflects whether the update was initiated by the customer or
    -- automatically applied
    nodeUpdateInitiatedBy :: Core.Maybe NodeUpdateInitiatedBy,
    -- | The date when the update is triggered
    nodeUpdateInitiatedDate :: Core.Maybe Core.ISO8601,
    -- | The node ID of the cache cluster
    cacheNodeId :: Core.Maybe Core.Text,
    -- | The deletion date of the node
    nodeDeletionDate :: Core.Maybe Core.ISO8601,
    -- | The start date of the update for a node
    nodeUpdateStartDate :: Core.Maybe Core.ISO8601,
    -- | The end date of the update for a node
    nodeUpdateEndDate :: Core.Maybe Core.ISO8601
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'NodeGroupMemberUpdateStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cacheClusterId', 'nodeGroupMemberUpdateStatus_cacheClusterId' - The cache cluster ID
--
-- 'nodeUpdateStatusModifiedDate', 'nodeGroupMemberUpdateStatus_nodeUpdateStatusModifiedDate' - The date when the NodeUpdateStatus was last modified
--
-- 'nodeUpdateStatus', 'nodeGroupMemberUpdateStatus_nodeUpdateStatus' - The update status of the node
--
-- 'nodeUpdateInitiatedBy', 'nodeGroupMemberUpdateStatus_nodeUpdateInitiatedBy' - Reflects whether the update was initiated by the customer or
-- automatically applied
--
-- 'nodeUpdateInitiatedDate', 'nodeGroupMemberUpdateStatus_nodeUpdateInitiatedDate' - The date when the update is triggered
--
-- 'cacheNodeId', 'nodeGroupMemberUpdateStatus_cacheNodeId' - The node ID of the cache cluster
--
-- 'nodeDeletionDate', 'nodeGroupMemberUpdateStatus_nodeDeletionDate' - The deletion date of the node
--
-- 'nodeUpdateStartDate', 'nodeGroupMemberUpdateStatus_nodeUpdateStartDate' - The start date of the update for a node
--
-- 'nodeUpdateEndDate', 'nodeGroupMemberUpdateStatus_nodeUpdateEndDate' - The end date of the update for a node
newNodeGroupMemberUpdateStatus ::
  NodeGroupMemberUpdateStatus
newNodeGroupMemberUpdateStatus =
  NodeGroupMemberUpdateStatus'
    { cacheClusterId =
        Core.Nothing,
      nodeUpdateStatusModifiedDate = Core.Nothing,
      nodeUpdateStatus = Core.Nothing,
      nodeUpdateInitiatedBy = Core.Nothing,
      nodeUpdateInitiatedDate = Core.Nothing,
      cacheNodeId = Core.Nothing,
      nodeDeletionDate = Core.Nothing,
      nodeUpdateStartDate = Core.Nothing,
      nodeUpdateEndDate = Core.Nothing
    }

-- | The cache cluster ID
nodeGroupMemberUpdateStatus_cacheClusterId :: Lens.Lens' NodeGroupMemberUpdateStatus (Core.Maybe Core.Text)
nodeGroupMemberUpdateStatus_cacheClusterId = Lens.lens (\NodeGroupMemberUpdateStatus' {cacheClusterId} -> cacheClusterId) (\s@NodeGroupMemberUpdateStatus' {} a -> s {cacheClusterId = a} :: NodeGroupMemberUpdateStatus)

-- | The date when the NodeUpdateStatus was last modified
nodeGroupMemberUpdateStatus_nodeUpdateStatusModifiedDate :: Lens.Lens' NodeGroupMemberUpdateStatus (Core.Maybe Core.UTCTime)
nodeGroupMemberUpdateStatus_nodeUpdateStatusModifiedDate = Lens.lens (\NodeGroupMemberUpdateStatus' {nodeUpdateStatusModifiedDate} -> nodeUpdateStatusModifiedDate) (\s@NodeGroupMemberUpdateStatus' {} a -> s {nodeUpdateStatusModifiedDate = a} :: NodeGroupMemberUpdateStatus) Core.. Lens.mapping Core._Time

-- | The update status of the node
nodeGroupMemberUpdateStatus_nodeUpdateStatus :: Lens.Lens' NodeGroupMemberUpdateStatus (Core.Maybe NodeUpdateStatus)
nodeGroupMemberUpdateStatus_nodeUpdateStatus = Lens.lens (\NodeGroupMemberUpdateStatus' {nodeUpdateStatus} -> nodeUpdateStatus) (\s@NodeGroupMemberUpdateStatus' {} a -> s {nodeUpdateStatus = a} :: NodeGroupMemberUpdateStatus)

-- | Reflects whether the update was initiated by the customer or
-- automatically applied
nodeGroupMemberUpdateStatus_nodeUpdateInitiatedBy :: Lens.Lens' NodeGroupMemberUpdateStatus (Core.Maybe NodeUpdateInitiatedBy)
nodeGroupMemberUpdateStatus_nodeUpdateInitiatedBy = Lens.lens (\NodeGroupMemberUpdateStatus' {nodeUpdateInitiatedBy} -> nodeUpdateInitiatedBy) (\s@NodeGroupMemberUpdateStatus' {} a -> s {nodeUpdateInitiatedBy = a} :: NodeGroupMemberUpdateStatus)

-- | The date when the update is triggered
nodeGroupMemberUpdateStatus_nodeUpdateInitiatedDate :: Lens.Lens' NodeGroupMemberUpdateStatus (Core.Maybe Core.UTCTime)
nodeGroupMemberUpdateStatus_nodeUpdateInitiatedDate = Lens.lens (\NodeGroupMemberUpdateStatus' {nodeUpdateInitiatedDate} -> nodeUpdateInitiatedDate) (\s@NodeGroupMemberUpdateStatus' {} a -> s {nodeUpdateInitiatedDate = a} :: NodeGroupMemberUpdateStatus) Core.. Lens.mapping Core._Time

-- | The node ID of the cache cluster
nodeGroupMemberUpdateStatus_cacheNodeId :: Lens.Lens' NodeGroupMemberUpdateStatus (Core.Maybe Core.Text)
nodeGroupMemberUpdateStatus_cacheNodeId = Lens.lens (\NodeGroupMemberUpdateStatus' {cacheNodeId} -> cacheNodeId) (\s@NodeGroupMemberUpdateStatus' {} a -> s {cacheNodeId = a} :: NodeGroupMemberUpdateStatus)

-- | The deletion date of the node
nodeGroupMemberUpdateStatus_nodeDeletionDate :: Lens.Lens' NodeGroupMemberUpdateStatus (Core.Maybe Core.UTCTime)
nodeGroupMemberUpdateStatus_nodeDeletionDate = Lens.lens (\NodeGroupMemberUpdateStatus' {nodeDeletionDate} -> nodeDeletionDate) (\s@NodeGroupMemberUpdateStatus' {} a -> s {nodeDeletionDate = a} :: NodeGroupMemberUpdateStatus) Core.. Lens.mapping Core._Time

-- | The start date of the update for a node
nodeGroupMemberUpdateStatus_nodeUpdateStartDate :: Lens.Lens' NodeGroupMemberUpdateStatus (Core.Maybe Core.UTCTime)
nodeGroupMemberUpdateStatus_nodeUpdateStartDate = Lens.lens (\NodeGroupMemberUpdateStatus' {nodeUpdateStartDate} -> nodeUpdateStartDate) (\s@NodeGroupMemberUpdateStatus' {} a -> s {nodeUpdateStartDate = a} :: NodeGroupMemberUpdateStatus) Core.. Lens.mapping Core._Time

-- | The end date of the update for a node
nodeGroupMemberUpdateStatus_nodeUpdateEndDate :: Lens.Lens' NodeGroupMemberUpdateStatus (Core.Maybe Core.UTCTime)
nodeGroupMemberUpdateStatus_nodeUpdateEndDate = Lens.lens (\NodeGroupMemberUpdateStatus' {nodeUpdateEndDate} -> nodeUpdateEndDate) (\s@NodeGroupMemberUpdateStatus' {} a -> s {nodeUpdateEndDate = a} :: NodeGroupMemberUpdateStatus) Core.. Lens.mapping Core._Time

instance Core.FromXML NodeGroupMemberUpdateStatus where
  parseXML x =
    NodeGroupMemberUpdateStatus'
      Core.<$> (x Core..@? "CacheClusterId")
      Core.<*> (x Core..@? "NodeUpdateStatusModifiedDate")
      Core.<*> (x Core..@? "NodeUpdateStatus")
      Core.<*> (x Core..@? "NodeUpdateInitiatedBy")
      Core.<*> (x Core..@? "NodeUpdateInitiatedDate")
      Core.<*> (x Core..@? "CacheNodeId")
      Core.<*> (x Core..@? "NodeDeletionDate")
      Core.<*> (x Core..@? "NodeUpdateStartDate")
      Core.<*> (x Core..@? "NodeUpdateEndDate")

instance Core.Hashable NodeGroupMemberUpdateStatus

instance Core.NFData NodeGroupMemberUpdateStatus
