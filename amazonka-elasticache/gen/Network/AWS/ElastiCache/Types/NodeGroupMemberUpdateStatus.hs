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
-- Module      : Network.AWS.ElastiCache.Types.NodeGroupMemberUpdateStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElastiCache.Types.NodeGroupMemberUpdateStatus where

import Network.AWS.ElastiCache.Types.NodeUpdateInitiatedBy
import Network.AWS.ElastiCache.Types.NodeUpdateStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The status of the service update on the node group member
--
-- /See:/ 'newNodeGroupMemberUpdateStatus' smart constructor.
data NodeGroupMemberUpdateStatus = NodeGroupMemberUpdateStatus'
  { -- | The cache cluster ID
    cacheClusterId :: Prelude.Maybe Prelude.Text,
    -- | The date when the NodeUpdateStatus was last modified
    nodeUpdateStatusModifiedDate :: Prelude.Maybe Prelude.ISO8601,
    -- | The update status of the node
    nodeUpdateStatus :: Prelude.Maybe NodeUpdateStatus,
    -- | Reflects whether the update was initiated by the customer or
    -- automatically applied
    nodeUpdateInitiatedBy :: Prelude.Maybe NodeUpdateInitiatedBy,
    -- | The date when the update is triggered
    nodeUpdateInitiatedDate :: Prelude.Maybe Prelude.ISO8601,
    -- | The node ID of the cache cluster
    cacheNodeId :: Prelude.Maybe Prelude.Text,
    -- | The deletion date of the node
    nodeDeletionDate :: Prelude.Maybe Prelude.ISO8601,
    -- | The start date of the update for a node
    nodeUpdateStartDate :: Prelude.Maybe Prelude.ISO8601,
    -- | The end date of the update for a node
    nodeUpdateEndDate :: Prelude.Maybe Prelude.ISO8601
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
        Prelude.Nothing,
      nodeUpdateStatusModifiedDate = Prelude.Nothing,
      nodeUpdateStatus = Prelude.Nothing,
      nodeUpdateInitiatedBy = Prelude.Nothing,
      nodeUpdateInitiatedDate = Prelude.Nothing,
      cacheNodeId = Prelude.Nothing,
      nodeDeletionDate = Prelude.Nothing,
      nodeUpdateStartDate = Prelude.Nothing,
      nodeUpdateEndDate = Prelude.Nothing
    }

-- | The cache cluster ID
nodeGroupMemberUpdateStatus_cacheClusterId :: Lens.Lens' NodeGroupMemberUpdateStatus (Prelude.Maybe Prelude.Text)
nodeGroupMemberUpdateStatus_cacheClusterId = Lens.lens (\NodeGroupMemberUpdateStatus' {cacheClusterId} -> cacheClusterId) (\s@NodeGroupMemberUpdateStatus' {} a -> s {cacheClusterId = a} :: NodeGroupMemberUpdateStatus)

-- | The date when the NodeUpdateStatus was last modified
nodeGroupMemberUpdateStatus_nodeUpdateStatusModifiedDate :: Lens.Lens' NodeGroupMemberUpdateStatus (Prelude.Maybe Prelude.UTCTime)
nodeGroupMemberUpdateStatus_nodeUpdateStatusModifiedDate = Lens.lens (\NodeGroupMemberUpdateStatus' {nodeUpdateStatusModifiedDate} -> nodeUpdateStatusModifiedDate) (\s@NodeGroupMemberUpdateStatus' {} a -> s {nodeUpdateStatusModifiedDate = a} :: NodeGroupMemberUpdateStatus) Prelude.. Lens.mapping Prelude._Time

-- | The update status of the node
nodeGroupMemberUpdateStatus_nodeUpdateStatus :: Lens.Lens' NodeGroupMemberUpdateStatus (Prelude.Maybe NodeUpdateStatus)
nodeGroupMemberUpdateStatus_nodeUpdateStatus = Lens.lens (\NodeGroupMemberUpdateStatus' {nodeUpdateStatus} -> nodeUpdateStatus) (\s@NodeGroupMemberUpdateStatus' {} a -> s {nodeUpdateStatus = a} :: NodeGroupMemberUpdateStatus)

-- | Reflects whether the update was initiated by the customer or
-- automatically applied
nodeGroupMemberUpdateStatus_nodeUpdateInitiatedBy :: Lens.Lens' NodeGroupMemberUpdateStatus (Prelude.Maybe NodeUpdateInitiatedBy)
nodeGroupMemberUpdateStatus_nodeUpdateInitiatedBy = Lens.lens (\NodeGroupMemberUpdateStatus' {nodeUpdateInitiatedBy} -> nodeUpdateInitiatedBy) (\s@NodeGroupMemberUpdateStatus' {} a -> s {nodeUpdateInitiatedBy = a} :: NodeGroupMemberUpdateStatus)

-- | The date when the update is triggered
nodeGroupMemberUpdateStatus_nodeUpdateInitiatedDate :: Lens.Lens' NodeGroupMemberUpdateStatus (Prelude.Maybe Prelude.UTCTime)
nodeGroupMemberUpdateStatus_nodeUpdateInitiatedDate = Lens.lens (\NodeGroupMemberUpdateStatus' {nodeUpdateInitiatedDate} -> nodeUpdateInitiatedDate) (\s@NodeGroupMemberUpdateStatus' {} a -> s {nodeUpdateInitiatedDate = a} :: NodeGroupMemberUpdateStatus) Prelude.. Lens.mapping Prelude._Time

-- | The node ID of the cache cluster
nodeGroupMemberUpdateStatus_cacheNodeId :: Lens.Lens' NodeGroupMemberUpdateStatus (Prelude.Maybe Prelude.Text)
nodeGroupMemberUpdateStatus_cacheNodeId = Lens.lens (\NodeGroupMemberUpdateStatus' {cacheNodeId} -> cacheNodeId) (\s@NodeGroupMemberUpdateStatus' {} a -> s {cacheNodeId = a} :: NodeGroupMemberUpdateStatus)

-- | The deletion date of the node
nodeGroupMemberUpdateStatus_nodeDeletionDate :: Lens.Lens' NodeGroupMemberUpdateStatus (Prelude.Maybe Prelude.UTCTime)
nodeGroupMemberUpdateStatus_nodeDeletionDate = Lens.lens (\NodeGroupMemberUpdateStatus' {nodeDeletionDate} -> nodeDeletionDate) (\s@NodeGroupMemberUpdateStatus' {} a -> s {nodeDeletionDate = a} :: NodeGroupMemberUpdateStatus) Prelude.. Lens.mapping Prelude._Time

-- | The start date of the update for a node
nodeGroupMemberUpdateStatus_nodeUpdateStartDate :: Lens.Lens' NodeGroupMemberUpdateStatus (Prelude.Maybe Prelude.UTCTime)
nodeGroupMemberUpdateStatus_nodeUpdateStartDate = Lens.lens (\NodeGroupMemberUpdateStatus' {nodeUpdateStartDate} -> nodeUpdateStartDate) (\s@NodeGroupMemberUpdateStatus' {} a -> s {nodeUpdateStartDate = a} :: NodeGroupMemberUpdateStatus) Prelude.. Lens.mapping Prelude._Time

-- | The end date of the update for a node
nodeGroupMemberUpdateStatus_nodeUpdateEndDate :: Lens.Lens' NodeGroupMemberUpdateStatus (Prelude.Maybe Prelude.UTCTime)
nodeGroupMemberUpdateStatus_nodeUpdateEndDate = Lens.lens (\NodeGroupMemberUpdateStatus' {nodeUpdateEndDate} -> nodeUpdateEndDate) (\s@NodeGroupMemberUpdateStatus' {} a -> s {nodeUpdateEndDate = a} :: NodeGroupMemberUpdateStatus) Prelude.. Lens.mapping Prelude._Time

instance Prelude.FromXML NodeGroupMemberUpdateStatus where
  parseXML x =
    NodeGroupMemberUpdateStatus'
      Prelude.<$> (x Prelude..@? "CacheClusterId")
      Prelude.<*> (x Prelude..@? "NodeUpdateStatusModifiedDate")
      Prelude.<*> (x Prelude..@? "NodeUpdateStatus")
      Prelude.<*> (x Prelude..@? "NodeUpdateInitiatedBy")
      Prelude.<*> (x Prelude..@? "NodeUpdateInitiatedDate")
      Prelude.<*> (x Prelude..@? "CacheNodeId")
      Prelude.<*> (x Prelude..@? "NodeDeletionDate")
      Prelude.<*> (x Prelude..@? "NodeUpdateStartDate")
      Prelude.<*> (x Prelude..@? "NodeUpdateEndDate")

instance Prelude.Hashable NodeGroupMemberUpdateStatus

instance Prelude.NFData NodeGroupMemberUpdateStatus
