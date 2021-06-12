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
-- Module      : Network.AWS.ElastiCache.Types.CacheNodeUpdateStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElastiCache.Types.CacheNodeUpdateStatus where

import qualified Network.AWS.Core as Core
import Network.AWS.ElastiCache.Types.NodeUpdateInitiatedBy
import Network.AWS.ElastiCache.Types.NodeUpdateStatus
import qualified Network.AWS.Lens as Lens

-- | The status of the service update on the cache node
--
-- /See:/ 'newCacheNodeUpdateStatus' smart constructor.
data CacheNodeUpdateStatus = CacheNodeUpdateStatus'
  { -- | The date when the NodeUpdateStatus was last modified>
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
-- Create a value of 'CacheNodeUpdateStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nodeUpdateStatusModifiedDate', 'cacheNodeUpdateStatus_nodeUpdateStatusModifiedDate' - The date when the NodeUpdateStatus was last modified>
--
-- 'nodeUpdateStatus', 'cacheNodeUpdateStatus_nodeUpdateStatus' - The update status of the node
--
-- 'nodeUpdateInitiatedBy', 'cacheNodeUpdateStatus_nodeUpdateInitiatedBy' - Reflects whether the update was initiated by the customer or
-- automatically applied
--
-- 'nodeUpdateInitiatedDate', 'cacheNodeUpdateStatus_nodeUpdateInitiatedDate' - The date when the update is triggered
--
-- 'cacheNodeId', 'cacheNodeUpdateStatus_cacheNodeId' - The node ID of the cache cluster
--
-- 'nodeDeletionDate', 'cacheNodeUpdateStatus_nodeDeletionDate' - The deletion date of the node
--
-- 'nodeUpdateStartDate', 'cacheNodeUpdateStatus_nodeUpdateStartDate' - The start date of the update for a node
--
-- 'nodeUpdateEndDate', 'cacheNodeUpdateStatus_nodeUpdateEndDate' - The end date of the update for a node
newCacheNodeUpdateStatus ::
  CacheNodeUpdateStatus
newCacheNodeUpdateStatus =
  CacheNodeUpdateStatus'
    { nodeUpdateStatusModifiedDate =
        Core.Nothing,
      nodeUpdateStatus = Core.Nothing,
      nodeUpdateInitiatedBy = Core.Nothing,
      nodeUpdateInitiatedDate = Core.Nothing,
      cacheNodeId = Core.Nothing,
      nodeDeletionDate = Core.Nothing,
      nodeUpdateStartDate = Core.Nothing,
      nodeUpdateEndDate = Core.Nothing
    }

-- | The date when the NodeUpdateStatus was last modified>
cacheNodeUpdateStatus_nodeUpdateStatusModifiedDate :: Lens.Lens' CacheNodeUpdateStatus (Core.Maybe Core.UTCTime)
cacheNodeUpdateStatus_nodeUpdateStatusModifiedDate = Lens.lens (\CacheNodeUpdateStatus' {nodeUpdateStatusModifiedDate} -> nodeUpdateStatusModifiedDate) (\s@CacheNodeUpdateStatus' {} a -> s {nodeUpdateStatusModifiedDate = a} :: CacheNodeUpdateStatus) Core.. Lens.mapping Core._Time

-- | The update status of the node
cacheNodeUpdateStatus_nodeUpdateStatus :: Lens.Lens' CacheNodeUpdateStatus (Core.Maybe NodeUpdateStatus)
cacheNodeUpdateStatus_nodeUpdateStatus = Lens.lens (\CacheNodeUpdateStatus' {nodeUpdateStatus} -> nodeUpdateStatus) (\s@CacheNodeUpdateStatus' {} a -> s {nodeUpdateStatus = a} :: CacheNodeUpdateStatus)

-- | Reflects whether the update was initiated by the customer or
-- automatically applied
cacheNodeUpdateStatus_nodeUpdateInitiatedBy :: Lens.Lens' CacheNodeUpdateStatus (Core.Maybe NodeUpdateInitiatedBy)
cacheNodeUpdateStatus_nodeUpdateInitiatedBy = Lens.lens (\CacheNodeUpdateStatus' {nodeUpdateInitiatedBy} -> nodeUpdateInitiatedBy) (\s@CacheNodeUpdateStatus' {} a -> s {nodeUpdateInitiatedBy = a} :: CacheNodeUpdateStatus)

-- | The date when the update is triggered
cacheNodeUpdateStatus_nodeUpdateInitiatedDate :: Lens.Lens' CacheNodeUpdateStatus (Core.Maybe Core.UTCTime)
cacheNodeUpdateStatus_nodeUpdateInitiatedDate = Lens.lens (\CacheNodeUpdateStatus' {nodeUpdateInitiatedDate} -> nodeUpdateInitiatedDate) (\s@CacheNodeUpdateStatus' {} a -> s {nodeUpdateInitiatedDate = a} :: CacheNodeUpdateStatus) Core.. Lens.mapping Core._Time

-- | The node ID of the cache cluster
cacheNodeUpdateStatus_cacheNodeId :: Lens.Lens' CacheNodeUpdateStatus (Core.Maybe Core.Text)
cacheNodeUpdateStatus_cacheNodeId = Lens.lens (\CacheNodeUpdateStatus' {cacheNodeId} -> cacheNodeId) (\s@CacheNodeUpdateStatus' {} a -> s {cacheNodeId = a} :: CacheNodeUpdateStatus)

-- | The deletion date of the node
cacheNodeUpdateStatus_nodeDeletionDate :: Lens.Lens' CacheNodeUpdateStatus (Core.Maybe Core.UTCTime)
cacheNodeUpdateStatus_nodeDeletionDate = Lens.lens (\CacheNodeUpdateStatus' {nodeDeletionDate} -> nodeDeletionDate) (\s@CacheNodeUpdateStatus' {} a -> s {nodeDeletionDate = a} :: CacheNodeUpdateStatus) Core.. Lens.mapping Core._Time

-- | The start date of the update for a node
cacheNodeUpdateStatus_nodeUpdateStartDate :: Lens.Lens' CacheNodeUpdateStatus (Core.Maybe Core.UTCTime)
cacheNodeUpdateStatus_nodeUpdateStartDate = Lens.lens (\CacheNodeUpdateStatus' {nodeUpdateStartDate} -> nodeUpdateStartDate) (\s@CacheNodeUpdateStatus' {} a -> s {nodeUpdateStartDate = a} :: CacheNodeUpdateStatus) Core.. Lens.mapping Core._Time

-- | The end date of the update for a node
cacheNodeUpdateStatus_nodeUpdateEndDate :: Lens.Lens' CacheNodeUpdateStatus (Core.Maybe Core.UTCTime)
cacheNodeUpdateStatus_nodeUpdateEndDate = Lens.lens (\CacheNodeUpdateStatus' {nodeUpdateEndDate} -> nodeUpdateEndDate) (\s@CacheNodeUpdateStatus' {} a -> s {nodeUpdateEndDate = a} :: CacheNodeUpdateStatus) Core.. Lens.mapping Core._Time

instance Core.FromXML CacheNodeUpdateStatus where
  parseXML x =
    CacheNodeUpdateStatus'
      Core.<$> (x Core..@? "NodeUpdateStatusModifiedDate")
      Core.<*> (x Core..@? "NodeUpdateStatus")
      Core.<*> (x Core..@? "NodeUpdateInitiatedBy")
      Core.<*> (x Core..@? "NodeUpdateInitiatedDate")
      Core.<*> (x Core..@? "CacheNodeId")
      Core.<*> (x Core..@? "NodeDeletionDate")
      Core.<*> (x Core..@? "NodeUpdateStartDate")
      Core.<*> (x Core..@? "NodeUpdateEndDate")

instance Core.Hashable CacheNodeUpdateStatus

instance Core.NFData CacheNodeUpdateStatus
