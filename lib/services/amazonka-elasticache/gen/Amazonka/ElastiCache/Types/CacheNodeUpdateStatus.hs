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
-- Module      : Amazonka.ElastiCache.Types.CacheNodeUpdateStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ElastiCache.Types.CacheNodeUpdateStatus where

import qualified Amazonka.Core as Core
import Amazonka.ElastiCache.Types.NodeUpdateInitiatedBy
import Amazonka.ElastiCache.Types.NodeUpdateStatus
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | The status of the service update on the cache node
--
-- /See:/ 'newCacheNodeUpdateStatus' smart constructor.
data CacheNodeUpdateStatus = CacheNodeUpdateStatus'
  { -- | The end date of the update for a node
    nodeUpdateEndDate :: Prelude.Maybe Core.ISO8601,
    -- | Reflects whether the update was initiated by the customer or
    -- automatically applied
    nodeUpdateInitiatedBy :: Prelude.Maybe NodeUpdateInitiatedBy,
    -- | The date when the NodeUpdateStatus was last modified>
    nodeUpdateStatusModifiedDate :: Prelude.Maybe Core.ISO8601,
    -- | The node ID of the cache cluster
    cacheNodeId :: Prelude.Maybe Prelude.Text,
    -- | The date when the update is triggered
    nodeUpdateInitiatedDate :: Prelude.Maybe Core.ISO8601,
    -- | The start date of the update for a node
    nodeUpdateStartDate :: Prelude.Maybe Core.ISO8601,
    -- | The update status of the node
    nodeUpdateStatus :: Prelude.Maybe NodeUpdateStatus,
    -- | The deletion date of the node
    nodeDeletionDate :: Prelude.Maybe Core.ISO8601
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CacheNodeUpdateStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nodeUpdateEndDate', 'cacheNodeUpdateStatus_nodeUpdateEndDate' - The end date of the update for a node
--
-- 'nodeUpdateInitiatedBy', 'cacheNodeUpdateStatus_nodeUpdateInitiatedBy' - Reflects whether the update was initiated by the customer or
-- automatically applied
--
-- 'nodeUpdateStatusModifiedDate', 'cacheNodeUpdateStatus_nodeUpdateStatusModifiedDate' - The date when the NodeUpdateStatus was last modified>
--
-- 'cacheNodeId', 'cacheNodeUpdateStatus_cacheNodeId' - The node ID of the cache cluster
--
-- 'nodeUpdateInitiatedDate', 'cacheNodeUpdateStatus_nodeUpdateInitiatedDate' - The date when the update is triggered
--
-- 'nodeUpdateStartDate', 'cacheNodeUpdateStatus_nodeUpdateStartDate' - The start date of the update for a node
--
-- 'nodeUpdateStatus', 'cacheNodeUpdateStatus_nodeUpdateStatus' - The update status of the node
--
-- 'nodeDeletionDate', 'cacheNodeUpdateStatus_nodeDeletionDate' - The deletion date of the node
newCacheNodeUpdateStatus ::
  CacheNodeUpdateStatus
newCacheNodeUpdateStatus =
  CacheNodeUpdateStatus'
    { nodeUpdateEndDate =
        Prelude.Nothing,
      nodeUpdateInitiatedBy = Prelude.Nothing,
      nodeUpdateStatusModifiedDate = Prelude.Nothing,
      cacheNodeId = Prelude.Nothing,
      nodeUpdateInitiatedDate = Prelude.Nothing,
      nodeUpdateStartDate = Prelude.Nothing,
      nodeUpdateStatus = Prelude.Nothing,
      nodeDeletionDate = Prelude.Nothing
    }

-- | The end date of the update for a node
cacheNodeUpdateStatus_nodeUpdateEndDate :: Lens.Lens' CacheNodeUpdateStatus (Prelude.Maybe Prelude.UTCTime)
cacheNodeUpdateStatus_nodeUpdateEndDate = Lens.lens (\CacheNodeUpdateStatus' {nodeUpdateEndDate} -> nodeUpdateEndDate) (\s@CacheNodeUpdateStatus' {} a -> s {nodeUpdateEndDate = a} :: CacheNodeUpdateStatus) Prelude.. Lens.mapping Core._Time

-- | Reflects whether the update was initiated by the customer or
-- automatically applied
cacheNodeUpdateStatus_nodeUpdateInitiatedBy :: Lens.Lens' CacheNodeUpdateStatus (Prelude.Maybe NodeUpdateInitiatedBy)
cacheNodeUpdateStatus_nodeUpdateInitiatedBy = Lens.lens (\CacheNodeUpdateStatus' {nodeUpdateInitiatedBy} -> nodeUpdateInitiatedBy) (\s@CacheNodeUpdateStatus' {} a -> s {nodeUpdateInitiatedBy = a} :: CacheNodeUpdateStatus)

-- | The date when the NodeUpdateStatus was last modified>
cacheNodeUpdateStatus_nodeUpdateStatusModifiedDate :: Lens.Lens' CacheNodeUpdateStatus (Prelude.Maybe Prelude.UTCTime)
cacheNodeUpdateStatus_nodeUpdateStatusModifiedDate = Lens.lens (\CacheNodeUpdateStatus' {nodeUpdateStatusModifiedDate} -> nodeUpdateStatusModifiedDate) (\s@CacheNodeUpdateStatus' {} a -> s {nodeUpdateStatusModifiedDate = a} :: CacheNodeUpdateStatus) Prelude.. Lens.mapping Core._Time

-- | The node ID of the cache cluster
cacheNodeUpdateStatus_cacheNodeId :: Lens.Lens' CacheNodeUpdateStatus (Prelude.Maybe Prelude.Text)
cacheNodeUpdateStatus_cacheNodeId = Lens.lens (\CacheNodeUpdateStatus' {cacheNodeId} -> cacheNodeId) (\s@CacheNodeUpdateStatus' {} a -> s {cacheNodeId = a} :: CacheNodeUpdateStatus)

-- | The date when the update is triggered
cacheNodeUpdateStatus_nodeUpdateInitiatedDate :: Lens.Lens' CacheNodeUpdateStatus (Prelude.Maybe Prelude.UTCTime)
cacheNodeUpdateStatus_nodeUpdateInitiatedDate = Lens.lens (\CacheNodeUpdateStatus' {nodeUpdateInitiatedDate} -> nodeUpdateInitiatedDate) (\s@CacheNodeUpdateStatus' {} a -> s {nodeUpdateInitiatedDate = a} :: CacheNodeUpdateStatus) Prelude.. Lens.mapping Core._Time

-- | The start date of the update for a node
cacheNodeUpdateStatus_nodeUpdateStartDate :: Lens.Lens' CacheNodeUpdateStatus (Prelude.Maybe Prelude.UTCTime)
cacheNodeUpdateStatus_nodeUpdateStartDate = Lens.lens (\CacheNodeUpdateStatus' {nodeUpdateStartDate} -> nodeUpdateStartDate) (\s@CacheNodeUpdateStatus' {} a -> s {nodeUpdateStartDate = a} :: CacheNodeUpdateStatus) Prelude.. Lens.mapping Core._Time

-- | The update status of the node
cacheNodeUpdateStatus_nodeUpdateStatus :: Lens.Lens' CacheNodeUpdateStatus (Prelude.Maybe NodeUpdateStatus)
cacheNodeUpdateStatus_nodeUpdateStatus = Lens.lens (\CacheNodeUpdateStatus' {nodeUpdateStatus} -> nodeUpdateStatus) (\s@CacheNodeUpdateStatus' {} a -> s {nodeUpdateStatus = a} :: CacheNodeUpdateStatus)

-- | The deletion date of the node
cacheNodeUpdateStatus_nodeDeletionDate :: Lens.Lens' CacheNodeUpdateStatus (Prelude.Maybe Prelude.UTCTime)
cacheNodeUpdateStatus_nodeDeletionDate = Lens.lens (\CacheNodeUpdateStatus' {nodeDeletionDate} -> nodeDeletionDate) (\s@CacheNodeUpdateStatus' {} a -> s {nodeDeletionDate = a} :: CacheNodeUpdateStatus) Prelude.. Lens.mapping Core._Time

instance Core.FromXML CacheNodeUpdateStatus where
  parseXML x =
    CacheNodeUpdateStatus'
      Prelude.<$> (x Core..@? "NodeUpdateEndDate")
      Prelude.<*> (x Core..@? "NodeUpdateInitiatedBy")
      Prelude.<*> (x Core..@? "NodeUpdateStatusModifiedDate")
      Prelude.<*> (x Core..@? "CacheNodeId")
      Prelude.<*> (x Core..@? "NodeUpdateInitiatedDate")
      Prelude.<*> (x Core..@? "NodeUpdateStartDate")
      Prelude.<*> (x Core..@? "NodeUpdateStatus")
      Prelude.<*> (x Core..@? "NodeDeletionDate")

instance Prelude.Hashable CacheNodeUpdateStatus

instance Prelude.NFData CacheNodeUpdateStatus
