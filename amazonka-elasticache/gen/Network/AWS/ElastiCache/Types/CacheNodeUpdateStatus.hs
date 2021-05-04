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
-- Module      : Network.AWS.ElastiCache.Types.CacheNodeUpdateStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElastiCache.Types.CacheNodeUpdateStatus where

import Network.AWS.ElastiCache.Types.NodeUpdateInitiatedBy
import Network.AWS.ElastiCache.Types.NodeUpdateStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The status of the service update on the cache node
--
-- /See:/ 'newCacheNodeUpdateStatus' smart constructor.
data CacheNodeUpdateStatus = CacheNodeUpdateStatus'
  { -- | The date when the NodeUpdateStatus was last modified>
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
        Prelude.Nothing,
      nodeUpdateStatus = Prelude.Nothing,
      nodeUpdateInitiatedBy = Prelude.Nothing,
      nodeUpdateInitiatedDate = Prelude.Nothing,
      cacheNodeId = Prelude.Nothing,
      nodeDeletionDate = Prelude.Nothing,
      nodeUpdateStartDate = Prelude.Nothing,
      nodeUpdateEndDate = Prelude.Nothing
    }

-- | The date when the NodeUpdateStatus was last modified>
cacheNodeUpdateStatus_nodeUpdateStatusModifiedDate :: Lens.Lens' CacheNodeUpdateStatus (Prelude.Maybe Prelude.UTCTime)
cacheNodeUpdateStatus_nodeUpdateStatusModifiedDate = Lens.lens (\CacheNodeUpdateStatus' {nodeUpdateStatusModifiedDate} -> nodeUpdateStatusModifiedDate) (\s@CacheNodeUpdateStatus' {} a -> s {nodeUpdateStatusModifiedDate = a} :: CacheNodeUpdateStatus) Prelude.. Lens.mapping Prelude._Time

-- | The update status of the node
cacheNodeUpdateStatus_nodeUpdateStatus :: Lens.Lens' CacheNodeUpdateStatus (Prelude.Maybe NodeUpdateStatus)
cacheNodeUpdateStatus_nodeUpdateStatus = Lens.lens (\CacheNodeUpdateStatus' {nodeUpdateStatus} -> nodeUpdateStatus) (\s@CacheNodeUpdateStatus' {} a -> s {nodeUpdateStatus = a} :: CacheNodeUpdateStatus)

-- | Reflects whether the update was initiated by the customer or
-- automatically applied
cacheNodeUpdateStatus_nodeUpdateInitiatedBy :: Lens.Lens' CacheNodeUpdateStatus (Prelude.Maybe NodeUpdateInitiatedBy)
cacheNodeUpdateStatus_nodeUpdateInitiatedBy = Lens.lens (\CacheNodeUpdateStatus' {nodeUpdateInitiatedBy} -> nodeUpdateInitiatedBy) (\s@CacheNodeUpdateStatus' {} a -> s {nodeUpdateInitiatedBy = a} :: CacheNodeUpdateStatus)

-- | The date when the update is triggered
cacheNodeUpdateStatus_nodeUpdateInitiatedDate :: Lens.Lens' CacheNodeUpdateStatus (Prelude.Maybe Prelude.UTCTime)
cacheNodeUpdateStatus_nodeUpdateInitiatedDate = Lens.lens (\CacheNodeUpdateStatus' {nodeUpdateInitiatedDate} -> nodeUpdateInitiatedDate) (\s@CacheNodeUpdateStatus' {} a -> s {nodeUpdateInitiatedDate = a} :: CacheNodeUpdateStatus) Prelude.. Lens.mapping Prelude._Time

-- | The node ID of the cache cluster
cacheNodeUpdateStatus_cacheNodeId :: Lens.Lens' CacheNodeUpdateStatus (Prelude.Maybe Prelude.Text)
cacheNodeUpdateStatus_cacheNodeId = Lens.lens (\CacheNodeUpdateStatus' {cacheNodeId} -> cacheNodeId) (\s@CacheNodeUpdateStatus' {} a -> s {cacheNodeId = a} :: CacheNodeUpdateStatus)

-- | The deletion date of the node
cacheNodeUpdateStatus_nodeDeletionDate :: Lens.Lens' CacheNodeUpdateStatus (Prelude.Maybe Prelude.UTCTime)
cacheNodeUpdateStatus_nodeDeletionDate = Lens.lens (\CacheNodeUpdateStatus' {nodeDeletionDate} -> nodeDeletionDate) (\s@CacheNodeUpdateStatus' {} a -> s {nodeDeletionDate = a} :: CacheNodeUpdateStatus) Prelude.. Lens.mapping Prelude._Time

-- | The start date of the update for a node
cacheNodeUpdateStatus_nodeUpdateStartDate :: Lens.Lens' CacheNodeUpdateStatus (Prelude.Maybe Prelude.UTCTime)
cacheNodeUpdateStatus_nodeUpdateStartDate = Lens.lens (\CacheNodeUpdateStatus' {nodeUpdateStartDate} -> nodeUpdateStartDate) (\s@CacheNodeUpdateStatus' {} a -> s {nodeUpdateStartDate = a} :: CacheNodeUpdateStatus) Prelude.. Lens.mapping Prelude._Time

-- | The end date of the update for a node
cacheNodeUpdateStatus_nodeUpdateEndDate :: Lens.Lens' CacheNodeUpdateStatus (Prelude.Maybe Prelude.UTCTime)
cacheNodeUpdateStatus_nodeUpdateEndDate = Lens.lens (\CacheNodeUpdateStatus' {nodeUpdateEndDate} -> nodeUpdateEndDate) (\s@CacheNodeUpdateStatus' {} a -> s {nodeUpdateEndDate = a} :: CacheNodeUpdateStatus) Prelude.. Lens.mapping Prelude._Time

instance Prelude.FromXML CacheNodeUpdateStatus where
  parseXML x =
    CacheNodeUpdateStatus'
      Prelude.<$> (x Prelude..@? "NodeUpdateStatusModifiedDate")
      Prelude.<*> (x Prelude..@? "NodeUpdateStatus")
      Prelude.<*> (x Prelude..@? "NodeUpdateInitiatedBy")
      Prelude.<*> (x Prelude..@? "NodeUpdateInitiatedDate")
      Prelude.<*> (x Prelude..@? "CacheNodeId")
      Prelude.<*> (x Prelude..@? "NodeDeletionDate")
      Prelude.<*> (x Prelude..@? "NodeUpdateStartDate")
      Prelude.<*> (x Prelude..@? "NodeUpdateEndDate")

instance Prelude.Hashable CacheNodeUpdateStatus

instance Prelude.NFData CacheNodeUpdateStatus
