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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ElastiCache.Types.CacheNodeUpdateStatus where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ElastiCache.Types.NodeUpdateInitiatedBy
import Amazonka.ElastiCache.Types.NodeUpdateStatus
import qualified Amazonka.Prelude as Prelude

-- | The status of the service update on the cache node
--
-- /See:/ 'newCacheNodeUpdateStatus' smart constructor.
data CacheNodeUpdateStatus = CacheNodeUpdateStatus'
  { -- | The node ID of the cache cluster
    cacheNodeId :: Prelude.Maybe Prelude.Text,
    -- | The deletion date of the node
    nodeDeletionDate :: Prelude.Maybe Data.ISO8601,
    -- | The end date of the update for a node
    nodeUpdateEndDate :: Prelude.Maybe Data.ISO8601,
    -- | Reflects whether the update was initiated by the customer or
    -- automatically applied
    nodeUpdateInitiatedBy :: Prelude.Maybe NodeUpdateInitiatedBy,
    -- | The date when the update is triggered
    nodeUpdateInitiatedDate :: Prelude.Maybe Data.ISO8601,
    -- | The start date of the update for a node
    nodeUpdateStartDate :: Prelude.Maybe Data.ISO8601,
    -- | The update status of the node
    nodeUpdateStatus :: Prelude.Maybe NodeUpdateStatus,
    -- | The date when the NodeUpdateStatus was last modified>
    nodeUpdateStatusModifiedDate :: Prelude.Maybe Data.ISO8601
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
-- 'cacheNodeId', 'cacheNodeUpdateStatus_cacheNodeId' - The node ID of the cache cluster
--
-- 'nodeDeletionDate', 'cacheNodeUpdateStatus_nodeDeletionDate' - The deletion date of the node
--
-- 'nodeUpdateEndDate', 'cacheNodeUpdateStatus_nodeUpdateEndDate' - The end date of the update for a node
--
-- 'nodeUpdateInitiatedBy', 'cacheNodeUpdateStatus_nodeUpdateInitiatedBy' - Reflects whether the update was initiated by the customer or
-- automatically applied
--
-- 'nodeUpdateInitiatedDate', 'cacheNodeUpdateStatus_nodeUpdateInitiatedDate' - The date when the update is triggered
--
-- 'nodeUpdateStartDate', 'cacheNodeUpdateStatus_nodeUpdateStartDate' - The start date of the update for a node
--
-- 'nodeUpdateStatus', 'cacheNodeUpdateStatus_nodeUpdateStatus' - The update status of the node
--
-- 'nodeUpdateStatusModifiedDate', 'cacheNodeUpdateStatus_nodeUpdateStatusModifiedDate' - The date when the NodeUpdateStatus was last modified>
newCacheNodeUpdateStatus ::
  CacheNodeUpdateStatus
newCacheNodeUpdateStatus =
  CacheNodeUpdateStatus'
    { cacheNodeId =
        Prelude.Nothing,
      nodeDeletionDate = Prelude.Nothing,
      nodeUpdateEndDate = Prelude.Nothing,
      nodeUpdateInitiatedBy = Prelude.Nothing,
      nodeUpdateInitiatedDate = Prelude.Nothing,
      nodeUpdateStartDate = Prelude.Nothing,
      nodeUpdateStatus = Prelude.Nothing,
      nodeUpdateStatusModifiedDate = Prelude.Nothing
    }

-- | The node ID of the cache cluster
cacheNodeUpdateStatus_cacheNodeId :: Lens.Lens' CacheNodeUpdateStatus (Prelude.Maybe Prelude.Text)
cacheNodeUpdateStatus_cacheNodeId = Lens.lens (\CacheNodeUpdateStatus' {cacheNodeId} -> cacheNodeId) (\s@CacheNodeUpdateStatus' {} a -> s {cacheNodeId = a} :: CacheNodeUpdateStatus)

-- | The deletion date of the node
cacheNodeUpdateStatus_nodeDeletionDate :: Lens.Lens' CacheNodeUpdateStatus (Prelude.Maybe Prelude.UTCTime)
cacheNodeUpdateStatus_nodeDeletionDate = Lens.lens (\CacheNodeUpdateStatus' {nodeDeletionDate} -> nodeDeletionDate) (\s@CacheNodeUpdateStatus' {} a -> s {nodeDeletionDate = a} :: CacheNodeUpdateStatus) Prelude.. Lens.mapping Data._Time

-- | The end date of the update for a node
cacheNodeUpdateStatus_nodeUpdateEndDate :: Lens.Lens' CacheNodeUpdateStatus (Prelude.Maybe Prelude.UTCTime)
cacheNodeUpdateStatus_nodeUpdateEndDate = Lens.lens (\CacheNodeUpdateStatus' {nodeUpdateEndDate} -> nodeUpdateEndDate) (\s@CacheNodeUpdateStatus' {} a -> s {nodeUpdateEndDate = a} :: CacheNodeUpdateStatus) Prelude.. Lens.mapping Data._Time

-- | Reflects whether the update was initiated by the customer or
-- automatically applied
cacheNodeUpdateStatus_nodeUpdateInitiatedBy :: Lens.Lens' CacheNodeUpdateStatus (Prelude.Maybe NodeUpdateInitiatedBy)
cacheNodeUpdateStatus_nodeUpdateInitiatedBy = Lens.lens (\CacheNodeUpdateStatus' {nodeUpdateInitiatedBy} -> nodeUpdateInitiatedBy) (\s@CacheNodeUpdateStatus' {} a -> s {nodeUpdateInitiatedBy = a} :: CacheNodeUpdateStatus)

-- | The date when the update is triggered
cacheNodeUpdateStatus_nodeUpdateInitiatedDate :: Lens.Lens' CacheNodeUpdateStatus (Prelude.Maybe Prelude.UTCTime)
cacheNodeUpdateStatus_nodeUpdateInitiatedDate = Lens.lens (\CacheNodeUpdateStatus' {nodeUpdateInitiatedDate} -> nodeUpdateInitiatedDate) (\s@CacheNodeUpdateStatus' {} a -> s {nodeUpdateInitiatedDate = a} :: CacheNodeUpdateStatus) Prelude.. Lens.mapping Data._Time

-- | The start date of the update for a node
cacheNodeUpdateStatus_nodeUpdateStartDate :: Lens.Lens' CacheNodeUpdateStatus (Prelude.Maybe Prelude.UTCTime)
cacheNodeUpdateStatus_nodeUpdateStartDate = Lens.lens (\CacheNodeUpdateStatus' {nodeUpdateStartDate} -> nodeUpdateStartDate) (\s@CacheNodeUpdateStatus' {} a -> s {nodeUpdateStartDate = a} :: CacheNodeUpdateStatus) Prelude.. Lens.mapping Data._Time

-- | The update status of the node
cacheNodeUpdateStatus_nodeUpdateStatus :: Lens.Lens' CacheNodeUpdateStatus (Prelude.Maybe NodeUpdateStatus)
cacheNodeUpdateStatus_nodeUpdateStatus = Lens.lens (\CacheNodeUpdateStatus' {nodeUpdateStatus} -> nodeUpdateStatus) (\s@CacheNodeUpdateStatus' {} a -> s {nodeUpdateStatus = a} :: CacheNodeUpdateStatus)

-- | The date when the NodeUpdateStatus was last modified>
cacheNodeUpdateStatus_nodeUpdateStatusModifiedDate :: Lens.Lens' CacheNodeUpdateStatus (Prelude.Maybe Prelude.UTCTime)
cacheNodeUpdateStatus_nodeUpdateStatusModifiedDate = Lens.lens (\CacheNodeUpdateStatus' {nodeUpdateStatusModifiedDate} -> nodeUpdateStatusModifiedDate) (\s@CacheNodeUpdateStatus' {} a -> s {nodeUpdateStatusModifiedDate = a} :: CacheNodeUpdateStatus) Prelude.. Lens.mapping Data._Time

instance Data.FromXML CacheNodeUpdateStatus where
  parseXML x =
    CacheNodeUpdateStatus'
      Prelude.<$> (x Data..@? "CacheNodeId")
      Prelude.<*> (x Data..@? "NodeDeletionDate")
      Prelude.<*> (x Data..@? "NodeUpdateEndDate")
      Prelude.<*> (x Data..@? "NodeUpdateInitiatedBy")
      Prelude.<*> (x Data..@? "NodeUpdateInitiatedDate")
      Prelude.<*> (x Data..@? "NodeUpdateStartDate")
      Prelude.<*> (x Data..@? "NodeUpdateStatus")
      Prelude.<*> (x Data..@? "NodeUpdateStatusModifiedDate")

instance Prelude.Hashable CacheNodeUpdateStatus where
  hashWithSalt _salt CacheNodeUpdateStatus' {..} =
    _salt `Prelude.hashWithSalt` cacheNodeId
      `Prelude.hashWithSalt` nodeDeletionDate
      `Prelude.hashWithSalt` nodeUpdateEndDate
      `Prelude.hashWithSalt` nodeUpdateInitiatedBy
      `Prelude.hashWithSalt` nodeUpdateInitiatedDate
      `Prelude.hashWithSalt` nodeUpdateStartDate
      `Prelude.hashWithSalt` nodeUpdateStatus
      `Prelude.hashWithSalt` nodeUpdateStatusModifiedDate

instance Prelude.NFData CacheNodeUpdateStatus where
  rnf CacheNodeUpdateStatus' {..} =
    Prelude.rnf cacheNodeId
      `Prelude.seq` Prelude.rnf nodeDeletionDate
      `Prelude.seq` Prelude.rnf nodeUpdateEndDate
      `Prelude.seq` Prelude.rnf nodeUpdateInitiatedBy
      `Prelude.seq` Prelude.rnf nodeUpdateInitiatedDate
      `Prelude.seq` Prelude.rnf nodeUpdateStartDate
      `Prelude.seq` Prelude.rnf nodeUpdateStatus
      `Prelude.seq` Prelude.rnf nodeUpdateStatusModifiedDate
