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
-- Module      : Network.AWS.ElastiCache.Types.NodeGroupUpdateStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElastiCache.Types.NodeGroupUpdateStatus where

import qualified Network.AWS.Core as Core
import Network.AWS.ElastiCache.Types.NodeGroupMemberUpdateStatus
import qualified Network.AWS.Lens as Lens

-- | The status of the service update on the node group
--
-- /See:/ 'newNodeGroupUpdateStatus' smart constructor.
data NodeGroupUpdateStatus = NodeGroupUpdateStatus'
  { -- | The ID of the node group
    nodeGroupId :: Core.Maybe Core.Text,
    -- | The status of the service update on the node group member
    nodeGroupMemberUpdateStatus :: Core.Maybe [NodeGroupMemberUpdateStatus]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'NodeGroupUpdateStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nodeGroupId', 'nodeGroupUpdateStatus_nodeGroupId' - The ID of the node group
--
-- 'nodeGroupMemberUpdateStatus', 'nodeGroupUpdateStatus_nodeGroupMemberUpdateStatus' - The status of the service update on the node group member
newNodeGroupUpdateStatus ::
  NodeGroupUpdateStatus
newNodeGroupUpdateStatus =
  NodeGroupUpdateStatus'
    { nodeGroupId = Core.Nothing,
      nodeGroupMemberUpdateStatus = Core.Nothing
    }

-- | The ID of the node group
nodeGroupUpdateStatus_nodeGroupId :: Lens.Lens' NodeGroupUpdateStatus (Core.Maybe Core.Text)
nodeGroupUpdateStatus_nodeGroupId = Lens.lens (\NodeGroupUpdateStatus' {nodeGroupId} -> nodeGroupId) (\s@NodeGroupUpdateStatus' {} a -> s {nodeGroupId = a} :: NodeGroupUpdateStatus)

-- | The status of the service update on the node group member
nodeGroupUpdateStatus_nodeGroupMemberUpdateStatus :: Lens.Lens' NodeGroupUpdateStatus (Core.Maybe [NodeGroupMemberUpdateStatus])
nodeGroupUpdateStatus_nodeGroupMemberUpdateStatus = Lens.lens (\NodeGroupUpdateStatus' {nodeGroupMemberUpdateStatus} -> nodeGroupMemberUpdateStatus) (\s@NodeGroupUpdateStatus' {} a -> s {nodeGroupMemberUpdateStatus = a} :: NodeGroupUpdateStatus) Core.. Lens.mapping Lens._Coerce

instance Core.FromXML NodeGroupUpdateStatus where
  parseXML x =
    NodeGroupUpdateStatus'
      Core.<$> (x Core..@? "NodeGroupId")
      Core.<*> ( x Core..@? "NodeGroupMemberUpdateStatus"
                   Core..!@ Core.mempty
                   Core.>>= Core.may
                     (Core.parseXMLList "NodeGroupMemberUpdateStatus")
               )

instance Core.Hashable NodeGroupUpdateStatus

instance Core.NFData NodeGroupUpdateStatus
