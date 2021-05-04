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
-- Module      : Network.AWS.ElastiCache.Types.NodeGroupUpdateStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElastiCache.Types.NodeGroupUpdateStatus where

import Network.AWS.ElastiCache.Types.NodeGroupMemberUpdateStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The status of the service update on the node group
--
-- /See:/ 'newNodeGroupUpdateStatus' smart constructor.
data NodeGroupUpdateStatus = NodeGroupUpdateStatus'
  { -- | The ID of the node group
    nodeGroupId :: Prelude.Maybe Prelude.Text,
    -- | The status of the service update on the node group member
    nodeGroupMemberUpdateStatus :: Prelude.Maybe [NodeGroupMemberUpdateStatus]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { nodeGroupId =
        Prelude.Nothing,
      nodeGroupMemberUpdateStatus = Prelude.Nothing
    }

-- | The ID of the node group
nodeGroupUpdateStatus_nodeGroupId :: Lens.Lens' NodeGroupUpdateStatus (Prelude.Maybe Prelude.Text)
nodeGroupUpdateStatus_nodeGroupId = Lens.lens (\NodeGroupUpdateStatus' {nodeGroupId} -> nodeGroupId) (\s@NodeGroupUpdateStatus' {} a -> s {nodeGroupId = a} :: NodeGroupUpdateStatus)

-- | The status of the service update on the node group member
nodeGroupUpdateStatus_nodeGroupMemberUpdateStatus :: Lens.Lens' NodeGroupUpdateStatus (Prelude.Maybe [NodeGroupMemberUpdateStatus])
nodeGroupUpdateStatus_nodeGroupMemberUpdateStatus = Lens.lens (\NodeGroupUpdateStatus' {nodeGroupMemberUpdateStatus} -> nodeGroupMemberUpdateStatus) (\s@NodeGroupUpdateStatus' {} a -> s {nodeGroupMemberUpdateStatus = a} :: NodeGroupUpdateStatus) Prelude.. Lens.mapping Prelude._Coerce

instance Prelude.FromXML NodeGroupUpdateStatus where
  parseXML x =
    NodeGroupUpdateStatus'
      Prelude.<$> (x Prelude..@? "NodeGroupId")
      Prelude.<*> ( x Prelude..@? "NodeGroupMemberUpdateStatus"
                      Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may
                        (Prelude.parseXMLList "NodeGroupMemberUpdateStatus")
                  )

instance Prelude.Hashable NodeGroupUpdateStatus

instance Prelude.NFData NodeGroupUpdateStatus
