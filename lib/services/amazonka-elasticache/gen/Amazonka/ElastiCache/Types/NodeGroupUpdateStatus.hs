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
-- Module      : Amazonka.ElastiCache.Types.NodeGroupUpdateStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ElastiCache.Types.NodeGroupUpdateStatus where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ElastiCache.Types.NodeGroupMemberUpdateStatus
import qualified Amazonka.Prelude as Prelude

-- | The status of the service update on the node group
--
-- /See:/ 'newNodeGroupUpdateStatus' smart constructor.
data NodeGroupUpdateStatus = NodeGroupUpdateStatus'
  { -- | The status of the service update on the node group member
    nodeGroupMemberUpdateStatus :: Prelude.Maybe [NodeGroupMemberUpdateStatus],
    -- | The ID of the node group
    nodeGroupId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'NodeGroupUpdateStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nodeGroupMemberUpdateStatus', 'nodeGroupUpdateStatus_nodeGroupMemberUpdateStatus' - The status of the service update on the node group member
--
-- 'nodeGroupId', 'nodeGroupUpdateStatus_nodeGroupId' - The ID of the node group
newNodeGroupUpdateStatus ::
  NodeGroupUpdateStatus
newNodeGroupUpdateStatus =
  NodeGroupUpdateStatus'
    { nodeGroupMemberUpdateStatus =
        Prelude.Nothing,
      nodeGroupId = Prelude.Nothing
    }

-- | The status of the service update on the node group member
nodeGroupUpdateStatus_nodeGroupMemberUpdateStatus :: Lens.Lens' NodeGroupUpdateStatus (Prelude.Maybe [NodeGroupMemberUpdateStatus])
nodeGroupUpdateStatus_nodeGroupMemberUpdateStatus = Lens.lens (\NodeGroupUpdateStatus' {nodeGroupMemberUpdateStatus} -> nodeGroupMemberUpdateStatus) (\s@NodeGroupUpdateStatus' {} a -> s {nodeGroupMemberUpdateStatus = a} :: NodeGroupUpdateStatus) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the node group
nodeGroupUpdateStatus_nodeGroupId :: Lens.Lens' NodeGroupUpdateStatus (Prelude.Maybe Prelude.Text)
nodeGroupUpdateStatus_nodeGroupId = Lens.lens (\NodeGroupUpdateStatus' {nodeGroupId} -> nodeGroupId) (\s@NodeGroupUpdateStatus' {} a -> s {nodeGroupId = a} :: NodeGroupUpdateStatus)

instance Data.FromXML NodeGroupUpdateStatus where
  parseXML x =
    NodeGroupUpdateStatus'
      Prelude.<$> ( x Data..@? "NodeGroupMemberUpdateStatus"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may
                        (Data.parseXMLList "NodeGroupMemberUpdateStatus")
                  )
      Prelude.<*> (x Data..@? "NodeGroupId")

instance Prelude.Hashable NodeGroupUpdateStatus where
  hashWithSalt _salt NodeGroupUpdateStatus' {..} =
    _salt
      `Prelude.hashWithSalt` nodeGroupMemberUpdateStatus
      `Prelude.hashWithSalt` nodeGroupId

instance Prelude.NFData NodeGroupUpdateStatus where
  rnf NodeGroupUpdateStatus' {..} =
    Prelude.rnf nodeGroupMemberUpdateStatus
      `Prelude.seq` Prelude.rnf nodeGroupId
