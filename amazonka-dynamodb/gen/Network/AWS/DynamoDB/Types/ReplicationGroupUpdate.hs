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
-- Module      : Network.AWS.DynamoDB.Types.ReplicationGroupUpdate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.ReplicationGroupUpdate where

import qualified Network.AWS.Core as Core
import Network.AWS.DynamoDB.Types.CreateReplicationGroupMemberAction
import Network.AWS.DynamoDB.Types.DeleteReplicationGroupMemberAction
import Network.AWS.DynamoDB.Types.UpdateReplicationGroupMemberAction
import qualified Network.AWS.Lens as Lens

-- | Represents one of the following:
--
-- -   A new replica to be added to an existing regional table or global
--     table. This request invokes the @CreateTableReplica@ action in the
--     destination Region.
--
-- -   New parameters for an existing replica. This request invokes the
--     @UpdateTable@ action in the destination Region.
--
-- -   An existing replica to be deleted. The request invokes the
--     @DeleteTableReplica@ action in the destination Region, deleting the
--     replica and all if its items in the destination Region.
--
-- /See:/ 'newReplicationGroupUpdate' smart constructor.
data ReplicationGroupUpdate = ReplicationGroupUpdate'
  { -- | The parameters required for creating a replica for the table.
    create :: Core.Maybe CreateReplicationGroupMemberAction,
    -- | The parameters required for updating a replica for the table.
    update :: Core.Maybe UpdateReplicationGroupMemberAction,
    -- | The parameters required for deleting a replica for the table.
    delete' :: Core.Maybe DeleteReplicationGroupMemberAction
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ReplicationGroupUpdate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'create', 'replicationGroupUpdate_create' - The parameters required for creating a replica for the table.
--
-- 'update', 'replicationGroupUpdate_update' - The parameters required for updating a replica for the table.
--
-- 'delete'', 'replicationGroupUpdate_delete' - The parameters required for deleting a replica for the table.
newReplicationGroupUpdate ::
  ReplicationGroupUpdate
newReplicationGroupUpdate =
  ReplicationGroupUpdate'
    { create = Core.Nothing,
      update = Core.Nothing,
      delete' = Core.Nothing
    }

-- | The parameters required for creating a replica for the table.
replicationGroupUpdate_create :: Lens.Lens' ReplicationGroupUpdate (Core.Maybe CreateReplicationGroupMemberAction)
replicationGroupUpdate_create = Lens.lens (\ReplicationGroupUpdate' {create} -> create) (\s@ReplicationGroupUpdate' {} a -> s {create = a} :: ReplicationGroupUpdate)

-- | The parameters required for updating a replica for the table.
replicationGroupUpdate_update :: Lens.Lens' ReplicationGroupUpdate (Core.Maybe UpdateReplicationGroupMemberAction)
replicationGroupUpdate_update = Lens.lens (\ReplicationGroupUpdate' {update} -> update) (\s@ReplicationGroupUpdate' {} a -> s {update = a} :: ReplicationGroupUpdate)

-- | The parameters required for deleting a replica for the table.
replicationGroupUpdate_delete :: Lens.Lens' ReplicationGroupUpdate (Core.Maybe DeleteReplicationGroupMemberAction)
replicationGroupUpdate_delete = Lens.lens (\ReplicationGroupUpdate' {delete'} -> delete') (\s@ReplicationGroupUpdate' {} a -> s {delete' = a} :: ReplicationGroupUpdate)

instance Core.Hashable ReplicationGroupUpdate

instance Core.NFData ReplicationGroupUpdate

instance Core.ToJSON ReplicationGroupUpdate where
  toJSON ReplicationGroupUpdate' {..} =
    Core.object
      ( Core.catMaybes
          [ ("Create" Core..=) Core.<$> create,
            ("Update" Core..=) Core.<$> update,
            ("Delete" Core..=) Core.<$> delete'
          ]
      )
