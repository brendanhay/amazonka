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
-- Module      : Amazonka.DynamoDB.Types.ReplicationGroupUpdate
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DynamoDB.Types.ReplicationGroupUpdate where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DynamoDB.Types.AttributeValue
import Amazonka.DynamoDB.Types.CreateReplicationGroupMemberAction
import Amazonka.DynamoDB.Types.DeleteReplicationGroupMemberAction
import Amazonka.DynamoDB.Types.UpdateReplicationGroupMemberAction
import Amazonka.DynamoDB.Types.WriteRequest
import qualified Amazonka.Prelude as Prelude

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
-- When you manually remove a table or global table replica, you do not
-- automatically remove any associated scalable targets, scaling policies,
-- or CloudWatch alarms.
--
-- /See:/ 'newReplicationGroupUpdate' smart constructor.
data ReplicationGroupUpdate = ReplicationGroupUpdate'
  { -- | The parameters required for creating a replica for the table.
    create :: Prelude.Maybe CreateReplicationGroupMemberAction,
    -- | The parameters required for deleting a replica for the table.
    delete' :: Prelude.Maybe DeleteReplicationGroupMemberAction,
    -- | The parameters required for updating a replica for the table.
    update :: Prelude.Maybe UpdateReplicationGroupMemberAction
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'delete'', 'replicationGroupUpdate_delete' - The parameters required for deleting a replica for the table.
--
-- 'update', 'replicationGroupUpdate_update' - The parameters required for updating a replica for the table.
newReplicationGroupUpdate ::
  ReplicationGroupUpdate
newReplicationGroupUpdate =
  ReplicationGroupUpdate'
    { create = Prelude.Nothing,
      delete' = Prelude.Nothing,
      update = Prelude.Nothing
    }

-- | The parameters required for creating a replica for the table.
replicationGroupUpdate_create :: Lens.Lens' ReplicationGroupUpdate (Prelude.Maybe CreateReplicationGroupMemberAction)
replicationGroupUpdate_create = Lens.lens (\ReplicationGroupUpdate' {create} -> create) (\s@ReplicationGroupUpdate' {} a -> s {create = a} :: ReplicationGroupUpdate)

-- | The parameters required for deleting a replica for the table.
replicationGroupUpdate_delete :: Lens.Lens' ReplicationGroupUpdate (Prelude.Maybe DeleteReplicationGroupMemberAction)
replicationGroupUpdate_delete = Lens.lens (\ReplicationGroupUpdate' {delete'} -> delete') (\s@ReplicationGroupUpdate' {} a -> s {delete' = a} :: ReplicationGroupUpdate)

-- | The parameters required for updating a replica for the table.
replicationGroupUpdate_update :: Lens.Lens' ReplicationGroupUpdate (Prelude.Maybe UpdateReplicationGroupMemberAction)
replicationGroupUpdate_update = Lens.lens (\ReplicationGroupUpdate' {update} -> update) (\s@ReplicationGroupUpdate' {} a -> s {update = a} :: ReplicationGroupUpdate)

instance Prelude.Hashable ReplicationGroupUpdate where
  hashWithSalt _salt ReplicationGroupUpdate' {..} =
    _salt
      `Prelude.hashWithSalt` create
      `Prelude.hashWithSalt` delete'
      `Prelude.hashWithSalt` update

instance Prelude.NFData ReplicationGroupUpdate where
  rnf ReplicationGroupUpdate' {..} =
    Prelude.rnf create
      `Prelude.seq` Prelude.rnf delete'
      `Prelude.seq` Prelude.rnf update

instance Data.ToJSON ReplicationGroupUpdate where
  toJSON ReplicationGroupUpdate' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Create" Data..=) Prelude.<$> create,
            ("Delete" Data..=) Prelude.<$> delete',
            ("Update" Data..=) Prelude.<$> update
          ]
      )
