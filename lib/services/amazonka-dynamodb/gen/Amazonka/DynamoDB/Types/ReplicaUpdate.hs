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
-- Module      : Amazonka.DynamoDB.Types.ReplicaUpdate
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DynamoDB.Types.ReplicaUpdate where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DynamoDB.Types.AttributeValue
import Amazonka.DynamoDB.Types.CreateReplicaAction
import Amazonka.DynamoDB.Types.DeleteReplicaAction
import Amazonka.DynamoDB.Types.TransactWriteItem
import Amazonka.DynamoDB.Types.WriteRequest
import qualified Amazonka.Prelude as Prelude

-- | Represents one of the following:
--
-- -   A new replica to be added to an existing global table.
--
-- -   New parameters for an existing replica.
--
-- -   An existing replica to be removed from an existing global table.
--
-- /See:/ 'newReplicaUpdate' smart constructor.
data ReplicaUpdate = ReplicaUpdate'
  { -- | The parameters required for creating a replica on an existing global
    -- table.
    create :: Prelude.Maybe CreateReplicaAction,
    -- | The name of the existing replica to be removed.
    delete' :: Prelude.Maybe DeleteReplicaAction
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ReplicaUpdate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'create', 'replicaUpdate_create' - The parameters required for creating a replica on an existing global
-- table.
--
-- 'delete'', 'replicaUpdate_delete' - The name of the existing replica to be removed.
newReplicaUpdate ::
  ReplicaUpdate
newReplicaUpdate =
  ReplicaUpdate'
    { create = Prelude.Nothing,
      delete' = Prelude.Nothing
    }

-- | The parameters required for creating a replica on an existing global
-- table.
replicaUpdate_create :: Lens.Lens' ReplicaUpdate (Prelude.Maybe CreateReplicaAction)
replicaUpdate_create = Lens.lens (\ReplicaUpdate' {create} -> create) (\s@ReplicaUpdate' {} a -> s {create = a} :: ReplicaUpdate)

-- | The name of the existing replica to be removed.
replicaUpdate_delete :: Lens.Lens' ReplicaUpdate (Prelude.Maybe DeleteReplicaAction)
replicaUpdate_delete = Lens.lens (\ReplicaUpdate' {delete'} -> delete') (\s@ReplicaUpdate' {} a -> s {delete' = a} :: ReplicaUpdate)

instance Prelude.Hashable ReplicaUpdate where
  hashWithSalt _salt ReplicaUpdate' {..} =
    _salt
      `Prelude.hashWithSalt` create
      `Prelude.hashWithSalt` delete'

instance Prelude.NFData ReplicaUpdate where
  rnf ReplicaUpdate' {..} =
    Prelude.rnf create `Prelude.seq`
      Prelude.rnf delete'

instance Data.ToJSON ReplicaUpdate where
  toJSON ReplicaUpdate' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Create" Data..=) Prelude.<$> create,
            ("Delete" Data..=) Prelude.<$> delete'
          ]
      )
