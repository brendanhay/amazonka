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
-- Module      : Network.AWS.DynamoDB.Types.ReplicaUpdate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.ReplicaUpdate where

import Network.AWS.DynamoDB.Types.CreateReplicaAction
import Network.AWS.DynamoDB.Types.DeleteReplicaAction
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.Hashable ReplicaUpdate

instance Prelude.NFData ReplicaUpdate

instance Prelude.ToJSON ReplicaUpdate where
  toJSON ReplicaUpdate' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("Create" Prelude..=) Prelude.<$> create,
            ("Delete" Prelude..=) Prelude.<$> delete'
          ]
      )
