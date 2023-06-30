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
-- Module      : Amazonka.SSMIncidents.Types.UpdateReplicationSetAction
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSMIncidents.Types.UpdateReplicationSetAction where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SSMIncidents.Types.AddRegionAction
import Amazonka.SSMIncidents.Types.DeleteRegionAction

-- | Details used when updating the replication set.
--
-- /See:/ 'newUpdateReplicationSetAction' smart constructor.
data UpdateReplicationSetAction = UpdateReplicationSetAction'
  { -- | Details about the Amazon Web Services Region that you\'re adding to the
    -- replication set.
    addRegionAction :: Prelude.Maybe AddRegionAction,
    -- | Details about the Amazon Web Services Region that you\'re deleting to
    -- the replication set.
    deleteRegionAction :: Prelude.Maybe DeleteRegionAction
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateReplicationSetAction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'addRegionAction', 'updateReplicationSetAction_addRegionAction' - Details about the Amazon Web Services Region that you\'re adding to the
-- replication set.
--
-- 'deleteRegionAction', 'updateReplicationSetAction_deleteRegionAction' - Details about the Amazon Web Services Region that you\'re deleting to
-- the replication set.
newUpdateReplicationSetAction ::
  UpdateReplicationSetAction
newUpdateReplicationSetAction =
  UpdateReplicationSetAction'
    { addRegionAction =
        Prelude.Nothing,
      deleteRegionAction = Prelude.Nothing
    }

-- | Details about the Amazon Web Services Region that you\'re adding to the
-- replication set.
updateReplicationSetAction_addRegionAction :: Lens.Lens' UpdateReplicationSetAction (Prelude.Maybe AddRegionAction)
updateReplicationSetAction_addRegionAction = Lens.lens (\UpdateReplicationSetAction' {addRegionAction} -> addRegionAction) (\s@UpdateReplicationSetAction' {} a -> s {addRegionAction = a} :: UpdateReplicationSetAction)

-- | Details about the Amazon Web Services Region that you\'re deleting to
-- the replication set.
updateReplicationSetAction_deleteRegionAction :: Lens.Lens' UpdateReplicationSetAction (Prelude.Maybe DeleteRegionAction)
updateReplicationSetAction_deleteRegionAction = Lens.lens (\UpdateReplicationSetAction' {deleteRegionAction} -> deleteRegionAction) (\s@UpdateReplicationSetAction' {} a -> s {deleteRegionAction = a} :: UpdateReplicationSetAction)

instance Prelude.Hashable UpdateReplicationSetAction where
  hashWithSalt _salt UpdateReplicationSetAction' {..} =
    _salt
      `Prelude.hashWithSalt` addRegionAction
      `Prelude.hashWithSalt` deleteRegionAction

instance Prelude.NFData UpdateReplicationSetAction where
  rnf UpdateReplicationSetAction' {..} =
    Prelude.rnf addRegionAction
      `Prelude.seq` Prelude.rnf deleteRegionAction

instance Data.ToJSON UpdateReplicationSetAction where
  toJSON UpdateReplicationSetAction' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("addRegionAction" Data..=)
              Prelude.<$> addRegionAction,
            ("deleteRegionAction" Data..=)
              Prelude.<$> deleteRegionAction
          ]
      )
