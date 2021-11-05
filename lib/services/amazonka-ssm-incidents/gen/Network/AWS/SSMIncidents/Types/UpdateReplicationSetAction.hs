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
-- Module      : Network.AWS.SSMIncidents.Types.UpdateReplicationSetAction
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSMIncidents.Types.UpdateReplicationSetAction where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SSMIncidents.Types.AddRegionAction
import Network.AWS.SSMIncidents.Types.DeleteRegionAction

-- | Details used when updating the replication set.
--
-- /See:/ 'newUpdateReplicationSetAction' smart constructor.
data UpdateReplicationSetAction = UpdateReplicationSetAction'
  { -- | Details about the Region that you\'re adding to the replication set.
    addRegionAction :: Prelude.Maybe AddRegionAction,
    -- | Details about the Region that you\'re deleting to the replication set.
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
-- 'addRegionAction', 'updateReplicationSetAction_addRegionAction' - Details about the Region that you\'re adding to the replication set.
--
-- 'deleteRegionAction', 'updateReplicationSetAction_deleteRegionAction' - Details about the Region that you\'re deleting to the replication set.
newUpdateReplicationSetAction ::
  UpdateReplicationSetAction
newUpdateReplicationSetAction =
  UpdateReplicationSetAction'
    { addRegionAction =
        Prelude.Nothing,
      deleteRegionAction = Prelude.Nothing
    }

-- | Details about the Region that you\'re adding to the replication set.
updateReplicationSetAction_addRegionAction :: Lens.Lens' UpdateReplicationSetAction (Prelude.Maybe AddRegionAction)
updateReplicationSetAction_addRegionAction = Lens.lens (\UpdateReplicationSetAction' {addRegionAction} -> addRegionAction) (\s@UpdateReplicationSetAction' {} a -> s {addRegionAction = a} :: UpdateReplicationSetAction)

-- | Details about the Region that you\'re deleting to the replication set.
updateReplicationSetAction_deleteRegionAction :: Lens.Lens' UpdateReplicationSetAction (Prelude.Maybe DeleteRegionAction)
updateReplicationSetAction_deleteRegionAction = Lens.lens (\UpdateReplicationSetAction' {deleteRegionAction} -> deleteRegionAction) (\s@UpdateReplicationSetAction' {} a -> s {deleteRegionAction = a} :: UpdateReplicationSetAction)

instance Prelude.Hashable UpdateReplicationSetAction

instance Prelude.NFData UpdateReplicationSetAction

instance Core.ToJSON UpdateReplicationSetAction where
  toJSON UpdateReplicationSetAction' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("addRegionAction" Core..=)
              Prelude.<$> addRegionAction,
            ("deleteRegionAction" Core..=)
              Prelude.<$> deleteRegionAction
          ]
      )
