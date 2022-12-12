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
-- Module      : Amazonka.EC2.Types.DeleteFleetSuccessItem
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.DeleteFleetSuccessItem where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.FleetStateCode
import qualified Amazonka.Prelude as Prelude

-- | Describes an EC2 Fleet that was successfully deleted.
--
-- /See:/ 'newDeleteFleetSuccessItem' smart constructor.
data DeleteFleetSuccessItem = DeleteFleetSuccessItem'
  { -- | The current state of the EC2 Fleet.
    currentFleetState :: Prelude.Maybe FleetStateCode,
    -- | The ID of the EC2 Fleet.
    fleetId :: Prelude.Maybe Prelude.Text,
    -- | The previous state of the EC2 Fleet.
    previousFleetState :: Prelude.Maybe FleetStateCode
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteFleetSuccessItem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'currentFleetState', 'deleteFleetSuccessItem_currentFleetState' - The current state of the EC2 Fleet.
--
-- 'fleetId', 'deleteFleetSuccessItem_fleetId' - The ID of the EC2 Fleet.
--
-- 'previousFleetState', 'deleteFleetSuccessItem_previousFleetState' - The previous state of the EC2 Fleet.
newDeleteFleetSuccessItem ::
  DeleteFleetSuccessItem
newDeleteFleetSuccessItem =
  DeleteFleetSuccessItem'
    { currentFleetState =
        Prelude.Nothing,
      fleetId = Prelude.Nothing,
      previousFleetState = Prelude.Nothing
    }

-- | The current state of the EC2 Fleet.
deleteFleetSuccessItem_currentFleetState :: Lens.Lens' DeleteFleetSuccessItem (Prelude.Maybe FleetStateCode)
deleteFleetSuccessItem_currentFleetState = Lens.lens (\DeleteFleetSuccessItem' {currentFleetState} -> currentFleetState) (\s@DeleteFleetSuccessItem' {} a -> s {currentFleetState = a} :: DeleteFleetSuccessItem)

-- | The ID of the EC2 Fleet.
deleteFleetSuccessItem_fleetId :: Lens.Lens' DeleteFleetSuccessItem (Prelude.Maybe Prelude.Text)
deleteFleetSuccessItem_fleetId = Lens.lens (\DeleteFleetSuccessItem' {fleetId} -> fleetId) (\s@DeleteFleetSuccessItem' {} a -> s {fleetId = a} :: DeleteFleetSuccessItem)

-- | The previous state of the EC2 Fleet.
deleteFleetSuccessItem_previousFleetState :: Lens.Lens' DeleteFleetSuccessItem (Prelude.Maybe FleetStateCode)
deleteFleetSuccessItem_previousFleetState = Lens.lens (\DeleteFleetSuccessItem' {previousFleetState} -> previousFleetState) (\s@DeleteFleetSuccessItem' {} a -> s {previousFleetState = a} :: DeleteFleetSuccessItem)

instance Data.FromXML DeleteFleetSuccessItem where
  parseXML x =
    DeleteFleetSuccessItem'
      Prelude.<$> (x Data..@? "currentFleetState")
      Prelude.<*> (x Data..@? "fleetId")
      Prelude.<*> (x Data..@? "previousFleetState")

instance Prelude.Hashable DeleteFleetSuccessItem where
  hashWithSalt _salt DeleteFleetSuccessItem' {..} =
    _salt `Prelude.hashWithSalt` currentFleetState
      `Prelude.hashWithSalt` fleetId
      `Prelude.hashWithSalt` previousFleetState

instance Prelude.NFData DeleteFleetSuccessItem where
  rnf DeleteFleetSuccessItem' {..} =
    Prelude.rnf currentFleetState
      `Prelude.seq` Prelude.rnf fleetId
      `Prelude.seq` Prelude.rnf previousFleetState
