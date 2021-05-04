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
-- Module      : Network.AWS.EC2.Types.DeleteFleetSuccessItem
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.DeleteFleetSuccessItem where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.FleetStateCode
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes an EC2 Fleet that was successfully deleted.
--
-- /See:/ 'newDeleteFleetSuccessItem' smart constructor.
data DeleteFleetSuccessItem = DeleteFleetSuccessItem'
  { -- | The ID of the EC2 Fleet.
    fleetId :: Prelude.Maybe Prelude.Text,
    -- | The current state of the EC2 Fleet.
    currentFleetState :: Prelude.Maybe FleetStateCode,
    -- | The previous state of the EC2 Fleet.
    previousFleetState :: Prelude.Maybe FleetStateCode
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteFleetSuccessItem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fleetId', 'deleteFleetSuccessItem_fleetId' - The ID of the EC2 Fleet.
--
-- 'currentFleetState', 'deleteFleetSuccessItem_currentFleetState' - The current state of the EC2 Fleet.
--
-- 'previousFleetState', 'deleteFleetSuccessItem_previousFleetState' - The previous state of the EC2 Fleet.
newDeleteFleetSuccessItem ::
  DeleteFleetSuccessItem
newDeleteFleetSuccessItem =
  DeleteFleetSuccessItem'
    { fleetId = Prelude.Nothing,
      currentFleetState = Prelude.Nothing,
      previousFleetState = Prelude.Nothing
    }

-- | The ID of the EC2 Fleet.
deleteFleetSuccessItem_fleetId :: Lens.Lens' DeleteFleetSuccessItem (Prelude.Maybe Prelude.Text)
deleteFleetSuccessItem_fleetId = Lens.lens (\DeleteFleetSuccessItem' {fleetId} -> fleetId) (\s@DeleteFleetSuccessItem' {} a -> s {fleetId = a} :: DeleteFleetSuccessItem)

-- | The current state of the EC2 Fleet.
deleteFleetSuccessItem_currentFleetState :: Lens.Lens' DeleteFleetSuccessItem (Prelude.Maybe FleetStateCode)
deleteFleetSuccessItem_currentFleetState = Lens.lens (\DeleteFleetSuccessItem' {currentFleetState} -> currentFleetState) (\s@DeleteFleetSuccessItem' {} a -> s {currentFleetState = a} :: DeleteFleetSuccessItem)

-- | The previous state of the EC2 Fleet.
deleteFleetSuccessItem_previousFleetState :: Lens.Lens' DeleteFleetSuccessItem (Prelude.Maybe FleetStateCode)
deleteFleetSuccessItem_previousFleetState = Lens.lens (\DeleteFleetSuccessItem' {previousFleetState} -> previousFleetState) (\s@DeleteFleetSuccessItem' {} a -> s {previousFleetState = a} :: DeleteFleetSuccessItem)

instance Prelude.FromXML DeleteFleetSuccessItem where
  parseXML x =
    DeleteFleetSuccessItem'
      Prelude.<$> (x Prelude..@? "fleetId")
      Prelude.<*> (x Prelude..@? "currentFleetState")
      Prelude.<*> (x Prelude..@? "previousFleetState")

instance Prelude.Hashable DeleteFleetSuccessItem

instance Prelude.NFData DeleteFleetSuccessItem
