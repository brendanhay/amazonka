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
-- Module      : Network.AWS.EC2.Types.DeleteFleetErrorItem
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.DeleteFleetErrorItem where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.DeleteFleetError
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes an EC2 Fleet that was not successfully deleted.
--
-- /See:/ 'newDeleteFleetErrorItem' smart constructor.
data DeleteFleetErrorItem = DeleteFleetErrorItem'
  { -- | The error.
    error :: Prelude.Maybe DeleteFleetError,
    -- | The ID of the EC2 Fleet.
    fleetId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteFleetErrorItem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'error', 'deleteFleetErrorItem_error' - The error.
--
-- 'fleetId', 'deleteFleetErrorItem_fleetId' - The ID of the EC2 Fleet.
newDeleteFleetErrorItem ::
  DeleteFleetErrorItem
newDeleteFleetErrorItem =
  DeleteFleetErrorItem'
    { error = Prelude.Nothing,
      fleetId = Prelude.Nothing
    }

-- | The error.
deleteFleetErrorItem_error :: Lens.Lens' DeleteFleetErrorItem (Prelude.Maybe DeleteFleetError)
deleteFleetErrorItem_error = Lens.lens (\DeleteFleetErrorItem' {error} -> error) (\s@DeleteFleetErrorItem' {} a -> s {error = a} :: DeleteFleetErrorItem)

-- | The ID of the EC2 Fleet.
deleteFleetErrorItem_fleetId :: Lens.Lens' DeleteFleetErrorItem (Prelude.Maybe Prelude.Text)
deleteFleetErrorItem_fleetId = Lens.lens (\DeleteFleetErrorItem' {fleetId} -> fleetId) (\s@DeleteFleetErrorItem' {} a -> s {fleetId = a} :: DeleteFleetErrorItem)

instance Core.FromXML DeleteFleetErrorItem where
  parseXML x =
    DeleteFleetErrorItem'
      Prelude.<$> (x Core..@? "error")
      Prelude.<*> (x Core..@? "fleetId")

instance Prelude.Hashable DeleteFleetErrorItem

instance Prelude.NFData DeleteFleetErrorItem
