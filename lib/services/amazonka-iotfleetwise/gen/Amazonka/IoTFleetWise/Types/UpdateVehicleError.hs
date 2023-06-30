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
-- Module      : Amazonka.IoTFleetWise.Types.UpdateVehicleError
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTFleetWise.Types.UpdateVehicleError where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An HTTP error resulting from updating the description for a vehicle.
--
-- /See:/ 'newUpdateVehicleError' smart constructor.
data UpdateVehicleError = UpdateVehicleError'
  { -- | The relevant HTTP error code (400+).
    code :: Prelude.Maybe Prelude.Int,
    -- | A message associated with the error.
    message :: Prelude.Maybe Prelude.Text,
    -- | The ID of the vehicle with the error.
    vehicleName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateVehicleError' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'code', 'updateVehicleError_code' - The relevant HTTP error code (400+).
--
-- 'message', 'updateVehicleError_message' - A message associated with the error.
--
-- 'vehicleName', 'updateVehicleError_vehicleName' - The ID of the vehicle with the error.
newUpdateVehicleError ::
  UpdateVehicleError
newUpdateVehicleError =
  UpdateVehicleError'
    { code = Prelude.Nothing,
      message = Prelude.Nothing,
      vehicleName = Prelude.Nothing
    }

-- | The relevant HTTP error code (400+).
updateVehicleError_code :: Lens.Lens' UpdateVehicleError (Prelude.Maybe Prelude.Int)
updateVehicleError_code = Lens.lens (\UpdateVehicleError' {code} -> code) (\s@UpdateVehicleError' {} a -> s {code = a} :: UpdateVehicleError)

-- | A message associated with the error.
updateVehicleError_message :: Lens.Lens' UpdateVehicleError (Prelude.Maybe Prelude.Text)
updateVehicleError_message = Lens.lens (\UpdateVehicleError' {message} -> message) (\s@UpdateVehicleError' {} a -> s {message = a} :: UpdateVehicleError)

-- | The ID of the vehicle with the error.
updateVehicleError_vehicleName :: Lens.Lens' UpdateVehicleError (Prelude.Maybe Prelude.Text)
updateVehicleError_vehicleName = Lens.lens (\UpdateVehicleError' {vehicleName} -> vehicleName) (\s@UpdateVehicleError' {} a -> s {vehicleName = a} :: UpdateVehicleError)

instance Data.FromJSON UpdateVehicleError where
  parseJSON =
    Data.withObject
      "UpdateVehicleError"
      ( \x ->
          UpdateVehicleError'
            Prelude.<$> (x Data..:? "code")
            Prelude.<*> (x Data..:? "message")
            Prelude.<*> (x Data..:? "vehicleName")
      )

instance Prelude.Hashable UpdateVehicleError where
  hashWithSalt _salt UpdateVehicleError' {..} =
    _salt
      `Prelude.hashWithSalt` code
      `Prelude.hashWithSalt` message
      `Prelude.hashWithSalt` vehicleName

instance Prelude.NFData UpdateVehicleError where
  rnf UpdateVehicleError' {..} =
    Prelude.rnf code
      `Prelude.seq` Prelude.rnf message
      `Prelude.seq` Prelude.rnf vehicleName
