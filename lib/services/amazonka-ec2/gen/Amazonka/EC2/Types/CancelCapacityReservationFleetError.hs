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
-- Module      : Amazonka.EC2.Types.CancelCapacityReservationFleetError
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.CancelCapacityReservationFleetError where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

-- | Describes a Capacity Reservation Fleet cancellation error.
--
-- /See:/ 'newCancelCapacityReservationFleetError' smart constructor.
data CancelCapacityReservationFleetError = CancelCapacityReservationFleetError'
  { -- | The error code.
    code :: Prelude.Maybe Prelude.Text,
    -- | The error message.
    message :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CancelCapacityReservationFleetError' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'code', 'cancelCapacityReservationFleetError_code' - The error code.
--
-- 'message', 'cancelCapacityReservationFleetError_message' - The error message.
newCancelCapacityReservationFleetError ::
  CancelCapacityReservationFleetError
newCancelCapacityReservationFleetError =
  CancelCapacityReservationFleetError'
    { code =
        Prelude.Nothing,
      message = Prelude.Nothing
    }

-- | The error code.
cancelCapacityReservationFleetError_code :: Lens.Lens' CancelCapacityReservationFleetError (Prelude.Maybe Prelude.Text)
cancelCapacityReservationFleetError_code = Lens.lens (\CancelCapacityReservationFleetError' {code} -> code) (\s@CancelCapacityReservationFleetError' {} a -> s {code = a} :: CancelCapacityReservationFleetError)

-- | The error message.
cancelCapacityReservationFleetError_message :: Lens.Lens' CancelCapacityReservationFleetError (Prelude.Maybe Prelude.Text)
cancelCapacityReservationFleetError_message = Lens.lens (\CancelCapacityReservationFleetError' {message} -> message) (\s@CancelCapacityReservationFleetError' {} a -> s {message = a} :: CancelCapacityReservationFleetError)

instance
  Data.FromXML
    CancelCapacityReservationFleetError
  where
  parseXML x =
    CancelCapacityReservationFleetError'
      Prelude.<$> (x Data..@? "code")
      Prelude.<*> (x Data..@? "message")

instance
  Prelude.Hashable
    CancelCapacityReservationFleetError
  where
  hashWithSalt
    _salt
    CancelCapacityReservationFleetError' {..} =
      _salt `Prelude.hashWithSalt` code
        `Prelude.hashWithSalt` message

instance
  Prelude.NFData
    CancelCapacityReservationFleetError
  where
  rnf CancelCapacityReservationFleetError' {..} =
    Prelude.rnf code `Prelude.seq` Prelude.rnf message
