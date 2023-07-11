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
-- Module      : Amazonka.EC2.Types.ReservedInstanceReservationValue
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.ReservedInstanceReservationValue where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.ReservationValue
import qualified Amazonka.Prelude as Prelude

-- | The total value of the Convertible Reserved Instance.
--
-- /See:/ 'newReservedInstanceReservationValue' smart constructor.
data ReservedInstanceReservationValue = ReservedInstanceReservationValue'
  { -- | The total value of the Convertible Reserved Instance that you are
    -- exchanging.
    reservationValue :: Prelude.Maybe ReservationValue,
    -- | The ID of the Convertible Reserved Instance that you are exchanging.
    reservedInstanceId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ReservedInstanceReservationValue' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'reservationValue', 'reservedInstanceReservationValue_reservationValue' - The total value of the Convertible Reserved Instance that you are
-- exchanging.
--
-- 'reservedInstanceId', 'reservedInstanceReservationValue_reservedInstanceId' - The ID of the Convertible Reserved Instance that you are exchanging.
newReservedInstanceReservationValue ::
  ReservedInstanceReservationValue
newReservedInstanceReservationValue =
  ReservedInstanceReservationValue'
    { reservationValue =
        Prelude.Nothing,
      reservedInstanceId = Prelude.Nothing
    }

-- | The total value of the Convertible Reserved Instance that you are
-- exchanging.
reservedInstanceReservationValue_reservationValue :: Lens.Lens' ReservedInstanceReservationValue (Prelude.Maybe ReservationValue)
reservedInstanceReservationValue_reservationValue = Lens.lens (\ReservedInstanceReservationValue' {reservationValue} -> reservationValue) (\s@ReservedInstanceReservationValue' {} a -> s {reservationValue = a} :: ReservedInstanceReservationValue)

-- | The ID of the Convertible Reserved Instance that you are exchanging.
reservedInstanceReservationValue_reservedInstanceId :: Lens.Lens' ReservedInstanceReservationValue (Prelude.Maybe Prelude.Text)
reservedInstanceReservationValue_reservedInstanceId = Lens.lens (\ReservedInstanceReservationValue' {reservedInstanceId} -> reservedInstanceId) (\s@ReservedInstanceReservationValue' {} a -> s {reservedInstanceId = a} :: ReservedInstanceReservationValue)

instance
  Data.FromXML
    ReservedInstanceReservationValue
  where
  parseXML x =
    ReservedInstanceReservationValue'
      Prelude.<$> (x Data..@? "reservationValue")
      Prelude.<*> (x Data..@? "reservedInstanceId")

instance
  Prelude.Hashable
    ReservedInstanceReservationValue
  where
  hashWithSalt
    _salt
    ReservedInstanceReservationValue' {..} =
      _salt
        `Prelude.hashWithSalt` reservationValue
        `Prelude.hashWithSalt` reservedInstanceId

instance
  Prelude.NFData
    ReservedInstanceReservationValue
  where
  rnf ReservedInstanceReservationValue' {..} =
    Prelude.rnf reservationValue
      `Prelude.seq` Prelude.rnf reservedInstanceId
