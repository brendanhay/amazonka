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
-- Module      : Network.AWS.EC2.Types.ReservedInstanceReservationValue
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ReservedInstanceReservationValue where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.ReservationValue
import qualified Network.AWS.Lens as Lens

-- | The total value of the Convertible Reserved Instance.
--
-- /See:/ 'newReservedInstanceReservationValue' smart constructor.
data ReservedInstanceReservationValue = ReservedInstanceReservationValue'
  { -- | The total value of the Convertible Reserved Instance that you are
    -- exchanging.
    reservationValue :: Core.Maybe ReservationValue,
    -- | The ID of the Convertible Reserved Instance that you are exchanging.
    reservedInstanceId :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
        Core.Nothing,
      reservedInstanceId = Core.Nothing
    }

-- | The total value of the Convertible Reserved Instance that you are
-- exchanging.
reservedInstanceReservationValue_reservationValue :: Lens.Lens' ReservedInstanceReservationValue (Core.Maybe ReservationValue)
reservedInstanceReservationValue_reservationValue = Lens.lens (\ReservedInstanceReservationValue' {reservationValue} -> reservationValue) (\s@ReservedInstanceReservationValue' {} a -> s {reservationValue = a} :: ReservedInstanceReservationValue)

-- | The ID of the Convertible Reserved Instance that you are exchanging.
reservedInstanceReservationValue_reservedInstanceId :: Lens.Lens' ReservedInstanceReservationValue (Core.Maybe Core.Text)
reservedInstanceReservationValue_reservedInstanceId = Lens.lens (\ReservedInstanceReservationValue' {reservedInstanceId} -> reservedInstanceId) (\s@ReservedInstanceReservationValue' {} a -> s {reservedInstanceId = a} :: ReservedInstanceReservationValue)

instance
  Core.FromXML
    ReservedInstanceReservationValue
  where
  parseXML x =
    ReservedInstanceReservationValue'
      Core.<$> (x Core..@? "reservationValue")
      Core.<*> (x Core..@? "reservedInstanceId")

instance
  Core.Hashable
    ReservedInstanceReservationValue

instance Core.NFData ReservedInstanceReservationValue
