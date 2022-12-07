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
-- Module      : Amazonka.EC2.Types.TargetReservationValue
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.TargetReservationValue where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.ReservationValue
import Amazonka.EC2.Types.TargetConfiguration
import qualified Amazonka.Prelude as Prelude

-- | The total value of the new Convertible Reserved Instances.
--
-- /See:/ 'newTargetReservationValue' smart constructor.
data TargetReservationValue = TargetReservationValue'
  { -- | The total value of the Convertible Reserved Instances that make up the
    -- exchange. This is the sum of the list value, remaining upfront price,
    -- and additional upfront cost of the exchange.
    reservationValue :: Prelude.Maybe ReservationValue,
    -- | The configuration of the Convertible Reserved Instances that make up the
    -- exchange.
    targetConfiguration :: Prelude.Maybe TargetConfiguration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TargetReservationValue' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'reservationValue', 'targetReservationValue_reservationValue' - The total value of the Convertible Reserved Instances that make up the
-- exchange. This is the sum of the list value, remaining upfront price,
-- and additional upfront cost of the exchange.
--
-- 'targetConfiguration', 'targetReservationValue_targetConfiguration' - The configuration of the Convertible Reserved Instances that make up the
-- exchange.
newTargetReservationValue ::
  TargetReservationValue
newTargetReservationValue =
  TargetReservationValue'
    { reservationValue =
        Prelude.Nothing,
      targetConfiguration = Prelude.Nothing
    }

-- | The total value of the Convertible Reserved Instances that make up the
-- exchange. This is the sum of the list value, remaining upfront price,
-- and additional upfront cost of the exchange.
targetReservationValue_reservationValue :: Lens.Lens' TargetReservationValue (Prelude.Maybe ReservationValue)
targetReservationValue_reservationValue = Lens.lens (\TargetReservationValue' {reservationValue} -> reservationValue) (\s@TargetReservationValue' {} a -> s {reservationValue = a} :: TargetReservationValue)

-- | The configuration of the Convertible Reserved Instances that make up the
-- exchange.
targetReservationValue_targetConfiguration :: Lens.Lens' TargetReservationValue (Prelude.Maybe TargetConfiguration)
targetReservationValue_targetConfiguration = Lens.lens (\TargetReservationValue' {targetConfiguration} -> targetConfiguration) (\s@TargetReservationValue' {} a -> s {targetConfiguration = a} :: TargetReservationValue)

instance Data.FromXML TargetReservationValue where
  parseXML x =
    TargetReservationValue'
      Prelude.<$> (x Data..@? "reservationValue")
      Prelude.<*> (x Data..@? "targetConfiguration")

instance Prelude.Hashable TargetReservationValue where
  hashWithSalt _salt TargetReservationValue' {..} =
    _salt `Prelude.hashWithSalt` reservationValue
      `Prelude.hashWithSalt` targetConfiguration

instance Prelude.NFData TargetReservationValue where
  rnf TargetReservationValue' {..} =
    Prelude.rnf reservationValue
      `Prelude.seq` Prelude.rnf targetConfiguration
