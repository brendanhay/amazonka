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
-- Module      : Amazonka.EC2.Types.CapacityReservationTargetResponse
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.CapacityReservationTargetResponse where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

-- | Describes a target Capacity Reservation or Capacity Reservation group.
--
-- /See:/ 'newCapacityReservationTargetResponse' smart constructor.
data CapacityReservationTargetResponse = CapacityReservationTargetResponse'
  { -- | The ID of the targeted Capacity Reservation.
    capacityReservationId :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the targeted Capacity Reservation group.
    capacityReservationResourceGroupArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CapacityReservationTargetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'capacityReservationId', 'capacityReservationTargetResponse_capacityReservationId' - The ID of the targeted Capacity Reservation.
--
-- 'capacityReservationResourceGroupArn', 'capacityReservationTargetResponse_capacityReservationResourceGroupArn' - The ARN of the targeted Capacity Reservation group.
newCapacityReservationTargetResponse ::
  CapacityReservationTargetResponse
newCapacityReservationTargetResponse =
  CapacityReservationTargetResponse'
    { capacityReservationId =
        Prelude.Nothing,
      capacityReservationResourceGroupArn =
        Prelude.Nothing
    }

-- | The ID of the targeted Capacity Reservation.
capacityReservationTargetResponse_capacityReservationId :: Lens.Lens' CapacityReservationTargetResponse (Prelude.Maybe Prelude.Text)
capacityReservationTargetResponse_capacityReservationId = Lens.lens (\CapacityReservationTargetResponse' {capacityReservationId} -> capacityReservationId) (\s@CapacityReservationTargetResponse' {} a -> s {capacityReservationId = a} :: CapacityReservationTargetResponse)

-- | The ARN of the targeted Capacity Reservation group.
capacityReservationTargetResponse_capacityReservationResourceGroupArn :: Lens.Lens' CapacityReservationTargetResponse (Prelude.Maybe Prelude.Text)
capacityReservationTargetResponse_capacityReservationResourceGroupArn = Lens.lens (\CapacityReservationTargetResponse' {capacityReservationResourceGroupArn} -> capacityReservationResourceGroupArn) (\s@CapacityReservationTargetResponse' {} a -> s {capacityReservationResourceGroupArn = a} :: CapacityReservationTargetResponse)

instance
  Data.FromXML
    CapacityReservationTargetResponse
  where
  parseXML x =
    CapacityReservationTargetResponse'
      Prelude.<$> (x Data..@? "capacityReservationId")
      Prelude.<*> (x Data..@? "capacityReservationResourceGroupArn")

instance
  Prelude.Hashable
    CapacityReservationTargetResponse
  where
  hashWithSalt
    _salt
    CapacityReservationTargetResponse' {..} =
      _salt `Prelude.hashWithSalt` capacityReservationId
        `Prelude.hashWithSalt` capacityReservationResourceGroupArn

instance
  Prelude.NFData
    CapacityReservationTargetResponse
  where
  rnf CapacityReservationTargetResponse' {..} =
    Prelude.rnf capacityReservationId
      `Prelude.seq` Prelude.rnf capacityReservationResourceGroupArn
