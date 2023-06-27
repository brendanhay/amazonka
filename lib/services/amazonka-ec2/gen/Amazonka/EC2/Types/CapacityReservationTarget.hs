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
-- Module      : Amazonka.EC2.Types.CapacityReservationTarget
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.CapacityReservationTarget where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

-- | Describes a target Capacity Reservation or Capacity Reservation group.
--
-- /See:/ 'newCapacityReservationTarget' smart constructor.
data CapacityReservationTarget = CapacityReservationTarget'
  { -- | The ID of the Capacity Reservation in which to run the instance.
    capacityReservationId :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the Capacity Reservation resource group in which to run the
    -- instance.
    capacityReservationResourceGroupArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CapacityReservationTarget' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'capacityReservationId', 'capacityReservationTarget_capacityReservationId' - The ID of the Capacity Reservation in which to run the instance.
--
-- 'capacityReservationResourceGroupArn', 'capacityReservationTarget_capacityReservationResourceGroupArn' - The ARN of the Capacity Reservation resource group in which to run the
-- instance.
newCapacityReservationTarget ::
  CapacityReservationTarget
newCapacityReservationTarget =
  CapacityReservationTarget'
    { capacityReservationId =
        Prelude.Nothing,
      capacityReservationResourceGroupArn =
        Prelude.Nothing
    }

-- | The ID of the Capacity Reservation in which to run the instance.
capacityReservationTarget_capacityReservationId :: Lens.Lens' CapacityReservationTarget (Prelude.Maybe Prelude.Text)
capacityReservationTarget_capacityReservationId = Lens.lens (\CapacityReservationTarget' {capacityReservationId} -> capacityReservationId) (\s@CapacityReservationTarget' {} a -> s {capacityReservationId = a} :: CapacityReservationTarget)

-- | The ARN of the Capacity Reservation resource group in which to run the
-- instance.
capacityReservationTarget_capacityReservationResourceGroupArn :: Lens.Lens' CapacityReservationTarget (Prelude.Maybe Prelude.Text)
capacityReservationTarget_capacityReservationResourceGroupArn = Lens.lens (\CapacityReservationTarget' {capacityReservationResourceGroupArn} -> capacityReservationResourceGroupArn) (\s@CapacityReservationTarget' {} a -> s {capacityReservationResourceGroupArn = a} :: CapacityReservationTarget)

instance Prelude.Hashable CapacityReservationTarget where
  hashWithSalt _salt CapacityReservationTarget' {..} =
    _salt
      `Prelude.hashWithSalt` capacityReservationId
      `Prelude.hashWithSalt` capacityReservationResourceGroupArn

instance Prelude.NFData CapacityReservationTarget where
  rnf CapacityReservationTarget' {..} =
    Prelude.rnf capacityReservationId
      `Prelude.seq` Prelude.rnf capacityReservationResourceGroupArn

instance Data.ToQuery CapacityReservationTarget where
  toQuery CapacityReservationTarget' {..} =
    Prelude.mconcat
      [ "CapacityReservationId"
          Data.=: capacityReservationId,
        "CapacityReservationResourceGroupArn"
          Data.=: capacityReservationResourceGroupArn
      ]
