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
-- Module      : Amazonka.EC2.Types.CapacityReservationGroup
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.CapacityReservationGroup where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

-- | Describes a resource group to which a Capacity Reservation has been
-- added.
--
-- /See:/ 'newCapacityReservationGroup' smart constructor.
data CapacityReservationGroup = CapacityReservationGroup'
  { -- | The ARN of the resource group.
    groupArn :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Amazon Web Services account that owns the resource group.
    ownerId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CapacityReservationGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'groupArn', 'capacityReservationGroup_groupArn' - The ARN of the resource group.
--
-- 'ownerId', 'capacityReservationGroup_ownerId' - The ID of the Amazon Web Services account that owns the resource group.
newCapacityReservationGroup ::
  CapacityReservationGroup
newCapacityReservationGroup =
  CapacityReservationGroup'
    { groupArn =
        Prelude.Nothing,
      ownerId = Prelude.Nothing
    }

-- | The ARN of the resource group.
capacityReservationGroup_groupArn :: Lens.Lens' CapacityReservationGroup (Prelude.Maybe Prelude.Text)
capacityReservationGroup_groupArn = Lens.lens (\CapacityReservationGroup' {groupArn} -> groupArn) (\s@CapacityReservationGroup' {} a -> s {groupArn = a} :: CapacityReservationGroup)

-- | The ID of the Amazon Web Services account that owns the resource group.
capacityReservationGroup_ownerId :: Lens.Lens' CapacityReservationGroup (Prelude.Maybe Prelude.Text)
capacityReservationGroup_ownerId = Lens.lens (\CapacityReservationGroup' {ownerId} -> ownerId) (\s@CapacityReservationGroup' {} a -> s {ownerId = a} :: CapacityReservationGroup)

instance Data.FromXML CapacityReservationGroup where
  parseXML x =
    CapacityReservationGroup'
      Prelude.<$> (x Data..@? "groupArn")
      Prelude.<*> (x Data..@? "ownerId")

instance Prelude.Hashable CapacityReservationGroup where
  hashWithSalt _salt CapacityReservationGroup' {..} =
    _salt `Prelude.hashWithSalt` groupArn
      `Prelude.hashWithSalt` ownerId

instance Prelude.NFData CapacityReservationGroup where
  rnf CapacityReservationGroup' {..} =
    Prelude.rnf groupArn
      `Prelude.seq` Prelude.rnf ownerId
