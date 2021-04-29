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
-- Module      : Network.AWS.EC2.Types.CapacityReservationGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.CapacityReservationGroup where

import Network.AWS.EC2.Internal
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes a resource group to which a Capacity Reservation has been
-- added.
--
-- /See:/ 'newCapacityReservationGroup' smart constructor.
data CapacityReservationGroup = CapacityReservationGroup'
  { -- | The ID of the AWS account that owns the resource group.
    ownerId :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the resource group.
    groupArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CapacityReservationGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ownerId', 'capacityReservationGroup_ownerId' - The ID of the AWS account that owns the resource group.
--
-- 'groupArn', 'capacityReservationGroup_groupArn' - The ARN of the resource group.
newCapacityReservationGroup ::
  CapacityReservationGroup
newCapacityReservationGroup =
  CapacityReservationGroup'
    { ownerId =
        Prelude.Nothing,
      groupArn = Prelude.Nothing
    }

-- | The ID of the AWS account that owns the resource group.
capacityReservationGroup_ownerId :: Lens.Lens' CapacityReservationGroup (Prelude.Maybe Prelude.Text)
capacityReservationGroup_ownerId = Lens.lens (\CapacityReservationGroup' {ownerId} -> ownerId) (\s@CapacityReservationGroup' {} a -> s {ownerId = a} :: CapacityReservationGroup)

-- | The ARN of the resource group.
capacityReservationGroup_groupArn :: Lens.Lens' CapacityReservationGroup (Prelude.Maybe Prelude.Text)
capacityReservationGroup_groupArn = Lens.lens (\CapacityReservationGroup' {groupArn} -> groupArn) (\s@CapacityReservationGroup' {} a -> s {groupArn = a} :: CapacityReservationGroup)

instance Prelude.FromXML CapacityReservationGroup where
  parseXML x =
    CapacityReservationGroup'
      Prelude.<$> (x Prelude..@? "ownerId")
      Prelude.<*> (x Prelude..@? "groupArn")

instance Prelude.Hashable CapacityReservationGroup

instance Prelude.NFData CapacityReservationGroup
