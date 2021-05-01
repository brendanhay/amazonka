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
-- Module      : Network.AWS.EC2.Types.ScheduledInstancesPlacement
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ScheduledInstancesPlacement where

import Network.AWS.EC2.Internal
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes the placement for a Scheduled Instance.
--
-- /See:/ 'newScheduledInstancesPlacement' smart constructor.
data ScheduledInstancesPlacement = ScheduledInstancesPlacement'
  { -- | The name of the placement group.
    groupName :: Prelude.Maybe Prelude.Text,
    -- | The Availability Zone.
    availabilityZone :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ScheduledInstancesPlacement' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'groupName', 'scheduledInstancesPlacement_groupName' - The name of the placement group.
--
-- 'availabilityZone', 'scheduledInstancesPlacement_availabilityZone' - The Availability Zone.
newScheduledInstancesPlacement ::
  ScheduledInstancesPlacement
newScheduledInstancesPlacement =
  ScheduledInstancesPlacement'
    { groupName =
        Prelude.Nothing,
      availabilityZone = Prelude.Nothing
    }

-- | The name of the placement group.
scheduledInstancesPlacement_groupName :: Lens.Lens' ScheduledInstancesPlacement (Prelude.Maybe Prelude.Text)
scheduledInstancesPlacement_groupName = Lens.lens (\ScheduledInstancesPlacement' {groupName} -> groupName) (\s@ScheduledInstancesPlacement' {} a -> s {groupName = a} :: ScheduledInstancesPlacement)

-- | The Availability Zone.
scheduledInstancesPlacement_availabilityZone :: Lens.Lens' ScheduledInstancesPlacement (Prelude.Maybe Prelude.Text)
scheduledInstancesPlacement_availabilityZone = Lens.lens (\ScheduledInstancesPlacement' {availabilityZone} -> availabilityZone) (\s@ScheduledInstancesPlacement' {} a -> s {availabilityZone = a} :: ScheduledInstancesPlacement)

instance Prelude.Hashable ScheduledInstancesPlacement

instance Prelude.NFData ScheduledInstancesPlacement

instance Prelude.ToQuery ScheduledInstancesPlacement where
  toQuery ScheduledInstancesPlacement' {..} =
    Prelude.mconcat
      [ "GroupName" Prelude.=: groupName,
        "AvailabilityZone" Prelude.=: availabilityZone
      ]
