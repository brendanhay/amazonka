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
-- Module      : Amazonka.EC2.Types.ScheduledInstancesPlacement
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.ScheduledInstancesPlacement where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

-- | Describes the placement for a Scheduled Instance.
--
-- /See:/ 'newScheduledInstancesPlacement' smart constructor.
data ScheduledInstancesPlacement = ScheduledInstancesPlacement'
  { -- | The Availability Zone.
    availabilityZone :: Prelude.Maybe Prelude.Text,
    -- | The name of the placement group.
    groupName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ScheduledInstancesPlacement' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'availabilityZone', 'scheduledInstancesPlacement_availabilityZone' - The Availability Zone.
--
-- 'groupName', 'scheduledInstancesPlacement_groupName' - The name of the placement group.
newScheduledInstancesPlacement ::
  ScheduledInstancesPlacement
newScheduledInstancesPlacement =
  ScheduledInstancesPlacement'
    { availabilityZone =
        Prelude.Nothing,
      groupName = Prelude.Nothing
    }

-- | The Availability Zone.
scheduledInstancesPlacement_availabilityZone :: Lens.Lens' ScheduledInstancesPlacement (Prelude.Maybe Prelude.Text)
scheduledInstancesPlacement_availabilityZone = Lens.lens (\ScheduledInstancesPlacement' {availabilityZone} -> availabilityZone) (\s@ScheduledInstancesPlacement' {} a -> s {availabilityZone = a} :: ScheduledInstancesPlacement)

-- | The name of the placement group.
scheduledInstancesPlacement_groupName :: Lens.Lens' ScheduledInstancesPlacement (Prelude.Maybe Prelude.Text)
scheduledInstancesPlacement_groupName = Lens.lens (\ScheduledInstancesPlacement' {groupName} -> groupName) (\s@ScheduledInstancesPlacement' {} a -> s {groupName = a} :: ScheduledInstancesPlacement)

instance Prelude.Hashable ScheduledInstancesPlacement where
  hashWithSalt _salt ScheduledInstancesPlacement' {..} =
    _salt
      `Prelude.hashWithSalt` availabilityZone
      `Prelude.hashWithSalt` groupName

instance Prelude.NFData ScheduledInstancesPlacement where
  rnf ScheduledInstancesPlacement' {..} =
    Prelude.rnf availabilityZone `Prelude.seq`
      Prelude.rnf groupName

instance Data.ToQuery ScheduledInstancesPlacement where
  toQuery ScheduledInstancesPlacement' {..} =
    Prelude.mconcat
      [ "AvailabilityZone" Data.=: availabilityZone,
        "GroupName" Data.=: groupName
      ]
