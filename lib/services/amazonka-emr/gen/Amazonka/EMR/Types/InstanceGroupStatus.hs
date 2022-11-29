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
-- Module      : Amazonka.EMR.Types.InstanceGroupStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EMR.Types.InstanceGroupStatus where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.EMR.Types.InstanceGroupState
import Amazonka.EMR.Types.InstanceGroupStateChangeReason
import Amazonka.EMR.Types.InstanceGroupTimeline
import qualified Amazonka.Prelude as Prelude

-- | The details of the instance group status.
--
-- /See:/ 'newInstanceGroupStatus' smart constructor.
data InstanceGroupStatus = InstanceGroupStatus'
  { -- | The status change reason details for the instance group.
    stateChangeReason :: Prelude.Maybe InstanceGroupStateChangeReason,
    -- | The timeline of the instance group status over time.
    timeline :: Prelude.Maybe InstanceGroupTimeline,
    -- | The current state of the instance group.
    state :: Prelude.Maybe InstanceGroupState
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InstanceGroupStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'stateChangeReason', 'instanceGroupStatus_stateChangeReason' - The status change reason details for the instance group.
--
-- 'timeline', 'instanceGroupStatus_timeline' - The timeline of the instance group status over time.
--
-- 'state', 'instanceGroupStatus_state' - The current state of the instance group.
newInstanceGroupStatus ::
  InstanceGroupStatus
newInstanceGroupStatus =
  InstanceGroupStatus'
    { stateChangeReason =
        Prelude.Nothing,
      timeline = Prelude.Nothing,
      state = Prelude.Nothing
    }

-- | The status change reason details for the instance group.
instanceGroupStatus_stateChangeReason :: Lens.Lens' InstanceGroupStatus (Prelude.Maybe InstanceGroupStateChangeReason)
instanceGroupStatus_stateChangeReason = Lens.lens (\InstanceGroupStatus' {stateChangeReason} -> stateChangeReason) (\s@InstanceGroupStatus' {} a -> s {stateChangeReason = a} :: InstanceGroupStatus)

-- | The timeline of the instance group status over time.
instanceGroupStatus_timeline :: Lens.Lens' InstanceGroupStatus (Prelude.Maybe InstanceGroupTimeline)
instanceGroupStatus_timeline = Lens.lens (\InstanceGroupStatus' {timeline} -> timeline) (\s@InstanceGroupStatus' {} a -> s {timeline = a} :: InstanceGroupStatus)

-- | The current state of the instance group.
instanceGroupStatus_state :: Lens.Lens' InstanceGroupStatus (Prelude.Maybe InstanceGroupState)
instanceGroupStatus_state = Lens.lens (\InstanceGroupStatus' {state} -> state) (\s@InstanceGroupStatus' {} a -> s {state = a} :: InstanceGroupStatus)

instance Core.FromJSON InstanceGroupStatus where
  parseJSON =
    Core.withObject
      "InstanceGroupStatus"
      ( \x ->
          InstanceGroupStatus'
            Prelude.<$> (x Core..:? "StateChangeReason")
            Prelude.<*> (x Core..:? "Timeline")
            Prelude.<*> (x Core..:? "State")
      )

instance Prelude.Hashable InstanceGroupStatus where
  hashWithSalt _salt InstanceGroupStatus' {..} =
    _salt `Prelude.hashWithSalt` stateChangeReason
      `Prelude.hashWithSalt` timeline
      `Prelude.hashWithSalt` state

instance Prelude.NFData InstanceGroupStatus where
  rnf InstanceGroupStatus' {..} =
    Prelude.rnf stateChangeReason
      `Prelude.seq` Prelude.rnf timeline
      `Prelude.seq` Prelude.rnf state
