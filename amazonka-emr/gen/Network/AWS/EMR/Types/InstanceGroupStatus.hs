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
-- Module      : Network.AWS.EMR.Types.InstanceGroupStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.InstanceGroupStatus where

import Network.AWS.EMR.Types.InstanceGroupState
import Network.AWS.EMR.Types.InstanceGroupStateChangeReason
import Network.AWS.EMR.Types.InstanceGroupTimeline
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The details of the instance group status.
--
-- /See:/ 'newInstanceGroupStatus' smart constructor.
data InstanceGroupStatus = InstanceGroupStatus'
  { -- | The status change reason details for the instance group.
    stateChangeReason :: Prelude.Maybe InstanceGroupStateChangeReason,
    -- | The current state of the instance group.
    state :: Prelude.Maybe InstanceGroupState,
    -- | The timeline of the instance group status over time.
    timeline :: Prelude.Maybe InstanceGroupTimeline
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
-- 'state', 'instanceGroupStatus_state' - The current state of the instance group.
--
-- 'timeline', 'instanceGroupStatus_timeline' - The timeline of the instance group status over time.
newInstanceGroupStatus ::
  InstanceGroupStatus
newInstanceGroupStatus =
  InstanceGroupStatus'
    { stateChangeReason =
        Prelude.Nothing,
      state = Prelude.Nothing,
      timeline = Prelude.Nothing
    }

-- | The status change reason details for the instance group.
instanceGroupStatus_stateChangeReason :: Lens.Lens' InstanceGroupStatus (Prelude.Maybe InstanceGroupStateChangeReason)
instanceGroupStatus_stateChangeReason = Lens.lens (\InstanceGroupStatus' {stateChangeReason} -> stateChangeReason) (\s@InstanceGroupStatus' {} a -> s {stateChangeReason = a} :: InstanceGroupStatus)

-- | The current state of the instance group.
instanceGroupStatus_state :: Lens.Lens' InstanceGroupStatus (Prelude.Maybe InstanceGroupState)
instanceGroupStatus_state = Lens.lens (\InstanceGroupStatus' {state} -> state) (\s@InstanceGroupStatus' {} a -> s {state = a} :: InstanceGroupStatus)

-- | The timeline of the instance group status over time.
instanceGroupStatus_timeline :: Lens.Lens' InstanceGroupStatus (Prelude.Maybe InstanceGroupTimeline)
instanceGroupStatus_timeline = Lens.lens (\InstanceGroupStatus' {timeline} -> timeline) (\s@InstanceGroupStatus' {} a -> s {timeline = a} :: InstanceGroupStatus)

instance Prelude.FromJSON InstanceGroupStatus where
  parseJSON =
    Prelude.withObject
      "InstanceGroupStatus"
      ( \x ->
          InstanceGroupStatus'
            Prelude.<$> (x Prelude..:? "StateChangeReason")
            Prelude.<*> (x Prelude..:? "State")
            Prelude.<*> (x Prelude..:? "Timeline")
      )

instance Prelude.Hashable InstanceGroupStatus

instance Prelude.NFData InstanceGroupStatus
