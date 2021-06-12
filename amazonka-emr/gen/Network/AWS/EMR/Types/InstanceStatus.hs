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
-- Module      : Network.AWS.EMR.Types.InstanceStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.InstanceStatus where

import qualified Network.AWS.Core as Core
import Network.AWS.EMR.Types.InstanceState
import Network.AWS.EMR.Types.InstanceStateChangeReason
import Network.AWS.EMR.Types.InstanceTimeline
import qualified Network.AWS.Lens as Lens

-- | The instance status details.
--
-- /See:/ 'newInstanceStatus' smart constructor.
data InstanceStatus = InstanceStatus'
  { -- | The details of the status change reason for the instance.
    stateChangeReason :: Core.Maybe InstanceStateChangeReason,
    -- | The current state of the instance.
    state :: Core.Maybe InstanceState,
    -- | The timeline of the instance status over time.
    timeline :: Core.Maybe InstanceTimeline
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'InstanceStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'stateChangeReason', 'instanceStatus_stateChangeReason' - The details of the status change reason for the instance.
--
-- 'state', 'instanceStatus_state' - The current state of the instance.
--
-- 'timeline', 'instanceStatus_timeline' - The timeline of the instance status over time.
newInstanceStatus ::
  InstanceStatus
newInstanceStatus =
  InstanceStatus'
    { stateChangeReason = Core.Nothing,
      state = Core.Nothing,
      timeline = Core.Nothing
    }

-- | The details of the status change reason for the instance.
instanceStatus_stateChangeReason :: Lens.Lens' InstanceStatus (Core.Maybe InstanceStateChangeReason)
instanceStatus_stateChangeReason = Lens.lens (\InstanceStatus' {stateChangeReason} -> stateChangeReason) (\s@InstanceStatus' {} a -> s {stateChangeReason = a} :: InstanceStatus)

-- | The current state of the instance.
instanceStatus_state :: Lens.Lens' InstanceStatus (Core.Maybe InstanceState)
instanceStatus_state = Lens.lens (\InstanceStatus' {state} -> state) (\s@InstanceStatus' {} a -> s {state = a} :: InstanceStatus)

-- | The timeline of the instance status over time.
instanceStatus_timeline :: Lens.Lens' InstanceStatus (Core.Maybe InstanceTimeline)
instanceStatus_timeline = Lens.lens (\InstanceStatus' {timeline} -> timeline) (\s@InstanceStatus' {} a -> s {timeline = a} :: InstanceStatus)

instance Core.FromJSON InstanceStatus where
  parseJSON =
    Core.withObject
      "InstanceStatus"
      ( \x ->
          InstanceStatus'
            Core.<$> (x Core..:? "StateChangeReason")
            Core.<*> (x Core..:? "State")
            Core.<*> (x Core..:? "Timeline")
      )

instance Core.Hashable InstanceStatus

instance Core.NFData InstanceStatus
