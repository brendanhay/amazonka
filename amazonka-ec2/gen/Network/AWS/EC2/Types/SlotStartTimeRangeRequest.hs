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
-- Module      : Network.AWS.EC2.Types.SlotStartTimeRangeRequest
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.SlotStartTimeRangeRequest where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import qualified Network.AWS.Lens as Lens

-- | Describes the time period for a Scheduled Instance to start its first
-- schedule.
--
-- /See:/ 'newSlotStartTimeRangeRequest' smart constructor.
data SlotStartTimeRangeRequest = SlotStartTimeRangeRequest'
  { -- | The earliest date and time, in UTC, for the Scheduled Instance to start.
    earliestTime :: Core.Maybe Core.ISO8601,
    -- | The latest date and time, in UTC, for the Scheduled Instance to start.
    latestTime :: Core.Maybe Core.ISO8601
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'SlotStartTimeRangeRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'earliestTime', 'slotStartTimeRangeRequest_earliestTime' - The earliest date and time, in UTC, for the Scheduled Instance to start.
--
-- 'latestTime', 'slotStartTimeRangeRequest_latestTime' - The latest date and time, in UTC, for the Scheduled Instance to start.
newSlotStartTimeRangeRequest ::
  SlotStartTimeRangeRequest
newSlotStartTimeRangeRequest =
  SlotStartTimeRangeRequest'
    { earliestTime =
        Core.Nothing,
      latestTime = Core.Nothing
    }

-- | The earliest date and time, in UTC, for the Scheduled Instance to start.
slotStartTimeRangeRequest_earliestTime :: Lens.Lens' SlotStartTimeRangeRequest (Core.Maybe Core.UTCTime)
slotStartTimeRangeRequest_earliestTime = Lens.lens (\SlotStartTimeRangeRequest' {earliestTime} -> earliestTime) (\s@SlotStartTimeRangeRequest' {} a -> s {earliestTime = a} :: SlotStartTimeRangeRequest) Core.. Lens.mapping Core._Time

-- | The latest date and time, in UTC, for the Scheduled Instance to start.
slotStartTimeRangeRequest_latestTime :: Lens.Lens' SlotStartTimeRangeRequest (Core.Maybe Core.UTCTime)
slotStartTimeRangeRequest_latestTime = Lens.lens (\SlotStartTimeRangeRequest' {latestTime} -> latestTime) (\s@SlotStartTimeRangeRequest' {} a -> s {latestTime = a} :: SlotStartTimeRangeRequest) Core.. Lens.mapping Core._Time

instance Core.Hashable SlotStartTimeRangeRequest

instance Core.NFData SlotStartTimeRangeRequest

instance Core.ToQuery SlotStartTimeRangeRequest where
  toQuery SlotStartTimeRangeRequest' {..} =
    Core.mconcat
      [ "EarliestTime" Core.=: earliestTime,
        "LatestTime" Core.=: latestTime
      ]
