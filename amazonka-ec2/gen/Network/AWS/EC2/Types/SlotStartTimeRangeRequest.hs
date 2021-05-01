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
-- Module      : Network.AWS.EC2.Types.SlotStartTimeRangeRequest
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.SlotStartTimeRangeRequest where

import Network.AWS.EC2.Internal
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes the time period for a Scheduled Instance to start its first
-- schedule.
--
-- /See:/ 'newSlotStartTimeRangeRequest' smart constructor.
data SlotStartTimeRangeRequest = SlotStartTimeRangeRequest'
  { -- | The earliest date and time, in UTC, for the Scheduled Instance to start.
    earliestTime :: Prelude.Maybe Prelude.ISO8601,
    -- | The latest date and time, in UTC, for the Scheduled Instance to start.
    latestTime :: Prelude.Maybe Prelude.ISO8601
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
        Prelude.Nothing,
      latestTime = Prelude.Nothing
    }

-- | The earliest date and time, in UTC, for the Scheduled Instance to start.
slotStartTimeRangeRequest_earliestTime :: Lens.Lens' SlotStartTimeRangeRequest (Prelude.Maybe Prelude.UTCTime)
slotStartTimeRangeRequest_earliestTime = Lens.lens (\SlotStartTimeRangeRequest' {earliestTime} -> earliestTime) (\s@SlotStartTimeRangeRequest' {} a -> s {earliestTime = a} :: SlotStartTimeRangeRequest) Prelude.. Lens.mapping Prelude._Time

-- | The latest date and time, in UTC, for the Scheduled Instance to start.
slotStartTimeRangeRequest_latestTime :: Lens.Lens' SlotStartTimeRangeRequest (Prelude.Maybe Prelude.UTCTime)
slotStartTimeRangeRequest_latestTime = Lens.lens (\SlotStartTimeRangeRequest' {latestTime} -> latestTime) (\s@SlotStartTimeRangeRequest' {} a -> s {latestTime = a} :: SlotStartTimeRangeRequest) Prelude.. Lens.mapping Prelude._Time

instance Prelude.Hashable SlotStartTimeRangeRequest

instance Prelude.NFData SlotStartTimeRangeRequest

instance Prelude.ToQuery SlotStartTimeRangeRequest where
  toQuery SlotStartTimeRangeRequest' {..} =
    Prelude.mconcat
      [ "EarliestTime" Prelude.=: earliestTime,
        "LatestTime" Prelude.=: latestTime
      ]
