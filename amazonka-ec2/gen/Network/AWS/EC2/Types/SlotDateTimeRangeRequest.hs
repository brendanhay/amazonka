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
-- Module      : Network.AWS.EC2.Types.SlotDateTimeRangeRequest
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.SlotDateTimeRangeRequest where

import Network.AWS.EC2.Internal
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes the time period for a Scheduled Instance to start its first
-- schedule. The time period must span less than one day.
--
-- /See:/ 'newSlotDateTimeRangeRequest' smart constructor.
data SlotDateTimeRangeRequest = SlotDateTimeRangeRequest'
  { -- | The earliest date and time, in UTC, for the Scheduled Instance to start.
    earliestTime :: Prelude.ISO8601,
    -- | The latest date and time, in UTC, for the Scheduled Instance to start.
    -- This value must be later than or equal to the earliest date and at most
    -- three months in the future.
    latestTime :: Prelude.ISO8601
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'SlotDateTimeRangeRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'earliestTime', 'slotDateTimeRangeRequest_earliestTime' - The earliest date and time, in UTC, for the Scheduled Instance to start.
--
-- 'latestTime', 'slotDateTimeRangeRequest_latestTime' - The latest date and time, in UTC, for the Scheduled Instance to start.
-- This value must be later than or equal to the earliest date and at most
-- three months in the future.
newSlotDateTimeRangeRequest ::
  -- | 'earliestTime'
  Prelude.UTCTime ->
  -- | 'latestTime'
  Prelude.UTCTime ->
  SlotDateTimeRangeRequest
newSlotDateTimeRangeRequest
  pEarliestTime_
  pLatestTime_ =
    SlotDateTimeRangeRequest'
      { earliestTime =
          Prelude._Time Lens.# pEarliestTime_,
        latestTime = Prelude._Time Lens.# pLatestTime_
      }

-- | The earliest date and time, in UTC, for the Scheduled Instance to start.
slotDateTimeRangeRequest_earliestTime :: Lens.Lens' SlotDateTimeRangeRequest Prelude.UTCTime
slotDateTimeRangeRequest_earliestTime = Lens.lens (\SlotDateTimeRangeRequest' {earliestTime} -> earliestTime) (\s@SlotDateTimeRangeRequest' {} a -> s {earliestTime = a} :: SlotDateTimeRangeRequest) Prelude.. Prelude._Time

-- | The latest date and time, in UTC, for the Scheduled Instance to start.
-- This value must be later than or equal to the earliest date and at most
-- three months in the future.
slotDateTimeRangeRequest_latestTime :: Lens.Lens' SlotDateTimeRangeRequest Prelude.UTCTime
slotDateTimeRangeRequest_latestTime = Lens.lens (\SlotDateTimeRangeRequest' {latestTime} -> latestTime) (\s@SlotDateTimeRangeRequest' {} a -> s {latestTime = a} :: SlotDateTimeRangeRequest) Prelude.. Prelude._Time

instance Prelude.Hashable SlotDateTimeRangeRequest

instance Prelude.NFData SlotDateTimeRangeRequest

instance Prelude.ToQuery SlotDateTimeRangeRequest where
  toQuery SlotDateTimeRangeRequest' {..} =
    Prelude.mconcat
      [ "EarliestTime" Prelude.=: earliestTime,
        "LatestTime" Prelude.=: latestTime
      ]
