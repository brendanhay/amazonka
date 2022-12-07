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
-- Module      : Amazonka.EC2.Types.SlotDateTimeRangeRequest
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.SlotDateTimeRangeRequest where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

-- | Describes the time period for a Scheduled Instance to start its first
-- schedule. The time period must span less than one day.
--
-- /See:/ 'newSlotDateTimeRangeRequest' smart constructor.
data SlotDateTimeRangeRequest = SlotDateTimeRangeRequest'
  { -- | The earliest date and time, in UTC, for the Scheduled Instance to start.
    earliestTime :: Data.ISO8601,
    -- | The latest date and time, in UTC, for the Scheduled Instance to start.
    -- This value must be later than or equal to the earliest date and at most
    -- three months in the future.
    latestTime :: Data.ISO8601
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
          Data._Time Lens.# pEarliestTime_,
        latestTime = Data._Time Lens.# pLatestTime_
      }

-- | The earliest date and time, in UTC, for the Scheduled Instance to start.
slotDateTimeRangeRequest_earliestTime :: Lens.Lens' SlotDateTimeRangeRequest Prelude.UTCTime
slotDateTimeRangeRequest_earliestTime = Lens.lens (\SlotDateTimeRangeRequest' {earliestTime} -> earliestTime) (\s@SlotDateTimeRangeRequest' {} a -> s {earliestTime = a} :: SlotDateTimeRangeRequest) Prelude.. Data._Time

-- | The latest date and time, in UTC, for the Scheduled Instance to start.
-- This value must be later than or equal to the earliest date and at most
-- three months in the future.
slotDateTimeRangeRequest_latestTime :: Lens.Lens' SlotDateTimeRangeRequest Prelude.UTCTime
slotDateTimeRangeRequest_latestTime = Lens.lens (\SlotDateTimeRangeRequest' {latestTime} -> latestTime) (\s@SlotDateTimeRangeRequest' {} a -> s {latestTime = a} :: SlotDateTimeRangeRequest) Prelude.. Data._Time

instance Prelude.Hashable SlotDateTimeRangeRequest where
  hashWithSalt _salt SlotDateTimeRangeRequest' {..} =
    _salt `Prelude.hashWithSalt` earliestTime
      `Prelude.hashWithSalt` latestTime

instance Prelude.NFData SlotDateTimeRangeRequest where
  rnf SlotDateTimeRangeRequest' {..} =
    Prelude.rnf earliestTime
      `Prelude.seq` Prelude.rnf latestTime

instance Data.ToQuery SlotDateTimeRangeRequest where
  toQuery SlotDateTimeRangeRequest' {..} =
    Prelude.mconcat
      [ "EarliestTime" Data.=: earliestTime,
        "LatestTime" Data.=: latestTime
      ]
