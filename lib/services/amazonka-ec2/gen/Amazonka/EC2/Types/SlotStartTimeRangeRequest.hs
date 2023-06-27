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
-- Module      : Amazonka.EC2.Types.SlotStartTimeRangeRequest
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.SlotStartTimeRangeRequest where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

-- | Describes the time period for a Scheduled Instance to start its first
-- schedule.
--
-- /See:/ 'newSlotStartTimeRangeRequest' smart constructor.
data SlotStartTimeRangeRequest = SlotStartTimeRangeRequest'
  { -- | The earliest date and time, in UTC, for the Scheduled Instance to start.
    earliestTime :: Prelude.Maybe Data.ISO8601,
    -- | The latest date and time, in UTC, for the Scheduled Instance to start.
    latestTime :: Prelude.Maybe Data.ISO8601
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
slotStartTimeRangeRequest_earliestTime = Lens.lens (\SlotStartTimeRangeRequest' {earliestTime} -> earliestTime) (\s@SlotStartTimeRangeRequest' {} a -> s {earliestTime = a} :: SlotStartTimeRangeRequest) Prelude.. Lens.mapping Data._Time

-- | The latest date and time, in UTC, for the Scheduled Instance to start.
slotStartTimeRangeRequest_latestTime :: Lens.Lens' SlotStartTimeRangeRequest (Prelude.Maybe Prelude.UTCTime)
slotStartTimeRangeRequest_latestTime = Lens.lens (\SlotStartTimeRangeRequest' {latestTime} -> latestTime) (\s@SlotStartTimeRangeRequest' {} a -> s {latestTime = a} :: SlotStartTimeRangeRequest) Prelude.. Lens.mapping Data._Time

instance Prelude.Hashable SlotStartTimeRangeRequest where
  hashWithSalt _salt SlotStartTimeRangeRequest' {..} =
    _salt
      `Prelude.hashWithSalt` earliestTime
      `Prelude.hashWithSalt` latestTime

instance Prelude.NFData SlotStartTimeRangeRequest where
  rnf SlotStartTimeRangeRequest' {..} =
    Prelude.rnf earliestTime
      `Prelude.seq` Prelude.rnf latestTime

instance Data.ToQuery SlotStartTimeRangeRequest where
  toQuery SlotStartTimeRangeRequest' {..} =
    Prelude.mconcat
      [ "EarliestTime" Data.=: earliestTime,
        "LatestTime" Data.=: latestTime
      ]
