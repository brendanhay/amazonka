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
-- Module      : Amazonka.IoT.Types.ViolationEventOccurrenceRange
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT.Types.ViolationEventOccurrenceRange where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specifies the time period of which violation events occurred between.
--
-- /See:/ 'newViolationEventOccurrenceRange' smart constructor.
data ViolationEventOccurrenceRange = ViolationEventOccurrenceRange'
  { -- | The start date and time of a time period in which violation events
    -- occurred.
    startTime :: Data.POSIX,
    -- | The end date and time of a time period in which violation events
    -- occurred.
    endTime :: Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ViolationEventOccurrenceRange' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'startTime', 'violationEventOccurrenceRange_startTime' - The start date and time of a time period in which violation events
-- occurred.
--
-- 'endTime', 'violationEventOccurrenceRange_endTime' - The end date and time of a time period in which violation events
-- occurred.
newViolationEventOccurrenceRange ::
  -- | 'startTime'
  Prelude.UTCTime ->
  -- | 'endTime'
  Prelude.UTCTime ->
  ViolationEventOccurrenceRange
newViolationEventOccurrenceRange
  pStartTime_
  pEndTime_ =
    ViolationEventOccurrenceRange'
      { startTime =
          Data._Time Lens.# pStartTime_,
        endTime = Data._Time Lens.# pEndTime_
      }

-- | The start date and time of a time period in which violation events
-- occurred.
violationEventOccurrenceRange_startTime :: Lens.Lens' ViolationEventOccurrenceRange Prelude.UTCTime
violationEventOccurrenceRange_startTime = Lens.lens (\ViolationEventOccurrenceRange' {startTime} -> startTime) (\s@ViolationEventOccurrenceRange' {} a -> s {startTime = a} :: ViolationEventOccurrenceRange) Prelude.. Data._Time

-- | The end date and time of a time period in which violation events
-- occurred.
violationEventOccurrenceRange_endTime :: Lens.Lens' ViolationEventOccurrenceRange Prelude.UTCTime
violationEventOccurrenceRange_endTime = Lens.lens (\ViolationEventOccurrenceRange' {endTime} -> endTime) (\s@ViolationEventOccurrenceRange' {} a -> s {endTime = a} :: ViolationEventOccurrenceRange) Prelude.. Data._Time

instance Data.FromJSON ViolationEventOccurrenceRange where
  parseJSON =
    Data.withObject
      "ViolationEventOccurrenceRange"
      ( \x ->
          ViolationEventOccurrenceRange'
            Prelude.<$> (x Data..: "startTime")
            Prelude.<*> (x Data..: "endTime")
      )

instance
  Prelude.Hashable
    ViolationEventOccurrenceRange
  where
  hashWithSalt _salt ViolationEventOccurrenceRange' {..} =
    _salt `Prelude.hashWithSalt` startTime
      `Prelude.hashWithSalt` endTime

instance Prelude.NFData ViolationEventOccurrenceRange where
  rnf ViolationEventOccurrenceRange' {..} =
    Prelude.rnf startTime
      `Prelude.seq` Prelude.rnf endTime

instance Data.ToJSON ViolationEventOccurrenceRange where
  toJSON ViolationEventOccurrenceRange' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("startTime" Data..= startTime),
            Prelude.Just ("endTime" Data..= endTime)
          ]
      )
