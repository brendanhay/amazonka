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
-- Module      : Network.AWS.IoT.Types.ViolationEventOccurrenceRange
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.ViolationEventOccurrenceRange where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Specifies the time period of which violation events occurred between.
--
-- /See:/ 'newViolationEventOccurrenceRange' smart constructor.
data ViolationEventOccurrenceRange = ViolationEventOccurrenceRange'
  { -- | The start date and time of a time period in which violation events
    -- occurred.
    startTime :: Core.POSIX,
    -- | The end date and time of a time period in which violation events
    -- occurred.
    endTime :: Core.POSIX
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
          Core._Time Lens.# pStartTime_,
        endTime = Core._Time Lens.# pEndTime_
      }

-- | The start date and time of a time period in which violation events
-- occurred.
violationEventOccurrenceRange_startTime :: Lens.Lens' ViolationEventOccurrenceRange Prelude.UTCTime
violationEventOccurrenceRange_startTime = Lens.lens (\ViolationEventOccurrenceRange' {startTime} -> startTime) (\s@ViolationEventOccurrenceRange' {} a -> s {startTime = a} :: ViolationEventOccurrenceRange) Prelude.. Core._Time

-- | The end date and time of a time period in which violation events
-- occurred.
violationEventOccurrenceRange_endTime :: Lens.Lens' ViolationEventOccurrenceRange Prelude.UTCTime
violationEventOccurrenceRange_endTime = Lens.lens (\ViolationEventOccurrenceRange' {endTime} -> endTime) (\s@ViolationEventOccurrenceRange' {} a -> s {endTime = a} :: ViolationEventOccurrenceRange) Prelude.. Core._Time

instance Core.FromJSON ViolationEventOccurrenceRange where
  parseJSON =
    Core.withObject
      "ViolationEventOccurrenceRange"
      ( \x ->
          ViolationEventOccurrenceRange'
            Prelude.<$> (x Core..: "startTime")
            Prelude.<*> (x Core..: "endTime")
      )

instance
  Prelude.Hashable
    ViolationEventOccurrenceRange

instance Prelude.NFData ViolationEventOccurrenceRange

instance Core.ToJSON ViolationEventOccurrenceRange where
  toJSON ViolationEventOccurrenceRange' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("startTime" Core..= startTime),
            Prelude.Just ("endTime" Core..= endTime)
          ]
      )
