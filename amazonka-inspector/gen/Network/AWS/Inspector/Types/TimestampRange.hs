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
-- Module      : Network.AWS.Inspector.Types.TimestampRange
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Inspector.Types.TimestampRange where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | This data type is used in the AssessmentRunFilter data type.
--
-- /See:/ 'newTimestampRange' smart constructor.
data TimestampRange = TimestampRange'
  { -- | The minimum value of the timestamp range.
    beginDate :: Core.Maybe Core.POSIX,
    -- | The maximum value of the timestamp range.
    endDate :: Core.Maybe Core.POSIX
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'TimestampRange' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'beginDate', 'timestampRange_beginDate' - The minimum value of the timestamp range.
--
-- 'endDate', 'timestampRange_endDate' - The maximum value of the timestamp range.
newTimestampRange ::
  TimestampRange
newTimestampRange =
  TimestampRange'
    { beginDate = Core.Nothing,
      endDate = Core.Nothing
    }

-- | The minimum value of the timestamp range.
timestampRange_beginDate :: Lens.Lens' TimestampRange (Core.Maybe Core.UTCTime)
timestampRange_beginDate = Lens.lens (\TimestampRange' {beginDate} -> beginDate) (\s@TimestampRange' {} a -> s {beginDate = a} :: TimestampRange) Core.. Lens.mapping Core._Time

-- | The maximum value of the timestamp range.
timestampRange_endDate :: Lens.Lens' TimestampRange (Core.Maybe Core.UTCTime)
timestampRange_endDate = Lens.lens (\TimestampRange' {endDate} -> endDate) (\s@TimestampRange' {} a -> s {endDate = a} :: TimestampRange) Core.. Lens.mapping Core._Time

instance Core.Hashable TimestampRange

instance Core.NFData TimestampRange

instance Core.ToJSON TimestampRange where
  toJSON TimestampRange' {..} =
    Core.object
      ( Core.catMaybes
          [ ("beginDate" Core..=) Core.<$> beginDate,
            ("endDate" Core..=) Core.<$> endDate
          ]
      )
