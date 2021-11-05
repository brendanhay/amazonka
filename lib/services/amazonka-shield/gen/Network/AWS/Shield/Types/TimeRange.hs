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
-- Module      : Amazonka.Shield.Types.TimeRange
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Shield.Types.TimeRange where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | The time range.
--
-- /See:/ 'newTimeRange' smart constructor.
data TimeRange = TimeRange'
  { -- | The start time, in Unix time in seconds. For more information see
    -- <http://docs.aws.amazon.com/cli/latest/userguide/cli-using-param.html#parameter-types timestamp>.
    fromInclusive :: Prelude.Maybe Core.POSIX,
    -- | The end time, in Unix time in seconds. For more information see
    -- <http://docs.aws.amazon.com/cli/latest/userguide/cli-using-param.html#parameter-types timestamp>.
    toExclusive :: Prelude.Maybe Core.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TimeRange' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fromInclusive', 'timeRange_fromInclusive' - The start time, in Unix time in seconds. For more information see
-- <http://docs.aws.amazon.com/cli/latest/userguide/cli-using-param.html#parameter-types timestamp>.
--
-- 'toExclusive', 'timeRange_toExclusive' - The end time, in Unix time in seconds. For more information see
-- <http://docs.aws.amazon.com/cli/latest/userguide/cli-using-param.html#parameter-types timestamp>.
newTimeRange ::
  TimeRange
newTimeRange =
  TimeRange'
    { fromInclusive = Prelude.Nothing,
      toExclusive = Prelude.Nothing
    }

-- | The start time, in Unix time in seconds. For more information see
-- <http://docs.aws.amazon.com/cli/latest/userguide/cli-using-param.html#parameter-types timestamp>.
timeRange_fromInclusive :: Lens.Lens' TimeRange (Prelude.Maybe Prelude.UTCTime)
timeRange_fromInclusive = Lens.lens (\TimeRange' {fromInclusive} -> fromInclusive) (\s@TimeRange' {} a -> s {fromInclusive = a} :: TimeRange) Prelude.. Lens.mapping Core._Time

-- | The end time, in Unix time in seconds. For more information see
-- <http://docs.aws.amazon.com/cli/latest/userguide/cli-using-param.html#parameter-types timestamp>.
timeRange_toExclusive :: Lens.Lens' TimeRange (Prelude.Maybe Prelude.UTCTime)
timeRange_toExclusive = Lens.lens (\TimeRange' {toExclusive} -> toExclusive) (\s@TimeRange' {} a -> s {toExclusive = a} :: TimeRange) Prelude.. Lens.mapping Core._Time

instance Core.FromJSON TimeRange where
  parseJSON =
    Core.withObject
      "TimeRange"
      ( \x ->
          TimeRange'
            Prelude.<$> (x Core..:? "FromInclusive")
            Prelude.<*> (x Core..:? "ToExclusive")
      )

instance Prelude.Hashable TimeRange

instance Prelude.NFData TimeRange

instance Core.ToJSON TimeRange where
  toJSON TimeRange' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("FromInclusive" Core..=) Prelude.<$> fromInclusive,
            ("ToExclusive" Core..=) Prelude.<$> toExclusive
          ]
      )
