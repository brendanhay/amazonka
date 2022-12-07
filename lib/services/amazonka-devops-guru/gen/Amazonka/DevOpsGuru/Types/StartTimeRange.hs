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
-- Module      : Amazonka.DevOpsGuru.Types.StartTimeRange
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DevOpsGuru.Types.StartTimeRange where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A time range used to specify when the behavior of an insight or anomaly
-- started.
--
-- /See:/ 'newStartTimeRange' smart constructor.
data StartTimeRange = StartTimeRange'
  { -- | The end time of the time range.
    toTime :: Prelude.Maybe Data.POSIX,
    -- | The start time of the time range.
    fromTime :: Prelude.Maybe Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartTimeRange' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'toTime', 'startTimeRange_toTime' - The end time of the time range.
--
-- 'fromTime', 'startTimeRange_fromTime' - The start time of the time range.
newStartTimeRange ::
  StartTimeRange
newStartTimeRange =
  StartTimeRange'
    { toTime = Prelude.Nothing,
      fromTime = Prelude.Nothing
    }

-- | The end time of the time range.
startTimeRange_toTime :: Lens.Lens' StartTimeRange (Prelude.Maybe Prelude.UTCTime)
startTimeRange_toTime = Lens.lens (\StartTimeRange' {toTime} -> toTime) (\s@StartTimeRange' {} a -> s {toTime = a} :: StartTimeRange) Prelude.. Lens.mapping Data._Time

-- | The start time of the time range.
startTimeRange_fromTime :: Lens.Lens' StartTimeRange (Prelude.Maybe Prelude.UTCTime)
startTimeRange_fromTime = Lens.lens (\StartTimeRange' {fromTime} -> fromTime) (\s@StartTimeRange' {} a -> s {fromTime = a} :: StartTimeRange) Prelude.. Lens.mapping Data._Time

instance Prelude.Hashable StartTimeRange where
  hashWithSalt _salt StartTimeRange' {..} =
    _salt `Prelude.hashWithSalt` toTime
      `Prelude.hashWithSalt` fromTime

instance Prelude.NFData StartTimeRange where
  rnf StartTimeRange' {..} =
    Prelude.rnf toTime
      `Prelude.seq` Prelude.rnf fromTime

instance Data.ToJSON StartTimeRange where
  toJSON StartTimeRange' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ToTime" Data..=) Prelude.<$> toTime,
            ("FromTime" Data..=) Prelude.<$> fromTime
          ]
      )
