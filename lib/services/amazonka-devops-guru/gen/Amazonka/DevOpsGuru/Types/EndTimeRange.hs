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
-- Module      : Amazonka.DevOpsGuru.Types.EndTimeRange
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DevOpsGuru.Types.EndTimeRange where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A range of time that specifies when anomalous behavior in an anomaly or
-- insight ended.
--
-- /See:/ 'newEndTimeRange' smart constructor.
data EndTimeRange = EndTimeRange'
  { -- | The latest end time in the time range.
    toTime :: Prelude.Maybe Data.POSIX,
    -- | The earliest end time in the time range.
    fromTime :: Prelude.Maybe Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EndTimeRange' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'toTime', 'endTimeRange_toTime' - The latest end time in the time range.
--
-- 'fromTime', 'endTimeRange_fromTime' - The earliest end time in the time range.
newEndTimeRange ::
  EndTimeRange
newEndTimeRange =
  EndTimeRange'
    { toTime = Prelude.Nothing,
      fromTime = Prelude.Nothing
    }

-- | The latest end time in the time range.
endTimeRange_toTime :: Lens.Lens' EndTimeRange (Prelude.Maybe Prelude.UTCTime)
endTimeRange_toTime = Lens.lens (\EndTimeRange' {toTime} -> toTime) (\s@EndTimeRange' {} a -> s {toTime = a} :: EndTimeRange) Prelude.. Lens.mapping Data._Time

-- | The earliest end time in the time range.
endTimeRange_fromTime :: Lens.Lens' EndTimeRange (Prelude.Maybe Prelude.UTCTime)
endTimeRange_fromTime = Lens.lens (\EndTimeRange' {fromTime} -> fromTime) (\s@EndTimeRange' {} a -> s {fromTime = a} :: EndTimeRange) Prelude.. Lens.mapping Data._Time

instance Prelude.Hashable EndTimeRange where
  hashWithSalt _salt EndTimeRange' {..} =
    _salt `Prelude.hashWithSalt` toTime
      `Prelude.hashWithSalt` fromTime

instance Prelude.NFData EndTimeRange where
  rnf EndTimeRange' {..} =
    Prelude.rnf toTime
      `Prelude.seq` Prelude.rnf fromTime

instance Data.ToJSON EndTimeRange where
  toJSON EndTimeRange' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ToTime" Data..=) Prelude.<$> toTime,
            ("FromTime" Data..=) Prelude.<$> fromTime
          ]
      )
