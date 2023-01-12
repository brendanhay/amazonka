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
-- Module      : Amazonka.CloudWatch.Types.Range
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudWatch.Types.Range where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specifies one range of days or times to exclude from use for training an
-- anomaly detection model.
--
-- /See:/ 'newRange' smart constructor.
data Range = Range'
  { -- | The start time of the range to exclude. The format is
    -- @yyyy-MM-dd\'T\'HH:mm:ss@. For example, @2019-07-01T23:59:59@.
    startTime :: Data.ISO8601,
    -- | The end time of the range to exclude. The format is
    -- @yyyy-MM-dd\'T\'HH:mm:ss@. For example, @2019-07-01T23:59:59@.
    endTime :: Data.ISO8601
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Range' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'startTime', 'range_startTime' - The start time of the range to exclude. The format is
-- @yyyy-MM-dd\'T\'HH:mm:ss@. For example, @2019-07-01T23:59:59@.
--
-- 'endTime', 'range_endTime' - The end time of the range to exclude. The format is
-- @yyyy-MM-dd\'T\'HH:mm:ss@. For example, @2019-07-01T23:59:59@.
newRange ::
  -- | 'startTime'
  Prelude.UTCTime ->
  -- | 'endTime'
  Prelude.UTCTime ->
  Range
newRange pStartTime_ pEndTime_ =
  Range'
    { startTime = Data._Time Lens.# pStartTime_,
      endTime = Data._Time Lens.# pEndTime_
    }

-- | The start time of the range to exclude. The format is
-- @yyyy-MM-dd\'T\'HH:mm:ss@. For example, @2019-07-01T23:59:59@.
range_startTime :: Lens.Lens' Range Prelude.UTCTime
range_startTime = Lens.lens (\Range' {startTime} -> startTime) (\s@Range' {} a -> s {startTime = a} :: Range) Prelude.. Data._Time

-- | The end time of the range to exclude. The format is
-- @yyyy-MM-dd\'T\'HH:mm:ss@. For example, @2019-07-01T23:59:59@.
range_endTime :: Lens.Lens' Range Prelude.UTCTime
range_endTime = Lens.lens (\Range' {endTime} -> endTime) (\s@Range' {} a -> s {endTime = a} :: Range) Prelude.. Data._Time

instance Data.FromXML Range where
  parseXML x =
    Range'
      Prelude.<$> (x Data..@ "StartTime")
      Prelude.<*> (x Data..@ "EndTime")

instance Prelude.Hashable Range where
  hashWithSalt _salt Range' {..} =
    _salt `Prelude.hashWithSalt` startTime
      `Prelude.hashWithSalt` endTime

instance Prelude.NFData Range where
  rnf Range' {..} =
    Prelude.rnf startTime
      `Prelude.seq` Prelude.rnf endTime

instance Data.ToQuery Range where
  toQuery Range' {..} =
    Prelude.mconcat
      [ "StartTime" Data.=: startTime,
        "EndTime" Data.=: endTime
      ]
