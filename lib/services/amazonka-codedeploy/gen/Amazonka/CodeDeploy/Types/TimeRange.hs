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
-- Module      : Amazonka.CodeDeploy.Types.TimeRange
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeDeploy.Types.TimeRange where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about a time range.
--
-- /See:/ 'newTimeRange' smart constructor.
data TimeRange = TimeRange'
  { -- | The end time of the time range.
    --
    -- Specify null to leave the end time open-ended.
    end :: Prelude.Maybe Data.POSIX,
    -- | The start time of the time range.
    --
    -- Specify null to leave the start time open-ended.
    start :: Prelude.Maybe Data.POSIX
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
-- 'end', 'timeRange_end' - The end time of the time range.
--
-- Specify null to leave the end time open-ended.
--
-- 'start', 'timeRange_start' - The start time of the time range.
--
-- Specify null to leave the start time open-ended.
newTimeRange ::
  TimeRange
newTimeRange =
  TimeRange'
    { end = Prelude.Nothing,
      start = Prelude.Nothing
    }

-- | The end time of the time range.
--
-- Specify null to leave the end time open-ended.
timeRange_end :: Lens.Lens' TimeRange (Prelude.Maybe Prelude.UTCTime)
timeRange_end = Lens.lens (\TimeRange' {end} -> end) (\s@TimeRange' {} a -> s {end = a} :: TimeRange) Prelude.. Lens.mapping Data._Time

-- | The start time of the time range.
--
-- Specify null to leave the start time open-ended.
timeRange_start :: Lens.Lens' TimeRange (Prelude.Maybe Prelude.UTCTime)
timeRange_start = Lens.lens (\TimeRange' {start} -> start) (\s@TimeRange' {} a -> s {start = a} :: TimeRange) Prelude.. Lens.mapping Data._Time

instance Prelude.Hashable TimeRange where
  hashWithSalt _salt TimeRange' {..} =
    _salt `Prelude.hashWithSalt` end
      `Prelude.hashWithSalt` start

instance Prelude.NFData TimeRange where
  rnf TimeRange' {..} =
    Prelude.rnf end `Prelude.seq` Prelude.rnf start

instance Data.ToJSON TimeRange where
  toJSON TimeRange' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("end" Data..=) Prelude.<$> end,
            ("start" Data..=) Prelude.<$> start
          ]
      )
