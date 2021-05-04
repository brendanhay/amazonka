{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.CloudWatch.Types.Range
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatch.Types.Range where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Specifies one range of days or times to exclude from use for training an
-- anomaly detection model.
--
-- /See:/ 'newRange' smart constructor.
data Range = Range'
  { -- | The start time of the range to exclude. The format is
    -- @yyyy-MM-dd\'T\'HH:mm:ss@. For example, @2019-07-01T23:59:59@.
    startTime :: Prelude.ISO8601,
    -- | The end time of the range to exclude. The format is
    -- @yyyy-MM-dd\'T\'HH:mm:ss@. For example, @2019-07-01T23:59:59@.
    endTime :: Prelude.ISO8601
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { startTime =
        Prelude._Time Lens.# pStartTime_,
      endTime = Prelude._Time Lens.# pEndTime_
    }

-- | The start time of the range to exclude. The format is
-- @yyyy-MM-dd\'T\'HH:mm:ss@. For example, @2019-07-01T23:59:59@.
range_startTime :: Lens.Lens' Range Prelude.UTCTime
range_startTime = Lens.lens (\Range' {startTime} -> startTime) (\s@Range' {} a -> s {startTime = a} :: Range) Prelude.. Prelude._Time

-- | The end time of the range to exclude. The format is
-- @yyyy-MM-dd\'T\'HH:mm:ss@. For example, @2019-07-01T23:59:59@.
range_endTime :: Lens.Lens' Range Prelude.UTCTime
range_endTime = Lens.lens (\Range' {endTime} -> endTime) (\s@Range' {} a -> s {endTime = a} :: Range) Prelude.. Prelude._Time

instance Prelude.FromXML Range where
  parseXML x =
    Range'
      Prelude.<$> (x Prelude..@ "StartTime")
      Prelude.<*> (x Prelude..@ "EndTime")

instance Prelude.Hashable Range

instance Prelude.NFData Range

instance Prelude.ToQuery Range where
  toQuery Range' {..} =
    Prelude.mconcat
      [ "StartTime" Prelude.=: startTime,
        "EndTime" Prelude.=: endTime
      ]
