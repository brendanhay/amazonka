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
-- Module      : Amazonka.DynamoDB.Types.PointInTimeRecoveryDescription
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DynamoDB.Types.PointInTimeRecoveryDescription where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DynamoDB.Types.AttributeValue
import Amazonka.DynamoDB.Types.PointInTimeRecoveryStatus
import Amazonka.DynamoDB.Types.WriteRequest
import qualified Amazonka.Prelude as Prelude

-- | The description of the point in time settings applied to the table.
--
-- /See:/ 'newPointInTimeRecoveryDescription' smart constructor.
data PointInTimeRecoveryDescription = PointInTimeRecoveryDescription'
  { -- | Specifies the earliest point in time you can restore your table to. You
    -- can restore your table to any point in time during the last 35 days.
    earliestRestorableDateTime :: Prelude.Maybe Data.POSIX,
    -- | The current state of point in time recovery:
    --
    -- -   @ENABLED@ - Point in time recovery is enabled.
    --
    -- -   @DISABLED@ - Point in time recovery is disabled.
    pointInTimeRecoveryStatus :: Prelude.Maybe PointInTimeRecoveryStatus,
    -- | @LatestRestorableDateTime@ is typically 5 minutes before the current
    -- time.
    latestRestorableDateTime :: Prelude.Maybe Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PointInTimeRecoveryDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'earliestRestorableDateTime', 'pointInTimeRecoveryDescription_earliestRestorableDateTime' - Specifies the earliest point in time you can restore your table to. You
-- can restore your table to any point in time during the last 35 days.
--
-- 'pointInTimeRecoveryStatus', 'pointInTimeRecoveryDescription_pointInTimeRecoveryStatus' - The current state of point in time recovery:
--
-- -   @ENABLED@ - Point in time recovery is enabled.
--
-- -   @DISABLED@ - Point in time recovery is disabled.
--
-- 'latestRestorableDateTime', 'pointInTimeRecoveryDescription_latestRestorableDateTime' - @LatestRestorableDateTime@ is typically 5 minutes before the current
-- time.
newPointInTimeRecoveryDescription ::
  PointInTimeRecoveryDescription
newPointInTimeRecoveryDescription =
  PointInTimeRecoveryDescription'
    { earliestRestorableDateTime =
        Prelude.Nothing,
      pointInTimeRecoveryStatus = Prelude.Nothing,
      latestRestorableDateTime = Prelude.Nothing
    }

-- | Specifies the earliest point in time you can restore your table to. You
-- can restore your table to any point in time during the last 35 days.
pointInTimeRecoveryDescription_earliestRestorableDateTime :: Lens.Lens' PointInTimeRecoveryDescription (Prelude.Maybe Prelude.UTCTime)
pointInTimeRecoveryDescription_earliestRestorableDateTime = Lens.lens (\PointInTimeRecoveryDescription' {earliestRestorableDateTime} -> earliestRestorableDateTime) (\s@PointInTimeRecoveryDescription' {} a -> s {earliestRestorableDateTime = a} :: PointInTimeRecoveryDescription) Prelude.. Lens.mapping Data._Time

-- | The current state of point in time recovery:
--
-- -   @ENABLED@ - Point in time recovery is enabled.
--
-- -   @DISABLED@ - Point in time recovery is disabled.
pointInTimeRecoveryDescription_pointInTimeRecoveryStatus :: Lens.Lens' PointInTimeRecoveryDescription (Prelude.Maybe PointInTimeRecoveryStatus)
pointInTimeRecoveryDescription_pointInTimeRecoveryStatus = Lens.lens (\PointInTimeRecoveryDescription' {pointInTimeRecoveryStatus} -> pointInTimeRecoveryStatus) (\s@PointInTimeRecoveryDescription' {} a -> s {pointInTimeRecoveryStatus = a} :: PointInTimeRecoveryDescription)

-- | @LatestRestorableDateTime@ is typically 5 minutes before the current
-- time.
pointInTimeRecoveryDescription_latestRestorableDateTime :: Lens.Lens' PointInTimeRecoveryDescription (Prelude.Maybe Prelude.UTCTime)
pointInTimeRecoveryDescription_latestRestorableDateTime = Lens.lens (\PointInTimeRecoveryDescription' {latestRestorableDateTime} -> latestRestorableDateTime) (\s@PointInTimeRecoveryDescription' {} a -> s {latestRestorableDateTime = a} :: PointInTimeRecoveryDescription) Prelude.. Lens.mapping Data._Time

instance Data.FromJSON PointInTimeRecoveryDescription where
  parseJSON =
    Data.withObject
      "PointInTimeRecoveryDescription"
      ( \x ->
          PointInTimeRecoveryDescription'
            Prelude.<$> (x Data..:? "EarliestRestorableDateTime")
            Prelude.<*> (x Data..:? "PointInTimeRecoveryStatus")
            Prelude.<*> (x Data..:? "LatestRestorableDateTime")
      )

instance
  Prelude.Hashable
    PointInTimeRecoveryDescription
  where
  hashWithSalt
    _salt
    PointInTimeRecoveryDescription' {..} =
      _salt
        `Prelude.hashWithSalt` earliestRestorableDateTime
        `Prelude.hashWithSalt` pointInTimeRecoveryStatus
        `Prelude.hashWithSalt` latestRestorableDateTime

instance
  Prelude.NFData
    PointInTimeRecoveryDescription
  where
  rnf PointInTimeRecoveryDescription' {..} =
    Prelude.rnf earliestRestorableDateTime
      `Prelude.seq` Prelude.rnf pointInTimeRecoveryStatus
      `Prelude.seq` Prelude.rnf latestRestorableDateTime
