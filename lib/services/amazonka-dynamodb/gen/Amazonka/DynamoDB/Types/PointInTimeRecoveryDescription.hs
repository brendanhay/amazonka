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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
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
    -- | @LatestRestorableDateTime@ is typically 5 minutes before the current
    -- time.
    latestRestorableDateTime :: Prelude.Maybe Data.POSIX,
    -- | The current state of point in time recovery:
    --
    -- -   @ENABLED@ - Point in time recovery is enabled.
    --
    -- -   @DISABLED@ - Point in time recovery is disabled.
    pointInTimeRecoveryStatus :: Prelude.Maybe PointInTimeRecoveryStatus
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
-- 'latestRestorableDateTime', 'pointInTimeRecoveryDescription_latestRestorableDateTime' - @LatestRestorableDateTime@ is typically 5 minutes before the current
-- time.
--
-- 'pointInTimeRecoveryStatus', 'pointInTimeRecoveryDescription_pointInTimeRecoveryStatus' - The current state of point in time recovery:
--
-- -   @ENABLED@ - Point in time recovery is enabled.
--
-- -   @DISABLED@ - Point in time recovery is disabled.
newPointInTimeRecoveryDescription ::
  PointInTimeRecoveryDescription
newPointInTimeRecoveryDescription =
  PointInTimeRecoveryDescription'
    { earliestRestorableDateTime =
        Prelude.Nothing,
      latestRestorableDateTime = Prelude.Nothing,
      pointInTimeRecoveryStatus = Prelude.Nothing
    }

-- | Specifies the earliest point in time you can restore your table to. You
-- can restore your table to any point in time during the last 35 days.
pointInTimeRecoveryDescription_earliestRestorableDateTime :: Lens.Lens' PointInTimeRecoveryDescription (Prelude.Maybe Prelude.UTCTime)
pointInTimeRecoveryDescription_earliestRestorableDateTime = Lens.lens (\PointInTimeRecoveryDescription' {earliestRestorableDateTime} -> earliestRestorableDateTime) (\s@PointInTimeRecoveryDescription' {} a -> s {earliestRestorableDateTime = a} :: PointInTimeRecoveryDescription) Prelude.. Lens.mapping Data._Time

-- | @LatestRestorableDateTime@ is typically 5 minutes before the current
-- time.
pointInTimeRecoveryDescription_latestRestorableDateTime :: Lens.Lens' PointInTimeRecoveryDescription (Prelude.Maybe Prelude.UTCTime)
pointInTimeRecoveryDescription_latestRestorableDateTime = Lens.lens (\PointInTimeRecoveryDescription' {latestRestorableDateTime} -> latestRestorableDateTime) (\s@PointInTimeRecoveryDescription' {} a -> s {latestRestorableDateTime = a} :: PointInTimeRecoveryDescription) Prelude.. Lens.mapping Data._Time

-- | The current state of point in time recovery:
--
-- -   @ENABLED@ - Point in time recovery is enabled.
--
-- -   @DISABLED@ - Point in time recovery is disabled.
pointInTimeRecoveryDescription_pointInTimeRecoveryStatus :: Lens.Lens' PointInTimeRecoveryDescription (Prelude.Maybe PointInTimeRecoveryStatus)
pointInTimeRecoveryDescription_pointInTimeRecoveryStatus = Lens.lens (\PointInTimeRecoveryDescription' {pointInTimeRecoveryStatus} -> pointInTimeRecoveryStatus) (\s@PointInTimeRecoveryDescription' {} a -> s {pointInTimeRecoveryStatus = a} :: PointInTimeRecoveryDescription)

instance Data.FromJSON PointInTimeRecoveryDescription where
  parseJSON =
    Data.withObject
      "PointInTimeRecoveryDescription"
      ( \x ->
          PointInTimeRecoveryDescription'
            Prelude.<$> (x Data..:? "EarliestRestorableDateTime")
            Prelude.<*> (x Data..:? "LatestRestorableDateTime")
            Prelude.<*> (x Data..:? "PointInTimeRecoveryStatus")
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
        `Prelude.hashWithSalt` latestRestorableDateTime
        `Prelude.hashWithSalt` pointInTimeRecoveryStatus

instance
  Prelude.NFData
    PointInTimeRecoveryDescription
  where
  rnf PointInTimeRecoveryDescription' {..} =
    Prelude.rnf earliestRestorableDateTime
      `Prelude.seq` Prelude.rnf latestRestorableDateTime
      `Prelude.seq` Prelude.rnf pointInTimeRecoveryStatus
