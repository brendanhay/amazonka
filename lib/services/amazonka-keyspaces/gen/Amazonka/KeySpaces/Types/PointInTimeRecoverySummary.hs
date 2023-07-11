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
-- Module      : Amazonka.KeySpaces.Types.PointInTimeRecoverySummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KeySpaces.Types.PointInTimeRecoverySummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.KeySpaces.Types.PointInTimeRecoveryStatus
import qualified Amazonka.Prelude as Prelude

-- | The point-in-time recovery status of the specified table.
--
-- /See:/ 'newPointInTimeRecoverySummary' smart constructor.
data PointInTimeRecoverySummary = PointInTimeRecoverySummary'
  { -- | Specifies the earliest possible restore point of the table in ISO 8601
    -- format.
    earliestRestorableTimestamp :: Prelude.Maybe Data.POSIX,
    -- | Shows if point-in-time recovery is enabled or disabled for the specified
    -- table.
    status :: PointInTimeRecoveryStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PointInTimeRecoverySummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'earliestRestorableTimestamp', 'pointInTimeRecoverySummary_earliestRestorableTimestamp' - Specifies the earliest possible restore point of the table in ISO 8601
-- format.
--
-- 'status', 'pointInTimeRecoverySummary_status' - Shows if point-in-time recovery is enabled or disabled for the specified
-- table.
newPointInTimeRecoverySummary ::
  -- | 'status'
  PointInTimeRecoveryStatus ->
  PointInTimeRecoverySummary
newPointInTimeRecoverySummary pStatus_ =
  PointInTimeRecoverySummary'
    { earliestRestorableTimestamp =
        Prelude.Nothing,
      status = pStatus_
    }

-- | Specifies the earliest possible restore point of the table in ISO 8601
-- format.
pointInTimeRecoverySummary_earliestRestorableTimestamp :: Lens.Lens' PointInTimeRecoverySummary (Prelude.Maybe Prelude.UTCTime)
pointInTimeRecoverySummary_earliestRestorableTimestamp = Lens.lens (\PointInTimeRecoverySummary' {earliestRestorableTimestamp} -> earliestRestorableTimestamp) (\s@PointInTimeRecoverySummary' {} a -> s {earliestRestorableTimestamp = a} :: PointInTimeRecoverySummary) Prelude.. Lens.mapping Data._Time

-- | Shows if point-in-time recovery is enabled or disabled for the specified
-- table.
pointInTimeRecoverySummary_status :: Lens.Lens' PointInTimeRecoverySummary PointInTimeRecoveryStatus
pointInTimeRecoverySummary_status = Lens.lens (\PointInTimeRecoverySummary' {status} -> status) (\s@PointInTimeRecoverySummary' {} a -> s {status = a} :: PointInTimeRecoverySummary)

instance Data.FromJSON PointInTimeRecoverySummary where
  parseJSON =
    Data.withObject
      "PointInTimeRecoverySummary"
      ( \x ->
          PointInTimeRecoverySummary'
            Prelude.<$> (x Data..:? "earliestRestorableTimestamp")
            Prelude.<*> (x Data..: "status")
      )

instance Prelude.Hashable PointInTimeRecoverySummary where
  hashWithSalt _salt PointInTimeRecoverySummary' {..} =
    _salt
      `Prelude.hashWithSalt` earliestRestorableTimestamp
      `Prelude.hashWithSalt` status

instance Prelude.NFData PointInTimeRecoverySummary where
  rnf PointInTimeRecoverySummary' {..} =
    Prelude.rnf earliestRestorableTimestamp
      `Prelude.seq` Prelude.rnf status
