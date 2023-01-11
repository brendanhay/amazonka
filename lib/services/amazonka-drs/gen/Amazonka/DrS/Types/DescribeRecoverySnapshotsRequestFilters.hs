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
-- Module      : Amazonka.DrS.Types.DescribeRecoverySnapshotsRequestFilters
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DrS.Types.DescribeRecoverySnapshotsRequestFilters where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A set of filters by which to return Recovery Snapshots.
--
-- /See:/ 'newDescribeRecoverySnapshotsRequestFilters' smart constructor.
data DescribeRecoverySnapshotsRequestFilters = DescribeRecoverySnapshotsRequestFilters'
  { -- | The start date in a date range query.
    fromDateTime :: Prelude.Maybe Prelude.Text,
    -- | The end date in a date range query.
    toDateTime :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeRecoverySnapshotsRequestFilters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fromDateTime', 'describeRecoverySnapshotsRequestFilters_fromDateTime' - The start date in a date range query.
--
-- 'toDateTime', 'describeRecoverySnapshotsRequestFilters_toDateTime' - The end date in a date range query.
newDescribeRecoverySnapshotsRequestFilters ::
  DescribeRecoverySnapshotsRequestFilters
newDescribeRecoverySnapshotsRequestFilters =
  DescribeRecoverySnapshotsRequestFilters'
    { fromDateTime =
        Prelude.Nothing,
      toDateTime = Prelude.Nothing
    }

-- | The start date in a date range query.
describeRecoverySnapshotsRequestFilters_fromDateTime :: Lens.Lens' DescribeRecoverySnapshotsRequestFilters (Prelude.Maybe Prelude.Text)
describeRecoverySnapshotsRequestFilters_fromDateTime = Lens.lens (\DescribeRecoverySnapshotsRequestFilters' {fromDateTime} -> fromDateTime) (\s@DescribeRecoverySnapshotsRequestFilters' {} a -> s {fromDateTime = a} :: DescribeRecoverySnapshotsRequestFilters)

-- | The end date in a date range query.
describeRecoverySnapshotsRequestFilters_toDateTime :: Lens.Lens' DescribeRecoverySnapshotsRequestFilters (Prelude.Maybe Prelude.Text)
describeRecoverySnapshotsRequestFilters_toDateTime = Lens.lens (\DescribeRecoverySnapshotsRequestFilters' {toDateTime} -> toDateTime) (\s@DescribeRecoverySnapshotsRequestFilters' {} a -> s {toDateTime = a} :: DescribeRecoverySnapshotsRequestFilters)

instance
  Prelude.Hashable
    DescribeRecoverySnapshotsRequestFilters
  where
  hashWithSalt
    _salt
    DescribeRecoverySnapshotsRequestFilters' {..} =
      _salt `Prelude.hashWithSalt` fromDateTime
        `Prelude.hashWithSalt` toDateTime

instance
  Prelude.NFData
    DescribeRecoverySnapshotsRequestFilters
  where
  rnf DescribeRecoverySnapshotsRequestFilters' {..} =
    Prelude.rnf fromDateTime
      `Prelude.seq` Prelude.rnf toDateTime

instance
  Data.ToJSON
    DescribeRecoverySnapshotsRequestFilters
  where
  toJSON DescribeRecoverySnapshotsRequestFilters' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("fromDateTime" Data..=) Prelude.<$> fromDateTime,
            ("toDateTime" Data..=) Prelude.<$> toDateTime
          ]
      )
