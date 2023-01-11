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
-- Module      : Amazonka.DrS.Types.DescribeJobsRequestFilters
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DrS.Types.DescribeJobsRequestFilters where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A set of filters by which to return Jobs.
--
-- /See:/ 'newDescribeJobsRequestFilters' smart constructor.
data DescribeJobsRequestFilters = DescribeJobsRequestFilters'
  { -- | The start date in a date range query.
    fromDate :: Prelude.Maybe Prelude.Text,
    -- | An array of Job IDs that should be returned. An empty array means all
    -- jobs.
    jobIDs :: Prelude.Maybe [Prelude.Text],
    -- | The end date in a date range query.
    toDate :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeJobsRequestFilters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fromDate', 'describeJobsRequestFilters_fromDate' - The start date in a date range query.
--
-- 'jobIDs', 'describeJobsRequestFilters_jobIDs' - An array of Job IDs that should be returned. An empty array means all
-- jobs.
--
-- 'toDate', 'describeJobsRequestFilters_toDate' - The end date in a date range query.
newDescribeJobsRequestFilters ::
  DescribeJobsRequestFilters
newDescribeJobsRequestFilters =
  DescribeJobsRequestFilters'
    { fromDate =
        Prelude.Nothing,
      jobIDs = Prelude.Nothing,
      toDate = Prelude.Nothing
    }

-- | The start date in a date range query.
describeJobsRequestFilters_fromDate :: Lens.Lens' DescribeJobsRequestFilters (Prelude.Maybe Prelude.Text)
describeJobsRequestFilters_fromDate = Lens.lens (\DescribeJobsRequestFilters' {fromDate} -> fromDate) (\s@DescribeJobsRequestFilters' {} a -> s {fromDate = a} :: DescribeJobsRequestFilters)

-- | An array of Job IDs that should be returned. An empty array means all
-- jobs.
describeJobsRequestFilters_jobIDs :: Lens.Lens' DescribeJobsRequestFilters (Prelude.Maybe [Prelude.Text])
describeJobsRequestFilters_jobIDs = Lens.lens (\DescribeJobsRequestFilters' {jobIDs} -> jobIDs) (\s@DescribeJobsRequestFilters' {} a -> s {jobIDs = a} :: DescribeJobsRequestFilters) Prelude.. Lens.mapping Lens.coerced

-- | The end date in a date range query.
describeJobsRequestFilters_toDate :: Lens.Lens' DescribeJobsRequestFilters (Prelude.Maybe Prelude.Text)
describeJobsRequestFilters_toDate = Lens.lens (\DescribeJobsRequestFilters' {toDate} -> toDate) (\s@DescribeJobsRequestFilters' {} a -> s {toDate = a} :: DescribeJobsRequestFilters)

instance Prelude.Hashable DescribeJobsRequestFilters where
  hashWithSalt _salt DescribeJobsRequestFilters' {..} =
    _salt `Prelude.hashWithSalt` fromDate
      `Prelude.hashWithSalt` jobIDs
      `Prelude.hashWithSalt` toDate

instance Prelude.NFData DescribeJobsRequestFilters where
  rnf DescribeJobsRequestFilters' {..} =
    Prelude.rnf fromDate
      `Prelude.seq` Prelude.rnf jobIDs
      `Prelude.seq` Prelude.rnf toDate

instance Data.ToJSON DescribeJobsRequestFilters where
  toJSON DescribeJobsRequestFilters' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("fromDate" Data..=) Prelude.<$> fromDate,
            ("jobIDs" Data..=) Prelude.<$> jobIDs,
            ("toDate" Data..=) Prelude.<$> toDate
          ]
      )
