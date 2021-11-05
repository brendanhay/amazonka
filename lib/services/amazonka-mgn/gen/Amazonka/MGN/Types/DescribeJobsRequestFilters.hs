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
-- Module      : Amazonka.MGN.Types.DescribeJobsRequestFilters
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MGN.Types.DescribeJobsRequestFilters where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Request to describe Job log filters.
--
-- /See:/ 'newDescribeJobsRequestFilters' smart constructor.
data DescribeJobsRequestFilters = DescribeJobsRequestFilters'
  { -- | Request to describe Job log filters by date.
    fromDate :: Prelude.Maybe Prelude.Text,
    -- | Request to describe Job log by last date.
    toDate :: Prelude.Maybe Prelude.Text,
    -- | Request to describe Job log filters by job ID.
    jobIDs :: Prelude.Maybe [Prelude.Text]
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
-- 'fromDate', 'describeJobsRequestFilters_fromDate' - Request to describe Job log filters by date.
--
-- 'toDate', 'describeJobsRequestFilters_toDate' - Request to describe Job log by last date.
--
-- 'jobIDs', 'describeJobsRequestFilters_jobIDs' - Request to describe Job log filters by job ID.
newDescribeJobsRequestFilters ::
  DescribeJobsRequestFilters
newDescribeJobsRequestFilters =
  DescribeJobsRequestFilters'
    { fromDate =
        Prelude.Nothing,
      toDate = Prelude.Nothing,
      jobIDs = Prelude.Nothing
    }

-- | Request to describe Job log filters by date.
describeJobsRequestFilters_fromDate :: Lens.Lens' DescribeJobsRequestFilters (Prelude.Maybe Prelude.Text)
describeJobsRequestFilters_fromDate = Lens.lens (\DescribeJobsRequestFilters' {fromDate} -> fromDate) (\s@DescribeJobsRequestFilters' {} a -> s {fromDate = a} :: DescribeJobsRequestFilters)

-- | Request to describe Job log by last date.
describeJobsRequestFilters_toDate :: Lens.Lens' DescribeJobsRequestFilters (Prelude.Maybe Prelude.Text)
describeJobsRequestFilters_toDate = Lens.lens (\DescribeJobsRequestFilters' {toDate} -> toDate) (\s@DescribeJobsRequestFilters' {} a -> s {toDate = a} :: DescribeJobsRequestFilters)

-- | Request to describe Job log filters by job ID.
describeJobsRequestFilters_jobIDs :: Lens.Lens' DescribeJobsRequestFilters (Prelude.Maybe [Prelude.Text])
describeJobsRequestFilters_jobIDs = Lens.lens (\DescribeJobsRequestFilters' {jobIDs} -> jobIDs) (\s@DescribeJobsRequestFilters' {} a -> s {jobIDs = a} :: DescribeJobsRequestFilters) Prelude.. Lens.mapping Lens.coerced

instance Prelude.Hashable DescribeJobsRequestFilters

instance Prelude.NFData DescribeJobsRequestFilters

instance Core.ToJSON DescribeJobsRequestFilters where
  toJSON DescribeJobsRequestFilters' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("fromDate" Core..=) Prelude.<$> fromDate,
            ("toDate" Core..=) Prelude.<$> toDate,
            ("jobIDs" Core..=) Prelude.<$> jobIDs
          ]
      )
