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
-- Module      : Amazonka.Personalize.Types.FilterSummary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Personalize.Types.FilterSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A short summary of a filter\'s attributes.
--
-- /See:/ 'newFilterSummary' smart constructor.
data FilterSummary = FilterSummary'
  { -- | The name of the filter.
    name :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the filter.
    filterArn :: Prelude.Maybe Prelude.Text,
    -- | The time at which the filter was created.
    creationDateTime :: Prelude.Maybe Data.POSIX,
    -- | The status of the filter.
    status :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the dataset group to which the filter belongs.
    datasetGroupArn :: Prelude.Maybe Prelude.Text,
    -- | The time at which the filter was last updated.
    lastUpdatedDateTime :: Prelude.Maybe Data.POSIX,
    -- | If the filter failed, the reason for the failure.
    failureReason :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FilterSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'filterSummary_name' - The name of the filter.
--
-- 'filterArn', 'filterSummary_filterArn' - The ARN of the filter.
--
-- 'creationDateTime', 'filterSummary_creationDateTime' - The time at which the filter was created.
--
-- 'status', 'filterSummary_status' - The status of the filter.
--
-- 'datasetGroupArn', 'filterSummary_datasetGroupArn' - The ARN of the dataset group to which the filter belongs.
--
-- 'lastUpdatedDateTime', 'filterSummary_lastUpdatedDateTime' - The time at which the filter was last updated.
--
-- 'failureReason', 'filterSummary_failureReason' - If the filter failed, the reason for the failure.
newFilterSummary ::
  FilterSummary
newFilterSummary =
  FilterSummary'
    { name = Prelude.Nothing,
      filterArn = Prelude.Nothing,
      creationDateTime = Prelude.Nothing,
      status = Prelude.Nothing,
      datasetGroupArn = Prelude.Nothing,
      lastUpdatedDateTime = Prelude.Nothing,
      failureReason = Prelude.Nothing
    }

-- | The name of the filter.
filterSummary_name :: Lens.Lens' FilterSummary (Prelude.Maybe Prelude.Text)
filterSummary_name = Lens.lens (\FilterSummary' {name} -> name) (\s@FilterSummary' {} a -> s {name = a} :: FilterSummary)

-- | The ARN of the filter.
filterSummary_filterArn :: Lens.Lens' FilterSummary (Prelude.Maybe Prelude.Text)
filterSummary_filterArn = Lens.lens (\FilterSummary' {filterArn} -> filterArn) (\s@FilterSummary' {} a -> s {filterArn = a} :: FilterSummary)

-- | The time at which the filter was created.
filterSummary_creationDateTime :: Lens.Lens' FilterSummary (Prelude.Maybe Prelude.UTCTime)
filterSummary_creationDateTime = Lens.lens (\FilterSummary' {creationDateTime} -> creationDateTime) (\s@FilterSummary' {} a -> s {creationDateTime = a} :: FilterSummary) Prelude.. Lens.mapping Data._Time

-- | The status of the filter.
filterSummary_status :: Lens.Lens' FilterSummary (Prelude.Maybe Prelude.Text)
filterSummary_status = Lens.lens (\FilterSummary' {status} -> status) (\s@FilterSummary' {} a -> s {status = a} :: FilterSummary)

-- | The ARN of the dataset group to which the filter belongs.
filterSummary_datasetGroupArn :: Lens.Lens' FilterSummary (Prelude.Maybe Prelude.Text)
filterSummary_datasetGroupArn = Lens.lens (\FilterSummary' {datasetGroupArn} -> datasetGroupArn) (\s@FilterSummary' {} a -> s {datasetGroupArn = a} :: FilterSummary)

-- | The time at which the filter was last updated.
filterSummary_lastUpdatedDateTime :: Lens.Lens' FilterSummary (Prelude.Maybe Prelude.UTCTime)
filterSummary_lastUpdatedDateTime = Lens.lens (\FilterSummary' {lastUpdatedDateTime} -> lastUpdatedDateTime) (\s@FilterSummary' {} a -> s {lastUpdatedDateTime = a} :: FilterSummary) Prelude.. Lens.mapping Data._Time

-- | If the filter failed, the reason for the failure.
filterSummary_failureReason :: Lens.Lens' FilterSummary (Prelude.Maybe Prelude.Text)
filterSummary_failureReason = Lens.lens (\FilterSummary' {failureReason} -> failureReason) (\s@FilterSummary' {} a -> s {failureReason = a} :: FilterSummary)

instance Data.FromJSON FilterSummary where
  parseJSON =
    Data.withObject
      "FilterSummary"
      ( \x ->
          FilterSummary'
            Prelude.<$> (x Data..:? "name")
            Prelude.<*> (x Data..:? "filterArn")
            Prelude.<*> (x Data..:? "creationDateTime")
            Prelude.<*> (x Data..:? "status")
            Prelude.<*> (x Data..:? "datasetGroupArn")
            Prelude.<*> (x Data..:? "lastUpdatedDateTime")
            Prelude.<*> (x Data..:? "failureReason")
      )

instance Prelude.Hashable FilterSummary where
  hashWithSalt _salt FilterSummary' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` filterArn
      `Prelude.hashWithSalt` creationDateTime
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` datasetGroupArn
      `Prelude.hashWithSalt` lastUpdatedDateTime
      `Prelude.hashWithSalt` failureReason

instance Prelude.NFData FilterSummary where
  rnf FilterSummary' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf filterArn
      `Prelude.seq` Prelude.rnf creationDateTime
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf datasetGroupArn
      `Prelude.seq` Prelude.rnf lastUpdatedDateTime
      `Prelude.seq` Prelude.rnf failureReason
