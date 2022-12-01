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
-- Module      : Amazonka.Personalize.Types.Filter
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Personalize.Types.Filter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Contains information on a recommendation filter, including its ARN,
-- status, and filter expression.
--
-- /See:/ 'newFilter' smart constructor.
data Filter = Filter'
  { -- | The name of the filter.
    name :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the filter.
    filterArn :: Prelude.Maybe Prelude.Text,
    -- | The time at which the filter was created.
    creationDateTime :: Prelude.Maybe Core.POSIX,
    -- | Specifies the type of item interactions to filter out of recommendation
    -- results. The filter expression must follow specific format rules. For
    -- information about filter expression structure and syntax, see
    -- <https://docs.aws.amazon.com/personalize/latest/dg/filter-expressions.html Filter expressions>.
    filterExpression :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | The status of the filter.
    status :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the dataset group to which the filter belongs.
    datasetGroupArn :: Prelude.Maybe Prelude.Text,
    -- | The time at which the filter was last updated.
    lastUpdatedDateTime :: Prelude.Maybe Core.POSIX,
    -- | If the filter failed, the reason for its failure.
    failureReason :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Filter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'filter_name' - The name of the filter.
--
-- 'filterArn', 'filter_filterArn' - The ARN of the filter.
--
-- 'creationDateTime', 'filter_creationDateTime' - The time at which the filter was created.
--
-- 'filterExpression', 'filter_filterExpression' - Specifies the type of item interactions to filter out of recommendation
-- results. The filter expression must follow specific format rules. For
-- information about filter expression structure and syntax, see
-- <https://docs.aws.amazon.com/personalize/latest/dg/filter-expressions.html Filter expressions>.
--
-- 'status', 'filter_status' - The status of the filter.
--
-- 'datasetGroupArn', 'filter_datasetGroupArn' - The ARN of the dataset group to which the filter belongs.
--
-- 'lastUpdatedDateTime', 'filter_lastUpdatedDateTime' - The time at which the filter was last updated.
--
-- 'failureReason', 'filter_failureReason' - If the filter failed, the reason for its failure.
newFilter ::
  Filter
newFilter =
  Filter'
    { name = Prelude.Nothing,
      filterArn = Prelude.Nothing,
      creationDateTime = Prelude.Nothing,
      filterExpression = Prelude.Nothing,
      status = Prelude.Nothing,
      datasetGroupArn = Prelude.Nothing,
      lastUpdatedDateTime = Prelude.Nothing,
      failureReason = Prelude.Nothing
    }

-- | The name of the filter.
filter_name :: Lens.Lens' Filter (Prelude.Maybe Prelude.Text)
filter_name = Lens.lens (\Filter' {name} -> name) (\s@Filter' {} a -> s {name = a} :: Filter)

-- | The ARN of the filter.
filter_filterArn :: Lens.Lens' Filter (Prelude.Maybe Prelude.Text)
filter_filterArn = Lens.lens (\Filter' {filterArn} -> filterArn) (\s@Filter' {} a -> s {filterArn = a} :: Filter)

-- | The time at which the filter was created.
filter_creationDateTime :: Lens.Lens' Filter (Prelude.Maybe Prelude.UTCTime)
filter_creationDateTime = Lens.lens (\Filter' {creationDateTime} -> creationDateTime) (\s@Filter' {} a -> s {creationDateTime = a} :: Filter) Prelude.. Lens.mapping Core._Time

-- | Specifies the type of item interactions to filter out of recommendation
-- results. The filter expression must follow specific format rules. For
-- information about filter expression structure and syntax, see
-- <https://docs.aws.amazon.com/personalize/latest/dg/filter-expressions.html Filter expressions>.
filter_filterExpression :: Lens.Lens' Filter (Prelude.Maybe Prelude.Text)
filter_filterExpression = Lens.lens (\Filter' {filterExpression} -> filterExpression) (\s@Filter' {} a -> s {filterExpression = a} :: Filter) Prelude.. Lens.mapping Core._Sensitive

-- | The status of the filter.
filter_status :: Lens.Lens' Filter (Prelude.Maybe Prelude.Text)
filter_status = Lens.lens (\Filter' {status} -> status) (\s@Filter' {} a -> s {status = a} :: Filter)

-- | The ARN of the dataset group to which the filter belongs.
filter_datasetGroupArn :: Lens.Lens' Filter (Prelude.Maybe Prelude.Text)
filter_datasetGroupArn = Lens.lens (\Filter' {datasetGroupArn} -> datasetGroupArn) (\s@Filter' {} a -> s {datasetGroupArn = a} :: Filter)

-- | The time at which the filter was last updated.
filter_lastUpdatedDateTime :: Lens.Lens' Filter (Prelude.Maybe Prelude.UTCTime)
filter_lastUpdatedDateTime = Lens.lens (\Filter' {lastUpdatedDateTime} -> lastUpdatedDateTime) (\s@Filter' {} a -> s {lastUpdatedDateTime = a} :: Filter) Prelude.. Lens.mapping Core._Time

-- | If the filter failed, the reason for its failure.
filter_failureReason :: Lens.Lens' Filter (Prelude.Maybe Prelude.Text)
filter_failureReason = Lens.lens (\Filter' {failureReason} -> failureReason) (\s@Filter' {} a -> s {failureReason = a} :: Filter)

instance Core.FromJSON Filter where
  parseJSON =
    Core.withObject
      "Filter"
      ( \x ->
          Filter'
            Prelude.<$> (x Core..:? "name")
            Prelude.<*> (x Core..:? "filterArn")
            Prelude.<*> (x Core..:? "creationDateTime")
            Prelude.<*> (x Core..:? "filterExpression")
            Prelude.<*> (x Core..:? "status")
            Prelude.<*> (x Core..:? "datasetGroupArn")
            Prelude.<*> (x Core..:? "lastUpdatedDateTime")
            Prelude.<*> (x Core..:? "failureReason")
      )

instance Prelude.Hashable Filter where
  hashWithSalt _salt Filter' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` filterArn
      `Prelude.hashWithSalt` creationDateTime
      `Prelude.hashWithSalt` filterExpression
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` datasetGroupArn
      `Prelude.hashWithSalt` lastUpdatedDateTime
      `Prelude.hashWithSalt` failureReason

instance Prelude.NFData Filter where
  rnf Filter' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf filterArn
      `Prelude.seq` Prelude.rnf creationDateTime
      `Prelude.seq` Prelude.rnf filterExpression
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf datasetGroupArn
      `Prelude.seq` Prelude.rnf lastUpdatedDateTime
      `Prelude.seq` Prelude.rnf failureReason
