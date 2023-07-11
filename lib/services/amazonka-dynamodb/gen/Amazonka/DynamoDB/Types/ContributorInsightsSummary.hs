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
-- Module      : Amazonka.DynamoDB.Types.ContributorInsightsSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DynamoDB.Types.ContributorInsightsSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DynamoDB.Types.AttributeValue
import Amazonka.DynamoDB.Types.ContributorInsightsStatus
import Amazonka.DynamoDB.Types.WriteRequest
import qualified Amazonka.Prelude as Prelude

-- | Represents a Contributor Insights summary entry.
--
-- /See:/ 'newContributorInsightsSummary' smart constructor.
data ContributorInsightsSummary = ContributorInsightsSummary'
  { -- | Describes the current status for contributor insights for the given
    -- table and index, if applicable.
    contributorInsightsStatus :: Prelude.Maybe ContributorInsightsStatus,
    -- | Name of the index associated with the summary, if any.
    indexName :: Prelude.Maybe Prelude.Text,
    -- | Name of the table associated with the summary.
    tableName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ContributorInsightsSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'contributorInsightsStatus', 'contributorInsightsSummary_contributorInsightsStatus' - Describes the current status for contributor insights for the given
-- table and index, if applicable.
--
-- 'indexName', 'contributorInsightsSummary_indexName' - Name of the index associated with the summary, if any.
--
-- 'tableName', 'contributorInsightsSummary_tableName' - Name of the table associated with the summary.
newContributorInsightsSummary ::
  ContributorInsightsSummary
newContributorInsightsSummary =
  ContributorInsightsSummary'
    { contributorInsightsStatus =
        Prelude.Nothing,
      indexName = Prelude.Nothing,
      tableName = Prelude.Nothing
    }

-- | Describes the current status for contributor insights for the given
-- table and index, if applicable.
contributorInsightsSummary_contributorInsightsStatus :: Lens.Lens' ContributorInsightsSummary (Prelude.Maybe ContributorInsightsStatus)
contributorInsightsSummary_contributorInsightsStatus = Lens.lens (\ContributorInsightsSummary' {contributorInsightsStatus} -> contributorInsightsStatus) (\s@ContributorInsightsSummary' {} a -> s {contributorInsightsStatus = a} :: ContributorInsightsSummary)

-- | Name of the index associated with the summary, if any.
contributorInsightsSummary_indexName :: Lens.Lens' ContributorInsightsSummary (Prelude.Maybe Prelude.Text)
contributorInsightsSummary_indexName = Lens.lens (\ContributorInsightsSummary' {indexName} -> indexName) (\s@ContributorInsightsSummary' {} a -> s {indexName = a} :: ContributorInsightsSummary)

-- | Name of the table associated with the summary.
contributorInsightsSummary_tableName :: Lens.Lens' ContributorInsightsSummary (Prelude.Maybe Prelude.Text)
contributorInsightsSummary_tableName = Lens.lens (\ContributorInsightsSummary' {tableName} -> tableName) (\s@ContributorInsightsSummary' {} a -> s {tableName = a} :: ContributorInsightsSummary)

instance Data.FromJSON ContributorInsightsSummary where
  parseJSON =
    Data.withObject
      "ContributorInsightsSummary"
      ( \x ->
          ContributorInsightsSummary'
            Prelude.<$> (x Data..:? "ContributorInsightsStatus")
            Prelude.<*> (x Data..:? "IndexName")
            Prelude.<*> (x Data..:? "TableName")
      )

instance Prelude.Hashable ContributorInsightsSummary where
  hashWithSalt _salt ContributorInsightsSummary' {..} =
    _salt
      `Prelude.hashWithSalt` contributorInsightsStatus
      `Prelude.hashWithSalt` indexName
      `Prelude.hashWithSalt` tableName

instance Prelude.NFData ContributorInsightsSummary where
  rnf ContributorInsightsSummary' {..} =
    Prelude.rnf contributorInsightsStatus
      `Prelude.seq` Prelude.rnf indexName
      `Prelude.seq` Prelude.rnf tableName
