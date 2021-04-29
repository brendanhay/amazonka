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
-- Module      : Network.AWS.DynamoDB.Types.ContributorInsightsSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.ContributorInsightsSummary where

import Network.AWS.DynamoDB.Types.ContributorInsightsStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Represents a Contributor Insights summary entry.
--
-- /See:/ 'newContributorInsightsSummary' smart constructor.
data ContributorInsightsSummary = ContributorInsightsSummary'
  { -- | Name of the table associated with the summary.
    tableName :: Prelude.Maybe Prelude.Text,
    -- | Name of the index associated with the summary, if any.
    indexName :: Prelude.Maybe Prelude.Text,
    -- | Describes the current status for contributor insights for the given
    -- table and index, if applicable.
    contributorInsightsStatus :: Prelude.Maybe ContributorInsightsStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ContributorInsightsSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tableName', 'contributorInsightsSummary_tableName' - Name of the table associated with the summary.
--
-- 'indexName', 'contributorInsightsSummary_indexName' - Name of the index associated with the summary, if any.
--
-- 'contributorInsightsStatus', 'contributorInsightsSummary_contributorInsightsStatus' - Describes the current status for contributor insights for the given
-- table and index, if applicable.
newContributorInsightsSummary ::
  ContributorInsightsSummary
newContributorInsightsSummary =
  ContributorInsightsSummary'
    { tableName =
        Prelude.Nothing,
      indexName = Prelude.Nothing,
      contributorInsightsStatus = Prelude.Nothing
    }

-- | Name of the table associated with the summary.
contributorInsightsSummary_tableName :: Lens.Lens' ContributorInsightsSummary (Prelude.Maybe Prelude.Text)
contributorInsightsSummary_tableName = Lens.lens (\ContributorInsightsSummary' {tableName} -> tableName) (\s@ContributorInsightsSummary' {} a -> s {tableName = a} :: ContributorInsightsSummary)

-- | Name of the index associated with the summary, if any.
contributorInsightsSummary_indexName :: Lens.Lens' ContributorInsightsSummary (Prelude.Maybe Prelude.Text)
contributorInsightsSummary_indexName = Lens.lens (\ContributorInsightsSummary' {indexName} -> indexName) (\s@ContributorInsightsSummary' {} a -> s {indexName = a} :: ContributorInsightsSummary)

-- | Describes the current status for contributor insights for the given
-- table and index, if applicable.
contributorInsightsSummary_contributorInsightsStatus :: Lens.Lens' ContributorInsightsSummary (Prelude.Maybe ContributorInsightsStatus)
contributorInsightsSummary_contributorInsightsStatus = Lens.lens (\ContributorInsightsSummary' {contributorInsightsStatus} -> contributorInsightsStatus) (\s@ContributorInsightsSummary' {} a -> s {contributorInsightsStatus = a} :: ContributorInsightsSummary)

instance Prelude.FromJSON ContributorInsightsSummary where
  parseJSON =
    Prelude.withObject
      "ContributorInsightsSummary"
      ( \x ->
          ContributorInsightsSummary'
            Prelude.<$> (x Prelude..:? "TableName")
            Prelude.<*> (x Prelude..:? "IndexName")
            Prelude.<*> (x Prelude..:? "ContributorInsightsStatus")
      )

instance Prelude.Hashable ContributorInsightsSummary

instance Prelude.NFData ContributorInsightsSummary
