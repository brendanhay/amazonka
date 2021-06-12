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

import qualified Network.AWS.Core as Core
import Network.AWS.DynamoDB.Types.ContributorInsightsStatus
import qualified Network.AWS.Lens as Lens

-- | Represents a Contributor Insights summary entry.
--
-- /See:/ 'newContributorInsightsSummary' smart constructor.
data ContributorInsightsSummary = ContributorInsightsSummary'
  { -- | Name of the table associated with the summary.
    tableName :: Core.Maybe Core.Text,
    -- | Name of the index associated with the summary, if any.
    indexName :: Core.Maybe Core.Text,
    -- | Describes the current status for contributor insights for the given
    -- table and index, if applicable.
    contributorInsightsStatus :: Core.Maybe ContributorInsightsStatus
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
        Core.Nothing,
      indexName = Core.Nothing,
      contributorInsightsStatus = Core.Nothing
    }

-- | Name of the table associated with the summary.
contributorInsightsSummary_tableName :: Lens.Lens' ContributorInsightsSummary (Core.Maybe Core.Text)
contributorInsightsSummary_tableName = Lens.lens (\ContributorInsightsSummary' {tableName} -> tableName) (\s@ContributorInsightsSummary' {} a -> s {tableName = a} :: ContributorInsightsSummary)

-- | Name of the index associated with the summary, if any.
contributorInsightsSummary_indexName :: Lens.Lens' ContributorInsightsSummary (Core.Maybe Core.Text)
contributorInsightsSummary_indexName = Lens.lens (\ContributorInsightsSummary' {indexName} -> indexName) (\s@ContributorInsightsSummary' {} a -> s {indexName = a} :: ContributorInsightsSummary)

-- | Describes the current status for contributor insights for the given
-- table and index, if applicable.
contributorInsightsSummary_contributorInsightsStatus :: Lens.Lens' ContributorInsightsSummary (Core.Maybe ContributorInsightsStatus)
contributorInsightsSummary_contributorInsightsStatus = Lens.lens (\ContributorInsightsSummary' {contributorInsightsStatus} -> contributorInsightsStatus) (\s@ContributorInsightsSummary' {} a -> s {contributorInsightsStatus = a} :: ContributorInsightsSummary)

instance Core.FromJSON ContributorInsightsSummary where
  parseJSON =
    Core.withObject
      "ContributorInsightsSummary"
      ( \x ->
          ContributorInsightsSummary'
            Core.<$> (x Core..:? "TableName")
            Core.<*> (x Core..:? "IndexName")
            Core.<*> (x Core..:? "ContributorInsightsStatus")
      )

instance Core.Hashable ContributorInsightsSummary

instance Core.NFData ContributorInsightsSummary
