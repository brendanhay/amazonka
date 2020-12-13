{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.ContributorInsightsSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.ContributorInsightsSummary
  ( ContributorInsightsSummary (..),

    -- * Smart constructor
    mkContributorInsightsSummary,

    -- * Lenses
    cisContributorInsightsStatus,
    cisTableName,
    cisIndexName,
  )
where

import Network.AWS.DynamoDB.Types.ContributorInsightsStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents a Contributor Insights summary entry.
--
-- /See:/ 'mkContributorInsightsSummary' smart constructor.
data ContributorInsightsSummary = ContributorInsightsSummary'
  { -- | Describes the current status for contributor insights for the given table and index, if applicable.
    contributorInsightsStatus :: Lude.Maybe ContributorInsightsStatus,
    -- | Name of the table associated with the summary.
    tableName :: Lude.Maybe Lude.Text,
    -- | Name of the index associated with the summary, if any.
    indexName :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ContributorInsightsSummary' with the minimum fields required to make a request.
--
-- * 'contributorInsightsStatus' - Describes the current status for contributor insights for the given table and index, if applicable.
-- * 'tableName' - Name of the table associated with the summary.
-- * 'indexName' - Name of the index associated with the summary, if any.
mkContributorInsightsSummary ::
  ContributorInsightsSummary
mkContributorInsightsSummary =
  ContributorInsightsSummary'
    { contributorInsightsStatus =
        Lude.Nothing,
      tableName = Lude.Nothing,
      indexName = Lude.Nothing
    }

-- | Describes the current status for contributor insights for the given table and index, if applicable.
--
-- /Note:/ Consider using 'contributorInsightsStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cisContributorInsightsStatus :: Lens.Lens' ContributorInsightsSummary (Lude.Maybe ContributorInsightsStatus)
cisContributorInsightsStatus = Lens.lens (contributorInsightsStatus :: ContributorInsightsSummary -> Lude.Maybe ContributorInsightsStatus) (\s a -> s {contributorInsightsStatus = a} :: ContributorInsightsSummary)
{-# DEPRECATED cisContributorInsightsStatus "Use generic-lens or generic-optics with 'contributorInsightsStatus' instead." #-}

-- | Name of the table associated with the summary.
--
-- /Note:/ Consider using 'tableName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cisTableName :: Lens.Lens' ContributorInsightsSummary (Lude.Maybe Lude.Text)
cisTableName = Lens.lens (tableName :: ContributorInsightsSummary -> Lude.Maybe Lude.Text) (\s a -> s {tableName = a} :: ContributorInsightsSummary)
{-# DEPRECATED cisTableName "Use generic-lens or generic-optics with 'tableName' instead." #-}

-- | Name of the index associated with the summary, if any.
--
-- /Note:/ Consider using 'indexName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cisIndexName :: Lens.Lens' ContributorInsightsSummary (Lude.Maybe Lude.Text)
cisIndexName = Lens.lens (indexName :: ContributorInsightsSummary -> Lude.Maybe Lude.Text) (\s a -> s {indexName = a} :: ContributorInsightsSummary)
{-# DEPRECATED cisIndexName "Use generic-lens or generic-optics with 'indexName' instead." #-}

instance Lude.FromJSON ContributorInsightsSummary where
  parseJSON =
    Lude.withObject
      "ContributorInsightsSummary"
      ( \x ->
          ContributorInsightsSummary'
            Lude.<$> (x Lude..:? "ContributorInsightsStatus")
            Lude.<*> (x Lude..:? "TableName")
            Lude.<*> (x Lude..:? "IndexName")
      )
