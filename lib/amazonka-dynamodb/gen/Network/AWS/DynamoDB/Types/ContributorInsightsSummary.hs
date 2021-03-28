{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.ContributorInsightsSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DynamoDB.Types.ContributorInsightsSummary
  ( ContributorInsightsSummary (..)
  -- * Smart constructor
  , mkContributorInsightsSummary
  -- * Lenses
  , cisContributorInsightsStatus
  , cisIndexName
  , cisTableName
  ) where

import qualified Network.AWS.DynamoDB.Types.ContributorInsightsStatus as Types
import qualified Network.AWS.DynamoDB.Types.IndexName as Types
import qualified Network.AWS.DynamoDB.Types.TableName as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents a Contributor Insights summary entry.
--
-- /See:/ 'mkContributorInsightsSummary' smart constructor.
data ContributorInsightsSummary = ContributorInsightsSummary'
  { contributorInsightsStatus :: Core.Maybe Types.ContributorInsightsStatus
    -- ^ Describes the current status for contributor insights for the given table and index, if applicable.
  , indexName :: Core.Maybe Types.IndexName
    -- ^ Name of the index associated with the summary, if any.
  , tableName :: Core.Maybe Types.TableName
    -- ^ Name of the table associated with the summary.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ContributorInsightsSummary' value with any optional fields omitted.
mkContributorInsightsSummary
    :: ContributorInsightsSummary
mkContributorInsightsSummary
  = ContributorInsightsSummary'{contributorInsightsStatus =
                                  Core.Nothing,
                                indexName = Core.Nothing, tableName = Core.Nothing}

-- | Describes the current status for contributor insights for the given table and index, if applicable.
--
-- /Note:/ Consider using 'contributorInsightsStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cisContributorInsightsStatus :: Lens.Lens' ContributorInsightsSummary (Core.Maybe Types.ContributorInsightsStatus)
cisContributorInsightsStatus = Lens.field @"contributorInsightsStatus"
{-# INLINEABLE cisContributorInsightsStatus #-}
{-# DEPRECATED contributorInsightsStatus "Use generic-lens or generic-optics with 'contributorInsightsStatus' instead"  #-}

-- | Name of the index associated with the summary, if any.
--
-- /Note:/ Consider using 'indexName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cisIndexName :: Lens.Lens' ContributorInsightsSummary (Core.Maybe Types.IndexName)
cisIndexName = Lens.field @"indexName"
{-# INLINEABLE cisIndexName #-}
{-# DEPRECATED indexName "Use generic-lens or generic-optics with 'indexName' instead"  #-}

-- | Name of the table associated with the summary.
--
-- /Note:/ Consider using 'tableName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cisTableName :: Lens.Lens' ContributorInsightsSummary (Core.Maybe Types.TableName)
cisTableName = Lens.field @"tableName"
{-# INLINEABLE cisTableName #-}
{-# DEPRECATED tableName "Use generic-lens or generic-optics with 'tableName' instead"  #-}

instance Core.FromJSON ContributorInsightsSummary where
        parseJSON
          = Core.withObject "ContributorInsightsSummary" Core.$
              \ x ->
                ContributorInsightsSummary' Core.<$>
                  (x Core..:? "ContributorInsightsStatus") Core.<*>
                    x Core..:? "IndexName"
                    Core.<*> x Core..:? "TableName"
