{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.DescribeContributorInsights
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about contributor insights, for a given table or global secondary index.
module Network.AWS.DynamoDB.DescribeContributorInsights
  ( -- * Creating a request
    DescribeContributorInsights (..),
    mkDescribeContributorInsights,

    -- ** Request lenses
    dciTableName,
    dciIndexName,

    -- * Destructuring the response
    DescribeContributorInsightsResponse (..),
    mkDescribeContributorInsightsResponse,

    -- ** Response lenses
    dcirrsContributorInsightsRuleList,
    dcirrsContributorInsightsStatus,
    dcirrsFailureException,
    dcirrsIndexName,
    dcirrsLastUpdateDateTime,
    dcirrsTableName,
    dcirrsResponseStatus,
  )
where

import qualified Network.AWS.DynamoDB.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeContributorInsights' smart constructor.
data DescribeContributorInsights = DescribeContributorInsights'
  { -- | The name of the table to describe.
    tableName :: Types.TableName,
    -- | The name of the global secondary index to describe, if applicable.
    indexName :: Core.Maybe Types.IndexName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeContributorInsights' value with any optional fields omitted.
mkDescribeContributorInsights ::
  -- | 'tableName'
  Types.TableName ->
  DescribeContributorInsights
mkDescribeContributorInsights tableName =
  DescribeContributorInsights' {tableName, indexName = Core.Nothing}

-- | The name of the table to describe.
--
-- /Note:/ Consider using 'tableName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dciTableName :: Lens.Lens' DescribeContributorInsights Types.TableName
dciTableName = Lens.field @"tableName"
{-# DEPRECATED dciTableName "Use generic-lens or generic-optics with 'tableName' instead." #-}

-- | The name of the global secondary index to describe, if applicable.
--
-- /Note:/ Consider using 'indexName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dciIndexName :: Lens.Lens' DescribeContributorInsights (Core.Maybe Types.IndexName)
dciIndexName = Lens.field @"indexName"
{-# DEPRECATED dciIndexName "Use generic-lens or generic-optics with 'indexName' instead." #-}

instance Core.FromJSON DescribeContributorInsights where
  toJSON DescribeContributorInsights {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("TableName" Core..= tableName),
            ("IndexName" Core..=) Core.<$> indexName
          ]
      )

instance Core.AWSRequest DescribeContributorInsights where
  type
    Rs DescribeContributorInsights =
      DescribeContributorInsightsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "DynamoDB_20120810.DescribeContributorInsights")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.0")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeContributorInsightsResponse'
            Core.<$> (x Core..:? "ContributorInsightsRuleList")
            Core.<*> (x Core..:? "ContributorInsightsStatus")
            Core.<*> (x Core..:? "FailureException")
            Core.<*> (x Core..:? "IndexName")
            Core.<*> (x Core..:? "LastUpdateDateTime")
            Core.<*> (x Core..:? "TableName")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDescribeContributorInsightsResponse' smart constructor.
data DescribeContributorInsightsResponse = DescribeContributorInsightsResponse'
  { -- | List of names of the associated Alpine rules.
    contributorInsightsRuleList :: Core.Maybe [Types.ContributorInsightsRule],
    -- | Current Status contributor insights.
    contributorInsightsStatus :: Core.Maybe Types.ContributorInsightsStatus,
    -- | Returns information about the last failure that encountered.
    --
    -- The most common exceptions for a FAILED status are:
    --
    --     * LimitExceededException - Per-account Amazon CloudWatch Contributor Insights rule limit reached. Please disable Contributor Insights for other tables/indexes OR disable Contributor Insights rules before retrying.
    --
    --
    --     * AccessDeniedException - Amazon CloudWatch Contributor Insights rules cannot be modified due to insufficient permissions.
    --
    --
    --     * AccessDeniedException - Failed to create service-linked role for Contributor Insights due to insufficient permissions.
    --
    --
    --     * InternalServerError - Failed to create Amazon CloudWatch Contributor Insights rules. Please retry request.
    failureException :: Core.Maybe Types.FailureException,
    -- | The name of the global secondary index being described.
    indexName :: Core.Maybe Types.IndexName,
    -- | Timestamp of the last time the status was changed.
    lastUpdateDateTime :: Core.Maybe Core.NominalDiffTime,
    -- | The name of the table being described.
    tableName :: Core.Maybe Types.TableName,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribeContributorInsightsResponse' value with any optional fields omitted.
mkDescribeContributorInsightsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeContributorInsightsResponse
mkDescribeContributorInsightsResponse responseStatus =
  DescribeContributorInsightsResponse'
    { contributorInsightsRuleList =
        Core.Nothing,
      contributorInsightsStatus = Core.Nothing,
      failureException = Core.Nothing,
      indexName = Core.Nothing,
      lastUpdateDateTime = Core.Nothing,
      tableName = Core.Nothing,
      responseStatus
    }

-- | List of names of the associated Alpine rules.
--
-- /Note:/ Consider using 'contributorInsightsRuleList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcirrsContributorInsightsRuleList :: Lens.Lens' DescribeContributorInsightsResponse (Core.Maybe [Types.ContributorInsightsRule])
dcirrsContributorInsightsRuleList = Lens.field @"contributorInsightsRuleList"
{-# DEPRECATED dcirrsContributorInsightsRuleList "Use generic-lens or generic-optics with 'contributorInsightsRuleList' instead." #-}

-- | Current Status contributor insights.
--
-- /Note:/ Consider using 'contributorInsightsStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcirrsContributorInsightsStatus :: Lens.Lens' DescribeContributorInsightsResponse (Core.Maybe Types.ContributorInsightsStatus)
dcirrsContributorInsightsStatus = Lens.field @"contributorInsightsStatus"
{-# DEPRECATED dcirrsContributorInsightsStatus "Use generic-lens or generic-optics with 'contributorInsightsStatus' instead." #-}

-- | Returns information about the last failure that encountered.
--
-- The most common exceptions for a FAILED status are:
--
--     * LimitExceededException - Per-account Amazon CloudWatch Contributor Insights rule limit reached. Please disable Contributor Insights for other tables/indexes OR disable Contributor Insights rules before retrying.
--
--
--     * AccessDeniedException - Amazon CloudWatch Contributor Insights rules cannot be modified due to insufficient permissions.
--
--
--     * AccessDeniedException - Failed to create service-linked role for Contributor Insights due to insufficient permissions.
--
--
--     * InternalServerError - Failed to create Amazon CloudWatch Contributor Insights rules. Please retry request.
--
--
--
-- /Note:/ Consider using 'failureException' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcirrsFailureException :: Lens.Lens' DescribeContributorInsightsResponse (Core.Maybe Types.FailureException)
dcirrsFailureException = Lens.field @"failureException"
{-# DEPRECATED dcirrsFailureException "Use generic-lens or generic-optics with 'failureException' instead." #-}

-- | The name of the global secondary index being described.
--
-- /Note:/ Consider using 'indexName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcirrsIndexName :: Lens.Lens' DescribeContributorInsightsResponse (Core.Maybe Types.IndexName)
dcirrsIndexName = Lens.field @"indexName"
{-# DEPRECATED dcirrsIndexName "Use generic-lens or generic-optics with 'indexName' instead." #-}

-- | Timestamp of the last time the status was changed.
--
-- /Note:/ Consider using 'lastUpdateDateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcirrsLastUpdateDateTime :: Lens.Lens' DescribeContributorInsightsResponse (Core.Maybe Core.NominalDiffTime)
dcirrsLastUpdateDateTime = Lens.field @"lastUpdateDateTime"
{-# DEPRECATED dcirrsLastUpdateDateTime "Use generic-lens or generic-optics with 'lastUpdateDateTime' instead." #-}

-- | The name of the table being described.
--
-- /Note:/ Consider using 'tableName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcirrsTableName :: Lens.Lens' DescribeContributorInsightsResponse (Core.Maybe Types.TableName)
dcirrsTableName = Lens.field @"tableName"
{-# DEPRECATED dcirrsTableName "Use generic-lens or generic-optics with 'tableName' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcirrsResponseStatus :: Lens.Lens' DescribeContributorInsightsResponse Core.Int
dcirrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dcirrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
