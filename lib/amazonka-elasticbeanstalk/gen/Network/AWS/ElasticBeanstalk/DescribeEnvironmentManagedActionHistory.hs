{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.DescribeEnvironmentManagedActionHistory
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists an environment's completed and failed managed actions.
--
-- This operation returns paginated results.
module Network.AWS.ElasticBeanstalk.DescribeEnvironmentManagedActionHistory
  ( -- * Creating a request
    DescribeEnvironmentManagedActionHistory (..),
    mkDescribeEnvironmentManagedActionHistory,

    -- ** Request lenses
    demahEnvironmentId,
    demahEnvironmentName,
    demahMaxItems,
    demahNextToken,

    -- * Destructuring the response
    DescribeEnvironmentManagedActionHistoryResponse (..),
    mkDescribeEnvironmentManagedActionHistoryResponse,

    -- ** Response lenses
    demahrrsManagedActionHistoryItems,
    demahrrsNextToken,
    demahrrsResponseStatus,
  )
where

import qualified Network.AWS.ElasticBeanstalk.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Request to list completed and failed managed actions.
--
-- /See:/ 'mkDescribeEnvironmentManagedActionHistory' smart constructor.
data DescribeEnvironmentManagedActionHistory = DescribeEnvironmentManagedActionHistory'
  { -- | The environment ID of the target environment.
    environmentId :: Core.Maybe Types.EnvironmentId,
    -- | The name of the target environment.
    environmentName :: Core.Maybe Types.EnvironmentName,
    -- | The maximum number of items to return for a single request.
    maxItems :: Core.Maybe Core.Natural,
    -- | The pagination token returned by a previous request.
    nextToken :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeEnvironmentManagedActionHistory' value with any optional fields omitted.
mkDescribeEnvironmentManagedActionHistory ::
  DescribeEnvironmentManagedActionHistory
mkDescribeEnvironmentManagedActionHistory =
  DescribeEnvironmentManagedActionHistory'
    { environmentId =
        Core.Nothing,
      environmentName = Core.Nothing,
      maxItems = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | The environment ID of the target environment.
--
-- /Note:/ Consider using 'environmentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
demahEnvironmentId :: Lens.Lens' DescribeEnvironmentManagedActionHistory (Core.Maybe Types.EnvironmentId)
demahEnvironmentId = Lens.field @"environmentId"
{-# DEPRECATED demahEnvironmentId "Use generic-lens or generic-optics with 'environmentId' instead." #-}

-- | The name of the target environment.
--
-- /Note:/ Consider using 'environmentName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
demahEnvironmentName :: Lens.Lens' DescribeEnvironmentManagedActionHistory (Core.Maybe Types.EnvironmentName)
demahEnvironmentName = Lens.field @"environmentName"
{-# DEPRECATED demahEnvironmentName "Use generic-lens or generic-optics with 'environmentName' instead." #-}

-- | The maximum number of items to return for a single request.
--
-- /Note:/ Consider using 'maxItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
demahMaxItems :: Lens.Lens' DescribeEnvironmentManagedActionHistory (Core.Maybe Core.Natural)
demahMaxItems = Lens.field @"maxItems"
{-# DEPRECATED demahMaxItems "Use generic-lens or generic-optics with 'maxItems' instead." #-}

-- | The pagination token returned by a previous request.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
demahNextToken :: Lens.Lens' DescribeEnvironmentManagedActionHistory (Core.Maybe Types.String)
demahNextToken = Lens.field @"nextToken"
{-# DEPRECATED demahNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.AWSRequest DescribeEnvironmentManagedActionHistory where
  type
    Rs DescribeEnvironmentManagedActionHistory =
      DescribeEnvironmentManagedActionHistoryResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "Content-Type",
              "application/x-www-form-urlencoded; charset=utf-8"
            ),
        Core._rqBody =
          Core.toFormBody
            ( Core.pure ("Action", "DescribeEnvironmentManagedActionHistory")
                Core.<> (Core.pure ("Version", "2010-12-01"))
                Core.<> (Core.toQueryValue "EnvironmentId" Core.<$> environmentId)
                Core.<> (Core.toQueryValue "EnvironmentName" Core.<$> environmentName)
                Core.<> (Core.toQueryValue "MaxItems" Core.<$> maxItems)
                Core.<> (Core.toQueryValue "NextToken" Core.<$> nextToken)
            )
      }
  response =
    Response.receiveXMLWrapper
      "DescribeEnvironmentManagedActionHistoryResult"
      ( \s h x ->
          DescribeEnvironmentManagedActionHistoryResponse'
            Core.<$> ( x Core..@? "ManagedActionHistoryItems"
                         Core..<@> Core.parseXMLNonEmpty "member"
                     )
            Core.<*> (x Core..@? "NextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager DescribeEnvironmentManagedActionHistory where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop
        ( rs
            Lens.^? Lens.field @"managedActionHistoryItems" Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | A result message containing a list of completed and failed managed actions.
--
-- /See:/ 'mkDescribeEnvironmentManagedActionHistoryResponse' smart constructor.
data DescribeEnvironmentManagedActionHistoryResponse = DescribeEnvironmentManagedActionHistoryResponse'
  { -- | A list of completed and failed managed actions.
    managedActionHistoryItems :: Core.Maybe (Core.NonEmpty Types.ManagedActionHistoryItem),
    -- | A pagination token that you pass to 'DescribeEnvironmentManagedActionHistory' to get the next page of results.
    nextToken :: Core.Maybe Types.String,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribeEnvironmentManagedActionHistoryResponse' value with any optional fields omitted.
mkDescribeEnvironmentManagedActionHistoryResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeEnvironmentManagedActionHistoryResponse
mkDescribeEnvironmentManagedActionHistoryResponse responseStatus =
  DescribeEnvironmentManagedActionHistoryResponse'
    { managedActionHistoryItems =
        Core.Nothing,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | A list of completed and failed managed actions.
--
-- /Note:/ Consider using 'managedActionHistoryItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
demahrrsManagedActionHistoryItems :: Lens.Lens' DescribeEnvironmentManagedActionHistoryResponse (Core.Maybe (Core.NonEmpty Types.ManagedActionHistoryItem))
demahrrsManagedActionHistoryItems = Lens.field @"managedActionHistoryItems"
{-# DEPRECATED demahrrsManagedActionHistoryItems "Use generic-lens or generic-optics with 'managedActionHistoryItems' instead." #-}

-- | A pagination token that you pass to 'DescribeEnvironmentManagedActionHistory' to get the next page of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
demahrrsNextToken :: Lens.Lens' DescribeEnvironmentManagedActionHistoryResponse (Core.Maybe Types.String)
demahrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED demahrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
demahrrsResponseStatus :: Lens.Lens' DescribeEnvironmentManagedActionHistoryResponse Core.Int
demahrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED demahrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
