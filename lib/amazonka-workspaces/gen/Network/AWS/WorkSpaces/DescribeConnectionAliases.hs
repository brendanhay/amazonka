{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.DescribeConnectionAliases
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list that describes the connection aliases used for cross-Region redirection. For more information, see <https://docs.aws.amazon.com/workspaces/latest/adminguide/cross-region-redirection.html Cross-Region Redirection for Amazon WorkSpaces> .
module Network.AWS.WorkSpaces.DescribeConnectionAliases
  ( -- * Creating a request
    DescribeConnectionAliases (..),
    mkDescribeConnectionAliases,

    -- ** Request lenses
    dcaAliasIds,
    dcaLimit,
    dcaNextToken,
    dcaResourceId,

    -- * Destructuring the response
    DescribeConnectionAliasesResponse (..),
    mkDescribeConnectionAliasesResponse,

    -- ** Response lenses
    dcargrsConnectionAliases,
    dcargrsNextToken,
    dcargrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WorkSpaces.Types as Types

-- | /See:/ 'mkDescribeConnectionAliases' smart constructor.
data DescribeConnectionAliases = DescribeConnectionAliases'
  { -- | The identifiers of the connection aliases to describe.
    aliasIds :: Core.Maybe (Core.NonEmpty Types.ConnectionAliasId),
    -- | The maximum number of connection aliases to return.
    limit :: Core.Maybe Core.Natural,
    -- | If you received a @NextToken@ from a previous call that was paginated, provide this token to receive the next set of results.
    nextToken :: Core.Maybe Types.PaginationToken,
    -- | The identifier of the directory associated with the connection alias.
    resourceId :: Core.Maybe Types.NonEmptyString
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeConnectionAliases' value with any optional fields omitted.
mkDescribeConnectionAliases ::
  DescribeConnectionAliases
mkDescribeConnectionAliases =
  DescribeConnectionAliases'
    { aliasIds = Core.Nothing,
      limit = Core.Nothing,
      nextToken = Core.Nothing,
      resourceId = Core.Nothing
    }

-- | The identifiers of the connection aliases to describe.
--
-- /Note:/ Consider using 'aliasIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcaAliasIds :: Lens.Lens' DescribeConnectionAliases (Core.Maybe (Core.NonEmpty Types.ConnectionAliasId))
dcaAliasIds = Lens.field @"aliasIds"
{-# DEPRECATED dcaAliasIds "Use generic-lens or generic-optics with 'aliasIds' instead." #-}

-- | The maximum number of connection aliases to return.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcaLimit :: Lens.Lens' DescribeConnectionAliases (Core.Maybe Core.Natural)
dcaLimit = Lens.field @"limit"
{-# DEPRECATED dcaLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

-- | If you received a @NextToken@ from a previous call that was paginated, provide this token to receive the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcaNextToken :: Lens.Lens' DescribeConnectionAliases (Core.Maybe Types.PaginationToken)
dcaNextToken = Lens.field @"nextToken"
{-# DEPRECATED dcaNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The identifier of the directory associated with the connection alias.
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcaResourceId :: Lens.Lens' DescribeConnectionAliases (Core.Maybe Types.NonEmptyString)
dcaResourceId = Lens.field @"resourceId"
{-# DEPRECATED dcaResourceId "Use generic-lens or generic-optics with 'resourceId' instead." #-}

instance Core.FromJSON DescribeConnectionAliases where
  toJSON DescribeConnectionAliases {..} =
    Core.object
      ( Core.catMaybes
          [ ("AliasIds" Core..=) Core.<$> aliasIds,
            ("Limit" Core..=) Core.<$> limit,
            ("NextToken" Core..=) Core.<$> nextToken,
            ("ResourceId" Core..=) Core.<$> resourceId
          ]
      )

instance Core.AWSRequest DescribeConnectionAliases where
  type
    Rs DescribeConnectionAliases =
      DescribeConnectionAliasesResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "WorkspacesService.DescribeConnectionAliases")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeConnectionAliasesResponse'
            Core.<$> (x Core..:? "ConnectionAliases")
            Core.<*> (x Core..:? "NextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDescribeConnectionAliasesResponse' smart constructor.
data DescribeConnectionAliasesResponse = DescribeConnectionAliasesResponse'
  { -- | Information about the specified connection aliases.
    connectionAliases :: Core.Maybe (Core.NonEmpty Types.ConnectionAlias),
    -- | The token to use to retrieve the next set of results, or null if no more results are available.
    nextToken :: Core.Maybe Types.PaginationToken,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeConnectionAliasesResponse' value with any optional fields omitted.
mkDescribeConnectionAliasesResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeConnectionAliasesResponse
mkDescribeConnectionAliasesResponse responseStatus =
  DescribeConnectionAliasesResponse'
    { connectionAliases =
        Core.Nothing,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | Information about the specified connection aliases.
--
-- /Note:/ Consider using 'connectionAliases' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcargrsConnectionAliases :: Lens.Lens' DescribeConnectionAliasesResponse (Core.Maybe (Core.NonEmpty Types.ConnectionAlias))
dcargrsConnectionAliases = Lens.field @"connectionAliases"
{-# DEPRECATED dcargrsConnectionAliases "Use generic-lens or generic-optics with 'connectionAliases' instead." #-}

-- | The token to use to retrieve the next set of results, or null if no more results are available.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcargrsNextToken :: Lens.Lens' DescribeConnectionAliasesResponse (Core.Maybe Types.PaginationToken)
dcargrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED dcargrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcargrsResponseStatus :: Lens.Lens' DescribeConnectionAliasesResponse Core.Int
dcargrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dcargrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
