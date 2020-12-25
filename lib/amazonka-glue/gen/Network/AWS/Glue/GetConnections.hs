{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.GetConnections
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list of connection definitions from the Data Catalog.
--
-- This operation returns paginated results.
module Network.AWS.Glue.GetConnections
  ( -- * Creating a request
    GetConnections (..),
    mkGetConnections,

    -- ** Request lenses
    gcsCatalogId,
    gcsFilter,
    gcsHidePassword,
    gcsMaxResults,
    gcsNextToken,

    -- * Destructuring the response
    GetConnectionsResponse (..),
    mkGetConnectionsResponse,

    -- ** Response lenses
    gcrhrsConnectionList,
    gcrhrsNextToken,
    gcrhrsResponseStatus,
  )
where

import qualified Network.AWS.Glue.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetConnections' smart constructor.
data GetConnections = GetConnections'
  { -- | The ID of the Data Catalog in which the connections reside. If none is provided, the AWS account ID is used by default.
    catalogId :: Core.Maybe Types.CatalogId,
    -- | A filter that controls which connections are returned.
    filter :: Core.Maybe Types.GetConnectionsFilter,
    -- | Allows you to retrieve the connection metadata without returning the password. For instance, the AWS Glue console uses this flag to retrieve the connection, and does not display the password. Set this parameter when the caller might not have permission to use the AWS KMS key to decrypt the password, but it does have permission to access the rest of the connection properties.
    hidePassword :: Core.Maybe Core.Bool,
    -- | The maximum number of connections to return in one response.
    maxResults :: Core.Maybe Core.Natural,
    -- | A continuation token, if this is a continuation call.
    nextToken :: Core.Maybe Types.Token
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetConnections' value with any optional fields omitted.
mkGetConnections ::
  GetConnections
mkGetConnections =
  GetConnections'
    { catalogId = Core.Nothing,
      filter = Core.Nothing,
      hidePassword = Core.Nothing,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | The ID of the Data Catalog in which the connections reside. If none is provided, the AWS account ID is used by default.
--
-- /Note:/ Consider using 'catalogId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcsCatalogId :: Lens.Lens' GetConnections (Core.Maybe Types.CatalogId)
gcsCatalogId = Lens.field @"catalogId"
{-# DEPRECATED gcsCatalogId "Use generic-lens or generic-optics with 'catalogId' instead." #-}

-- | A filter that controls which connections are returned.
--
-- /Note:/ Consider using 'filter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcsFilter :: Lens.Lens' GetConnections (Core.Maybe Types.GetConnectionsFilter)
gcsFilter = Lens.field @"filter"
{-# DEPRECATED gcsFilter "Use generic-lens or generic-optics with 'filter' instead." #-}

-- | Allows you to retrieve the connection metadata without returning the password. For instance, the AWS Glue console uses this flag to retrieve the connection, and does not display the password. Set this parameter when the caller might not have permission to use the AWS KMS key to decrypt the password, but it does have permission to access the rest of the connection properties.
--
-- /Note:/ Consider using 'hidePassword' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcsHidePassword :: Lens.Lens' GetConnections (Core.Maybe Core.Bool)
gcsHidePassword = Lens.field @"hidePassword"
{-# DEPRECATED gcsHidePassword "Use generic-lens or generic-optics with 'hidePassword' instead." #-}

-- | The maximum number of connections to return in one response.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcsMaxResults :: Lens.Lens' GetConnections (Core.Maybe Core.Natural)
gcsMaxResults = Lens.field @"maxResults"
{-# DEPRECATED gcsMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | A continuation token, if this is a continuation call.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcsNextToken :: Lens.Lens' GetConnections (Core.Maybe Types.Token)
gcsNextToken = Lens.field @"nextToken"
{-# DEPRECATED gcsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.FromJSON GetConnections where
  toJSON GetConnections {..} =
    Core.object
      ( Core.catMaybes
          [ ("CatalogId" Core..=) Core.<$> catalogId,
            ("Filter" Core..=) Core.<$> filter,
            ("HidePassword" Core..=) Core.<$> hidePassword,
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("NextToken" Core..=) Core.<$> nextToken
          ]
      )

instance Core.AWSRequest GetConnections where
  type Rs GetConnections = GetConnectionsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AWSGlue.GetConnections")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetConnectionsResponse'
            Core.<$> (x Core..:? "ConnectionList")
            Core.<*> (x Core..:? "NextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager GetConnections where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"connectionList" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkGetConnectionsResponse' smart constructor.
data GetConnectionsResponse = GetConnectionsResponse'
  { -- | A list of requested connection definitions.
    connectionList :: Core.Maybe [Types.Connection],
    -- | A continuation token, if the list of connections returned does not include the last of the filtered connections.
    nextToken :: Core.Maybe Types.Token,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'GetConnectionsResponse' value with any optional fields omitted.
mkGetConnectionsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetConnectionsResponse
mkGetConnectionsResponse responseStatus =
  GetConnectionsResponse'
    { connectionList = Core.Nothing,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | A list of requested connection definitions.
--
-- /Note:/ Consider using 'connectionList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcrhrsConnectionList :: Lens.Lens' GetConnectionsResponse (Core.Maybe [Types.Connection])
gcrhrsConnectionList = Lens.field @"connectionList"
{-# DEPRECATED gcrhrsConnectionList "Use generic-lens or generic-optics with 'connectionList' instead." #-}

-- | A continuation token, if the list of connections returned does not include the last of the filtered connections.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcrhrsNextToken :: Lens.Lens' GetConnectionsResponse (Core.Maybe Types.Token)
gcrhrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED gcrhrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcrhrsResponseStatus :: Lens.Lens' GetConnectionsResponse Core.Int
gcrhrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gcrhrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
