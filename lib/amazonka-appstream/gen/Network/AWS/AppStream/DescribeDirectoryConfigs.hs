{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppStream.DescribeDirectoryConfigs
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list that describes one or more specified Directory Config objects for AppStream 2.0, if the names for these objects are provided. Otherwise, all Directory Config objects in the account are described. These objects include the configuration information required to join fleets and image builders to Microsoft Active Directory domains.
--
-- Although the response syntax in this topic includes the account password, this password is not returned in the actual response.
--
-- This operation returns paginated results.
module Network.AWS.AppStream.DescribeDirectoryConfigs
  ( -- * Creating a request
    DescribeDirectoryConfigs (..),
    mkDescribeDirectoryConfigs,

    -- ** Request lenses
    ddcDirectoryNames,
    ddcMaxResults,
    ddcNextToken,

    -- * Destructuring the response
    DescribeDirectoryConfigsResponse (..),
    mkDescribeDirectoryConfigsResponse,

    -- ** Response lenses
    ddcrrsDirectoryConfigs,
    ddcrrsNextToken,
    ddcrrsResponseStatus,
  )
where

import qualified Network.AWS.AppStream.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeDirectoryConfigs' smart constructor.
data DescribeDirectoryConfigs = DescribeDirectoryConfigs'
  { -- | The directory names.
    directoryNames :: Core.Maybe [Types.DirectoryName],
    -- | The maximum size of each page of results.
    maxResults :: Core.Maybe Core.Int,
    -- | The pagination token to use to retrieve the next page of results for this operation. If this value is null, it retrieves the first page.
    nextToken :: Core.Maybe Types.NextToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeDirectoryConfigs' value with any optional fields omitted.
mkDescribeDirectoryConfigs ::
  DescribeDirectoryConfigs
mkDescribeDirectoryConfigs =
  DescribeDirectoryConfigs'
    { directoryNames = Core.Nothing,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | The directory names.
--
-- /Note:/ Consider using 'directoryNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddcDirectoryNames :: Lens.Lens' DescribeDirectoryConfigs (Core.Maybe [Types.DirectoryName])
ddcDirectoryNames = Lens.field @"directoryNames"
{-# DEPRECATED ddcDirectoryNames "Use generic-lens or generic-optics with 'directoryNames' instead." #-}

-- | The maximum size of each page of results.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddcMaxResults :: Lens.Lens' DescribeDirectoryConfigs (Core.Maybe Core.Int)
ddcMaxResults = Lens.field @"maxResults"
{-# DEPRECATED ddcMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The pagination token to use to retrieve the next page of results for this operation. If this value is null, it retrieves the first page.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddcNextToken :: Lens.Lens' DescribeDirectoryConfigs (Core.Maybe Types.NextToken)
ddcNextToken = Lens.field @"nextToken"
{-# DEPRECATED ddcNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.FromJSON DescribeDirectoryConfigs where
  toJSON DescribeDirectoryConfigs {..} =
    Core.object
      ( Core.catMaybes
          [ ("DirectoryNames" Core..=) Core.<$> directoryNames,
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("NextToken" Core..=) Core.<$> nextToken
          ]
      )

instance Core.AWSRequest DescribeDirectoryConfigs where
  type Rs DescribeDirectoryConfigs = DescribeDirectoryConfigsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "PhotonAdminProxyService.DescribeDirectoryConfigs"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeDirectoryConfigsResponse'
            Core.<$> (x Core..:? "DirectoryConfigs")
            Core.<*> (x Core..:? "NextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager DescribeDirectoryConfigs where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"directoryConfigs" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkDescribeDirectoryConfigsResponse' smart constructor.
data DescribeDirectoryConfigsResponse = DescribeDirectoryConfigsResponse'
  { -- | Information about the directory configurations. Note that although the response syntax in this topic includes the account password, this password is not returned in the actual response.
    directoryConfigs :: Core.Maybe [Types.DirectoryConfig],
    -- | The pagination token to use to retrieve the next page of results for this operation. If there are no more pages, this value is null.
    nextToken :: Core.Maybe Types.NextToken,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribeDirectoryConfigsResponse' value with any optional fields omitted.
mkDescribeDirectoryConfigsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeDirectoryConfigsResponse
mkDescribeDirectoryConfigsResponse responseStatus =
  DescribeDirectoryConfigsResponse'
    { directoryConfigs =
        Core.Nothing,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | Information about the directory configurations. Note that although the response syntax in this topic includes the account password, this password is not returned in the actual response.
--
-- /Note:/ Consider using 'directoryConfigs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddcrrsDirectoryConfigs :: Lens.Lens' DescribeDirectoryConfigsResponse (Core.Maybe [Types.DirectoryConfig])
ddcrrsDirectoryConfigs = Lens.field @"directoryConfigs"
{-# DEPRECATED ddcrrsDirectoryConfigs "Use generic-lens or generic-optics with 'directoryConfigs' instead." #-}

-- | The pagination token to use to retrieve the next page of results for this operation. If there are no more pages, this value is null.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddcrrsNextToken :: Lens.Lens' DescribeDirectoryConfigsResponse (Core.Maybe Types.NextToken)
ddcrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED ddcrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddcrrsResponseStatus :: Lens.Lens' DescribeDirectoryConfigsResponse Core.Int
ddcrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ddcrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
