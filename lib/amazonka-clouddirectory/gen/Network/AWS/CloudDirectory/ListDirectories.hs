{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.ListDirectories
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists directories created within an account.
--
-- This operation returns paginated results.
module Network.AWS.CloudDirectory.ListDirectories
  ( -- * Creating a request
    ListDirectories (..),
    mkListDirectories,

    -- ** Request lenses
    ldMaxResults,
    ldNextToken,
    ldState,

    -- * Destructuring the response
    ListDirectoriesResponse (..),
    mkListDirectoriesResponse,

    -- ** Response lenses
    ldrrsDirectories,
    ldrrsNextToken,
    ldrrsResponseStatus,
  )
where

import qualified Network.AWS.CloudDirectory.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListDirectories' smart constructor.
data ListDirectories = ListDirectories'
  { -- | The maximum number of results to retrieve.
    maxResults :: Core.Maybe Core.Natural,
    -- | The pagination token.
    nextToken :: Core.Maybe Types.NextToken,
    -- | The state of the directories in the list. Can be either Enabled, Disabled, or Deleted.
    state :: Core.Maybe Types.DirectoryState
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListDirectories' value with any optional fields omitted.
mkListDirectories ::
  ListDirectories
mkListDirectories =
  ListDirectories'
    { maxResults = Core.Nothing,
      nextToken = Core.Nothing,
      state = Core.Nothing
    }

-- | The maximum number of results to retrieve.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldMaxResults :: Lens.Lens' ListDirectories (Core.Maybe Core.Natural)
ldMaxResults = Lens.field @"maxResults"
{-# DEPRECATED ldMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The pagination token.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldNextToken :: Lens.Lens' ListDirectories (Core.Maybe Types.NextToken)
ldNextToken = Lens.field @"nextToken"
{-# DEPRECATED ldNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The state of the directories in the list. Can be either Enabled, Disabled, or Deleted.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldState :: Lens.Lens' ListDirectories (Core.Maybe Types.DirectoryState)
ldState = Lens.field @"state"
{-# DEPRECATED ldState "Use generic-lens or generic-optics with 'state' instead." #-}

instance Core.FromJSON ListDirectories where
  toJSON ListDirectories {..} =
    Core.object
      ( Core.catMaybes
          [ ("MaxResults" Core..=) Core.<$> maxResults,
            ("NextToken" Core..=) Core.<$> nextToken,
            ("state" Core..=) Core.<$> state
          ]
      )

instance Core.AWSRequest ListDirectories where
  type Rs ListDirectories = ListDirectoriesResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath =
          Core.rawPath "/amazonclouddirectory/2017-01-11/directory/list",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListDirectoriesResponse'
            Core.<$> (x Core..:? "Directories" Core..!= Core.mempty)
            Core.<*> (x Core..:? "NextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListDirectories where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop (rs Lens.^. Lens.field @"directories") = Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkListDirectoriesResponse' smart constructor.
data ListDirectoriesResponse = ListDirectoriesResponse'
  { -- | Lists all directories that are associated with your account in pagination fashion.
    directories :: [Types.Directory],
    -- | The pagination token.
    nextToken :: Core.Maybe Types.NextToken,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ListDirectoriesResponse' value with any optional fields omitted.
mkListDirectoriesResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListDirectoriesResponse
mkListDirectoriesResponse responseStatus =
  ListDirectoriesResponse'
    { directories = Core.mempty,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | Lists all directories that are associated with your account in pagination fashion.
--
-- /Note:/ Consider using 'directories' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldrrsDirectories :: Lens.Lens' ListDirectoriesResponse [Types.Directory]
ldrrsDirectories = Lens.field @"directories"
{-# DEPRECATED ldrrsDirectories "Use generic-lens or generic-optics with 'directories' instead." #-}

-- | The pagination token.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldrrsNextToken :: Lens.Lens' ListDirectoriesResponse (Core.Maybe Types.NextToken)
ldrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED ldrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldrrsResponseStatus :: Lens.Lens' ListDirectoriesResponse Core.Int
ldrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ldrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
