{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.GetDifferences
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the differences in a valid commit specifier (such as a branch, tag, HEAD, commit ID, or other fully qualified reference). Results can be limited to a specified path.
--
-- This operation returns paginated results.
module Network.AWS.CodeCommit.GetDifferences
  ( -- * Creating a request
    GetDifferences (..),
    mkGetDifferences,

    -- ** Request lenses
    gdRepositoryName,
    gdAfterCommitSpecifier,
    gdMaxResults,
    gdNextToken,
    gdAfterPath,
    gdBeforeCommitSpecifier,
    gdBeforePath,

    -- * Destructuring the response
    GetDifferencesResponse (..),
    mkGetDifferencesResponse,

    -- ** Response lenses
    gdrrsNextToken,
    gdrrsDifferences,
    gdrrsResponseStatus,
  )
where

import qualified Network.AWS.CodeCommit.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetDifferences' smart constructor.
data GetDifferences = GetDifferences'
  { -- | The name of the repository where you want to get differences.
    repositoryName :: Types.RepositoryName,
    -- | The branch, tag, HEAD, or other fully qualified reference used to identify a commit.
    afterCommitSpecifier :: Types.AfterCommitSpecifier,
    -- | A non-zero, non-negative integer used to limit the number of returned results.
    maxResults :: Core.Maybe Core.Int,
    -- | An enumeration token that, when provided in a request, returns the next batch of the results.
    nextToken :: Core.Maybe Types.NextToken,
    -- | The file path in which to check differences. Limits the results to this path. Can also be used to specify the changed name of a directory or folder, if it has changed. If not specified, differences are shown for all paths.
    afterPath :: Core.Maybe Types.Path,
    -- | The branch, tag, HEAD, or other fully qualified reference used to identify a commit (for example, the full commit ID). Optional. If not specified, all changes before the @afterCommitSpecifier@ value are shown. If you do not use @beforeCommitSpecifier@ in your request, consider limiting the results with @maxResults@ .
    beforeCommitSpecifier :: Core.Maybe Types.BeforeCommitSpecifier,
    -- | The file path in which to check for differences. Limits the results to this path. Can also be used to specify the previous name of a directory or folder. If @beforePath@ and @afterPath@ are not specified, differences are shown for all paths.
    beforePath :: Core.Maybe Types.Path
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetDifferences' value with any optional fields omitted.
mkGetDifferences ::
  -- | 'repositoryName'
  Types.RepositoryName ->
  -- | 'afterCommitSpecifier'
  Types.AfterCommitSpecifier ->
  GetDifferences
mkGetDifferences repositoryName afterCommitSpecifier =
  GetDifferences'
    { repositoryName,
      afterCommitSpecifier,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing,
      afterPath = Core.Nothing,
      beforeCommitSpecifier = Core.Nothing,
      beforePath = Core.Nothing
    }

-- | The name of the repository where you want to get differences.
--
-- /Note:/ Consider using 'repositoryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdRepositoryName :: Lens.Lens' GetDifferences Types.RepositoryName
gdRepositoryName = Lens.field @"repositoryName"
{-# DEPRECATED gdRepositoryName "Use generic-lens or generic-optics with 'repositoryName' instead." #-}

-- | The branch, tag, HEAD, or other fully qualified reference used to identify a commit.
--
-- /Note:/ Consider using 'afterCommitSpecifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdAfterCommitSpecifier :: Lens.Lens' GetDifferences Types.AfterCommitSpecifier
gdAfterCommitSpecifier = Lens.field @"afterCommitSpecifier"
{-# DEPRECATED gdAfterCommitSpecifier "Use generic-lens or generic-optics with 'afterCommitSpecifier' instead." #-}

-- | A non-zero, non-negative integer used to limit the number of returned results.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdMaxResults :: Lens.Lens' GetDifferences (Core.Maybe Core.Int)
gdMaxResults = Lens.field @"maxResults"
{-# DEPRECATED gdMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | An enumeration token that, when provided in a request, returns the next batch of the results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdNextToken :: Lens.Lens' GetDifferences (Core.Maybe Types.NextToken)
gdNextToken = Lens.field @"nextToken"
{-# DEPRECATED gdNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The file path in which to check differences. Limits the results to this path. Can also be used to specify the changed name of a directory or folder, if it has changed. If not specified, differences are shown for all paths.
--
-- /Note:/ Consider using 'afterPath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdAfterPath :: Lens.Lens' GetDifferences (Core.Maybe Types.Path)
gdAfterPath = Lens.field @"afterPath"
{-# DEPRECATED gdAfterPath "Use generic-lens or generic-optics with 'afterPath' instead." #-}

-- | The branch, tag, HEAD, or other fully qualified reference used to identify a commit (for example, the full commit ID). Optional. If not specified, all changes before the @afterCommitSpecifier@ value are shown. If you do not use @beforeCommitSpecifier@ in your request, consider limiting the results with @maxResults@ .
--
-- /Note:/ Consider using 'beforeCommitSpecifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdBeforeCommitSpecifier :: Lens.Lens' GetDifferences (Core.Maybe Types.BeforeCommitSpecifier)
gdBeforeCommitSpecifier = Lens.field @"beforeCommitSpecifier"
{-# DEPRECATED gdBeforeCommitSpecifier "Use generic-lens or generic-optics with 'beforeCommitSpecifier' instead." #-}

-- | The file path in which to check for differences. Limits the results to this path. Can also be used to specify the previous name of a directory or folder. If @beforePath@ and @afterPath@ are not specified, differences are shown for all paths.
--
-- /Note:/ Consider using 'beforePath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdBeforePath :: Lens.Lens' GetDifferences (Core.Maybe Types.Path)
gdBeforePath = Lens.field @"beforePath"
{-# DEPRECATED gdBeforePath "Use generic-lens or generic-optics with 'beforePath' instead." #-}

instance Core.FromJSON GetDifferences where
  toJSON GetDifferences {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("repositoryName" Core..= repositoryName),
            Core.Just ("afterCommitSpecifier" Core..= afterCommitSpecifier),
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("NextToken" Core..=) Core.<$> nextToken,
            ("afterPath" Core..=) Core.<$> afterPath,
            ("beforeCommitSpecifier" Core..=) Core.<$> beforeCommitSpecifier,
            ("beforePath" Core..=) Core.<$> beforePath
          ]
      )

instance Core.AWSRequest GetDifferences where
  type Rs GetDifferences = GetDifferencesResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "CodeCommit_20150413.GetDifferences")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetDifferencesResponse'
            Core.<$> (x Core..:? "NextToken")
            Core.<*> (x Core..:? "differences")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager GetDifferences where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"differences" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkGetDifferencesResponse' smart constructor.
data GetDifferencesResponse = GetDifferencesResponse'
  { -- | An enumeration token that can be used in a request to return the next batch of the results.
    nextToken :: Core.Maybe Types.NextToken,
    -- | A data type object that contains information about the differences, including whether the difference is added, modified, or deleted (A, D, M).
    differences :: Core.Maybe [Types.Difference],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetDifferencesResponse' value with any optional fields omitted.
mkGetDifferencesResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetDifferencesResponse
mkGetDifferencesResponse responseStatus =
  GetDifferencesResponse'
    { nextToken = Core.Nothing,
      differences = Core.Nothing,
      responseStatus
    }

-- | An enumeration token that can be used in a request to return the next batch of the results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdrrsNextToken :: Lens.Lens' GetDifferencesResponse (Core.Maybe Types.NextToken)
gdrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED gdrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | A data type object that contains information about the differences, including whether the difference is added, modified, or deleted (A, D, M).
--
-- /Note:/ Consider using 'differences' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdrrsDifferences :: Lens.Lens' GetDifferencesResponse (Core.Maybe [Types.Difference])
gdrrsDifferences = Lens.field @"differences"
{-# DEPRECATED gdrrsDifferences "Use generic-lens or generic-optics with 'differences' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdrrsResponseStatus :: Lens.Lens' GetDifferencesResponse Core.Int
gdrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gdrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
