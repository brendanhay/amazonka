{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.ListObjectParentPaths
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves all available parent paths for any object type such as node, leaf node, policy node, and index node objects. For more information about objects, see <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/key_concepts_directorystructure.html Directory Structure> .
--
-- Use this API to evaluate all parents for an object. The call returns all objects from the root of the directory up to the requested object. The API returns the number of paths based on user-defined @MaxResults@ , in case there are multiple paths to the parent. The order of the paths and nodes returned is consistent among multiple API calls unless the objects are deleted or moved. Paths not leading to the directory root are ignored from the target object.
--
-- This operation returns paginated results.
module Network.AWS.CloudDirectory.ListObjectParentPaths
  ( -- * Creating a request
    ListObjectParentPaths (..),
    mkListObjectParentPaths,

    -- ** Request lenses
    loppDirectoryArn,
    loppObjectReference,
    loppMaxResults,
    loppNextToken,

    -- * Destructuring the response
    ListObjectParentPathsResponse (..),
    mkListObjectParentPathsResponse,

    -- ** Response lenses
    lopprrsNextToken,
    lopprrsPathToObjectIdentifiersList,
    lopprrsResponseStatus,
  )
where

import qualified Network.AWS.CloudDirectory.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListObjectParentPaths' smart constructor.
data ListObjectParentPaths = ListObjectParentPaths'
  { -- | The ARN of the directory to which the parent path applies.
    directoryArn :: Types.DirectoryArn,
    -- | The reference that identifies the object whose parent paths are listed.
    objectReference :: Types.ObjectReference,
    -- | The maximum number of items to be retrieved in a single call. This is an approximate number.
    maxResults :: Core.Maybe Core.Natural,
    -- | The pagination token.
    nextToken :: Core.Maybe Types.NextToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListObjectParentPaths' value with any optional fields omitted.
mkListObjectParentPaths ::
  -- | 'directoryArn'
  Types.DirectoryArn ->
  -- | 'objectReference'
  Types.ObjectReference ->
  ListObjectParentPaths
mkListObjectParentPaths directoryArn objectReference =
  ListObjectParentPaths'
    { directoryArn,
      objectReference,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | The ARN of the directory to which the parent path applies.
--
-- /Note:/ Consider using 'directoryArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
loppDirectoryArn :: Lens.Lens' ListObjectParentPaths Types.DirectoryArn
loppDirectoryArn = Lens.field @"directoryArn"
{-# DEPRECATED loppDirectoryArn "Use generic-lens or generic-optics with 'directoryArn' instead." #-}

-- | The reference that identifies the object whose parent paths are listed.
--
-- /Note:/ Consider using 'objectReference' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
loppObjectReference :: Lens.Lens' ListObjectParentPaths Types.ObjectReference
loppObjectReference = Lens.field @"objectReference"
{-# DEPRECATED loppObjectReference "Use generic-lens or generic-optics with 'objectReference' instead." #-}

-- | The maximum number of items to be retrieved in a single call. This is an approximate number.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
loppMaxResults :: Lens.Lens' ListObjectParentPaths (Core.Maybe Core.Natural)
loppMaxResults = Lens.field @"maxResults"
{-# DEPRECATED loppMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The pagination token.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
loppNextToken :: Lens.Lens' ListObjectParentPaths (Core.Maybe Types.NextToken)
loppNextToken = Lens.field @"nextToken"
{-# DEPRECATED loppNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.FromJSON ListObjectParentPaths where
  toJSON ListObjectParentPaths {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("ObjectReference" Core..= objectReference),
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("NextToken" Core..=) Core.<$> nextToken
          ]
      )

instance Core.AWSRequest ListObjectParentPaths where
  type Rs ListObjectParentPaths = ListObjectParentPathsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath =
          Core.rawPath "/amazonclouddirectory/2017-01-11/object/parentpaths",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.toHeaders "x-amz-data-partition" directoryArn,
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListObjectParentPathsResponse'
            Core.<$> (x Core..:? "NextToken")
            Core.<*> (x Core..:? "PathToObjectIdentifiersList")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListObjectParentPaths where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop
        ( rs
            Lens.^? Lens.field @"pathToObjectIdentifiersList" Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkListObjectParentPathsResponse' smart constructor.
data ListObjectParentPathsResponse = ListObjectParentPathsResponse'
  { -- | The pagination token.
    nextToken :: Core.Maybe Types.NextToken,
    -- | Returns the path to the @ObjectIdentifiers@ that are associated with the directory.
    pathToObjectIdentifiersList :: Core.Maybe [Types.PathToObjectIdentifiers],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListObjectParentPathsResponse' value with any optional fields omitted.
mkListObjectParentPathsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListObjectParentPathsResponse
mkListObjectParentPathsResponse responseStatus =
  ListObjectParentPathsResponse'
    { nextToken = Core.Nothing,
      pathToObjectIdentifiersList = Core.Nothing,
      responseStatus
    }

-- | The pagination token.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lopprrsNextToken :: Lens.Lens' ListObjectParentPathsResponse (Core.Maybe Types.NextToken)
lopprrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED lopprrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Returns the path to the @ObjectIdentifiers@ that are associated with the directory.
--
-- /Note:/ Consider using 'pathToObjectIdentifiersList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lopprrsPathToObjectIdentifiersList :: Lens.Lens' ListObjectParentPathsResponse (Core.Maybe [Types.PathToObjectIdentifiers])
lopprrsPathToObjectIdentifiersList = Lens.field @"pathToObjectIdentifiersList"
{-# DEPRECATED lopprrsPathToObjectIdentifiersList "Use generic-lens or generic-optics with 'pathToObjectIdentifiersList' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lopprrsResponseStatus :: Lens.Lens' ListObjectParentPathsResponse Core.Int
lopprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED lopprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
