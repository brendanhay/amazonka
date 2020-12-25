{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.ListObjectChildren
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a paginated list of child objects that are associated with a given object.
module Network.AWS.CloudDirectory.ListObjectChildren
  ( -- * Creating a request
    ListObjectChildren (..),
    mkListObjectChildren,

    -- ** Request lenses
    locDirectoryArn,
    locObjectReference,
    locConsistencyLevel,
    locMaxResults,
    locNextToken,

    -- * Destructuring the response
    ListObjectChildrenResponse (..),
    mkListObjectChildrenResponse,

    -- ** Response lenses
    locrrsChildren,
    locrrsNextToken,
    locrrsResponseStatus,
  )
where

import qualified Network.AWS.CloudDirectory.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListObjectChildren' smart constructor.
data ListObjectChildren = ListObjectChildren'
  { -- | The Amazon Resource Name (ARN) that is associated with the 'Directory' where the object resides. For more information, see 'arns' .
    directoryArn :: Types.Arn,
    -- | The reference that identifies the object for which child objects are being listed.
    objectReference :: Types.ObjectReference,
    -- | Represents the manner and timing in which the successful write or update of an object is reflected in a subsequent read operation of that same object.
    consistencyLevel :: Core.Maybe Types.ConsistencyLevel,
    -- | The maximum number of items to be retrieved in a single call. This is an approximate number.
    maxResults :: Core.Maybe Core.Natural,
    -- | The pagination token.
    nextToken :: Core.Maybe Types.NextToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListObjectChildren' value with any optional fields omitted.
mkListObjectChildren ::
  -- | 'directoryArn'
  Types.Arn ->
  -- | 'objectReference'
  Types.ObjectReference ->
  ListObjectChildren
mkListObjectChildren directoryArn objectReference =
  ListObjectChildren'
    { directoryArn,
      objectReference,
      consistencyLevel = Core.Nothing,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | The Amazon Resource Name (ARN) that is associated with the 'Directory' where the object resides. For more information, see 'arns' .
--
-- /Note:/ Consider using 'directoryArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
locDirectoryArn :: Lens.Lens' ListObjectChildren Types.Arn
locDirectoryArn = Lens.field @"directoryArn"
{-# DEPRECATED locDirectoryArn "Use generic-lens or generic-optics with 'directoryArn' instead." #-}

-- | The reference that identifies the object for which child objects are being listed.
--
-- /Note:/ Consider using 'objectReference' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
locObjectReference :: Lens.Lens' ListObjectChildren Types.ObjectReference
locObjectReference = Lens.field @"objectReference"
{-# DEPRECATED locObjectReference "Use generic-lens or generic-optics with 'objectReference' instead." #-}

-- | Represents the manner and timing in which the successful write or update of an object is reflected in a subsequent read operation of that same object.
--
-- /Note:/ Consider using 'consistencyLevel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
locConsistencyLevel :: Lens.Lens' ListObjectChildren (Core.Maybe Types.ConsistencyLevel)
locConsistencyLevel = Lens.field @"consistencyLevel"
{-# DEPRECATED locConsistencyLevel "Use generic-lens or generic-optics with 'consistencyLevel' instead." #-}

-- | The maximum number of items to be retrieved in a single call. This is an approximate number.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
locMaxResults :: Lens.Lens' ListObjectChildren (Core.Maybe Core.Natural)
locMaxResults = Lens.field @"maxResults"
{-# DEPRECATED locMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The pagination token.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
locNextToken :: Lens.Lens' ListObjectChildren (Core.Maybe Types.NextToken)
locNextToken = Lens.field @"nextToken"
{-# DEPRECATED locNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.FromJSON ListObjectChildren where
  toJSON ListObjectChildren {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("ObjectReference" Core..= objectReference),
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("NextToken" Core..=) Core.<$> nextToken
          ]
      )

instance Core.AWSRequest ListObjectChildren where
  type Rs ListObjectChildren = ListObjectChildrenResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath =
          Core.rawPath "/amazonclouddirectory/2017-01-11/object/children",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.toHeaders "x-amz-data-partition" directoryArn
            Core.<> (Core.toHeaders "x-amz-consistency-level" consistencyLevel),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListObjectChildrenResponse'
            Core.<$> (x Core..:? "Children")
            Core.<*> (x Core..:? "NextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkListObjectChildrenResponse' smart constructor.
data ListObjectChildrenResponse = ListObjectChildrenResponse'
  { -- | Children structure, which is a map with key as the @LinkName@ and @ObjectIdentifier@ as the value.
    children :: Core.Maybe (Core.HashMap Types.LinkName Types.ObjectIdentifier),
    -- | The pagination token.
    nextToken :: Core.Maybe Types.NextToken,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListObjectChildrenResponse' value with any optional fields omitted.
mkListObjectChildrenResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListObjectChildrenResponse
mkListObjectChildrenResponse responseStatus =
  ListObjectChildrenResponse'
    { children = Core.Nothing,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | Children structure, which is a map with key as the @LinkName@ and @ObjectIdentifier@ as the value.
--
-- /Note:/ Consider using 'children' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
locrrsChildren :: Lens.Lens' ListObjectChildrenResponse (Core.Maybe (Core.HashMap Types.LinkName Types.ObjectIdentifier))
locrrsChildren = Lens.field @"children"
{-# DEPRECATED locrrsChildren "Use generic-lens or generic-optics with 'children' instead." #-}

-- | The pagination token.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
locrrsNextToken :: Lens.Lens' ListObjectChildrenResponse (Core.Maybe Types.NextToken)
locrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED locrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
locrrsResponseStatus :: Lens.Lens' ListObjectChildrenResponse Core.Int
locrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED locrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
