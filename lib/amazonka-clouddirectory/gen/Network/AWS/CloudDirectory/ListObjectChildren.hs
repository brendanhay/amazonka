{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      ListObjectChildren (..)
    , mkListObjectChildren
    -- ** Request lenses
    , locDirectoryArn
    , locObjectReference
    , locConsistencyLevel
    , locMaxResults
    , locNextToken

    -- * Destructuring the response
    , ListObjectChildrenResponse (..)
    , mkListObjectChildrenResponse
    -- ** Response lenses
    , locrrsChildren
    , locrrsNextToken
    , locrrsResponseStatus
    ) where

import qualified Network.AWS.CloudDirectory.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListObjectChildren' smart constructor.
data ListObjectChildren = ListObjectChildren'
  { directoryArn :: Types.Arn
    -- ^ The Amazon Resource Name (ARN) that is associated with the 'Directory' where the object resides. For more information, see 'arns' .
  , objectReference :: Types.ObjectReference
    -- ^ The reference that identifies the object for which child objects are being listed.
  , consistencyLevel :: Core.Maybe Types.ConsistencyLevel
    -- ^ Represents the manner and timing in which the successful write or update of an object is reflected in a subsequent read operation of that same object.
  , maxResults :: Core.Maybe Core.Natural
    -- ^ The maximum number of items to be retrieved in a single call. This is an approximate number.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ The pagination token.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListObjectChildren' value with any optional fields omitted.
mkListObjectChildren
    :: Types.Arn -- ^ 'directoryArn'
    -> Types.ObjectReference -- ^ 'objectReference'
    -> ListObjectChildren
mkListObjectChildren directoryArn objectReference
  = ListObjectChildren'{directoryArn, objectReference,
                        consistencyLevel = Core.Nothing, maxResults = Core.Nothing,
                        nextToken = Core.Nothing}

-- | The Amazon Resource Name (ARN) that is associated with the 'Directory' where the object resides. For more information, see 'arns' .
--
-- /Note:/ Consider using 'directoryArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
locDirectoryArn :: Lens.Lens' ListObjectChildren Types.Arn
locDirectoryArn = Lens.field @"directoryArn"
{-# INLINEABLE locDirectoryArn #-}
{-# DEPRECATED directoryArn "Use generic-lens or generic-optics with 'directoryArn' instead"  #-}

-- | The reference that identifies the object for which child objects are being listed.
--
-- /Note:/ Consider using 'objectReference' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
locObjectReference :: Lens.Lens' ListObjectChildren Types.ObjectReference
locObjectReference = Lens.field @"objectReference"
{-# INLINEABLE locObjectReference #-}
{-# DEPRECATED objectReference "Use generic-lens or generic-optics with 'objectReference' instead"  #-}

-- | Represents the manner and timing in which the successful write or update of an object is reflected in a subsequent read operation of that same object.
--
-- /Note:/ Consider using 'consistencyLevel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
locConsistencyLevel :: Lens.Lens' ListObjectChildren (Core.Maybe Types.ConsistencyLevel)
locConsistencyLevel = Lens.field @"consistencyLevel"
{-# INLINEABLE locConsistencyLevel #-}
{-# DEPRECATED consistencyLevel "Use generic-lens or generic-optics with 'consistencyLevel' instead"  #-}

-- | The maximum number of items to be retrieved in a single call. This is an approximate number.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
locMaxResults :: Lens.Lens' ListObjectChildren (Core.Maybe Core.Natural)
locMaxResults = Lens.field @"maxResults"
{-# INLINEABLE locMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | The pagination token.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
locNextToken :: Lens.Lens' ListObjectChildren (Core.Maybe Types.NextToken)
locNextToken = Lens.field @"nextToken"
{-# INLINEABLE locNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery ListObjectChildren where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ListObjectChildren where
        toHeaders ListObjectChildren{..}
          = Core.toHeaders "x-amz-data-partition" directoryArn Core.<>
              Core.toHeaders "x-amz-consistency-level" consistencyLevel

instance Core.FromJSON ListObjectChildren where
        toJSON ListObjectChildren{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("ObjectReference" Core..= objectReference),
                  ("MaxResults" Core..=) Core.<$> maxResults,
                  ("NextToken" Core..=) Core.<$> nextToken])

instance Core.AWSRequest ListObjectChildren where
        type Rs ListObjectChildren = ListObjectChildrenResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST,
                         Core._rqPath = "/amazonclouddirectory/2017-01-11/object/children",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListObjectChildrenResponse' Core.<$>
                   (x Core..:? "Children") Core.<*> x Core..:? "NextToken" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkListObjectChildrenResponse' smart constructor.
data ListObjectChildrenResponse = ListObjectChildrenResponse'
  { children :: Core.Maybe (Core.HashMap Types.LinkName Types.ObjectIdentifier)
    -- ^ Children structure, which is a map with key as the @LinkName@ and @ObjectIdentifier@ as the value.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ The pagination token.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListObjectChildrenResponse' value with any optional fields omitted.
mkListObjectChildrenResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListObjectChildrenResponse
mkListObjectChildrenResponse responseStatus
  = ListObjectChildrenResponse'{children = Core.Nothing,
                                nextToken = Core.Nothing, responseStatus}

-- | Children structure, which is a map with key as the @LinkName@ and @ObjectIdentifier@ as the value.
--
-- /Note:/ Consider using 'children' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
locrrsChildren :: Lens.Lens' ListObjectChildrenResponse (Core.Maybe (Core.HashMap Types.LinkName Types.ObjectIdentifier))
locrrsChildren = Lens.field @"children"
{-# INLINEABLE locrrsChildren #-}
{-# DEPRECATED children "Use generic-lens or generic-optics with 'children' instead"  #-}

-- | The pagination token.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
locrrsNextToken :: Lens.Lens' ListObjectChildrenResponse (Core.Maybe Types.NextToken)
locrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE locrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
locrrsResponseStatus :: Lens.Lens' ListObjectChildrenResponse Core.Int
locrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE locrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
