{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.ListObjectParents
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists parent objects that are associated with a given object in pagination fashion.
module Network.AWS.CloudDirectory.ListObjectParents
    (
    -- * Creating a request
      ListObjectParents (..)
    , mkListObjectParents
    -- ** Request lenses
    , lopDirectoryArn
    , lopObjectReference
    , lopConsistencyLevel
    , lopIncludeAllLinksToEachParent
    , lopMaxResults
    , lopNextToken

    -- * Destructuring the response
    , ListObjectParentsResponse (..)
    , mkListObjectParentsResponse
    -- ** Response lenses
    , lrsNextToken
    , lrsParentLinks
    , lrsParents
    , lrsResponseStatus
    ) where

import qualified Network.AWS.CloudDirectory.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListObjectParents' smart constructor.
data ListObjectParents = ListObjectParents'
  { directoryArn :: Types.Arn
    -- ^ The Amazon Resource Name (ARN) that is associated with the 'Directory' where the object resides. For more information, see 'arns' .
  , objectReference :: Types.ObjectReference
    -- ^ The reference that identifies the object for which parent objects are being listed.
  , consistencyLevel :: Core.Maybe Types.ConsistencyLevel
    -- ^ Represents the manner and timing in which the successful write or update of an object is reflected in a subsequent read operation of that same object.
  , includeAllLinksToEachParent :: Core.Maybe Core.Bool
    -- ^ When set to True, returns all 'ListObjectParentsResponse$ParentLinks' . There could be multiple links between a parent-child pair.
  , maxResults :: Core.Maybe Core.Natural
    -- ^ The maximum number of items to be retrieved in a single call. This is an approximate number.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ The pagination token.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListObjectParents' value with any optional fields omitted.
mkListObjectParents
    :: Types.Arn -- ^ 'directoryArn'
    -> Types.ObjectReference -- ^ 'objectReference'
    -> ListObjectParents
mkListObjectParents directoryArn objectReference
  = ListObjectParents'{directoryArn, objectReference,
                       consistencyLevel = Core.Nothing,
                       includeAllLinksToEachParent = Core.Nothing,
                       maxResults = Core.Nothing, nextToken = Core.Nothing}

-- | The Amazon Resource Name (ARN) that is associated with the 'Directory' where the object resides. For more information, see 'arns' .
--
-- /Note:/ Consider using 'directoryArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lopDirectoryArn :: Lens.Lens' ListObjectParents Types.Arn
lopDirectoryArn = Lens.field @"directoryArn"
{-# INLINEABLE lopDirectoryArn #-}
{-# DEPRECATED directoryArn "Use generic-lens or generic-optics with 'directoryArn' instead"  #-}

-- | The reference that identifies the object for which parent objects are being listed.
--
-- /Note:/ Consider using 'objectReference' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lopObjectReference :: Lens.Lens' ListObjectParents Types.ObjectReference
lopObjectReference = Lens.field @"objectReference"
{-# INLINEABLE lopObjectReference #-}
{-# DEPRECATED objectReference "Use generic-lens or generic-optics with 'objectReference' instead"  #-}

-- | Represents the manner and timing in which the successful write or update of an object is reflected in a subsequent read operation of that same object.
--
-- /Note:/ Consider using 'consistencyLevel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lopConsistencyLevel :: Lens.Lens' ListObjectParents (Core.Maybe Types.ConsistencyLevel)
lopConsistencyLevel = Lens.field @"consistencyLevel"
{-# INLINEABLE lopConsistencyLevel #-}
{-# DEPRECATED consistencyLevel "Use generic-lens or generic-optics with 'consistencyLevel' instead"  #-}

-- | When set to True, returns all 'ListObjectParentsResponse$ParentLinks' . There could be multiple links between a parent-child pair.
--
-- /Note:/ Consider using 'includeAllLinksToEachParent' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lopIncludeAllLinksToEachParent :: Lens.Lens' ListObjectParents (Core.Maybe Core.Bool)
lopIncludeAllLinksToEachParent = Lens.field @"includeAllLinksToEachParent"
{-# INLINEABLE lopIncludeAllLinksToEachParent #-}
{-# DEPRECATED includeAllLinksToEachParent "Use generic-lens or generic-optics with 'includeAllLinksToEachParent' instead"  #-}

-- | The maximum number of items to be retrieved in a single call. This is an approximate number.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lopMaxResults :: Lens.Lens' ListObjectParents (Core.Maybe Core.Natural)
lopMaxResults = Lens.field @"maxResults"
{-# INLINEABLE lopMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | The pagination token.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lopNextToken :: Lens.Lens' ListObjectParents (Core.Maybe Types.NextToken)
lopNextToken = Lens.field @"nextToken"
{-# INLINEABLE lopNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery ListObjectParents where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ListObjectParents where
        toHeaders ListObjectParents{..}
          = Core.toHeaders "x-amz-data-partition" directoryArn Core.<>
              Core.toHeaders "x-amz-consistency-level" consistencyLevel

instance Core.FromJSON ListObjectParents where
        toJSON ListObjectParents{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("ObjectReference" Core..= objectReference),
                  ("IncludeAllLinksToEachParent" Core..=) Core.<$>
                    includeAllLinksToEachParent,
                  ("MaxResults" Core..=) Core.<$> maxResults,
                  ("NextToken" Core..=) Core.<$> nextToken])

instance Core.AWSRequest ListObjectParents where
        type Rs ListObjectParents = ListObjectParentsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST,
                         Core._rqPath = "/amazonclouddirectory/2017-01-11/object/parent",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListObjectParentsResponse' Core.<$>
                   (x Core..:? "NextToken") Core.<*> x Core..:? "ParentLinks" Core.<*>
                     x Core..:? "Parents"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkListObjectParentsResponse' smart constructor.
data ListObjectParentsResponse = ListObjectParentsResponse'
  { nextToken :: Core.Maybe Types.NextToken
    -- ^ The pagination token.
  , parentLinks :: Core.Maybe [Types.ObjectIdentifierAndLinkNameTuple]
    -- ^ Returns a list of parent reference and LinkName Tuples.
  , parents :: Core.Maybe (Core.HashMap Types.ObjectIdentifier Types.LinkName)
    -- ^ The parent structure, which is a map with key as the @ObjectIdentifier@ and LinkName as the value.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListObjectParentsResponse' value with any optional fields omitted.
mkListObjectParentsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListObjectParentsResponse
mkListObjectParentsResponse responseStatus
  = ListObjectParentsResponse'{nextToken = Core.Nothing,
                               parentLinks = Core.Nothing, parents = Core.Nothing, responseStatus}

-- | The pagination token.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrsNextToken :: Lens.Lens' ListObjectParentsResponse (Core.Maybe Types.NextToken)
lrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE lrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | Returns a list of parent reference and LinkName Tuples.
--
-- /Note:/ Consider using 'parentLinks' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrsParentLinks :: Lens.Lens' ListObjectParentsResponse (Core.Maybe [Types.ObjectIdentifierAndLinkNameTuple])
lrsParentLinks = Lens.field @"parentLinks"
{-# INLINEABLE lrsParentLinks #-}
{-# DEPRECATED parentLinks "Use generic-lens or generic-optics with 'parentLinks' instead"  #-}

-- | The parent structure, which is a map with key as the @ObjectIdentifier@ and LinkName as the value.
--
-- /Note:/ Consider using 'parents' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrsParents :: Lens.Lens' ListObjectParentsResponse (Core.Maybe (Core.HashMap Types.ObjectIdentifier Types.LinkName))
lrsParents = Lens.field @"parents"
{-# INLINEABLE lrsParents #-}
{-# DEPRECATED parents "Use generic-lens or generic-optics with 'parents' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrsResponseStatus :: Lens.Lens' ListObjectParentsResponse Core.Int
lrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE lrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
