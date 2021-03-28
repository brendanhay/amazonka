{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.ListPolicyAttachments
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns all of the @ObjectIdentifiers@ to which a given policy is attached.
--
-- This operation returns paginated results.
module Network.AWS.CloudDirectory.ListPolicyAttachments
    (
    -- * Creating a request
      ListPolicyAttachments (..)
    , mkListPolicyAttachments
    -- ** Request lenses
    , lpaDirectoryArn
    , lpaPolicyReference
    , lpaConsistencyLevel
    , lpaMaxResults
    , lpaNextToken

    -- * Destructuring the response
    , ListPolicyAttachmentsResponse (..)
    , mkListPolicyAttachmentsResponse
    -- ** Response lenses
    , lparrsNextToken
    , lparrsObjectIdentifiers
    , lparrsResponseStatus
    ) where

import qualified Network.AWS.CloudDirectory.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListPolicyAttachments' smart constructor.
data ListPolicyAttachments = ListPolicyAttachments'
  { directoryArn :: Types.Arn
    -- ^ The Amazon Resource Name (ARN) that is associated with the 'Directory' where objects reside. For more information, see 'arns' .
  , policyReference :: Types.ObjectReference
    -- ^ The reference that identifies the policy object.
  , consistencyLevel :: Core.Maybe Types.ConsistencyLevel
    -- ^ Represents the manner and timing in which the successful write or update of an object is reflected in a subsequent read operation of that same object.
  , maxResults :: Core.Maybe Core.Natural
    -- ^ The maximum number of items to be retrieved in a single call. This is an approximate number.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ The pagination token.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListPolicyAttachments' value with any optional fields omitted.
mkListPolicyAttachments
    :: Types.Arn -- ^ 'directoryArn'
    -> Types.ObjectReference -- ^ 'policyReference'
    -> ListPolicyAttachments
mkListPolicyAttachments directoryArn policyReference
  = ListPolicyAttachments'{directoryArn, policyReference,
                           consistencyLevel = Core.Nothing, maxResults = Core.Nothing,
                           nextToken = Core.Nothing}

-- | The Amazon Resource Name (ARN) that is associated with the 'Directory' where objects reside. For more information, see 'arns' .
--
-- /Note:/ Consider using 'directoryArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpaDirectoryArn :: Lens.Lens' ListPolicyAttachments Types.Arn
lpaDirectoryArn = Lens.field @"directoryArn"
{-# INLINEABLE lpaDirectoryArn #-}
{-# DEPRECATED directoryArn "Use generic-lens or generic-optics with 'directoryArn' instead"  #-}

-- | The reference that identifies the policy object.
--
-- /Note:/ Consider using 'policyReference' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpaPolicyReference :: Lens.Lens' ListPolicyAttachments Types.ObjectReference
lpaPolicyReference = Lens.field @"policyReference"
{-# INLINEABLE lpaPolicyReference #-}
{-# DEPRECATED policyReference "Use generic-lens or generic-optics with 'policyReference' instead"  #-}

-- | Represents the manner and timing in which the successful write or update of an object is reflected in a subsequent read operation of that same object.
--
-- /Note:/ Consider using 'consistencyLevel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpaConsistencyLevel :: Lens.Lens' ListPolicyAttachments (Core.Maybe Types.ConsistencyLevel)
lpaConsistencyLevel = Lens.field @"consistencyLevel"
{-# INLINEABLE lpaConsistencyLevel #-}
{-# DEPRECATED consistencyLevel "Use generic-lens or generic-optics with 'consistencyLevel' instead"  #-}

-- | The maximum number of items to be retrieved in a single call. This is an approximate number.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpaMaxResults :: Lens.Lens' ListPolicyAttachments (Core.Maybe Core.Natural)
lpaMaxResults = Lens.field @"maxResults"
{-# INLINEABLE lpaMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | The pagination token.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpaNextToken :: Lens.Lens' ListPolicyAttachments (Core.Maybe Types.NextToken)
lpaNextToken = Lens.field @"nextToken"
{-# INLINEABLE lpaNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery ListPolicyAttachments where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ListPolicyAttachments where
        toHeaders ListPolicyAttachments{..}
          = Core.toHeaders "x-amz-data-partition" directoryArn Core.<>
              Core.toHeaders "x-amz-consistency-level" consistencyLevel

instance Core.FromJSON ListPolicyAttachments where
        toJSON ListPolicyAttachments{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("PolicyReference" Core..= policyReference),
                  ("MaxResults" Core..=) Core.<$> maxResults,
                  ("NextToken" Core..=) Core.<$> nextToken])

instance Core.AWSRequest ListPolicyAttachments where
        type Rs ListPolicyAttachments = ListPolicyAttachmentsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST,
                         Core._rqPath =
                           "/amazonclouddirectory/2017-01-11/policy/attachment",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListPolicyAttachmentsResponse' Core.<$>
                   (x Core..:? "NextToken") Core.<*> x Core..:? "ObjectIdentifiers"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListPolicyAttachments where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"objectIdentifiers" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkListPolicyAttachmentsResponse' smart constructor.
data ListPolicyAttachmentsResponse = ListPolicyAttachmentsResponse'
  { nextToken :: Core.Maybe Types.NextToken
    -- ^ The pagination token.
  , objectIdentifiers :: Core.Maybe [Types.ObjectIdentifier]
    -- ^ A list of @ObjectIdentifiers@ to which the policy is attached.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListPolicyAttachmentsResponse' value with any optional fields omitted.
mkListPolicyAttachmentsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListPolicyAttachmentsResponse
mkListPolicyAttachmentsResponse responseStatus
  = ListPolicyAttachmentsResponse'{nextToken = Core.Nothing,
                                   objectIdentifiers = Core.Nothing, responseStatus}

-- | The pagination token.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lparrsNextToken :: Lens.Lens' ListPolicyAttachmentsResponse (Core.Maybe Types.NextToken)
lparrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE lparrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | A list of @ObjectIdentifiers@ to which the policy is attached.
--
-- /Note:/ Consider using 'objectIdentifiers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lparrsObjectIdentifiers :: Lens.Lens' ListPolicyAttachmentsResponse (Core.Maybe [Types.ObjectIdentifier])
lparrsObjectIdentifiers = Lens.field @"objectIdentifiers"
{-# INLINEABLE lparrsObjectIdentifiers #-}
{-# DEPRECATED objectIdentifiers "Use generic-lens or generic-optics with 'objectIdentifiers' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lparrsResponseStatus :: Lens.Lens' ListPolicyAttachmentsResponse Core.Int
lparrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE lparrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
