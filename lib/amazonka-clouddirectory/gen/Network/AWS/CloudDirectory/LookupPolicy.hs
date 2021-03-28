{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.LookupPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all policies from the root of the 'Directory' to the object specified. If there are no policies present, an empty list is returned. If policies are present, and if some objects don't have the policies attached, it returns the @ObjectIdentifier@ for such objects. If policies are present, it returns @ObjectIdentifier@ , @policyId@ , and @policyType@ . Paths that don't lead to the root from the target object are ignored. For more information, see <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/key_concepts_directory.html#key_concepts_policies Policies> .
--
-- This operation returns paginated results.
module Network.AWS.CloudDirectory.LookupPolicy
    (
    -- * Creating a request
      LookupPolicy (..)
    , mkLookupPolicy
    -- ** Request lenses
    , lpDirectoryArn
    , lpObjectReference
    , lpMaxResults
    , lpNextToken

    -- * Destructuring the response
    , LookupPolicyResponse (..)
    , mkLookupPolicyResponse
    -- ** Response lenses
    , lprrsNextToken
    , lprrsPolicyToPathList
    , lprrsResponseStatus
    ) where

import qualified Network.AWS.CloudDirectory.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkLookupPolicy' smart constructor.
data LookupPolicy = LookupPolicy'
  { directoryArn :: Types.Arn
    -- ^ The Amazon Resource Name (ARN) that is associated with the 'Directory' . For more information, see 'arns' .
  , objectReference :: Types.ObjectReference
    -- ^ Reference that identifies the object whose policies will be looked up.
  , maxResults :: Core.Maybe Core.Natural
    -- ^ The maximum number of items to be retrieved in a single call. This is an approximate number.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ The token to request the next page of results.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'LookupPolicy' value with any optional fields omitted.
mkLookupPolicy
    :: Types.Arn -- ^ 'directoryArn'
    -> Types.ObjectReference -- ^ 'objectReference'
    -> LookupPolicy
mkLookupPolicy directoryArn objectReference
  = LookupPolicy'{directoryArn, objectReference,
                  maxResults = Core.Nothing, nextToken = Core.Nothing}

-- | The Amazon Resource Name (ARN) that is associated with the 'Directory' . For more information, see 'arns' .
--
-- /Note:/ Consider using 'directoryArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpDirectoryArn :: Lens.Lens' LookupPolicy Types.Arn
lpDirectoryArn = Lens.field @"directoryArn"
{-# INLINEABLE lpDirectoryArn #-}
{-# DEPRECATED directoryArn "Use generic-lens or generic-optics with 'directoryArn' instead"  #-}

-- | Reference that identifies the object whose policies will be looked up.
--
-- /Note:/ Consider using 'objectReference' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpObjectReference :: Lens.Lens' LookupPolicy Types.ObjectReference
lpObjectReference = Lens.field @"objectReference"
{-# INLINEABLE lpObjectReference #-}
{-# DEPRECATED objectReference "Use generic-lens or generic-optics with 'objectReference' instead"  #-}

-- | The maximum number of items to be retrieved in a single call. This is an approximate number.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpMaxResults :: Lens.Lens' LookupPolicy (Core.Maybe Core.Natural)
lpMaxResults = Lens.field @"maxResults"
{-# INLINEABLE lpMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | The token to request the next page of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpNextToken :: Lens.Lens' LookupPolicy (Core.Maybe Types.NextToken)
lpNextToken = Lens.field @"nextToken"
{-# INLINEABLE lpNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery LookupPolicy where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders LookupPolicy where
        toHeaders LookupPolicy{..}
          = Core.toHeaders "x-amz-data-partition" directoryArn

instance Core.FromJSON LookupPolicy where
        toJSON LookupPolicy{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("ObjectReference" Core..= objectReference),
                  ("MaxResults" Core..=) Core.<$> maxResults,
                  ("NextToken" Core..=) Core.<$> nextToken])

instance Core.AWSRequest LookupPolicy where
        type Rs LookupPolicy = LookupPolicyResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST,
                         Core._rqPath = "/amazonclouddirectory/2017-01-11/policy/lookup",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 LookupPolicyResponse' Core.<$>
                   (x Core..:? "NextToken") Core.<*> x Core..:? "PolicyToPathList"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager LookupPolicy where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"policyToPathList" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkLookupPolicyResponse' smart constructor.
data LookupPolicyResponse = LookupPolicyResponse'
  { nextToken :: Core.Maybe Types.NextToken
    -- ^ The pagination token.
  , policyToPathList :: Core.Maybe [Types.PolicyToPath]
    -- ^ Provides list of path to policies. Policies contain @PolicyId@ , @ObjectIdentifier@ , and @PolicyType@ . For more information, see <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/key_concepts_directory.html#key_concepts_policies Policies> .
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'LookupPolicyResponse' value with any optional fields omitted.
mkLookupPolicyResponse
    :: Core.Int -- ^ 'responseStatus'
    -> LookupPolicyResponse
mkLookupPolicyResponse responseStatus
  = LookupPolicyResponse'{nextToken = Core.Nothing,
                          policyToPathList = Core.Nothing, responseStatus}

-- | The pagination token.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lprrsNextToken :: Lens.Lens' LookupPolicyResponse (Core.Maybe Types.NextToken)
lprrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE lprrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | Provides list of path to policies. Policies contain @PolicyId@ , @ObjectIdentifier@ , and @PolicyType@ . For more information, see <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/key_concepts_directory.html#key_concepts_policies Policies> .
--
-- /Note:/ Consider using 'policyToPathList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lprrsPolicyToPathList :: Lens.Lens' LookupPolicyResponse (Core.Maybe [Types.PolicyToPath])
lprrsPolicyToPathList = Lens.field @"policyToPathList"
{-# INLINEABLE lprrsPolicyToPathList #-}
{-# DEPRECATED policyToPathList "Use generic-lens or generic-optics with 'policyToPathList' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lprrsResponseStatus :: Lens.Lens' LookupPolicyResponse Core.Int
lprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE lprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
