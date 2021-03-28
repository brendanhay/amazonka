{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.ListBuckets
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of all buckets owned by the authenticated sender of the request.
module Network.AWS.S3.ListBuckets
    (
    -- * Creating a request
      ListBuckets (..)
    , mkListBuckets

    -- * Destructuring the response
    , ListBucketsResponse (..)
    , mkListBucketsResponse
    -- ** Response lenses
    , lbrrsBuckets
    , lbrrsOwner
    , lbrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.S3.Types as Types

-- | /See:/ 'mkListBuckets' smart constructor.
data ListBuckets = ListBuckets'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListBuckets' value with any optional fields omitted.
mkListBuckets
    :: ListBuckets
mkListBuckets = ListBuckets'

instance Core.ToQuery ListBuckets where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ListBuckets where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest ListBuckets where
        type Rs ListBuckets = ListBucketsResponse
        toRequest x@_
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXML
              (\ s h x ->
                 ListBucketsResponse' Core.<$>
                   (x Core..@? "Buckets" Core..<@> Core.parseXMLList "Bucket")
                     Core.<*> x Core..@? "Owner"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkListBucketsResponse' smart constructor.
data ListBucketsResponse = ListBucketsResponse'
  { buckets :: Core.Maybe [Types.Bucket]
    -- ^ The list of buckets owned by the requestor.
  , owner :: Core.Maybe Types.Owner
    -- ^ The owner of the buckets listed.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ListBucketsResponse' value with any optional fields omitted.
mkListBucketsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListBucketsResponse
mkListBucketsResponse responseStatus
  = ListBucketsResponse'{buckets = Core.Nothing,
                         owner = Core.Nothing, responseStatus}

-- | The list of buckets owned by the requestor.
--
-- /Note:/ Consider using 'buckets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbrrsBuckets :: Lens.Lens' ListBucketsResponse (Core.Maybe [Types.Bucket])
lbrrsBuckets = Lens.field @"buckets"
{-# INLINEABLE lbrrsBuckets #-}
{-# DEPRECATED buckets "Use generic-lens or generic-optics with 'buckets' instead"  #-}

-- | The owner of the buckets listed.
--
-- /Note:/ Consider using 'owner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbrrsOwner :: Lens.Lens' ListBucketsResponse (Core.Maybe Types.Owner)
lbrrsOwner = Lens.field @"owner"
{-# INLINEABLE lbrrsOwner #-}
{-# DEPRECATED owner "Use generic-lens or generic-optics with 'owner' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbrrsResponseStatus :: Lens.Lens' ListBucketsResponse Core.Int
lbrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE lbrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
