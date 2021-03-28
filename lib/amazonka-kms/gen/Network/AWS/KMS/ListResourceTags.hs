{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KMS.ListResourceTags
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of all tags for the specified customer master key (CMK).
--
-- You cannot perform this operation on a CMK in a different AWS account.
module Network.AWS.KMS.ListResourceTags
    (
    -- * Creating a request
      ListResourceTags (..)
    , mkListResourceTags
    -- ** Request lenses
    , lrtKeyId
    , lrtLimit
    , lrtMarker

    -- * Destructuring the response
    , ListResourceTagsResponse (..)
    , mkListResourceTagsResponse
    -- ** Response lenses
    , lrtrrsNextMarker
    , lrtrrsTags
    , lrtrrsTruncated
    , lrtrrsResponseStatus
    ) where

import qualified Network.AWS.KMS.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListResourceTags' smart constructor.
data ListResourceTags = ListResourceTags'
  { keyId :: Types.KeyId
    -- ^ A unique identifier for the customer master key (CMK).
--
-- Specify the key ID or the Amazon Resource Name (ARN) of the CMK.
-- For example:
--
--     * Key ID: @1234abcd-12ab-34cd-56ef-1234567890ab@ 
--
--
--     * Key ARN: @arn:aws:kms:us-east-2:111122223333:key/1234abcd-12ab-34cd-56ef-1234567890ab@ 
--
--
-- To get the key ID and key ARN for a CMK, use 'ListKeys' or 'DescribeKey' .
  , limit :: Core.Maybe Core.Natural
    -- ^ Use this parameter to specify the maximum number of items to return. When this value is present, AWS KMS does not return more than the specified number of items, but it might return fewer.
--
-- This value is optional. If you include a value, it must be between 1 and 50, inclusive. If you do not include a value, it defaults to 50.
  , marker :: Core.Maybe Types.MarkerType
    -- ^ Use this parameter in a subsequent request after you receive a response with truncated results. Set it to the value of @NextMarker@ from the truncated response you just received.
--
-- Do not attempt to construct this value. Use only the value of @NextMarker@ from the truncated response you just received.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListResourceTags' value with any optional fields omitted.
mkListResourceTags
    :: Types.KeyId -- ^ 'keyId'
    -> ListResourceTags
mkListResourceTags keyId
  = ListResourceTags'{keyId, limit = Core.Nothing,
                      marker = Core.Nothing}

-- | A unique identifier for the customer master key (CMK).
--
-- Specify the key ID or the Amazon Resource Name (ARN) of the CMK.
-- For example:
--
--     * Key ID: @1234abcd-12ab-34cd-56ef-1234567890ab@ 
--
--
--     * Key ARN: @arn:aws:kms:us-east-2:111122223333:key/1234abcd-12ab-34cd-56ef-1234567890ab@ 
--
--
-- To get the key ID and key ARN for a CMK, use 'ListKeys' or 'DescribeKey' .
--
-- /Note:/ Consider using 'keyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrtKeyId :: Lens.Lens' ListResourceTags Types.KeyId
lrtKeyId = Lens.field @"keyId"
{-# INLINEABLE lrtKeyId #-}
{-# DEPRECATED keyId "Use generic-lens or generic-optics with 'keyId' instead"  #-}

-- | Use this parameter to specify the maximum number of items to return. When this value is present, AWS KMS does not return more than the specified number of items, but it might return fewer.
--
-- This value is optional. If you include a value, it must be between 1 and 50, inclusive. If you do not include a value, it defaults to 50.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrtLimit :: Lens.Lens' ListResourceTags (Core.Maybe Core.Natural)
lrtLimit = Lens.field @"limit"
{-# INLINEABLE lrtLimit #-}
{-# DEPRECATED limit "Use generic-lens or generic-optics with 'limit' instead"  #-}

-- | Use this parameter in a subsequent request after you receive a response with truncated results. Set it to the value of @NextMarker@ from the truncated response you just received.
--
-- Do not attempt to construct this value. Use only the value of @NextMarker@ from the truncated response you just received.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrtMarker :: Lens.Lens' ListResourceTags (Core.Maybe Types.MarkerType)
lrtMarker = Lens.field @"marker"
{-# INLINEABLE lrtMarker #-}
{-# DEPRECATED marker "Use generic-lens or generic-optics with 'marker' instead"  #-}

instance Core.ToQuery ListResourceTags where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ListResourceTags where
        toHeaders ListResourceTags{..}
          = Core.pure ("X-Amz-Target", "TrentService.ListResourceTags")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ListResourceTags where
        toJSON ListResourceTags{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("KeyId" Core..= keyId),
                  ("Limit" Core..=) Core.<$> limit,
                  ("Marker" Core..=) Core.<$> marker])

instance Core.AWSRequest ListResourceTags where
        type Rs ListResourceTags = ListResourceTagsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListResourceTagsResponse' Core.<$>
                   (x Core..:? "NextMarker") Core.<*> x Core..:? "Tags" Core.<*>
                     x Core..:? "Truncated"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkListResourceTagsResponse' smart constructor.
data ListResourceTagsResponse = ListResourceTagsResponse'
  { nextMarker :: Core.Maybe Types.MarkerType
    -- ^ When @Truncated@ is true, this element is present and contains the value to use for the @Marker@ parameter in a subsequent request.
--
-- Do not assume or infer any information from this value.
  , tags :: Core.Maybe [Types.Tag]
    -- ^ A list of tags. Each tag consists of a tag key and a tag value.
  , truncated :: Core.Maybe Core.Bool
    -- ^ A flag that indicates whether there are more items in the list. When this value is true, the list in this response is truncated. To get more items, pass the value of the @NextMarker@ element in thisresponse to the @Marker@ parameter in a subsequent request.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListResourceTagsResponse' value with any optional fields omitted.
mkListResourceTagsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListResourceTagsResponse
mkListResourceTagsResponse responseStatus
  = ListResourceTagsResponse'{nextMarker = Core.Nothing,
                              tags = Core.Nothing, truncated = Core.Nothing, responseStatus}

-- | When @Truncated@ is true, this element is present and contains the value to use for the @Marker@ parameter in a subsequent request.
--
-- Do not assume or infer any information from this value.
--
-- /Note:/ Consider using 'nextMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrtrrsNextMarker :: Lens.Lens' ListResourceTagsResponse (Core.Maybe Types.MarkerType)
lrtrrsNextMarker = Lens.field @"nextMarker"
{-# INLINEABLE lrtrrsNextMarker #-}
{-# DEPRECATED nextMarker "Use generic-lens or generic-optics with 'nextMarker' instead"  #-}

-- | A list of tags. Each tag consists of a tag key and a tag value.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrtrrsTags :: Lens.Lens' ListResourceTagsResponse (Core.Maybe [Types.Tag])
lrtrrsTags = Lens.field @"tags"
{-# INLINEABLE lrtrrsTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

-- | A flag that indicates whether there are more items in the list. When this value is true, the list in this response is truncated. To get more items, pass the value of the @NextMarker@ element in thisresponse to the @Marker@ parameter in a subsequent request.
--
-- /Note:/ Consider using 'truncated' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrtrrsTruncated :: Lens.Lens' ListResourceTagsResponse (Core.Maybe Core.Bool)
lrtrrsTruncated = Lens.field @"truncated"
{-# INLINEABLE lrtrrsTruncated #-}
{-# DEPRECATED truncated "Use generic-lens or generic-optics with 'truncated' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrtrrsResponseStatus :: Lens.Lens' ListResourceTagsResponse Core.Int
lrtrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE lrtrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
