{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KMS.ListKeyPolicies
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the names of the key policies that are attached to a customer master key (CMK). This operation is designed to get policy names that you can use in a 'GetKeyPolicy' operation. However, the only valid policy name is @default@ . You cannot perform this operation on a CMK in a different AWS account.
--
-- This operation returns paginated results.
module Network.AWS.KMS.ListKeyPolicies
    (
    -- * Creating a request
      ListKeyPolicies (..)
    , mkListKeyPolicies
    -- ** Request lenses
    , lkpKeyId
    , lkpLimit
    , lkpMarker

    -- * Destructuring the response
    , ListKeyPoliciesResponse (..)
    , mkListKeyPoliciesResponse
    -- ** Response lenses
    , lkprrsNextMarker
    , lkprrsPolicyNames
    , lkprrsTruncated
    , lkprrsResponseStatus
    ) where

import qualified Network.AWS.KMS.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListKeyPolicies' smart constructor.
data ListKeyPolicies = ListKeyPolicies'
  { keyId :: Types.KeyIdType
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
-- This value is optional. If you include a value, it must be between 1 and 1000, inclusive. If you do not include a value, it defaults to 100.
-- Only one policy can be attached to a key.
  , marker :: Core.Maybe Types.MarkerType
    -- ^ Use this parameter in a subsequent request after you receive a response with truncated results. Set it to the value of @NextMarker@ from the truncated response you just received.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListKeyPolicies' value with any optional fields omitted.
mkListKeyPolicies
    :: Types.KeyIdType -- ^ 'keyId'
    -> ListKeyPolicies
mkListKeyPolicies keyId
  = ListKeyPolicies'{keyId, limit = Core.Nothing,
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
lkpKeyId :: Lens.Lens' ListKeyPolicies Types.KeyIdType
lkpKeyId = Lens.field @"keyId"
{-# INLINEABLE lkpKeyId #-}
{-# DEPRECATED keyId "Use generic-lens or generic-optics with 'keyId' instead"  #-}

-- | Use this parameter to specify the maximum number of items to return. When this value is present, AWS KMS does not return more than the specified number of items, but it might return fewer.
--
-- This value is optional. If you include a value, it must be between 1 and 1000, inclusive. If you do not include a value, it defaults to 100.
-- Only one policy can be attached to a key.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lkpLimit :: Lens.Lens' ListKeyPolicies (Core.Maybe Core.Natural)
lkpLimit = Lens.field @"limit"
{-# INLINEABLE lkpLimit #-}
{-# DEPRECATED limit "Use generic-lens or generic-optics with 'limit' instead"  #-}

-- | Use this parameter in a subsequent request after you receive a response with truncated results. Set it to the value of @NextMarker@ from the truncated response you just received.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lkpMarker :: Lens.Lens' ListKeyPolicies (Core.Maybe Types.MarkerType)
lkpMarker = Lens.field @"marker"
{-# INLINEABLE lkpMarker #-}
{-# DEPRECATED marker "Use generic-lens or generic-optics with 'marker' instead"  #-}

instance Core.ToQuery ListKeyPolicies where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ListKeyPolicies where
        toHeaders ListKeyPolicies{..}
          = Core.pure ("X-Amz-Target", "TrentService.ListKeyPolicies")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ListKeyPolicies where
        toJSON ListKeyPolicies{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("KeyId" Core..= keyId),
                  ("Limit" Core..=) Core.<$> limit,
                  ("Marker" Core..=) Core.<$> marker])

instance Core.AWSRequest ListKeyPolicies where
        type Rs ListKeyPolicies = ListKeyPoliciesResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListKeyPoliciesResponse' Core.<$>
                   (x Core..:? "NextMarker") Core.<*> x Core..:? "PolicyNames"
                     Core.<*> x Core..:? "Truncated"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListKeyPolicies where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"truncated") = Core.Nothing
          | Core.isNothing (rs Lens.^. Lens.field @"nextMarker") =
            Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"marker" Lens..~ rs Lens.^. Lens.field @"nextMarker")

-- | /See:/ 'mkListKeyPoliciesResponse' smart constructor.
data ListKeyPoliciesResponse = ListKeyPoliciesResponse'
  { nextMarker :: Core.Maybe Types.MarkerType
    -- ^ When @Truncated@ is true, this element is present and contains the value to use for the @Marker@ parameter in a subsequent request.
  , policyNames :: Core.Maybe [Types.PolicyNameType]
    -- ^ A list of key policy names. The only valid value is @default@ .
  , truncated :: Core.Maybe Core.Bool
    -- ^ A flag that indicates whether there are more items in the list. When this value is true, the list in this response is truncated. To get more items, pass the value of the @NextMarker@ element in thisresponse to the @Marker@ parameter in a subsequent request.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListKeyPoliciesResponse' value with any optional fields omitted.
mkListKeyPoliciesResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListKeyPoliciesResponse
mkListKeyPoliciesResponse responseStatus
  = ListKeyPoliciesResponse'{nextMarker = Core.Nothing,
                             policyNames = Core.Nothing, truncated = Core.Nothing,
                             responseStatus}

-- | When @Truncated@ is true, this element is present and contains the value to use for the @Marker@ parameter in a subsequent request.
--
-- /Note:/ Consider using 'nextMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lkprrsNextMarker :: Lens.Lens' ListKeyPoliciesResponse (Core.Maybe Types.MarkerType)
lkprrsNextMarker = Lens.field @"nextMarker"
{-# INLINEABLE lkprrsNextMarker #-}
{-# DEPRECATED nextMarker "Use generic-lens or generic-optics with 'nextMarker' instead"  #-}

-- | A list of key policy names. The only valid value is @default@ .
--
-- /Note:/ Consider using 'policyNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lkprrsPolicyNames :: Lens.Lens' ListKeyPoliciesResponse (Core.Maybe [Types.PolicyNameType])
lkprrsPolicyNames = Lens.field @"policyNames"
{-# INLINEABLE lkprrsPolicyNames #-}
{-# DEPRECATED policyNames "Use generic-lens or generic-optics with 'policyNames' instead"  #-}

-- | A flag that indicates whether there are more items in the list. When this value is true, the list in this response is truncated. To get more items, pass the value of the @NextMarker@ element in thisresponse to the @Marker@ parameter in a subsequent request.
--
-- /Note:/ Consider using 'truncated' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lkprrsTruncated :: Lens.Lens' ListKeyPoliciesResponse (Core.Maybe Core.Bool)
lkprrsTruncated = Lens.field @"truncated"
{-# INLINEABLE lkprrsTruncated #-}
{-# DEPRECATED truncated "Use generic-lens or generic-optics with 'truncated' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lkprrsResponseStatus :: Lens.Lens' ListKeyPoliciesResponse Core.Int
lkprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE lkprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
