{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.ListPolicyVersions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists information about the versions of the specified managed policy, including the version that is currently set as the policy's default version.
--
-- For more information about managed policies, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html Managed Policies and Inline Policies> in the /IAM User Guide/ .
--
-- This operation returns paginated results.
module Network.AWS.IAM.ListPolicyVersions
    (
    -- * Creating a request
      ListPolicyVersions (..)
    , mkListPolicyVersions
    -- ** Request lenses
    , lpvPolicyArn
    , lpvMarker
    , lpvMaxItems

    -- * Destructuring the response
    , ListPolicyVersionsResponse (..)
    , mkListPolicyVersionsResponse
    -- ** Response lenses
    , lpvrrsIsTruncated
    , lpvrrsMarker
    , lpvrrsVersions
    , lpvrrsResponseStatus
    ) where

import qualified Network.AWS.IAM.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListPolicyVersions' smart constructor.
data ListPolicyVersions = ListPolicyVersions'
  { policyArn :: Types.PolicyArn
    -- ^ The Amazon Resource Name (ARN) of the IAM policy for which you want the versions.
--
-- For more information about ARNs, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> in the /AWS General Reference/ .
  , marker :: Core.Maybe Types.MarkerType
    -- ^ Use this parameter only when paginating results and only after you receive a response indicating that the results are truncated. Set it to the value of the @Marker@ element in the response that you received to indicate where the next call should start.
  , maxItems :: Core.Maybe Core.Natural
    -- ^ Use this only when paginating results to indicate the maximum number of items you want in the response. If additional items exist beyond the maximum you specify, the @IsTruncated@ response element is @true@ .
--
-- If you do not include this parameter, the number of items defaults to 100. Note that IAM might return fewer results, even when there are more results available. In that case, the @IsTruncated@ response element returns @true@ , and @Marker@ contains a value to include in the subsequent call that tells the service where to continue from.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListPolicyVersions' value with any optional fields omitted.
mkListPolicyVersions
    :: Types.PolicyArn -- ^ 'policyArn'
    -> ListPolicyVersions
mkListPolicyVersions policyArn
  = ListPolicyVersions'{policyArn, marker = Core.Nothing,
                        maxItems = Core.Nothing}

-- | The Amazon Resource Name (ARN) of the IAM policy for which you want the versions.
--
-- For more information about ARNs, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> in the /AWS General Reference/ .
--
-- /Note:/ Consider using 'policyArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpvPolicyArn :: Lens.Lens' ListPolicyVersions Types.PolicyArn
lpvPolicyArn = Lens.field @"policyArn"
{-# INLINEABLE lpvPolicyArn #-}
{-# DEPRECATED policyArn "Use generic-lens or generic-optics with 'policyArn' instead"  #-}

-- | Use this parameter only when paginating results and only after you receive a response indicating that the results are truncated. Set it to the value of the @Marker@ element in the response that you received to indicate where the next call should start.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpvMarker :: Lens.Lens' ListPolicyVersions (Core.Maybe Types.MarkerType)
lpvMarker = Lens.field @"marker"
{-# INLINEABLE lpvMarker #-}
{-# DEPRECATED marker "Use generic-lens or generic-optics with 'marker' instead"  #-}

-- | Use this only when paginating results to indicate the maximum number of items you want in the response. If additional items exist beyond the maximum you specify, the @IsTruncated@ response element is @true@ .
--
-- If you do not include this parameter, the number of items defaults to 100. Note that IAM might return fewer results, even when there are more results available. In that case, the @IsTruncated@ response element returns @true@ , and @Marker@ contains a value to include in the subsequent call that tells the service where to continue from.
--
-- /Note:/ Consider using 'maxItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpvMaxItems :: Lens.Lens' ListPolicyVersions (Core.Maybe Core.Natural)
lpvMaxItems = Lens.field @"maxItems"
{-# INLINEABLE lpvMaxItems #-}
{-# DEPRECATED maxItems "Use generic-lens or generic-optics with 'maxItems' instead"  #-}

instance Core.ToQuery ListPolicyVersions where
        toQuery ListPolicyVersions{..}
          = Core.toQueryPair "Action" ("ListPolicyVersions" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2010-05-08" :: Core.Text)
              Core.<> Core.toQueryPair "PolicyArn" policyArn
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "Marker") marker
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "MaxItems") maxItems

instance Core.ToHeaders ListPolicyVersions where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest ListPolicyVersions where
        type Rs ListPolicyVersions = ListPolicyVersionsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.mempty,
                         Core._rqHeaders =
                           Core.pure
                             ("Content-Type",
                              "application/x-www-form-urlencoded; charset=utf-8")
                             Core.<> Core.toHeaders x,
                         Core._rqBody = Core.toFormBody (Core.toQuery x)}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXMLWrapper "ListPolicyVersionsResult"
              (\ s h x ->
                 ListPolicyVersionsResponse' Core.<$>
                   (x Core..@? "IsTruncated") Core.<*> x Core..@? "Marker" Core.<*>
                     x Core..@? "Versions" Core..<@> Core.parseXMLList "member"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListPolicyVersions where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"isTruncated") = Core.Nothing
          | Core.isNothing (rs Lens.^. Lens.field @"marker") = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"marker" Lens..~ rs Lens.^. Lens.field @"marker")

-- | Contains the response to a successful 'ListPolicyVersions' request. 
--
-- /See:/ 'mkListPolicyVersionsResponse' smart constructor.
data ListPolicyVersionsResponse = ListPolicyVersionsResponse'
  { isTruncated :: Core.Maybe Core.Bool
    -- ^ A flag that indicates whether there are more items to return. If your results were truncated, you can make a subsequent pagination request using the @Marker@ request parameter to retrieve more items. Note that IAM might return fewer than the @MaxItems@ number of results even when there are more results available. We recommend that you check @IsTruncated@ after every call to ensure that you receive all your results.
  , marker :: Core.Maybe Types.ResponseMarkerType
    -- ^ When @IsTruncated@ is @true@ , this element is present and contains the value to use for the @Marker@ parameter in a subsequent pagination request.
  , versions :: Core.Maybe [Types.PolicyVersion]
    -- ^ A list of policy versions.
--
-- For more information about managed policy versions, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-versions.html Versioning for Managed Policies> in the /IAM User Guide/ .
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ListPolicyVersionsResponse' value with any optional fields omitted.
mkListPolicyVersionsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListPolicyVersionsResponse
mkListPolicyVersionsResponse responseStatus
  = ListPolicyVersionsResponse'{isTruncated = Core.Nothing,
                                marker = Core.Nothing, versions = Core.Nothing, responseStatus}

-- | A flag that indicates whether there are more items to return. If your results were truncated, you can make a subsequent pagination request using the @Marker@ request parameter to retrieve more items. Note that IAM might return fewer than the @MaxItems@ number of results even when there are more results available. We recommend that you check @IsTruncated@ after every call to ensure that you receive all your results.
--
-- /Note:/ Consider using 'isTruncated' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpvrrsIsTruncated :: Lens.Lens' ListPolicyVersionsResponse (Core.Maybe Core.Bool)
lpvrrsIsTruncated = Lens.field @"isTruncated"
{-# INLINEABLE lpvrrsIsTruncated #-}
{-# DEPRECATED isTruncated "Use generic-lens or generic-optics with 'isTruncated' instead"  #-}

-- | When @IsTruncated@ is @true@ , this element is present and contains the value to use for the @Marker@ parameter in a subsequent pagination request.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpvrrsMarker :: Lens.Lens' ListPolicyVersionsResponse (Core.Maybe Types.ResponseMarkerType)
lpvrrsMarker = Lens.field @"marker"
{-# INLINEABLE lpvrrsMarker #-}
{-# DEPRECATED marker "Use generic-lens or generic-optics with 'marker' instead"  #-}

-- | A list of policy versions.
--
-- For more information about managed policy versions, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-versions.html Versioning for Managed Policies> in the /IAM User Guide/ .
--
-- /Note:/ Consider using 'versions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpvrrsVersions :: Lens.Lens' ListPolicyVersionsResponse (Core.Maybe [Types.PolicyVersion])
lpvrrsVersions = Lens.field @"versions"
{-# INLINEABLE lpvrrsVersions #-}
{-# DEPRECATED versions "Use generic-lens or generic-optics with 'versions' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpvrrsResponseStatus :: Lens.Lens' ListPolicyVersionsResponse Core.Int
lpvrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE lpvrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
