{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    ListPolicyVersions (..),
    mkListPolicyVersions,

    -- ** Request lenses
    lpvMarker,
    lpvMaxItems,
    lpvPolicyARN,

    -- * Destructuring the response
    ListPolicyVersionsResponse (..),
    mkListPolicyVersionsResponse,

    -- ** Response lenses
    lpvrsVersions,
    lpvrsMarker,
    lpvrsIsTruncated,
    lpvrsResponseStatus,
  )
where

import Network.AWS.IAM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListPolicyVersions' smart constructor.
data ListPolicyVersions = ListPolicyVersions'
  { -- | Use this parameter only when paginating results and only after you receive a response indicating that the results are truncated. Set it to the value of the @Marker@ element in the response that you received to indicate where the next call should start.
    marker :: Lude.Maybe Lude.Text,
    -- | Use this only when paginating results to indicate the maximum number of items you want in the response. If additional items exist beyond the maximum you specify, the @IsTruncated@ response element is @true@ .
    --
    -- If you do not include this parameter, the number of items defaults to 100. Note that IAM might return fewer results, even when there are more results available. In that case, the @IsTruncated@ response element returns @true@ , and @Marker@ contains a value to include in the subsequent call that tells the service where to continue from.
    maxItems :: Lude.Maybe Lude.Natural,
    -- | The Amazon Resource Name (ARN) of the IAM policy for which you want the versions.
    --
    -- For more information about ARNs, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> in the /AWS General Reference/ .
    policyARN :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListPolicyVersions' with the minimum fields required to make a request.
--
-- * 'marker' - Use this parameter only when paginating results and only after you receive a response indicating that the results are truncated. Set it to the value of the @Marker@ element in the response that you received to indicate where the next call should start.
-- * 'maxItems' - Use this only when paginating results to indicate the maximum number of items you want in the response. If additional items exist beyond the maximum you specify, the @IsTruncated@ response element is @true@ .
--
-- If you do not include this parameter, the number of items defaults to 100. Note that IAM might return fewer results, even when there are more results available. In that case, the @IsTruncated@ response element returns @true@ , and @Marker@ contains a value to include in the subsequent call that tells the service where to continue from.
-- * 'policyARN' - The Amazon Resource Name (ARN) of the IAM policy for which you want the versions.
--
-- For more information about ARNs, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> in the /AWS General Reference/ .
mkListPolicyVersions ::
  -- | 'policyARN'
  Lude.Text ->
  ListPolicyVersions
mkListPolicyVersions pPolicyARN_ =
  ListPolicyVersions'
    { marker = Lude.Nothing,
      maxItems = Lude.Nothing,
      policyARN = pPolicyARN_
    }

-- | Use this parameter only when paginating results and only after you receive a response indicating that the results are truncated. Set it to the value of the @Marker@ element in the response that you received to indicate where the next call should start.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpvMarker :: Lens.Lens' ListPolicyVersions (Lude.Maybe Lude.Text)
lpvMarker = Lens.lens (marker :: ListPolicyVersions -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: ListPolicyVersions)
{-# DEPRECATED lpvMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | Use this only when paginating results to indicate the maximum number of items you want in the response. If additional items exist beyond the maximum you specify, the @IsTruncated@ response element is @true@ .
--
-- If you do not include this parameter, the number of items defaults to 100. Note that IAM might return fewer results, even when there are more results available. In that case, the @IsTruncated@ response element returns @true@ , and @Marker@ contains a value to include in the subsequent call that tells the service where to continue from.
--
-- /Note:/ Consider using 'maxItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpvMaxItems :: Lens.Lens' ListPolicyVersions (Lude.Maybe Lude.Natural)
lpvMaxItems = Lens.lens (maxItems :: ListPolicyVersions -> Lude.Maybe Lude.Natural) (\s a -> s {maxItems = a} :: ListPolicyVersions)
{-# DEPRECATED lpvMaxItems "Use generic-lens or generic-optics with 'maxItems' instead." #-}

-- | The Amazon Resource Name (ARN) of the IAM policy for which you want the versions.
--
-- For more information about ARNs, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> in the /AWS General Reference/ .
--
-- /Note:/ Consider using 'policyARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpvPolicyARN :: Lens.Lens' ListPolicyVersions Lude.Text
lpvPolicyARN = Lens.lens (policyARN :: ListPolicyVersions -> Lude.Text) (\s a -> s {policyARN = a} :: ListPolicyVersions)
{-# DEPRECATED lpvPolicyARN "Use generic-lens or generic-optics with 'policyARN' instead." #-}

instance Page.AWSPager ListPolicyVersions where
  page rq rs
    | Page.stop (rs Lens.^. lpvrsIsTruncated) = Lude.Nothing
    | Lude.isNothing (rs Lens.^. lpvrsMarker) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$ rq Lude.& lpvMarker Lens..~ rs Lens.^. lpvrsMarker

instance Lude.AWSRequest ListPolicyVersions where
  type Rs ListPolicyVersions = ListPolicyVersionsResponse
  request = Req.postQuery iamService
  response =
    Res.receiveXMLWrapper
      "ListPolicyVersionsResult"
      ( \s h x ->
          ListPolicyVersionsResponse'
            Lude.<$> ( x Lude..@? "Versions" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "member")
                     )
            Lude.<*> (x Lude..@? "Marker")
            Lude.<*> (x Lude..@? "IsTruncated")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListPolicyVersions where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ListPolicyVersions where
  toPath = Lude.const "/"

instance Lude.ToQuery ListPolicyVersions where
  toQuery ListPolicyVersions' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("ListPolicyVersions" :: Lude.ByteString),
        "Version" Lude.=: ("2010-05-08" :: Lude.ByteString),
        "Marker" Lude.=: marker,
        "MaxItems" Lude.=: maxItems,
        "PolicyArn" Lude.=: policyARN
      ]

-- | Contains the response to a successful 'ListPolicyVersions' request.
--
-- /See:/ 'mkListPolicyVersionsResponse' smart constructor.
data ListPolicyVersionsResponse = ListPolicyVersionsResponse'
  { -- | A list of policy versions.
    --
    -- For more information about managed policy versions, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-versions.html Versioning for Managed Policies> in the /IAM User Guide/ .
    versions :: Lude.Maybe [PolicyVersion],
    -- | When @IsTruncated@ is @true@ , this element is present and contains the value to use for the @Marker@ parameter in a subsequent pagination request.
    marker :: Lude.Maybe Lude.Text,
    -- | A flag that indicates whether there are more items to return. If your results were truncated, you can make a subsequent pagination request using the @Marker@ request parameter to retrieve more items. Note that IAM might return fewer than the @MaxItems@ number of results even when there are more results available. We recommend that you check @IsTruncated@ after every call to ensure that you receive all your results.
    isTruncated :: Lude.Maybe Lude.Bool,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListPolicyVersionsResponse' with the minimum fields required to make a request.
--
-- * 'versions' - A list of policy versions.
--
-- For more information about managed policy versions, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-versions.html Versioning for Managed Policies> in the /IAM User Guide/ .
-- * 'marker' - When @IsTruncated@ is @true@ , this element is present and contains the value to use for the @Marker@ parameter in a subsequent pagination request.
-- * 'isTruncated' - A flag that indicates whether there are more items to return. If your results were truncated, you can make a subsequent pagination request using the @Marker@ request parameter to retrieve more items. Note that IAM might return fewer than the @MaxItems@ number of results even when there are more results available. We recommend that you check @IsTruncated@ after every call to ensure that you receive all your results.
-- * 'responseStatus' - The response status code.
mkListPolicyVersionsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListPolicyVersionsResponse
mkListPolicyVersionsResponse pResponseStatus_ =
  ListPolicyVersionsResponse'
    { versions = Lude.Nothing,
      marker = Lude.Nothing,
      isTruncated = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A list of policy versions.
--
-- For more information about managed policy versions, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-versions.html Versioning for Managed Policies> in the /IAM User Guide/ .
--
-- /Note:/ Consider using 'versions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpvrsVersions :: Lens.Lens' ListPolicyVersionsResponse (Lude.Maybe [PolicyVersion])
lpvrsVersions = Lens.lens (versions :: ListPolicyVersionsResponse -> Lude.Maybe [PolicyVersion]) (\s a -> s {versions = a} :: ListPolicyVersionsResponse)
{-# DEPRECATED lpvrsVersions "Use generic-lens or generic-optics with 'versions' instead." #-}

-- | When @IsTruncated@ is @true@ , this element is present and contains the value to use for the @Marker@ parameter in a subsequent pagination request.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpvrsMarker :: Lens.Lens' ListPolicyVersionsResponse (Lude.Maybe Lude.Text)
lpvrsMarker = Lens.lens (marker :: ListPolicyVersionsResponse -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: ListPolicyVersionsResponse)
{-# DEPRECATED lpvrsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | A flag that indicates whether there are more items to return. If your results were truncated, you can make a subsequent pagination request using the @Marker@ request parameter to retrieve more items. Note that IAM might return fewer than the @MaxItems@ number of results even when there are more results available. We recommend that you check @IsTruncated@ after every call to ensure that you receive all your results.
--
-- /Note:/ Consider using 'isTruncated' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpvrsIsTruncated :: Lens.Lens' ListPolicyVersionsResponse (Lude.Maybe Lude.Bool)
lpvrsIsTruncated = Lens.lens (isTruncated :: ListPolicyVersionsResponse -> Lude.Maybe Lude.Bool) (\s a -> s {isTruncated = a} :: ListPolicyVersionsResponse)
{-# DEPRECATED lpvrsIsTruncated "Use generic-lens or generic-optics with 'isTruncated' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpvrsResponseStatus :: Lens.Lens' ListPolicyVersionsResponse Lude.Int
lpvrsResponseStatus = Lens.lens (responseStatus :: ListPolicyVersionsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListPolicyVersionsResponse)
{-# DEPRECATED lpvrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
