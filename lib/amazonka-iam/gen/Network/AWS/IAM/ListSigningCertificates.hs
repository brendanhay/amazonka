{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.ListSigningCertificates
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the signing certificates associated with the specified IAM user. If none exists, the operation returns an empty list.
--
-- Although each user is limited to a small number of signing certificates, you can still paginate the results using the @MaxItems@ and @Marker@ parameters.
-- If the @UserName@ field is not specified, the user name is determined implicitly based on the AWS access key ID used to sign the request for this API. This operation works for access keys under the AWS account. Consequently, you can use this operation to manage AWS account root user credentials even if the AWS account has no associated users.
--
-- This operation returns paginated results.
module Network.AWS.IAM.ListSigningCertificates
  ( -- * Creating a request
    ListSigningCertificates (..),
    mkListSigningCertificates,

    -- ** Request lenses
    lUserName,
    lMarker,
    lMaxItems,

    -- * Destructuring the response
    ListSigningCertificatesResponse (..),
    mkListSigningCertificatesResponse,

    -- ** Response lenses
    lrsCertificates,
    lrsMarker,
    lrsIsTruncated,
    lrsResponseStatus,
  )
where

import Network.AWS.IAM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListSigningCertificates' smart constructor.
data ListSigningCertificates = ListSigningCertificates'
  { -- | The name of the IAM user whose signing certificates you want to examine.
    --
    -- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
    userName :: Lude.Maybe Lude.Text,
    -- | Use this parameter only when paginating results and only after you receive a response indicating that the results are truncated. Set it to the value of the @Marker@ element in the response that you received to indicate where the next call should start.
    marker :: Lude.Maybe Lude.Text,
    -- | Use this only when paginating results to indicate the maximum number of items you want in the response. If additional items exist beyond the maximum you specify, the @IsTruncated@ response element is @true@ .
    --
    -- If you do not include this parameter, the number of items defaults to 100. Note that IAM might return fewer results, even when there are more results available. In that case, the @IsTruncated@ response element returns @true@ , and @Marker@ contains a value to include in the subsequent call that tells the service where to continue from.
    maxItems :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListSigningCertificates' with the minimum fields required to make a request.
--
-- * 'userName' - The name of the IAM user whose signing certificates you want to examine.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
-- * 'marker' - Use this parameter only when paginating results and only after you receive a response indicating that the results are truncated. Set it to the value of the @Marker@ element in the response that you received to indicate where the next call should start.
-- * 'maxItems' - Use this only when paginating results to indicate the maximum number of items you want in the response. If additional items exist beyond the maximum you specify, the @IsTruncated@ response element is @true@ .
--
-- If you do not include this parameter, the number of items defaults to 100. Note that IAM might return fewer results, even when there are more results available. In that case, the @IsTruncated@ response element returns @true@ , and @Marker@ contains a value to include in the subsequent call that tells the service where to continue from.
mkListSigningCertificates ::
  ListSigningCertificates
mkListSigningCertificates =
  ListSigningCertificates'
    { userName = Lude.Nothing,
      marker = Lude.Nothing,
      maxItems = Lude.Nothing
    }

-- | The name of the IAM user whose signing certificates you want to examine.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
--
-- /Note:/ Consider using 'userName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lUserName :: Lens.Lens' ListSigningCertificates (Lude.Maybe Lude.Text)
lUserName = Lens.lens (userName :: ListSigningCertificates -> Lude.Maybe Lude.Text) (\s a -> s {userName = a} :: ListSigningCertificates)
{-# DEPRECATED lUserName "Use generic-lens or generic-optics with 'userName' instead." #-}

-- | Use this parameter only when paginating results and only after you receive a response indicating that the results are truncated. Set it to the value of the @Marker@ element in the response that you received to indicate where the next call should start.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lMarker :: Lens.Lens' ListSigningCertificates (Lude.Maybe Lude.Text)
lMarker = Lens.lens (marker :: ListSigningCertificates -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: ListSigningCertificates)
{-# DEPRECATED lMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | Use this only when paginating results to indicate the maximum number of items you want in the response. If additional items exist beyond the maximum you specify, the @IsTruncated@ response element is @true@ .
--
-- If you do not include this parameter, the number of items defaults to 100. Note that IAM might return fewer results, even when there are more results available. In that case, the @IsTruncated@ response element returns @true@ , and @Marker@ contains a value to include in the subsequent call that tells the service where to continue from.
--
-- /Note:/ Consider using 'maxItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lMaxItems :: Lens.Lens' ListSigningCertificates (Lude.Maybe Lude.Natural)
lMaxItems = Lens.lens (maxItems :: ListSigningCertificates -> Lude.Maybe Lude.Natural) (\s a -> s {maxItems = a} :: ListSigningCertificates)
{-# DEPRECATED lMaxItems "Use generic-lens or generic-optics with 'maxItems' instead." #-}

instance Page.AWSPager ListSigningCertificates where
  page rq rs
    | Page.stop (rs Lens.^. lrsIsTruncated) = Lude.Nothing
    | Lude.isNothing (rs Lens.^. lrsMarker) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$ rq Lude.& lMarker Lens..~ rs Lens.^. lrsMarker

instance Lude.AWSRequest ListSigningCertificates where
  type Rs ListSigningCertificates = ListSigningCertificatesResponse
  request = Req.postQuery iamService
  response =
    Res.receiveXMLWrapper
      "ListSigningCertificatesResult"
      ( \s h x ->
          ListSigningCertificatesResponse'
            Lude.<$> ( x Lude..@? "Certificates" Lude..!@ Lude.mempty
                         Lude.>>= Lude.parseXMLList "member"
                     )
            Lude.<*> (x Lude..@? "Marker")
            Lude.<*> (x Lude..@? "IsTruncated")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListSigningCertificates where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ListSigningCertificates where
  toPath = Lude.const "/"

instance Lude.ToQuery ListSigningCertificates where
  toQuery ListSigningCertificates' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("ListSigningCertificates" :: Lude.ByteString),
        "Version" Lude.=: ("2010-05-08" :: Lude.ByteString),
        "UserName" Lude.=: userName,
        "Marker" Lude.=: marker,
        "MaxItems" Lude.=: maxItems
      ]

-- | Contains the response to a successful 'ListSigningCertificates' request.
--
-- /See:/ 'mkListSigningCertificatesResponse' smart constructor.
data ListSigningCertificatesResponse = ListSigningCertificatesResponse'
  { -- | A list of the user's signing certificate information.
    certificates :: [SigningCertificate],
    -- | When @IsTruncated@ is @true@ , this element is present and contains the value to use for the @Marker@ parameter in a subsequent pagination request.
    marker :: Lude.Maybe Lude.Text,
    -- | A flag that indicates whether there are more items to return. If your results were truncated, you can make a subsequent pagination request using the @Marker@ request parameter to retrieve more items. Note that IAM might return fewer than the @MaxItems@ number of results even when there are more results available. We recommend that you check @IsTruncated@ after every call to ensure that you receive all your results.
    isTruncated :: Lude.Maybe Lude.Bool,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListSigningCertificatesResponse' with the minimum fields required to make a request.
--
-- * 'certificates' - A list of the user's signing certificate information.
-- * 'marker' - When @IsTruncated@ is @true@ , this element is present and contains the value to use for the @Marker@ parameter in a subsequent pagination request.
-- * 'isTruncated' - A flag that indicates whether there are more items to return. If your results were truncated, you can make a subsequent pagination request using the @Marker@ request parameter to retrieve more items. Note that IAM might return fewer than the @MaxItems@ number of results even when there are more results available. We recommend that you check @IsTruncated@ after every call to ensure that you receive all your results.
-- * 'responseStatus' - The response status code.
mkListSigningCertificatesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListSigningCertificatesResponse
mkListSigningCertificatesResponse pResponseStatus_ =
  ListSigningCertificatesResponse'
    { certificates = Lude.mempty,
      marker = Lude.Nothing,
      isTruncated = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A list of the user's signing certificate information.
--
-- /Note:/ Consider using 'certificates' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrsCertificates :: Lens.Lens' ListSigningCertificatesResponse [SigningCertificate]
lrsCertificates = Lens.lens (certificates :: ListSigningCertificatesResponse -> [SigningCertificate]) (\s a -> s {certificates = a} :: ListSigningCertificatesResponse)
{-# DEPRECATED lrsCertificates "Use generic-lens or generic-optics with 'certificates' instead." #-}

-- | When @IsTruncated@ is @true@ , this element is present and contains the value to use for the @Marker@ parameter in a subsequent pagination request.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrsMarker :: Lens.Lens' ListSigningCertificatesResponse (Lude.Maybe Lude.Text)
lrsMarker = Lens.lens (marker :: ListSigningCertificatesResponse -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: ListSigningCertificatesResponse)
{-# DEPRECATED lrsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | A flag that indicates whether there are more items to return. If your results were truncated, you can make a subsequent pagination request using the @Marker@ request parameter to retrieve more items. Note that IAM might return fewer than the @MaxItems@ number of results even when there are more results available. We recommend that you check @IsTruncated@ after every call to ensure that you receive all your results.
--
-- /Note:/ Consider using 'isTruncated' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrsIsTruncated :: Lens.Lens' ListSigningCertificatesResponse (Lude.Maybe Lude.Bool)
lrsIsTruncated = Lens.lens (isTruncated :: ListSigningCertificatesResponse -> Lude.Maybe Lude.Bool) (\s a -> s {isTruncated = a} :: ListSigningCertificatesResponse)
{-# DEPRECATED lrsIsTruncated "Use generic-lens or generic-optics with 'isTruncated' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrsResponseStatus :: Lens.Lens' ListSigningCertificatesResponse Lude.Int
lrsResponseStatus = Lens.lens (responseStatus :: ListSigningCertificatesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListSigningCertificatesResponse)
{-# DEPRECATED lrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
