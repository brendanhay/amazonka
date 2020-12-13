{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.ListServerCertificates
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the server certificates stored in IAM that have the specified path prefix. If none exist, the operation returns an empty list.
--
-- You can paginate the results using the @MaxItems@ and @Marker@ parameters.
-- For more information about working with server certificates, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_server-certs.html Working with Server Certificates> in the /IAM User Guide/ . This topic also includes a list of AWS services that can use the server certificates that you manage with IAM.
--
-- This operation returns paginated results.
module Network.AWS.IAM.ListServerCertificates
  ( -- * Creating a request
    ListServerCertificates (..),
    mkListServerCertificates,

    -- ** Request lenses
    lscPathPrefix,
    lscMarker,
    lscMaxItems,

    -- * Destructuring the response
    ListServerCertificatesResponse (..),
    mkListServerCertificatesResponse,

    -- ** Response lenses
    lscrsServerCertificateMetadataList,
    lscrsMarker,
    lscrsIsTruncated,
    lscrsResponseStatus,
  )
where

import Network.AWS.IAM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListServerCertificates' smart constructor.
data ListServerCertificates = ListServerCertificates'
  { -- | The path prefix for filtering the results. For example: @/company/servercerts@ would get all server certificates for which the path starts with @/company/servercerts@ .
    --
    -- This parameter is optional. If it is not included, it defaults to a slash (/), listing all server certificates. This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of either a forward slash (/) by itself or a string that must begin and end with forward slashes. In addition, it can contain any ASCII character from the ! (@\u0021@ ) through the DEL character (@\u007F@ ), including most punctuation characters, digits, and upper and lowercased letters.
    pathPrefix :: Lude.Maybe Lude.Text,
    -- | Use this parameter only when paginating results and only after you receive a response indicating that the results are truncated. Set it to the value of the @Marker@ element in the response that you received to indicate where the next call should start.
    marker :: Lude.Maybe Lude.Text,
    -- | Use this only when paginating results to indicate the maximum number of items you want in the response. If additional items exist beyond the maximum you specify, the @IsTruncated@ response element is @true@ .
    --
    -- If you do not include this parameter, the number of items defaults to 100. Note that IAM might return fewer results, even when there are more results available. In that case, the @IsTruncated@ response element returns @true@ , and @Marker@ contains a value to include in the subsequent call that tells the service where to continue from.
    maxItems :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListServerCertificates' with the minimum fields required to make a request.
--
-- * 'pathPrefix' - The path prefix for filtering the results. For example: @/company/servercerts@ would get all server certificates for which the path starts with @/company/servercerts@ .
--
-- This parameter is optional. If it is not included, it defaults to a slash (/), listing all server certificates. This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of either a forward slash (/) by itself or a string that must begin and end with forward slashes. In addition, it can contain any ASCII character from the ! (@\u0021@ ) through the DEL character (@\u007F@ ), including most punctuation characters, digits, and upper and lowercased letters.
-- * 'marker' - Use this parameter only when paginating results and only after you receive a response indicating that the results are truncated. Set it to the value of the @Marker@ element in the response that you received to indicate where the next call should start.
-- * 'maxItems' - Use this only when paginating results to indicate the maximum number of items you want in the response. If additional items exist beyond the maximum you specify, the @IsTruncated@ response element is @true@ .
--
-- If you do not include this parameter, the number of items defaults to 100. Note that IAM might return fewer results, even when there are more results available. In that case, the @IsTruncated@ response element returns @true@ , and @Marker@ contains a value to include in the subsequent call that tells the service where to continue from.
mkListServerCertificates ::
  ListServerCertificates
mkListServerCertificates =
  ListServerCertificates'
    { pathPrefix = Lude.Nothing,
      marker = Lude.Nothing,
      maxItems = Lude.Nothing
    }

-- | The path prefix for filtering the results. For example: @/company/servercerts@ would get all server certificates for which the path starts with @/company/servercerts@ .
--
-- This parameter is optional. If it is not included, it defaults to a slash (/), listing all server certificates. This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of either a forward slash (/) by itself or a string that must begin and end with forward slashes. In addition, it can contain any ASCII character from the ! (@\u0021@ ) through the DEL character (@\u007F@ ), including most punctuation characters, digits, and upper and lowercased letters.
--
-- /Note:/ Consider using 'pathPrefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lscPathPrefix :: Lens.Lens' ListServerCertificates (Lude.Maybe Lude.Text)
lscPathPrefix = Lens.lens (pathPrefix :: ListServerCertificates -> Lude.Maybe Lude.Text) (\s a -> s {pathPrefix = a} :: ListServerCertificates)
{-# DEPRECATED lscPathPrefix "Use generic-lens or generic-optics with 'pathPrefix' instead." #-}

-- | Use this parameter only when paginating results and only after you receive a response indicating that the results are truncated. Set it to the value of the @Marker@ element in the response that you received to indicate where the next call should start.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lscMarker :: Lens.Lens' ListServerCertificates (Lude.Maybe Lude.Text)
lscMarker = Lens.lens (marker :: ListServerCertificates -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: ListServerCertificates)
{-# DEPRECATED lscMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | Use this only when paginating results to indicate the maximum number of items you want in the response. If additional items exist beyond the maximum you specify, the @IsTruncated@ response element is @true@ .
--
-- If you do not include this parameter, the number of items defaults to 100. Note that IAM might return fewer results, even when there are more results available. In that case, the @IsTruncated@ response element returns @true@ , and @Marker@ contains a value to include in the subsequent call that tells the service where to continue from.
--
-- /Note:/ Consider using 'maxItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lscMaxItems :: Lens.Lens' ListServerCertificates (Lude.Maybe Lude.Natural)
lscMaxItems = Lens.lens (maxItems :: ListServerCertificates -> Lude.Maybe Lude.Natural) (\s a -> s {maxItems = a} :: ListServerCertificates)
{-# DEPRECATED lscMaxItems "Use generic-lens or generic-optics with 'maxItems' instead." #-}

instance Page.AWSPager ListServerCertificates where
  page rq rs
    | Page.stop (rs Lens.^. lscrsIsTruncated) = Lude.Nothing
    | Lude.isNothing (rs Lens.^. lscrsMarker) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$ rq Lude.& lscMarker Lens..~ rs Lens.^. lscrsMarker

instance Lude.AWSRequest ListServerCertificates where
  type Rs ListServerCertificates = ListServerCertificatesResponse
  request = Req.postQuery iamService
  response =
    Res.receiveXMLWrapper
      "ListServerCertificatesResult"
      ( \s h x ->
          ListServerCertificatesResponse'
            Lude.<$> ( x Lude..@? "ServerCertificateMetadataList" Lude..!@ Lude.mempty
                         Lude.>>= Lude.parseXMLList "member"
                     )
            Lude.<*> (x Lude..@? "Marker")
            Lude.<*> (x Lude..@? "IsTruncated")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListServerCertificates where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ListServerCertificates where
  toPath = Lude.const "/"

instance Lude.ToQuery ListServerCertificates where
  toQuery ListServerCertificates' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("ListServerCertificates" :: Lude.ByteString),
        "Version" Lude.=: ("2010-05-08" :: Lude.ByteString),
        "PathPrefix" Lude.=: pathPrefix,
        "Marker" Lude.=: marker,
        "MaxItems" Lude.=: maxItems
      ]

-- | Contains the response to a successful 'ListServerCertificates' request.
--
-- /See:/ 'mkListServerCertificatesResponse' smart constructor.
data ListServerCertificatesResponse = ListServerCertificatesResponse'
  { -- | A list of server certificates.
    serverCertificateMetadataList :: [ServerCertificateMetadata],
    -- | When @IsTruncated@ is @true@ , this element is present and contains the value to use for the @Marker@ parameter in a subsequent pagination request.
    marker :: Lude.Maybe Lude.Text,
    -- | A flag that indicates whether there are more items to return. If your results were truncated, you can make a subsequent pagination request using the @Marker@ request parameter to retrieve more items. Note that IAM might return fewer than the @MaxItems@ number of results even when there are more results available. We recommend that you check @IsTruncated@ after every call to ensure that you receive all your results.
    isTruncated :: Lude.Maybe Lude.Bool,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListServerCertificatesResponse' with the minimum fields required to make a request.
--
-- * 'serverCertificateMetadataList' - A list of server certificates.
-- * 'marker' - When @IsTruncated@ is @true@ , this element is present and contains the value to use for the @Marker@ parameter in a subsequent pagination request.
-- * 'isTruncated' - A flag that indicates whether there are more items to return. If your results were truncated, you can make a subsequent pagination request using the @Marker@ request parameter to retrieve more items. Note that IAM might return fewer than the @MaxItems@ number of results even when there are more results available. We recommend that you check @IsTruncated@ after every call to ensure that you receive all your results.
-- * 'responseStatus' - The response status code.
mkListServerCertificatesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListServerCertificatesResponse
mkListServerCertificatesResponse pResponseStatus_ =
  ListServerCertificatesResponse'
    { serverCertificateMetadataList =
        Lude.mempty,
      marker = Lude.Nothing,
      isTruncated = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A list of server certificates.
--
-- /Note:/ Consider using 'serverCertificateMetadataList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lscrsServerCertificateMetadataList :: Lens.Lens' ListServerCertificatesResponse [ServerCertificateMetadata]
lscrsServerCertificateMetadataList = Lens.lens (serverCertificateMetadataList :: ListServerCertificatesResponse -> [ServerCertificateMetadata]) (\s a -> s {serverCertificateMetadataList = a} :: ListServerCertificatesResponse)
{-# DEPRECATED lscrsServerCertificateMetadataList "Use generic-lens or generic-optics with 'serverCertificateMetadataList' instead." #-}

-- | When @IsTruncated@ is @true@ , this element is present and contains the value to use for the @Marker@ parameter in a subsequent pagination request.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lscrsMarker :: Lens.Lens' ListServerCertificatesResponse (Lude.Maybe Lude.Text)
lscrsMarker = Lens.lens (marker :: ListServerCertificatesResponse -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: ListServerCertificatesResponse)
{-# DEPRECATED lscrsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | A flag that indicates whether there are more items to return. If your results were truncated, you can make a subsequent pagination request using the @Marker@ request parameter to retrieve more items. Note that IAM might return fewer than the @MaxItems@ number of results even when there are more results available. We recommend that you check @IsTruncated@ after every call to ensure that you receive all your results.
--
-- /Note:/ Consider using 'isTruncated' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lscrsIsTruncated :: Lens.Lens' ListServerCertificatesResponse (Lude.Maybe Lude.Bool)
lscrsIsTruncated = Lens.lens (isTruncated :: ListServerCertificatesResponse -> Lude.Maybe Lude.Bool) (\s a -> s {isTruncated = a} :: ListServerCertificatesResponse)
{-# DEPRECATED lscrsIsTruncated "Use generic-lens or generic-optics with 'isTruncated' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lscrsResponseStatus :: Lens.Lens' ListServerCertificatesResponse Lude.Int
lscrsResponseStatus = Lens.lens (responseStatus :: ListServerCertificatesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListServerCertificatesResponse)
{-# DEPRECATED lscrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
