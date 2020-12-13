{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.ListAccessKeys
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the access key IDs associated with the specified IAM user. If there is none, the operation returns an empty list.
--
-- Although each user is limited to a small number of keys, you can still paginate the results using the @MaxItems@ and @Marker@ parameters.
-- If the @UserName@ field is not specified, the user name is determined implicitly based on the AWS access key ID used to sign the request. This operation works for access keys under the AWS account. Consequently, you can use this operation to manage AWS account root user credentials even if the AWS account has no associated users.
--
-- This operation returns paginated results.
module Network.AWS.IAM.ListAccessKeys
  ( -- * Creating a request
    ListAccessKeys (..),
    mkListAccessKeys,

    -- ** Request lenses
    lakUserName,
    lakMarker,
    lakMaxItems,

    -- * Destructuring the response
    ListAccessKeysResponse (..),
    mkListAccessKeysResponse,

    -- ** Response lenses
    lakrsMarker,
    lakrsIsTruncated,
    lakrsAccessKeyMetadata,
    lakrsResponseStatus,
  )
where

import Network.AWS.IAM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListAccessKeys' smart constructor.
data ListAccessKeys = ListAccessKeys'
  { -- | The name of the user.
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

-- | Creates a value of 'ListAccessKeys' with the minimum fields required to make a request.
--
-- * 'userName' - The name of the user.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
-- * 'marker' - Use this parameter only when paginating results and only after you receive a response indicating that the results are truncated. Set it to the value of the @Marker@ element in the response that you received to indicate where the next call should start.
-- * 'maxItems' - Use this only when paginating results to indicate the maximum number of items you want in the response. If additional items exist beyond the maximum you specify, the @IsTruncated@ response element is @true@ .
--
-- If you do not include this parameter, the number of items defaults to 100. Note that IAM might return fewer results, even when there are more results available. In that case, the @IsTruncated@ response element returns @true@ , and @Marker@ contains a value to include in the subsequent call that tells the service where to continue from.
mkListAccessKeys ::
  ListAccessKeys
mkListAccessKeys =
  ListAccessKeys'
    { userName = Lude.Nothing,
      marker = Lude.Nothing,
      maxItems = Lude.Nothing
    }

-- | The name of the user.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
--
-- /Note:/ Consider using 'userName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lakUserName :: Lens.Lens' ListAccessKeys (Lude.Maybe Lude.Text)
lakUserName = Lens.lens (userName :: ListAccessKeys -> Lude.Maybe Lude.Text) (\s a -> s {userName = a} :: ListAccessKeys)
{-# DEPRECATED lakUserName "Use generic-lens or generic-optics with 'userName' instead." #-}

-- | Use this parameter only when paginating results and only after you receive a response indicating that the results are truncated. Set it to the value of the @Marker@ element in the response that you received to indicate where the next call should start.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lakMarker :: Lens.Lens' ListAccessKeys (Lude.Maybe Lude.Text)
lakMarker = Lens.lens (marker :: ListAccessKeys -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: ListAccessKeys)
{-# DEPRECATED lakMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | Use this only when paginating results to indicate the maximum number of items you want in the response. If additional items exist beyond the maximum you specify, the @IsTruncated@ response element is @true@ .
--
-- If you do not include this parameter, the number of items defaults to 100. Note that IAM might return fewer results, even when there are more results available. In that case, the @IsTruncated@ response element returns @true@ , and @Marker@ contains a value to include in the subsequent call that tells the service where to continue from.
--
-- /Note:/ Consider using 'maxItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lakMaxItems :: Lens.Lens' ListAccessKeys (Lude.Maybe Lude.Natural)
lakMaxItems = Lens.lens (maxItems :: ListAccessKeys -> Lude.Maybe Lude.Natural) (\s a -> s {maxItems = a} :: ListAccessKeys)
{-# DEPRECATED lakMaxItems "Use generic-lens or generic-optics with 'maxItems' instead." #-}

instance Page.AWSPager ListAccessKeys where
  page rq rs
    | Page.stop (rs Lens.^. lakrsIsTruncated) = Lude.Nothing
    | Lude.isNothing (rs Lens.^. lakrsMarker) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$ rq Lude.& lakMarker Lens..~ rs Lens.^. lakrsMarker

instance Lude.AWSRequest ListAccessKeys where
  type Rs ListAccessKeys = ListAccessKeysResponse
  request = Req.postQuery iamService
  response =
    Res.receiveXMLWrapper
      "ListAccessKeysResult"
      ( \s h x ->
          ListAccessKeysResponse'
            Lude.<$> (x Lude..@? "Marker")
            Lude.<*> (x Lude..@? "IsTruncated")
            Lude.<*> ( x Lude..@? "AccessKeyMetadata" Lude..!@ Lude.mempty
                         Lude.>>= Lude.parseXMLList "member"
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListAccessKeys where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ListAccessKeys where
  toPath = Lude.const "/"

instance Lude.ToQuery ListAccessKeys where
  toQuery ListAccessKeys' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("ListAccessKeys" :: Lude.ByteString),
        "Version" Lude.=: ("2010-05-08" :: Lude.ByteString),
        "UserName" Lude.=: userName,
        "Marker" Lude.=: marker,
        "MaxItems" Lude.=: maxItems
      ]

-- | Contains the response to a successful 'ListAccessKeys' request.
--
-- /See:/ 'mkListAccessKeysResponse' smart constructor.
data ListAccessKeysResponse = ListAccessKeysResponse'
  { -- | When @IsTruncated@ is @true@ , this element is present and contains the value to use for the @Marker@ parameter in a subsequent pagination request.
    marker :: Lude.Maybe Lude.Text,
    -- | A flag that indicates whether there are more items to return. If your results were truncated, you can make a subsequent pagination request using the @Marker@ request parameter to retrieve more items. Note that IAM might return fewer than the @MaxItems@ number of results even when there are more results available. We recommend that you check @IsTruncated@ after every call to ensure that you receive all your results.
    isTruncated :: Lude.Maybe Lude.Bool,
    -- | A list of objects containing metadata about the access keys.
    accessKeyMetadata :: [AccessKeyMetadata],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListAccessKeysResponse' with the minimum fields required to make a request.
--
-- * 'marker' - When @IsTruncated@ is @true@ , this element is present and contains the value to use for the @Marker@ parameter in a subsequent pagination request.
-- * 'isTruncated' - A flag that indicates whether there are more items to return. If your results were truncated, you can make a subsequent pagination request using the @Marker@ request parameter to retrieve more items. Note that IAM might return fewer than the @MaxItems@ number of results even when there are more results available. We recommend that you check @IsTruncated@ after every call to ensure that you receive all your results.
-- * 'accessKeyMetadata' - A list of objects containing metadata about the access keys.
-- * 'responseStatus' - The response status code.
mkListAccessKeysResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListAccessKeysResponse
mkListAccessKeysResponse pResponseStatus_ =
  ListAccessKeysResponse'
    { marker = Lude.Nothing,
      isTruncated = Lude.Nothing,
      accessKeyMetadata = Lude.mempty,
      responseStatus = pResponseStatus_
    }

-- | When @IsTruncated@ is @true@ , this element is present and contains the value to use for the @Marker@ parameter in a subsequent pagination request.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lakrsMarker :: Lens.Lens' ListAccessKeysResponse (Lude.Maybe Lude.Text)
lakrsMarker = Lens.lens (marker :: ListAccessKeysResponse -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: ListAccessKeysResponse)
{-# DEPRECATED lakrsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | A flag that indicates whether there are more items to return. If your results were truncated, you can make a subsequent pagination request using the @Marker@ request parameter to retrieve more items. Note that IAM might return fewer than the @MaxItems@ number of results even when there are more results available. We recommend that you check @IsTruncated@ after every call to ensure that you receive all your results.
--
-- /Note:/ Consider using 'isTruncated' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lakrsIsTruncated :: Lens.Lens' ListAccessKeysResponse (Lude.Maybe Lude.Bool)
lakrsIsTruncated = Lens.lens (isTruncated :: ListAccessKeysResponse -> Lude.Maybe Lude.Bool) (\s a -> s {isTruncated = a} :: ListAccessKeysResponse)
{-# DEPRECATED lakrsIsTruncated "Use generic-lens or generic-optics with 'isTruncated' instead." #-}

-- | A list of objects containing metadata about the access keys.
--
-- /Note:/ Consider using 'accessKeyMetadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lakrsAccessKeyMetadata :: Lens.Lens' ListAccessKeysResponse [AccessKeyMetadata]
lakrsAccessKeyMetadata = Lens.lens (accessKeyMetadata :: ListAccessKeysResponse -> [AccessKeyMetadata]) (\s a -> s {accessKeyMetadata = a} :: ListAccessKeysResponse)
{-# DEPRECATED lakrsAccessKeyMetadata "Use generic-lens or generic-optics with 'accessKeyMetadata' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lakrsResponseStatus :: Lens.Lens' ListAccessKeysResponse Lude.Int
lakrsResponseStatus = Lens.lens (responseStatus :: ListAccessKeysResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListAccessKeysResponse)
{-# DEPRECATED lakrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
