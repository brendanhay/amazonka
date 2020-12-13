{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.ListMFADevices
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the MFA devices for an IAM user. If the request includes a IAM user name, then this operation lists all the MFA devices associated with the specified user. If you do not specify a user name, IAM determines the user name implicitly based on the AWS access key ID signing the request for this API.
--
-- You can paginate the results using the @MaxItems@ and @Marker@ parameters.
--
-- This operation returns paginated results.
module Network.AWS.IAM.ListMFADevices
  ( -- * Creating a request
    ListMFADevices (..),
    mkListMFADevices,

    -- ** Request lenses
    lmdUserName,
    lmdMarker,
    lmdMaxItems,

    -- * Destructuring the response
    ListMFADevicesResponse (..),
    mkListMFADevicesResponse,

    -- ** Response lenses
    lmdrsMarker,
    lmdrsIsTruncated,
    lmdrsMFADevices,
    lmdrsResponseStatus,
  )
where

import Network.AWS.IAM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListMFADevices' smart constructor.
data ListMFADevices = ListMFADevices'
  { -- | The name of the user whose MFA devices you want to list.
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

-- | Creates a value of 'ListMFADevices' with the minimum fields required to make a request.
--
-- * 'userName' - The name of the user whose MFA devices you want to list.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
-- * 'marker' - Use this parameter only when paginating results and only after you receive a response indicating that the results are truncated. Set it to the value of the @Marker@ element in the response that you received to indicate where the next call should start.
-- * 'maxItems' - Use this only when paginating results to indicate the maximum number of items you want in the response. If additional items exist beyond the maximum you specify, the @IsTruncated@ response element is @true@ .
--
-- If you do not include this parameter, the number of items defaults to 100. Note that IAM might return fewer results, even when there are more results available. In that case, the @IsTruncated@ response element returns @true@ , and @Marker@ contains a value to include in the subsequent call that tells the service where to continue from.
mkListMFADevices ::
  ListMFADevices
mkListMFADevices =
  ListMFADevices'
    { userName = Lude.Nothing,
      marker = Lude.Nothing,
      maxItems = Lude.Nothing
    }

-- | The name of the user whose MFA devices you want to list.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
--
-- /Note:/ Consider using 'userName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmdUserName :: Lens.Lens' ListMFADevices (Lude.Maybe Lude.Text)
lmdUserName = Lens.lens (userName :: ListMFADevices -> Lude.Maybe Lude.Text) (\s a -> s {userName = a} :: ListMFADevices)
{-# DEPRECATED lmdUserName "Use generic-lens or generic-optics with 'userName' instead." #-}

-- | Use this parameter only when paginating results and only after you receive a response indicating that the results are truncated. Set it to the value of the @Marker@ element in the response that you received to indicate where the next call should start.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmdMarker :: Lens.Lens' ListMFADevices (Lude.Maybe Lude.Text)
lmdMarker = Lens.lens (marker :: ListMFADevices -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: ListMFADevices)
{-# DEPRECATED lmdMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | Use this only when paginating results to indicate the maximum number of items you want in the response. If additional items exist beyond the maximum you specify, the @IsTruncated@ response element is @true@ .
--
-- If you do not include this parameter, the number of items defaults to 100. Note that IAM might return fewer results, even when there are more results available. In that case, the @IsTruncated@ response element returns @true@ , and @Marker@ contains a value to include in the subsequent call that tells the service where to continue from.
--
-- /Note:/ Consider using 'maxItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmdMaxItems :: Lens.Lens' ListMFADevices (Lude.Maybe Lude.Natural)
lmdMaxItems = Lens.lens (maxItems :: ListMFADevices -> Lude.Maybe Lude.Natural) (\s a -> s {maxItems = a} :: ListMFADevices)
{-# DEPRECATED lmdMaxItems "Use generic-lens or generic-optics with 'maxItems' instead." #-}

instance Page.AWSPager ListMFADevices where
  page rq rs
    | Page.stop (rs Lens.^. lmdrsIsTruncated) = Lude.Nothing
    | Lude.isNothing (rs Lens.^. lmdrsMarker) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$ rq Lude.& lmdMarker Lens..~ rs Lens.^. lmdrsMarker

instance Lude.AWSRequest ListMFADevices where
  type Rs ListMFADevices = ListMFADevicesResponse
  request = Req.postQuery iamService
  response =
    Res.receiveXMLWrapper
      "ListMFADevicesResult"
      ( \s h x ->
          ListMFADevicesResponse'
            Lude.<$> (x Lude..@? "Marker")
            Lude.<*> (x Lude..@? "IsTruncated")
            Lude.<*> ( x Lude..@? "MFADevices" Lude..!@ Lude.mempty
                         Lude.>>= Lude.parseXMLList "member"
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListMFADevices where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ListMFADevices where
  toPath = Lude.const "/"

instance Lude.ToQuery ListMFADevices where
  toQuery ListMFADevices' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("ListMFADevices" :: Lude.ByteString),
        "Version" Lude.=: ("2010-05-08" :: Lude.ByteString),
        "UserName" Lude.=: userName,
        "Marker" Lude.=: marker,
        "MaxItems" Lude.=: maxItems
      ]

-- | Contains the response to a successful 'ListMFADevices' request.
--
-- /See:/ 'mkListMFADevicesResponse' smart constructor.
data ListMFADevicesResponse = ListMFADevicesResponse'
  { -- | When @IsTruncated@ is @true@ , this element is present and contains the value to use for the @Marker@ parameter in a subsequent pagination request.
    marker :: Lude.Maybe Lude.Text,
    -- | A flag that indicates whether there are more items to return. If your results were truncated, you can make a subsequent pagination request using the @Marker@ request parameter to retrieve more items. Note that IAM might return fewer than the @MaxItems@ number of results even when there are more results available. We recommend that you check @IsTruncated@ after every call to ensure that you receive all your results.
    isTruncated :: Lude.Maybe Lude.Bool,
    -- | A list of MFA devices.
    mfaDevices :: [MFADevice],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListMFADevicesResponse' with the minimum fields required to make a request.
--
-- * 'marker' - When @IsTruncated@ is @true@ , this element is present and contains the value to use for the @Marker@ parameter in a subsequent pagination request.
-- * 'isTruncated' - A flag that indicates whether there are more items to return. If your results were truncated, you can make a subsequent pagination request using the @Marker@ request parameter to retrieve more items. Note that IAM might return fewer than the @MaxItems@ number of results even when there are more results available. We recommend that you check @IsTruncated@ after every call to ensure that you receive all your results.
-- * 'mfaDevices' - A list of MFA devices.
-- * 'responseStatus' - The response status code.
mkListMFADevicesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListMFADevicesResponse
mkListMFADevicesResponse pResponseStatus_ =
  ListMFADevicesResponse'
    { marker = Lude.Nothing,
      isTruncated = Lude.Nothing,
      mfaDevices = Lude.mempty,
      responseStatus = pResponseStatus_
    }

-- | When @IsTruncated@ is @true@ , this element is present and contains the value to use for the @Marker@ parameter in a subsequent pagination request.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmdrsMarker :: Lens.Lens' ListMFADevicesResponse (Lude.Maybe Lude.Text)
lmdrsMarker = Lens.lens (marker :: ListMFADevicesResponse -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: ListMFADevicesResponse)
{-# DEPRECATED lmdrsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | A flag that indicates whether there are more items to return. If your results were truncated, you can make a subsequent pagination request using the @Marker@ request parameter to retrieve more items. Note that IAM might return fewer than the @MaxItems@ number of results even when there are more results available. We recommend that you check @IsTruncated@ after every call to ensure that you receive all your results.
--
-- /Note:/ Consider using 'isTruncated' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmdrsIsTruncated :: Lens.Lens' ListMFADevicesResponse (Lude.Maybe Lude.Bool)
lmdrsIsTruncated = Lens.lens (isTruncated :: ListMFADevicesResponse -> Lude.Maybe Lude.Bool) (\s a -> s {isTruncated = a} :: ListMFADevicesResponse)
{-# DEPRECATED lmdrsIsTruncated "Use generic-lens or generic-optics with 'isTruncated' instead." #-}

-- | A list of MFA devices.
--
-- /Note:/ Consider using 'mfaDevices' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmdrsMFADevices :: Lens.Lens' ListMFADevicesResponse [MFADevice]
lmdrsMFADevices = Lens.lens (mfaDevices :: ListMFADevicesResponse -> [MFADevice]) (\s a -> s {mfaDevices = a} :: ListMFADevicesResponse)
{-# DEPRECATED lmdrsMFADevices "Use generic-lens or generic-optics with 'mfaDevices' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmdrsResponseStatus :: Lens.Lens' ListMFADevicesResponse Lude.Int
lmdrsResponseStatus = Lens.lens (responseStatus :: ListMFADevicesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListMFADevicesResponse)
{-# DEPRECATED lmdrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
