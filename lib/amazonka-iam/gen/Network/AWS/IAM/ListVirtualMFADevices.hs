{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.ListVirtualMFADevices
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the virtual MFA devices defined in the AWS account by assignment status. If you do not specify an assignment status, the operation returns a list of all virtual MFA devices. Assignment status can be @Assigned@ , @Unassigned@ , or @Any@ .
--
-- You can paginate the results using the @MaxItems@ and @Marker@ parameters.
--
-- This operation returns paginated results.
module Network.AWS.IAM.ListVirtualMFADevices
  ( -- * Creating a request
    ListVirtualMFADevices (..),
    mkListVirtualMFADevices,

    -- ** Request lenses
    lvmdAssignmentStatus,
    lvmdMarker,
    lvmdMaxItems,

    -- * Destructuring the response
    ListVirtualMFADevicesResponse (..),
    mkListVirtualMFADevicesResponse,

    -- ** Response lenses
    lvmdrsMarker,
    lvmdrsIsTruncated,
    lvmdrsResponseStatus,
    lvmdrsVirtualMFADevices,
  )
where

import Network.AWS.IAM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListVirtualMFADevices' smart constructor.
data ListVirtualMFADevices = ListVirtualMFADevices'
  { assignmentStatus ::
      Lude.Maybe AssignmentStatusType,
    marker :: Lude.Maybe Lude.Text,
    maxItems :: Lude.Maybe Lude.Natural
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListVirtualMFADevices' with the minimum fields required to make a request.
--
-- * 'assignmentStatus' - The status (@Unassigned@ or @Assigned@ ) of the devices to list. If you do not specify an @AssignmentStatus@ , the operation defaults to @Any@ , which lists both assigned and unassigned virtual MFA devices.,
-- * 'marker' - Use this parameter only when paginating results and only after you receive a response indicating that the results are truncated. Set it to the value of the @Marker@ element in the response that you received to indicate where the next call should start.
-- * 'maxItems' - Use this only when paginating results to indicate the maximum number of items you want in the response. If additional items exist beyond the maximum you specify, the @IsTruncated@ response element is @true@ .
--
-- If you do not include this parameter, the number of items defaults to 100. Note that IAM might return fewer results, even when there are more results available. In that case, the @IsTruncated@ response element returns @true@ , and @Marker@ contains a value to include in the subsequent call that tells the service where to continue from.
mkListVirtualMFADevices ::
  ListVirtualMFADevices
mkListVirtualMFADevices =
  ListVirtualMFADevices'
    { assignmentStatus = Lude.Nothing,
      marker = Lude.Nothing,
      maxItems = Lude.Nothing
    }

-- | The status (@Unassigned@ or @Assigned@ ) of the devices to list. If you do not specify an @AssignmentStatus@ , the operation defaults to @Any@ , which lists both assigned and unassigned virtual MFA devices.,
--
-- /Note:/ Consider using 'assignmentStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lvmdAssignmentStatus :: Lens.Lens' ListVirtualMFADevices (Lude.Maybe AssignmentStatusType)
lvmdAssignmentStatus = Lens.lens (assignmentStatus :: ListVirtualMFADevices -> Lude.Maybe AssignmentStatusType) (\s a -> s {assignmentStatus = a} :: ListVirtualMFADevices)
{-# DEPRECATED lvmdAssignmentStatus "Use generic-lens or generic-optics with 'assignmentStatus' instead." #-}

-- | Use this parameter only when paginating results and only after you receive a response indicating that the results are truncated. Set it to the value of the @Marker@ element in the response that you received to indicate where the next call should start.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lvmdMarker :: Lens.Lens' ListVirtualMFADevices (Lude.Maybe Lude.Text)
lvmdMarker = Lens.lens (marker :: ListVirtualMFADevices -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: ListVirtualMFADevices)
{-# DEPRECATED lvmdMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | Use this only when paginating results to indicate the maximum number of items you want in the response. If additional items exist beyond the maximum you specify, the @IsTruncated@ response element is @true@ .
--
-- If you do not include this parameter, the number of items defaults to 100. Note that IAM might return fewer results, even when there are more results available. In that case, the @IsTruncated@ response element returns @true@ , and @Marker@ contains a value to include in the subsequent call that tells the service where to continue from.
--
-- /Note:/ Consider using 'maxItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lvmdMaxItems :: Lens.Lens' ListVirtualMFADevices (Lude.Maybe Lude.Natural)
lvmdMaxItems = Lens.lens (maxItems :: ListVirtualMFADevices -> Lude.Maybe Lude.Natural) (\s a -> s {maxItems = a} :: ListVirtualMFADevices)
{-# DEPRECATED lvmdMaxItems "Use generic-lens or generic-optics with 'maxItems' instead." #-}

instance Page.AWSPager ListVirtualMFADevices where
  page rq rs
    | Page.stop (rs Lens.^. lvmdrsIsTruncated) = Lude.Nothing
    | Lude.isNothing (rs Lens.^. lvmdrsMarker) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lvmdMarker Lens..~ rs Lens.^. lvmdrsMarker

instance Lude.AWSRequest ListVirtualMFADevices where
  type Rs ListVirtualMFADevices = ListVirtualMFADevicesResponse
  request = Req.postQuery iamService
  response =
    Res.receiveXMLWrapper
      "ListVirtualMFADevicesResult"
      ( \s h x ->
          ListVirtualMFADevicesResponse'
            Lude.<$> (x Lude..@? "Marker")
            Lude.<*> (x Lude..@? "IsTruncated")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
            Lude.<*> ( x Lude..@? "VirtualMFADevices" Lude..!@ Lude.mempty
                         Lude.>>= Lude.parseXMLList "member"
                     )
      )

instance Lude.ToHeaders ListVirtualMFADevices where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ListVirtualMFADevices where
  toPath = Lude.const "/"

instance Lude.ToQuery ListVirtualMFADevices where
  toQuery ListVirtualMFADevices' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("ListVirtualMFADevices" :: Lude.ByteString),
        "Version" Lude.=: ("2010-05-08" :: Lude.ByteString),
        "AssignmentStatus" Lude.=: assignmentStatus,
        "Marker" Lude.=: marker,
        "MaxItems" Lude.=: maxItems
      ]

-- | Contains the response to a successful 'ListVirtualMFADevices' request.
--
-- /See:/ 'mkListVirtualMFADevicesResponse' smart constructor.
data ListVirtualMFADevicesResponse = ListVirtualMFADevicesResponse'
  { marker ::
      Lude.Maybe Lude.Text,
    isTruncated ::
      Lude.Maybe Lude.Bool,
    responseStatus :: Lude.Int,
    virtualMFADevices ::
      [VirtualMFADevice]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListVirtualMFADevicesResponse' with the minimum fields required to make a request.
--
-- * 'isTruncated' - A flag that indicates whether there are more items to return. If your results were truncated, you can make a subsequent pagination request using the @Marker@ request parameter to retrieve more items. Note that IAM might return fewer than the @MaxItems@ number of results even when there are more results available. We recommend that you check @IsTruncated@ after every call to ensure that you receive all your results.
-- * 'marker' - When @IsTruncated@ is @true@ , this element is present and contains the value to use for the @Marker@ parameter in a subsequent pagination request.
-- * 'responseStatus' - The response status code.
-- * 'virtualMFADevices' - The list of virtual MFA devices in the current account that match the @AssignmentStatus@ value that was passed in the request.
mkListVirtualMFADevicesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListVirtualMFADevicesResponse
mkListVirtualMFADevicesResponse pResponseStatus_ =
  ListVirtualMFADevicesResponse'
    { marker = Lude.Nothing,
      isTruncated = Lude.Nothing,
      responseStatus = pResponseStatus_,
      virtualMFADevices = Lude.mempty
    }

-- | When @IsTruncated@ is @true@ , this element is present and contains the value to use for the @Marker@ parameter in a subsequent pagination request.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lvmdrsMarker :: Lens.Lens' ListVirtualMFADevicesResponse (Lude.Maybe Lude.Text)
lvmdrsMarker = Lens.lens (marker :: ListVirtualMFADevicesResponse -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: ListVirtualMFADevicesResponse)
{-# DEPRECATED lvmdrsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | A flag that indicates whether there are more items to return. If your results were truncated, you can make a subsequent pagination request using the @Marker@ request parameter to retrieve more items. Note that IAM might return fewer than the @MaxItems@ number of results even when there are more results available. We recommend that you check @IsTruncated@ after every call to ensure that you receive all your results.
--
-- /Note:/ Consider using 'isTruncated' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lvmdrsIsTruncated :: Lens.Lens' ListVirtualMFADevicesResponse (Lude.Maybe Lude.Bool)
lvmdrsIsTruncated = Lens.lens (isTruncated :: ListVirtualMFADevicesResponse -> Lude.Maybe Lude.Bool) (\s a -> s {isTruncated = a} :: ListVirtualMFADevicesResponse)
{-# DEPRECATED lvmdrsIsTruncated "Use generic-lens or generic-optics with 'isTruncated' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lvmdrsResponseStatus :: Lens.Lens' ListVirtualMFADevicesResponse Lude.Int
lvmdrsResponseStatus = Lens.lens (responseStatus :: ListVirtualMFADevicesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListVirtualMFADevicesResponse)
{-# DEPRECATED lvmdrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | The list of virtual MFA devices in the current account that match the @AssignmentStatus@ value that was passed in the request.
--
-- /Note:/ Consider using 'virtualMFADevices' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lvmdrsVirtualMFADevices :: Lens.Lens' ListVirtualMFADevicesResponse [VirtualMFADevice]
lvmdrsVirtualMFADevices = Lens.lens (virtualMFADevices :: ListVirtualMFADevicesResponse -> [VirtualMFADevice]) (\s a -> s {virtualMFADevices = a} :: ListVirtualMFADevicesResponse)
{-# DEPRECATED lvmdrsVirtualMFADevices "Use generic-lens or generic-optics with 'virtualMFADevices' instead." #-}
