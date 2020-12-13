{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.GetAccountAuthorizationDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about all IAM users, groups, roles, and policies in your AWS account, including their relationships to one another. Use this API to obtain a snapshot of the configuration of IAM permissions (users, groups, roles, and policies) in your account.
--
-- You can optionally filter the results using the @Filter@ parameter. You can paginate the results using the @MaxItems@ and @Marker@ parameters.
--
-- This operation returns paginated results.
module Network.AWS.IAM.GetAccountAuthorizationDetails
  ( -- * Creating a request
    GetAccountAuthorizationDetails (..),
    mkGetAccountAuthorizationDetails,

    -- ** Request lenses
    gaadMarker,
    gaadMaxItems,
    gaadFilter,

    -- * Destructuring the response
    GetAccountAuthorizationDetailsResponse (..),
    mkGetAccountAuthorizationDetailsResponse,

    -- ** Response lenses
    gaadrsRoleDetailList,
    gaadrsGroupDetailList,
    gaadrsUserDetailList,
    gaadrsMarker,
    gaadrsIsTruncated,
    gaadrsPolicies,
    gaadrsResponseStatus,
  )
where

import Network.AWS.IAM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetAccountAuthorizationDetails' smart constructor.
data GetAccountAuthorizationDetails = GetAccountAuthorizationDetails'
  { -- | Use this parameter only when paginating results and only after you receive a response indicating that the results are truncated. Set it to the value of the @Marker@ element in the response that you received to indicate where the next call should start.
    marker :: Lude.Maybe Lude.Text,
    -- | Use this only when paginating results to indicate the maximum number of items you want in the response. If additional items exist beyond the maximum you specify, the @IsTruncated@ response element is @true@ .
    --
    -- If you do not include this parameter, the number of items defaults to 100. Note that IAM might return fewer results, even when there are more results available. In that case, the @IsTruncated@ response element returns @true@ , and @Marker@ contains a value to include in the subsequent call that tells the service where to continue from.
    maxItems :: Lude.Maybe Lude.Natural,
    -- | A list of entity types used to filter the results. Only the entities that match the types you specify are included in the output. Use the value @LocalManagedPolicy@ to include customer managed policies.
    --
    -- The format for this parameter is a comma-separated (if more than one) list of strings. Each string value in the list must be one of the valid values listed below.
    filter :: Lude.Maybe [EntityType]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetAccountAuthorizationDetails' with the minimum fields required to make a request.
--
-- * 'marker' - Use this parameter only when paginating results and only after you receive a response indicating that the results are truncated. Set it to the value of the @Marker@ element in the response that you received to indicate where the next call should start.
-- * 'maxItems' - Use this only when paginating results to indicate the maximum number of items you want in the response. If additional items exist beyond the maximum you specify, the @IsTruncated@ response element is @true@ .
--
-- If you do not include this parameter, the number of items defaults to 100. Note that IAM might return fewer results, even when there are more results available. In that case, the @IsTruncated@ response element returns @true@ , and @Marker@ contains a value to include in the subsequent call that tells the service where to continue from.
-- * 'filter' - A list of entity types used to filter the results. Only the entities that match the types you specify are included in the output. Use the value @LocalManagedPolicy@ to include customer managed policies.
--
-- The format for this parameter is a comma-separated (if more than one) list of strings. Each string value in the list must be one of the valid values listed below.
mkGetAccountAuthorizationDetails ::
  GetAccountAuthorizationDetails
mkGetAccountAuthorizationDetails =
  GetAccountAuthorizationDetails'
    { marker = Lude.Nothing,
      maxItems = Lude.Nothing,
      filter = Lude.Nothing
    }

-- | Use this parameter only when paginating results and only after you receive a response indicating that the results are truncated. Set it to the value of the @Marker@ element in the response that you received to indicate where the next call should start.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gaadMarker :: Lens.Lens' GetAccountAuthorizationDetails (Lude.Maybe Lude.Text)
gaadMarker = Lens.lens (marker :: GetAccountAuthorizationDetails -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: GetAccountAuthorizationDetails)
{-# DEPRECATED gaadMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | Use this only when paginating results to indicate the maximum number of items you want in the response. If additional items exist beyond the maximum you specify, the @IsTruncated@ response element is @true@ .
--
-- If you do not include this parameter, the number of items defaults to 100. Note that IAM might return fewer results, even when there are more results available. In that case, the @IsTruncated@ response element returns @true@ , and @Marker@ contains a value to include in the subsequent call that tells the service where to continue from.
--
-- /Note:/ Consider using 'maxItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gaadMaxItems :: Lens.Lens' GetAccountAuthorizationDetails (Lude.Maybe Lude.Natural)
gaadMaxItems = Lens.lens (maxItems :: GetAccountAuthorizationDetails -> Lude.Maybe Lude.Natural) (\s a -> s {maxItems = a} :: GetAccountAuthorizationDetails)
{-# DEPRECATED gaadMaxItems "Use generic-lens or generic-optics with 'maxItems' instead." #-}

-- | A list of entity types used to filter the results. Only the entities that match the types you specify are included in the output. Use the value @LocalManagedPolicy@ to include customer managed policies.
--
-- The format for this parameter is a comma-separated (if more than one) list of strings. Each string value in the list must be one of the valid values listed below.
--
-- /Note:/ Consider using 'filter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gaadFilter :: Lens.Lens' GetAccountAuthorizationDetails (Lude.Maybe [EntityType])
gaadFilter = Lens.lens (filter :: GetAccountAuthorizationDetails -> Lude.Maybe [EntityType]) (\s a -> s {filter = a} :: GetAccountAuthorizationDetails)
{-# DEPRECATED gaadFilter "Use generic-lens or generic-optics with 'filter' instead." #-}

instance Page.AWSPager GetAccountAuthorizationDetails where
  page rq rs
    | Page.stop (rs Lens.^. gaadrsIsTruncated) = Lude.Nothing
    | Lude.isNothing (rs Lens.^. gaadrsMarker) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& gaadMarker Lens..~ rs Lens.^. gaadrsMarker

instance Lude.AWSRequest GetAccountAuthorizationDetails where
  type
    Rs GetAccountAuthorizationDetails =
      GetAccountAuthorizationDetailsResponse
  request = Req.postQuery iamService
  response =
    Res.receiveXMLWrapper
      "GetAccountAuthorizationDetailsResult"
      ( \s h x ->
          GetAccountAuthorizationDetailsResponse'
            Lude.<$> ( x Lude..@? "RoleDetailList" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "member")
                     )
            Lude.<*> ( x Lude..@? "GroupDetailList" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "member")
                     )
            Lude.<*> ( x Lude..@? "UserDetailList" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "member")
                     )
            Lude.<*> (x Lude..@? "Marker")
            Lude.<*> (x Lude..@? "IsTruncated")
            Lude.<*> ( x Lude..@? "Policies" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "member")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetAccountAuthorizationDetails where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath GetAccountAuthorizationDetails where
  toPath = Lude.const "/"

instance Lude.ToQuery GetAccountAuthorizationDetails where
  toQuery GetAccountAuthorizationDetails' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("GetAccountAuthorizationDetails" :: Lude.ByteString),
        "Version" Lude.=: ("2010-05-08" :: Lude.ByteString),
        "Marker" Lude.=: marker,
        "MaxItems" Lude.=: maxItems,
        "Filter"
          Lude.=: Lude.toQuery (Lude.toQueryList "member" Lude.<$> filter)
      ]

-- | Contains the response to a successful 'GetAccountAuthorizationDetails' request.
--
-- /See:/ 'mkGetAccountAuthorizationDetailsResponse' smart constructor.
data GetAccountAuthorizationDetailsResponse = GetAccountAuthorizationDetailsResponse'
  { -- | A list containing information about IAM roles.
    roleDetailList :: Lude.Maybe [RoleDetail],
    -- | A list containing information about IAM groups.
    groupDetailList :: Lude.Maybe [GroupDetail],
    -- | A list containing information about IAM users.
    userDetailList :: Lude.Maybe [UserDetail],
    -- | When @IsTruncated@ is @true@ , this element is present and contains the value to use for the @Marker@ parameter in a subsequent pagination request.
    marker :: Lude.Maybe Lude.Text,
    -- | A flag that indicates whether there are more items to return. If your results were truncated, you can make a subsequent pagination request using the @Marker@ request parameter to retrieve more items. Note that IAM might return fewer than the @MaxItems@ number of results even when there are more results available. We recommend that you check @IsTruncated@ after every call to ensure that you receive all your results.
    isTruncated :: Lude.Maybe Lude.Bool,
    -- | A list containing information about managed policies.
    policies :: Lude.Maybe [ManagedPolicyDetail],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetAccountAuthorizationDetailsResponse' with the minimum fields required to make a request.
--
-- * 'roleDetailList' - A list containing information about IAM roles.
-- * 'groupDetailList' - A list containing information about IAM groups.
-- * 'userDetailList' - A list containing information about IAM users.
-- * 'marker' - When @IsTruncated@ is @true@ , this element is present and contains the value to use for the @Marker@ parameter in a subsequent pagination request.
-- * 'isTruncated' - A flag that indicates whether there are more items to return. If your results were truncated, you can make a subsequent pagination request using the @Marker@ request parameter to retrieve more items. Note that IAM might return fewer than the @MaxItems@ number of results even when there are more results available. We recommend that you check @IsTruncated@ after every call to ensure that you receive all your results.
-- * 'policies' - A list containing information about managed policies.
-- * 'responseStatus' - The response status code.
mkGetAccountAuthorizationDetailsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetAccountAuthorizationDetailsResponse
mkGetAccountAuthorizationDetailsResponse pResponseStatus_ =
  GetAccountAuthorizationDetailsResponse'
    { roleDetailList =
        Lude.Nothing,
      groupDetailList = Lude.Nothing,
      userDetailList = Lude.Nothing,
      marker = Lude.Nothing,
      isTruncated = Lude.Nothing,
      policies = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A list containing information about IAM roles.
--
-- /Note:/ Consider using 'roleDetailList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gaadrsRoleDetailList :: Lens.Lens' GetAccountAuthorizationDetailsResponse (Lude.Maybe [RoleDetail])
gaadrsRoleDetailList = Lens.lens (roleDetailList :: GetAccountAuthorizationDetailsResponse -> Lude.Maybe [RoleDetail]) (\s a -> s {roleDetailList = a} :: GetAccountAuthorizationDetailsResponse)
{-# DEPRECATED gaadrsRoleDetailList "Use generic-lens or generic-optics with 'roleDetailList' instead." #-}

-- | A list containing information about IAM groups.
--
-- /Note:/ Consider using 'groupDetailList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gaadrsGroupDetailList :: Lens.Lens' GetAccountAuthorizationDetailsResponse (Lude.Maybe [GroupDetail])
gaadrsGroupDetailList = Lens.lens (groupDetailList :: GetAccountAuthorizationDetailsResponse -> Lude.Maybe [GroupDetail]) (\s a -> s {groupDetailList = a} :: GetAccountAuthorizationDetailsResponse)
{-# DEPRECATED gaadrsGroupDetailList "Use generic-lens or generic-optics with 'groupDetailList' instead." #-}

-- | A list containing information about IAM users.
--
-- /Note:/ Consider using 'userDetailList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gaadrsUserDetailList :: Lens.Lens' GetAccountAuthorizationDetailsResponse (Lude.Maybe [UserDetail])
gaadrsUserDetailList = Lens.lens (userDetailList :: GetAccountAuthorizationDetailsResponse -> Lude.Maybe [UserDetail]) (\s a -> s {userDetailList = a} :: GetAccountAuthorizationDetailsResponse)
{-# DEPRECATED gaadrsUserDetailList "Use generic-lens or generic-optics with 'userDetailList' instead." #-}

-- | When @IsTruncated@ is @true@ , this element is present and contains the value to use for the @Marker@ parameter in a subsequent pagination request.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gaadrsMarker :: Lens.Lens' GetAccountAuthorizationDetailsResponse (Lude.Maybe Lude.Text)
gaadrsMarker = Lens.lens (marker :: GetAccountAuthorizationDetailsResponse -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: GetAccountAuthorizationDetailsResponse)
{-# DEPRECATED gaadrsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | A flag that indicates whether there are more items to return. If your results were truncated, you can make a subsequent pagination request using the @Marker@ request parameter to retrieve more items. Note that IAM might return fewer than the @MaxItems@ number of results even when there are more results available. We recommend that you check @IsTruncated@ after every call to ensure that you receive all your results.
--
-- /Note:/ Consider using 'isTruncated' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gaadrsIsTruncated :: Lens.Lens' GetAccountAuthorizationDetailsResponse (Lude.Maybe Lude.Bool)
gaadrsIsTruncated = Lens.lens (isTruncated :: GetAccountAuthorizationDetailsResponse -> Lude.Maybe Lude.Bool) (\s a -> s {isTruncated = a} :: GetAccountAuthorizationDetailsResponse)
{-# DEPRECATED gaadrsIsTruncated "Use generic-lens or generic-optics with 'isTruncated' instead." #-}

-- | A list containing information about managed policies.
--
-- /Note:/ Consider using 'policies' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gaadrsPolicies :: Lens.Lens' GetAccountAuthorizationDetailsResponse (Lude.Maybe [ManagedPolicyDetail])
gaadrsPolicies = Lens.lens (policies :: GetAccountAuthorizationDetailsResponse -> Lude.Maybe [ManagedPolicyDetail]) (\s a -> s {policies = a} :: GetAccountAuthorizationDetailsResponse)
{-# DEPRECATED gaadrsPolicies "Use generic-lens or generic-optics with 'policies' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gaadrsResponseStatus :: Lens.Lens' GetAccountAuthorizationDetailsResponse Lude.Int
gaadrsResponseStatus = Lens.lens (responseStatus :: GetAccountAuthorizationDetailsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetAccountAuthorizationDetailsResponse)
{-# DEPRECATED gaadrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
