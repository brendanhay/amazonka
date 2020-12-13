{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.ListInstanceProfilesForRole
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the instance profiles that have the specified associated IAM role. If there are none, the operation returns an empty list. For more information about instance profiles, go to <https://docs.aws.amazon.com/IAM/latest/UserGuide/AboutInstanceProfiles.html About Instance Profiles> .
--
-- You can paginate the results using the @MaxItems@ and @Marker@ parameters.
--
-- This operation returns paginated results.
module Network.AWS.IAM.ListInstanceProfilesForRole
  ( -- * Creating a request
    ListInstanceProfilesForRole (..),
    mkListInstanceProfilesForRole,

    -- ** Request lenses
    lipfrRoleName,
    lipfrMarker,
    lipfrMaxItems,

    -- * Destructuring the response
    ListInstanceProfilesForRoleResponse (..),
    mkListInstanceProfilesForRoleResponse,

    -- ** Response lenses
    lipfrrsMarker,
    lipfrrsIsTruncated,
    lipfrrsInstanceProfiles,
    lipfrrsResponseStatus,
  )
where

import Network.AWS.IAM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListInstanceProfilesForRole' smart constructor.
data ListInstanceProfilesForRole = ListInstanceProfilesForRole'
  { -- | The name of the role to list instance profiles for.
    --
    -- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
    roleName :: Lude.Text,
    -- | Use this parameter only when paginating results and only after you receive a response indicating that the results are truncated. Set it to the value of the @Marker@ element in the response that you received to indicate where the next call should start.
    marker :: Lude.Maybe Lude.Text,
    -- | Use this only when paginating results to indicate the maximum number of items you want in the response. If additional items exist beyond the maximum you specify, the @IsTruncated@ response element is @true@ .
    --
    -- If you do not include this parameter, the number of items defaults to 100. Note that IAM might return fewer results, even when there are more results available. In that case, the @IsTruncated@ response element returns @true@ , and @Marker@ contains a value to include in the subsequent call that tells the service where to continue from.
    maxItems :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListInstanceProfilesForRole' with the minimum fields required to make a request.
--
-- * 'roleName' - The name of the role to list instance profiles for.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
-- * 'marker' - Use this parameter only when paginating results and only after you receive a response indicating that the results are truncated. Set it to the value of the @Marker@ element in the response that you received to indicate where the next call should start.
-- * 'maxItems' - Use this only when paginating results to indicate the maximum number of items you want in the response. If additional items exist beyond the maximum you specify, the @IsTruncated@ response element is @true@ .
--
-- If you do not include this parameter, the number of items defaults to 100. Note that IAM might return fewer results, even when there are more results available. In that case, the @IsTruncated@ response element returns @true@ , and @Marker@ contains a value to include in the subsequent call that tells the service where to continue from.
mkListInstanceProfilesForRole ::
  -- | 'roleName'
  Lude.Text ->
  ListInstanceProfilesForRole
mkListInstanceProfilesForRole pRoleName_ =
  ListInstanceProfilesForRole'
    { roleName = pRoleName_,
      marker = Lude.Nothing,
      maxItems = Lude.Nothing
    }

-- | The name of the role to list instance profiles for.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
--
-- /Note:/ Consider using 'roleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lipfrRoleName :: Lens.Lens' ListInstanceProfilesForRole Lude.Text
lipfrRoleName = Lens.lens (roleName :: ListInstanceProfilesForRole -> Lude.Text) (\s a -> s {roleName = a} :: ListInstanceProfilesForRole)
{-# DEPRECATED lipfrRoleName "Use generic-lens or generic-optics with 'roleName' instead." #-}

-- | Use this parameter only when paginating results and only after you receive a response indicating that the results are truncated. Set it to the value of the @Marker@ element in the response that you received to indicate where the next call should start.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lipfrMarker :: Lens.Lens' ListInstanceProfilesForRole (Lude.Maybe Lude.Text)
lipfrMarker = Lens.lens (marker :: ListInstanceProfilesForRole -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: ListInstanceProfilesForRole)
{-# DEPRECATED lipfrMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | Use this only when paginating results to indicate the maximum number of items you want in the response. If additional items exist beyond the maximum you specify, the @IsTruncated@ response element is @true@ .
--
-- If you do not include this parameter, the number of items defaults to 100. Note that IAM might return fewer results, even when there are more results available. In that case, the @IsTruncated@ response element returns @true@ , and @Marker@ contains a value to include in the subsequent call that tells the service where to continue from.
--
-- /Note:/ Consider using 'maxItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lipfrMaxItems :: Lens.Lens' ListInstanceProfilesForRole (Lude.Maybe Lude.Natural)
lipfrMaxItems = Lens.lens (maxItems :: ListInstanceProfilesForRole -> Lude.Maybe Lude.Natural) (\s a -> s {maxItems = a} :: ListInstanceProfilesForRole)
{-# DEPRECATED lipfrMaxItems "Use generic-lens or generic-optics with 'maxItems' instead." #-}

instance Page.AWSPager ListInstanceProfilesForRole where
  page rq rs
    | Page.stop (rs Lens.^. lipfrrsIsTruncated) = Lude.Nothing
    | Lude.isNothing (rs Lens.^. lipfrrsMarker) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lipfrMarker Lens..~ rs Lens.^. lipfrrsMarker

instance Lude.AWSRequest ListInstanceProfilesForRole where
  type
    Rs ListInstanceProfilesForRole =
      ListInstanceProfilesForRoleResponse
  request = Req.postQuery iamService
  response =
    Res.receiveXMLWrapper
      "ListInstanceProfilesForRoleResult"
      ( \s h x ->
          ListInstanceProfilesForRoleResponse'
            Lude.<$> (x Lude..@? "Marker")
            Lude.<*> (x Lude..@? "IsTruncated")
            Lude.<*> ( x Lude..@? "InstanceProfiles" Lude..!@ Lude.mempty
                         Lude.>>= Lude.parseXMLList "member"
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListInstanceProfilesForRole where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ListInstanceProfilesForRole where
  toPath = Lude.const "/"

instance Lude.ToQuery ListInstanceProfilesForRole where
  toQuery ListInstanceProfilesForRole' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("ListInstanceProfilesForRole" :: Lude.ByteString),
        "Version" Lude.=: ("2010-05-08" :: Lude.ByteString),
        "RoleName" Lude.=: roleName,
        "Marker" Lude.=: marker,
        "MaxItems" Lude.=: maxItems
      ]

-- | Contains the response to a successful 'ListInstanceProfilesForRole' request.
--
-- /See:/ 'mkListInstanceProfilesForRoleResponse' smart constructor.
data ListInstanceProfilesForRoleResponse = ListInstanceProfilesForRoleResponse'
  { -- | When @IsTruncated@ is @true@ , this element is present and contains the value to use for the @Marker@ parameter in a subsequent pagination request.
    marker :: Lude.Maybe Lude.Text,
    -- | A flag that indicates whether there are more items to return. If your results were truncated, you can make a subsequent pagination request using the @Marker@ request parameter to retrieve more items. Note that IAM might return fewer than the @MaxItems@ number of results even when there are more results available. We recommend that you check @IsTruncated@ after every call to ensure that you receive all your results.
    isTruncated :: Lude.Maybe Lude.Bool,
    -- | A list of instance profiles.
    instanceProfiles :: [InstanceProfile],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListInstanceProfilesForRoleResponse' with the minimum fields required to make a request.
--
-- * 'marker' - When @IsTruncated@ is @true@ , this element is present and contains the value to use for the @Marker@ parameter in a subsequent pagination request.
-- * 'isTruncated' - A flag that indicates whether there are more items to return. If your results were truncated, you can make a subsequent pagination request using the @Marker@ request parameter to retrieve more items. Note that IAM might return fewer than the @MaxItems@ number of results even when there are more results available. We recommend that you check @IsTruncated@ after every call to ensure that you receive all your results.
-- * 'instanceProfiles' - A list of instance profiles.
-- * 'responseStatus' - The response status code.
mkListInstanceProfilesForRoleResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListInstanceProfilesForRoleResponse
mkListInstanceProfilesForRoleResponse pResponseStatus_ =
  ListInstanceProfilesForRoleResponse'
    { marker = Lude.Nothing,
      isTruncated = Lude.Nothing,
      instanceProfiles = Lude.mempty,
      responseStatus = pResponseStatus_
    }

-- | When @IsTruncated@ is @true@ , this element is present and contains the value to use for the @Marker@ parameter in a subsequent pagination request.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lipfrrsMarker :: Lens.Lens' ListInstanceProfilesForRoleResponse (Lude.Maybe Lude.Text)
lipfrrsMarker = Lens.lens (marker :: ListInstanceProfilesForRoleResponse -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: ListInstanceProfilesForRoleResponse)
{-# DEPRECATED lipfrrsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | A flag that indicates whether there are more items to return. If your results were truncated, you can make a subsequent pagination request using the @Marker@ request parameter to retrieve more items. Note that IAM might return fewer than the @MaxItems@ number of results even when there are more results available. We recommend that you check @IsTruncated@ after every call to ensure that you receive all your results.
--
-- /Note:/ Consider using 'isTruncated' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lipfrrsIsTruncated :: Lens.Lens' ListInstanceProfilesForRoleResponse (Lude.Maybe Lude.Bool)
lipfrrsIsTruncated = Lens.lens (isTruncated :: ListInstanceProfilesForRoleResponse -> Lude.Maybe Lude.Bool) (\s a -> s {isTruncated = a} :: ListInstanceProfilesForRoleResponse)
{-# DEPRECATED lipfrrsIsTruncated "Use generic-lens or generic-optics with 'isTruncated' instead." #-}

-- | A list of instance profiles.
--
-- /Note:/ Consider using 'instanceProfiles' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lipfrrsInstanceProfiles :: Lens.Lens' ListInstanceProfilesForRoleResponse [InstanceProfile]
lipfrrsInstanceProfiles = Lens.lens (instanceProfiles :: ListInstanceProfilesForRoleResponse -> [InstanceProfile]) (\s a -> s {instanceProfiles = a} :: ListInstanceProfilesForRoleResponse)
{-# DEPRECATED lipfrrsInstanceProfiles "Use generic-lens or generic-optics with 'instanceProfiles' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lipfrrsResponseStatus :: Lens.Lens' ListInstanceProfilesForRoleResponse Lude.Int
lipfrrsResponseStatus = Lens.lens (responseStatus :: ListInstanceProfilesForRoleResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListInstanceProfilesForRoleResponse)
{-# DEPRECATED lipfrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
