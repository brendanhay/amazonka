{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.DescribeWorkspaceImagePermissions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the permissions that the owner of an image has granted to other AWS accounts for an image.
module Network.AWS.WorkSpaces.DescribeWorkspaceImagePermissions
  ( -- * Creating a request
    DescribeWorkspaceImagePermissions (..),
    mkDescribeWorkspaceImagePermissions,

    -- ** Request lenses
    dwipNextToken,
    dwipMaxResults,
    dwipImageId,

    -- * Destructuring the response
    DescribeWorkspaceImagePermissionsResponse (..),
    mkDescribeWorkspaceImagePermissionsResponse,

    -- ** Response lenses
    dwiprsImagePermissions,
    dwiprsNextToken,
    dwiprsImageId,
    dwiprsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.WorkSpaces.Types

-- | /See:/ 'mkDescribeWorkspaceImagePermissions' smart constructor.
data DescribeWorkspaceImagePermissions = DescribeWorkspaceImagePermissions'
  { nextToken ::
      Lude.Maybe Lude.Text,
    maxResults ::
      Lude.Maybe Lude.Natural,
    imageId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeWorkspaceImagePermissions' with the minimum fields required to make a request.
--
-- * 'imageId' - The identifier of the image.
-- * 'maxResults' - The maximum number of items to return.
-- * 'nextToken' - If you received a @NextToken@ from a previous call that was paginated, provide this token to receive the next set of results.
mkDescribeWorkspaceImagePermissions ::
  -- | 'imageId'
  Lude.Text ->
  DescribeWorkspaceImagePermissions
mkDescribeWorkspaceImagePermissions pImageId_ =
  DescribeWorkspaceImagePermissions'
    { nextToken = Lude.Nothing,
      maxResults = Lude.Nothing,
      imageId = pImageId_
    }

-- | If you received a @NextToken@ from a previous call that was paginated, provide this token to receive the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dwipNextToken :: Lens.Lens' DescribeWorkspaceImagePermissions (Lude.Maybe Lude.Text)
dwipNextToken = Lens.lens (nextToken :: DescribeWorkspaceImagePermissions -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeWorkspaceImagePermissions)
{-# DEPRECATED dwipNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of items to return.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dwipMaxResults :: Lens.Lens' DescribeWorkspaceImagePermissions (Lude.Maybe Lude.Natural)
dwipMaxResults = Lens.lens (maxResults :: DescribeWorkspaceImagePermissions -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: DescribeWorkspaceImagePermissions)
{-# DEPRECATED dwipMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The identifier of the image.
--
-- /Note:/ Consider using 'imageId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dwipImageId :: Lens.Lens' DescribeWorkspaceImagePermissions Lude.Text
dwipImageId = Lens.lens (imageId :: DescribeWorkspaceImagePermissions -> Lude.Text) (\s a -> s {imageId = a} :: DescribeWorkspaceImagePermissions)
{-# DEPRECATED dwipImageId "Use generic-lens or generic-optics with 'imageId' instead." #-}

instance Lude.AWSRequest DescribeWorkspaceImagePermissions where
  type
    Rs DescribeWorkspaceImagePermissions =
      DescribeWorkspaceImagePermissionsResponse
  request = Req.postJSON workSpacesService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeWorkspaceImagePermissionsResponse'
            Lude.<$> (x Lude..?> "ImagePermissions" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (x Lude..?> "ImageId")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeWorkspaceImagePermissions where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "WorkspacesService.DescribeWorkspaceImagePermissions" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeWorkspaceImagePermissions where
  toJSON DescribeWorkspaceImagePermissions' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NextToken" Lude..=) Lude.<$> nextToken,
            ("MaxResults" Lude..=) Lude.<$> maxResults,
            Lude.Just ("ImageId" Lude..= imageId)
          ]
      )

instance Lude.ToPath DescribeWorkspaceImagePermissions where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeWorkspaceImagePermissions where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeWorkspaceImagePermissionsResponse' smart constructor.
data DescribeWorkspaceImagePermissionsResponse = DescribeWorkspaceImagePermissionsResponse'
  { imagePermissions ::
      Lude.Maybe
        [ImagePermission],
    nextToken ::
      Lude.Maybe
        Lude.Text,
    imageId ::
      Lude.Maybe
        Lude.Text,
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeWorkspaceImagePermissionsResponse' with the minimum fields required to make a request.
--
-- * 'imageId' - The identifier of the image.
-- * 'imagePermissions' - The identifiers of the AWS accounts that the image has been shared with.
-- * 'nextToken' - The token to use to retrieve the next set of results, or null if no more results are available.
-- * 'responseStatus' - The response status code.
mkDescribeWorkspaceImagePermissionsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeWorkspaceImagePermissionsResponse
mkDescribeWorkspaceImagePermissionsResponse pResponseStatus_ =
  DescribeWorkspaceImagePermissionsResponse'
    { imagePermissions =
        Lude.Nothing,
      nextToken = Lude.Nothing,
      imageId = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The identifiers of the AWS accounts that the image has been shared with.
--
-- /Note:/ Consider using 'imagePermissions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dwiprsImagePermissions :: Lens.Lens' DescribeWorkspaceImagePermissionsResponse (Lude.Maybe [ImagePermission])
dwiprsImagePermissions = Lens.lens (imagePermissions :: DescribeWorkspaceImagePermissionsResponse -> Lude.Maybe [ImagePermission]) (\s a -> s {imagePermissions = a} :: DescribeWorkspaceImagePermissionsResponse)
{-# DEPRECATED dwiprsImagePermissions "Use generic-lens or generic-optics with 'imagePermissions' instead." #-}

-- | The token to use to retrieve the next set of results, or null if no more results are available.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dwiprsNextToken :: Lens.Lens' DescribeWorkspaceImagePermissionsResponse (Lude.Maybe Lude.Text)
dwiprsNextToken = Lens.lens (nextToken :: DescribeWorkspaceImagePermissionsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeWorkspaceImagePermissionsResponse)
{-# DEPRECATED dwiprsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The identifier of the image.
--
-- /Note:/ Consider using 'imageId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dwiprsImageId :: Lens.Lens' DescribeWorkspaceImagePermissionsResponse (Lude.Maybe Lude.Text)
dwiprsImageId = Lens.lens (imageId :: DescribeWorkspaceImagePermissionsResponse -> Lude.Maybe Lude.Text) (\s a -> s {imageId = a} :: DescribeWorkspaceImagePermissionsResponse)
{-# DEPRECATED dwiprsImageId "Use generic-lens or generic-optics with 'imageId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dwiprsResponseStatus :: Lens.Lens' DescribeWorkspaceImagePermissionsResponse Lude.Int
dwiprsResponseStatus = Lens.lens (responseStatus :: DescribeWorkspaceImagePermissionsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeWorkspaceImagePermissionsResponse)
{-# DEPRECATED dwiprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
