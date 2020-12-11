{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.CopyWorkspaceImage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Copies the specified image from the specified Region to the current Region. For more information about copying images, see <https://docs.aws.amazon.com/workspaces/latest/adminguide/copy-custom-image.html Copy a Custom WorkSpaces Image> .
--
-- /Important:/ Before copying a shared image, be sure to verify that it has been shared from the correct AWS account. To determine if an image has been shared and to see the AWS account ID that owns an image, use the <https://docs.aws.amazon.com/workspaces/latest/api/API_DescribeWorkspaceImages.html DescribeWorkSpaceImages> and <https://docs.aws.amazon.com/workspaces/latest/api/API_DescribeWorkspaceImagePermissions.html DescribeWorkspaceImagePermissions> API operations.
module Network.AWS.WorkSpaces.CopyWorkspaceImage
  ( -- * Creating a request
    CopyWorkspaceImage (..),
    mkCopyWorkspaceImage,

    -- ** Request lenses
    cwiDescription,
    cwiTags,
    cwiName,
    cwiSourceImageId,
    cwiSourceRegion,

    -- * Destructuring the response
    CopyWorkspaceImageResponse (..),
    mkCopyWorkspaceImageResponse,

    -- ** Response lenses
    cwirsImageId,
    cwirsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.WorkSpaces.Types

-- | /See:/ 'mkCopyWorkspaceImage' smart constructor.
data CopyWorkspaceImage = CopyWorkspaceImage'
  { description ::
      Lude.Maybe Lude.Text,
    tags :: Lude.Maybe [Tag],
    name :: Lude.Text,
    sourceImageId :: Lude.Text,
    sourceRegion :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CopyWorkspaceImage' with the minimum fields required to make a request.
--
-- * 'description' - A description of the image.
-- * 'name' - The name of the image.
-- * 'sourceImageId' - The identifier of the source image.
-- * 'sourceRegion' - The identifier of the source Region.
-- * 'tags' - The tags for the image.
mkCopyWorkspaceImage ::
  -- | 'name'
  Lude.Text ->
  -- | 'sourceImageId'
  Lude.Text ->
  -- | 'sourceRegion'
  Lude.Text ->
  CopyWorkspaceImage
mkCopyWorkspaceImage pName_ pSourceImageId_ pSourceRegion_ =
  CopyWorkspaceImage'
    { description = Lude.Nothing,
      tags = Lude.Nothing,
      name = pName_,
      sourceImageId = pSourceImageId_,
      sourceRegion = pSourceRegion_
    }

-- | A description of the image.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cwiDescription :: Lens.Lens' CopyWorkspaceImage (Lude.Maybe Lude.Text)
cwiDescription = Lens.lens (description :: CopyWorkspaceImage -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: CopyWorkspaceImage)
{-# DEPRECATED cwiDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The tags for the image.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cwiTags :: Lens.Lens' CopyWorkspaceImage (Lude.Maybe [Tag])
cwiTags = Lens.lens (tags :: CopyWorkspaceImage -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: CopyWorkspaceImage)
{-# DEPRECATED cwiTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The name of the image.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cwiName :: Lens.Lens' CopyWorkspaceImage Lude.Text
cwiName = Lens.lens (name :: CopyWorkspaceImage -> Lude.Text) (\s a -> s {name = a} :: CopyWorkspaceImage)
{-# DEPRECATED cwiName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The identifier of the source image.
--
-- /Note:/ Consider using 'sourceImageId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cwiSourceImageId :: Lens.Lens' CopyWorkspaceImage Lude.Text
cwiSourceImageId = Lens.lens (sourceImageId :: CopyWorkspaceImage -> Lude.Text) (\s a -> s {sourceImageId = a} :: CopyWorkspaceImage)
{-# DEPRECATED cwiSourceImageId "Use generic-lens or generic-optics with 'sourceImageId' instead." #-}

-- | The identifier of the source Region.
--
-- /Note:/ Consider using 'sourceRegion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cwiSourceRegion :: Lens.Lens' CopyWorkspaceImage Lude.Text
cwiSourceRegion = Lens.lens (sourceRegion :: CopyWorkspaceImage -> Lude.Text) (\s a -> s {sourceRegion = a} :: CopyWorkspaceImage)
{-# DEPRECATED cwiSourceRegion "Use generic-lens or generic-optics with 'sourceRegion' instead." #-}

instance Lude.AWSRequest CopyWorkspaceImage where
  type Rs CopyWorkspaceImage = CopyWorkspaceImageResponse
  request = Req.postJSON workSpacesService
  response =
    Res.receiveJSON
      ( \s h x ->
          CopyWorkspaceImageResponse'
            Lude.<$> (x Lude..?> "ImageId") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CopyWorkspaceImage where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("WorkspacesService.CopyWorkspaceImage" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CopyWorkspaceImage where
  toJSON CopyWorkspaceImage' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Description" Lude..=) Lude.<$> description,
            ("Tags" Lude..=) Lude.<$> tags,
            Lude.Just ("Name" Lude..= name),
            Lude.Just ("SourceImageId" Lude..= sourceImageId),
            Lude.Just ("SourceRegion" Lude..= sourceRegion)
          ]
      )

instance Lude.ToPath CopyWorkspaceImage where
  toPath = Lude.const "/"

instance Lude.ToQuery CopyWorkspaceImage where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCopyWorkspaceImageResponse' smart constructor.
data CopyWorkspaceImageResponse = CopyWorkspaceImageResponse'
  { imageId ::
      Lude.Maybe Lude.Text,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CopyWorkspaceImageResponse' with the minimum fields required to make a request.
--
-- * 'imageId' - The identifier of the image.
-- * 'responseStatus' - The response status code.
mkCopyWorkspaceImageResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CopyWorkspaceImageResponse
mkCopyWorkspaceImageResponse pResponseStatus_ =
  CopyWorkspaceImageResponse'
    { imageId = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The identifier of the image.
--
-- /Note:/ Consider using 'imageId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cwirsImageId :: Lens.Lens' CopyWorkspaceImageResponse (Lude.Maybe Lude.Text)
cwirsImageId = Lens.lens (imageId :: CopyWorkspaceImageResponse -> Lude.Maybe Lude.Text) (\s a -> s {imageId = a} :: CopyWorkspaceImageResponse)
{-# DEPRECATED cwirsImageId "Use generic-lens or generic-optics with 'imageId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cwirsResponseStatus :: Lens.Lens' CopyWorkspaceImageResponse Lude.Int
cwirsResponseStatus = Lens.lens (responseStatus :: CopyWorkspaceImageResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CopyWorkspaceImageResponse)
{-# DEPRECATED cwirsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
