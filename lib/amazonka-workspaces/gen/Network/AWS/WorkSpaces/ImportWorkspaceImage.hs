{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.ImportWorkspaceImage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Imports the specified Windows 10 Bring Your Own License (BYOL) image into Amazon WorkSpaces. The image must be an already licensed Amazon EC2 image that is in your AWS account, and you must own the image. For more information about creating BYOL images, see <https://docs.aws.amazon.com/workspaces/latest/adminguide/byol-windows-images.html Bring Your Own Windows Desktop Licenses> .
module Network.AWS.WorkSpaces.ImportWorkspaceImage
  ( -- * Creating a request
    ImportWorkspaceImage (..),
    mkImportWorkspaceImage,

    -- ** Request lenses
    iwiEC2ImageId,
    iwiIngestionProcess,
    iwiImageName,
    iwiApplications,
    iwiTags,
    iwiImageDescription,

    -- * Destructuring the response
    ImportWorkspaceImageResponse (..),
    mkImportWorkspaceImageResponse,

    -- ** Response lenses
    iwirsImageId,
    iwirsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.WorkSpaces.Types

-- | /See:/ 'mkImportWorkspaceImage' smart constructor.
data ImportWorkspaceImage = ImportWorkspaceImage'
  { -- | The identifier of the EC2 image.
    ec2ImageId :: Lude.Text,
    -- | The ingestion process to be used when importing the image. For non-GPU-enabled bundles (bundles other than Graphics or GraphicsPro), specify @BYOL_REGULAR@ .
    ingestionProcess :: WorkspaceImageIngestionProcess,
    -- | The name of the WorkSpace image.
    imageName :: Lude.Text,
    -- | If specified, the version of Microsoft Office to subscribe to. Valid only for Windows 10 BYOL images. For more information about subscribing to Office for BYOL images, see <https://docs.aws.amazon.com/workspaces/latest/adminguide/byol-windows-images.html Bring Your Own Windows Desktop Licenses> .
    applications :: Lude.Maybe (Lude.NonEmpty Application),
    -- | The tags. Each WorkSpaces resource can have a maximum of 50 tags.
    tags :: Lude.Maybe [Tag],
    -- | The description of the WorkSpace image.
    imageDescription :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ImportWorkspaceImage' with the minimum fields required to make a request.
--
-- * 'ec2ImageId' - The identifier of the EC2 image.
-- * 'ingestionProcess' - The ingestion process to be used when importing the image. For non-GPU-enabled bundles (bundles other than Graphics or GraphicsPro), specify @BYOL_REGULAR@ .
-- * 'imageName' - The name of the WorkSpace image.
-- * 'applications' - If specified, the version of Microsoft Office to subscribe to. Valid only for Windows 10 BYOL images. For more information about subscribing to Office for BYOL images, see <https://docs.aws.amazon.com/workspaces/latest/adminguide/byol-windows-images.html Bring Your Own Windows Desktop Licenses> .
-- * 'tags' - The tags. Each WorkSpaces resource can have a maximum of 50 tags.
-- * 'imageDescription' - The description of the WorkSpace image.
mkImportWorkspaceImage ::
  -- | 'ec2ImageId'
  Lude.Text ->
  -- | 'ingestionProcess'
  WorkspaceImageIngestionProcess ->
  -- | 'imageName'
  Lude.Text ->
  -- | 'imageDescription'
  Lude.Text ->
  ImportWorkspaceImage
mkImportWorkspaceImage
  pEC2ImageId_
  pIngestionProcess_
  pImageName_
  pImageDescription_ =
    ImportWorkspaceImage'
      { ec2ImageId = pEC2ImageId_,
        ingestionProcess = pIngestionProcess_,
        imageName = pImageName_,
        applications = Lude.Nothing,
        tags = Lude.Nothing,
        imageDescription = pImageDescription_
      }

-- | The identifier of the EC2 image.
--
-- /Note:/ Consider using 'ec2ImageId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iwiEC2ImageId :: Lens.Lens' ImportWorkspaceImage Lude.Text
iwiEC2ImageId = Lens.lens (ec2ImageId :: ImportWorkspaceImage -> Lude.Text) (\s a -> s {ec2ImageId = a} :: ImportWorkspaceImage)
{-# DEPRECATED iwiEC2ImageId "Use generic-lens or generic-optics with 'ec2ImageId' instead." #-}

-- | The ingestion process to be used when importing the image. For non-GPU-enabled bundles (bundles other than Graphics or GraphicsPro), specify @BYOL_REGULAR@ .
--
-- /Note:/ Consider using 'ingestionProcess' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iwiIngestionProcess :: Lens.Lens' ImportWorkspaceImage WorkspaceImageIngestionProcess
iwiIngestionProcess = Lens.lens (ingestionProcess :: ImportWorkspaceImage -> WorkspaceImageIngestionProcess) (\s a -> s {ingestionProcess = a} :: ImportWorkspaceImage)
{-# DEPRECATED iwiIngestionProcess "Use generic-lens or generic-optics with 'ingestionProcess' instead." #-}

-- | The name of the WorkSpace image.
--
-- /Note:/ Consider using 'imageName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iwiImageName :: Lens.Lens' ImportWorkspaceImage Lude.Text
iwiImageName = Lens.lens (imageName :: ImportWorkspaceImage -> Lude.Text) (\s a -> s {imageName = a} :: ImportWorkspaceImage)
{-# DEPRECATED iwiImageName "Use generic-lens or generic-optics with 'imageName' instead." #-}

-- | If specified, the version of Microsoft Office to subscribe to. Valid only for Windows 10 BYOL images. For more information about subscribing to Office for BYOL images, see <https://docs.aws.amazon.com/workspaces/latest/adminguide/byol-windows-images.html Bring Your Own Windows Desktop Licenses> .
--
-- /Note:/ Consider using 'applications' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iwiApplications :: Lens.Lens' ImportWorkspaceImage (Lude.Maybe (Lude.NonEmpty Application))
iwiApplications = Lens.lens (applications :: ImportWorkspaceImage -> Lude.Maybe (Lude.NonEmpty Application)) (\s a -> s {applications = a} :: ImportWorkspaceImage)
{-# DEPRECATED iwiApplications "Use generic-lens or generic-optics with 'applications' instead." #-}

-- | The tags. Each WorkSpaces resource can have a maximum of 50 tags.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iwiTags :: Lens.Lens' ImportWorkspaceImage (Lude.Maybe [Tag])
iwiTags = Lens.lens (tags :: ImportWorkspaceImage -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: ImportWorkspaceImage)
{-# DEPRECATED iwiTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The description of the WorkSpace image.
--
-- /Note:/ Consider using 'imageDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iwiImageDescription :: Lens.Lens' ImportWorkspaceImage Lude.Text
iwiImageDescription = Lens.lens (imageDescription :: ImportWorkspaceImage -> Lude.Text) (\s a -> s {imageDescription = a} :: ImportWorkspaceImage)
{-# DEPRECATED iwiImageDescription "Use generic-lens or generic-optics with 'imageDescription' instead." #-}

instance Lude.AWSRequest ImportWorkspaceImage where
  type Rs ImportWorkspaceImage = ImportWorkspaceImageResponse
  request = Req.postJSON workSpacesService
  response =
    Res.receiveJSON
      ( \s h x ->
          ImportWorkspaceImageResponse'
            Lude.<$> (x Lude..?> "ImageId") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ImportWorkspaceImage where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("WorkspacesService.ImportWorkspaceImage" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ImportWorkspaceImage where
  toJSON ImportWorkspaceImage' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("Ec2ImageId" Lude..= ec2ImageId),
            Lude.Just ("IngestionProcess" Lude..= ingestionProcess),
            Lude.Just ("ImageName" Lude..= imageName),
            ("Applications" Lude..=) Lude.<$> applications,
            ("Tags" Lude..=) Lude.<$> tags,
            Lude.Just ("ImageDescription" Lude..= imageDescription)
          ]
      )

instance Lude.ToPath ImportWorkspaceImage where
  toPath = Lude.const "/"

instance Lude.ToQuery ImportWorkspaceImage where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkImportWorkspaceImageResponse' smart constructor.
data ImportWorkspaceImageResponse = ImportWorkspaceImageResponse'
  { -- | The identifier of the WorkSpace image.
    imageId :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ImportWorkspaceImageResponse' with the minimum fields required to make a request.
--
-- * 'imageId' - The identifier of the WorkSpace image.
-- * 'responseStatus' - The response status code.
mkImportWorkspaceImageResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ImportWorkspaceImageResponse
mkImportWorkspaceImageResponse pResponseStatus_ =
  ImportWorkspaceImageResponse'
    { imageId = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The identifier of the WorkSpace image.
--
-- /Note:/ Consider using 'imageId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iwirsImageId :: Lens.Lens' ImportWorkspaceImageResponse (Lude.Maybe Lude.Text)
iwirsImageId = Lens.lens (imageId :: ImportWorkspaceImageResponse -> Lude.Maybe Lude.Text) (\s a -> s {imageId = a} :: ImportWorkspaceImageResponse)
{-# DEPRECATED iwirsImageId "Use generic-lens or generic-optics with 'imageId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iwirsResponseStatus :: Lens.Lens' ImportWorkspaceImageResponse Lude.Int
iwirsResponseStatus = Lens.lens (responseStatus :: ImportWorkspaceImageResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ImportWorkspaceImageResponse)
{-# DEPRECATED iwirsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
