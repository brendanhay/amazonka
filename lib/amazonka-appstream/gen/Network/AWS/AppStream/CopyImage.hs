{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppStream.CopyImage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Copies the image within the same region or to a new region within the same AWS account. Note that any tags you added to the image will not be copied.
module Network.AWS.AppStream.CopyImage
  ( -- * Creating a request
    CopyImage (..),
    mkCopyImage,

    -- ** Request lenses
    ciDestinationRegion,
    ciDestinationImageName,
    ciSourceImageName,
    ciDestinationImageDescription,

    -- * Destructuring the response
    CopyImageResponse (..),
    mkCopyImageResponse,

    -- ** Response lenses
    cirsDestinationImageName,
    cirsResponseStatus,
  )
where

import Network.AWS.AppStream.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCopyImage' smart constructor.
data CopyImage = CopyImage'
  { -- | The destination region to which the image will be copied. This parameter is required, even if you are copying an image within the same region.
    destinationRegion :: Lude.Text,
    -- | The name that the image will have when it is copied to the destination.
    destinationImageName :: Lude.Text,
    -- | The name of the image to copy.
    sourceImageName :: Lude.Text,
    -- | The description that the image will have when it is copied to the destination.
    destinationImageDescription :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CopyImage' with the minimum fields required to make a request.
--
-- * 'destinationRegion' - The destination region to which the image will be copied. This parameter is required, even if you are copying an image within the same region.
-- * 'destinationImageName' - The name that the image will have when it is copied to the destination.
-- * 'sourceImageName' - The name of the image to copy.
-- * 'destinationImageDescription' - The description that the image will have when it is copied to the destination.
mkCopyImage ::
  -- | 'destinationRegion'
  Lude.Text ->
  -- | 'destinationImageName'
  Lude.Text ->
  -- | 'sourceImageName'
  Lude.Text ->
  CopyImage
mkCopyImage
  pDestinationRegion_
  pDestinationImageName_
  pSourceImageName_ =
    CopyImage'
      { destinationRegion = pDestinationRegion_,
        destinationImageName = pDestinationImageName_,
        sourceImageName = pSourceImageName_,
        destinationImageDescription = Lude.Nothing
      }

-- | The destination region to which the image will be copied. This parameter is required, even if you are copying an image within the same region.
--
-- /Note:/ Consider using 'destinationRegion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciDestinationRegion :: Lens.Lens' CopyImage Lude.Text
ciDestinationRegion = Lens.lens (destinationRegion :: CopyImage -> Lude.Text) (\s a -> s {destinationRegion = a} :: CopyImage)
{-# DEPRECATED ciDestinationRegion "Use generic-lens or generic-optics with 'destinationRegion' instead." #-}

-- | The name that the image will have when it is copied to the destination.
--
-- /Note:/ Consider using 'destinationImageName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciDestinationImageName :: Lens.Lens' CopyImage Lude.Text
ciDestinationImageName = Lens.lens (destinationImageName :: CopyImage -> Lude.Text) (\s a -> s {destinationImageName = a} :: CopyImage)
{-# DEPRECATED ciDestinationImageName "Use generic-lens or generic-optics with 'destinationImageName' instead." #-}

-- | The name of the image to copy.
--
-- /Note:/ Consider using 'sourceImageName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciSourceImageName :: Lens.Lens' CopyImage Lude.Text
ciSourceImageName = Lens.lens (sourceImageName :: CopyImage -> Lude.Text) (\s a -> s {sourceImageName = a} :: CopyImage)
{-# DEPRECATED ciSourceImageName "Use generic-lens or generic-optics with 'sourceImageName' instead." #-}

-- | The description that the image will have when it is copied to the destination.
--
-- /Note:/ Consider using 'destinationImageDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciDestinationImageDescription :: Lens.Lens' CopyImage (Lude.Maybe Lude.Text)
ciDestinationImageDescription = Lens.lens (destinationImageDescription :: CopyImage -> Lude.Maybe Lude.Text) (\s a -> s {destinationImageDescription = a} :: CopyImage)
{-# DEPRECATED ciDestinationImageDescription "Use generic-lens or generic-optics with 'destinationImageDescription' instead." #-}

instance Lude.AWSRequest CopyImage where
  type Rs CopyImage = CopyImageResponse
  request = Req.postJSON appStreamService
  response =
    Res.receiveJSON
      ( \s h x ->
          CopyImageResponse'
            Lude.<$> (x Lude..?> "DestinationImageName")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CopyImage where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("PhotonAdminProxyService.CopyImage" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CopyImage where
  toJSON CopyImage' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("DestinationRegion" Lude..= destinationRegion),
            Lude.Just ("DestinationImageName" Lude..= destinationImageName),
            Lude.Just ("SourceImageName" Lude..= sourceImageName),
            ("DestinationImageDescription" Lude..=)
              Lude.<$> destinationImageDescription
          ]
      )

instance Lude.ToPath CopyImage where
  toPath = Lude.const "/"

instance Lude.ToQuery CopyImage where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCopyImageResponse' smart constructor.
data CopyImageResponse = CopyImageResponse'
  { -- | The name of the destination image.
    destinationImageName :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CopyImageResponse' with the minimum fields required to make a request.
--
-- * 'destinationImageName' - The name of the destination image.
-- * 'responseStatus' - The response status code.
mkCopyImageResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CopyImageResponse
mkCopyImageResponse pResponseStatus_ =
  CopyImageResponse'
    { destinationImageName = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The name of the destination image.
--
-- /Note:/ Consider using 'destinationImageName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cirsDestinationImageName :: Lens.Lens' CopyImageResponse (Lude.Maybe Lude.Text)
cirsDestinationImageName = Lens.lens (destinationImageName :: CopyImageResponse -> Lude.Maybe Lude.Text) (\s a -> s {destinationImageName = a} :: CopyImageResponse)
{-# DEPRECATED cirsDestinationImageName "Use generic-lens or generic-optics with 'destinationImageName' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cirsResponseStatus :: Lens.Lens' CopyImageResponse Lude.Int
cirsResponseStatus = Lens.lens (responseStatus :: CopyImageResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CopyImageResponse)
{-# DEPRECATED cirsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
