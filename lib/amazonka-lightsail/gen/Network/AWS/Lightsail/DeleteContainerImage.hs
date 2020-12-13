{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.DeleteContainerImage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a container image that is registered to your Amazon Lightsail container service.
module Network.AWS.Lightsail.DeleteContainerImage
  ( -- * Creating a request
    DeleteContainerImage (..),
    mkDeleteContainerImage,

    -- ** Request lenses
    dciImage,
    dciServiceName,

    -- * Destructuring the response
    DeleteContainerImageResponse (..),
    mkDeleteContainerImageResponse,

    -- ** Response lenses
    dcirsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteContainerImage' smart constructor.
data DeleteContainerImage = DeleteContainerImage'
  { -- | The name of the container image to delete from the container service.
    --
    -- Use the @GetContainerImages@ action to get the name of the container images that are registered to a container service.
    image :: Lude.Text,
    -- | The name of the container service for which to delete a registered container image.
    serviceName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteContainerImage' with the minimum fields required to make a request.
--
-- * 'image' - The name of the container image to delete from the container service.
--
-- Use the @GetContainerImages@ action to get the name of the container images that are registered to a container service.
-- * 'serviceName' - The name of the container service for which to delete a registered container image.
mkDeleteContainerImage ::
  -- | 'image'
  Lude.Text ->
  -- | 'serviceName'
  Lude.Text ->
  DeleteContainerImage
mkDeleteContainerImage pImage_ pServiceName_ =
  DeleteContainerImage'
    { image = pImage_,
      serviceName = pServiceName_
    }

-- | The name of the container image to delete from the container service.
--
-- Use the @GetContainerImages@ action to get the name of the container images that are registered to a container service.
--
-- /Note:/ Consider using 'image' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dciImage :: Lens.Lens' DeleteContainerImage Lude.Text
dciImage = Lens.lens (image :: DeleteContainerImage -> Lude.Text) (\s a -> s {image = a} :: DeleteContainerImage)
{-# DEPRECATED dciImage "Use generic-lens or generic-optics with 'image' instead." #-}

-- | The name of the container service for which to delete a registered container image.
--
-- /Note:/ Consider using 'serviceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dciServiceName :: Lens.Lens' DeleteContainerImage Lude.Text
dciServiceName = Lens.lens (serviceName :: DeleteContainerImage -> Lude.Text) (\s a -> s {serviceName = a} :: DeleteContainerImage)
{-# DEPRECATED dciServiceName "Use generic-lens or generic-optics with 'serviceName' instead." #-}

instance Lude.AWSRequest DeleteContainerImage where
  type Rs DeleteContainerImage = DeleteContainerImageResponse
  request = Req.postJSON lightsailService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DeleteContainerImageResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteContainerImage where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Lightsail_20161128.DeleteContainerImage" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteContainerImage where
  toJSON DeleteContainerImage' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("image" Lude..= image),
            Lude.Just ("serviceName" Lude..= serviceName)
          ]
      )

instance Lude.ToPath DeleteContainerImage where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteContainerImage where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteContainerImageResponse' smart constructor.
newtype DeleteContainerImageResponse = DeleteContainerImageResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteContainerImageResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDeleteContainerImageResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteContainerImageResponse
mkDeleteContainerImageResponse pResponseStatus_ =
  DeleteContainerImageResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcirsResponseStatus :: Lens.Lens' DeleteContainerImageResponse Lude.Int
dcirsResponseStatus = Lens.lens (responseStatus :: DeleteContainerImageResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteContainerImageResponse)
{-# DEPRECATED dcirsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
