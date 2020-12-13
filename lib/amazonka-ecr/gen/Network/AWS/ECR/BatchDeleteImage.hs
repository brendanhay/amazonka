{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECR.BatchDeleteImage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a list of specified images within a repository. Images are specified with either an @imageTag@ or @imageDigest@ .
--
-- You can remove a tag from an image by specifying the image's tag in your request. When you remove the last tag from an image, the image is deleted from your repository.
-- You can completely delete an image (and all of its tags) by specifying the image's digest in your request.
module Network.AWS.ECR.BatchDeleteImage
  ( -- * Creating a request
    BatchDeleteImage (..),
    mkBatchDeleteImage,

    -- ** Request lenses
    bdiRegistryId,
    bdiImageIds,
    bdiRepositoryName,

    -- * Destructuring the response
    BatchDeleteImageResponse (..),
    mkBatchDeleteImageResponse,

    -- ** Response lenses
    bdirsFailures,
    bdirsImageIds,
    bdirsResponseStatus,
  )
where

import Network.AWS.ECR.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Deletes specified images within a specified repository. Images are specified with either the @imageTag@ or @imageDigest@ .
--
-- /See:/ 'mkBatchDeleteImage' smart constructor.
data BatchDeleteImage = BatchDeleteImage'
  { -- | The AWS account ID associated with the registry that contains the image to delete. If you do not specify a registry, the default registry is assumed.
    registryId :: Lude.Maybe Lude.Text,
    -- | A list of image ID references that correspond to images to delete. The format of the @imageIds@ reference is @imageTag=tag@ or @imageDigest=digest@ .
    imageIds :: [ImageIdentifier],
    -- | The repository that contains the image to delete.
    repositoryName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BatchDeleteImage' with the minimum fields required to make a request.
--
-- * 'registryId' - The AWS account ID associated with the registry that contains the image to delete. If you do not specify a registry, the default registry is assumed.
-- * 'imageIds' - A list of image ID references that correspond to images to delete. The format of the @imageIds@ reference is @imageTag=tag@ or @imageDigest=digest@ .
-- * 'repositoryName' - The repository that contains the image to delete.
mkBatchDeleteImage ::
  -- | 'repositoryName'
  Lude.Text ->
  BatchDeleteImage
mkBatchDeleteImage pRepositoryName_ =
  BatchDeleteImage'
    { registryId = Lude.Nothing,
      imageIds = Lude.mempty,
      repositoryName = pRepositoryName_
    }

-- | The AWS account ID associated with the registry that contains the image to delete. If you do not specify a registry, the default registry is assumed.
--
-- /Note:/ Consider using 'registryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdiRegistryId :: Lens.Lens' BatchDeleteImage (Lude.Maybe Lude.Text)
bdiRegistryId = Lens.lens (registryId :: BatchDeleteImage -> Lude.Maybe Lude.Text) (\s a -> s {registryId = a} :: BatchDeleteImage)
{-# DEPRECATED bdiRegistryId "Use generic-lens or generic-optics with 'registryId' instead." #-}

-- | A list of image ID references that correspond to images to delete. The format of the @imageIds@ reference is @imageTag=tag@ or @imageDigest=digest@ .
--
-- /Note:/ Consider using 'imageIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdiImageIds :: Lens.Lens' BatchDeleteImage [ImageIdentifier]
bdiImageIds = Lens.lens (imageIds :: BatchDeleteImage -> [ImageIdentifier]) (\s a -> s {imageIds = a} :: BatchDeleteImage)
{-# DEPRECATED bdiImageIds "Use generic-lens or generic-optics with 'imageIds' instead." #-}

-- | The repository that contains the image to delete.
--
-- /Note:/ Consider using 'repositoryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdiRepositoryName :: Lens.Lens' BatchDeleteImage Lude.Text
bdiRepositoryName = Lens.lens (repositoryName :: BatchDeleteImage -> Lude.Text) (\s a -> s {repositoryName = a} :: BatchDeleteImage)
{-# DEPRECATED bdiRepositoryName "Use generic-lens or generic-optics with 'repositoryName' instead." #-}

instance Lude.AWSRequest BatchDeleteImage where
  type Rs BatchDeleteImage = BatchDeleteImageResponse
  request = Req.postJSON ecrService
  response =
    Res.receiveJSON
      ( \s h x ->
          BatchDeleteImageResponse'
            Lude.<$> (x Lude..?> "failures" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "imageIds" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders BatchDeleteImage where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AmazonEC2ContainerRegistry_V20150921.BatchDeleteImage" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON BatchDeleteImage where
  toJSON BatchDeleteImage' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("registryId" Lude..=) Lude.<$> registryId,
            Lude.Just ("imageIds" Lude..= imageIds),
            Lude.Just ("repositoryName" Lude..= repositoryName)
          ]
      )

instance Lude.ToPath BatchDeleteImage where
  toPath = Lude.const "/"

instance Lude.ToQuery BatchDeleteImage where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkBatchDeleteImageResponse' smart constructor.
data BatchDeleteImageResponse = BatchDeleteImageResponse'
  { -- | Any failures associated with the call.
    failures :: Lude.Maybe [ImageFailure],
    -- | The image IDs of the deleted images.
    imageIds :: Lude.Maybe [ImageIdentifier],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BatchDeleteImageResponse' with the minimum fields required to make a request.
--
-- * 'failures' - Any failures associated with the call.
-- * 'imageIds' - The image IDs of the deleted images.
-- * 'responseStatus' - The response status code.
mkBatchDeleteImageResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  BatchDeleteImageResponse
mkBatchDeleteImageResponse pResponseStatus_ =
  BatchDeleteImageResponse'
    { failures = Lude.Nothing,
      imageIds = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Any failures associated with the call.
--
-- /Note:/ Consider using 'failures' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdirsFailures :: Lens.Lens' BatchDeleteImageResponse (Lude.Maybe [ImageFailure])
bdirsFailures = Lens.lens (failures :: BatchDeleteImageResponse -> Lude.Maybe [ImageFailure]) (\s a -> s {failures = a} :: BatchDeleteImageResponse)
{-# DEPRECATED bdirsFailures "Use generic-lens or generic-optics with 'failures' instead." #-}

-- | The image IDs of the deleted images.
--
-- /Note:/ Consider using 'imageIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdirsImageIds :: Lens.Lens' BatchDeleteImageResponse (Lude.Maybe [ImageIdentifier])
bdirsImageIds = Lens.lens (imageIds :: BatchDeleteImageResponse -> Lude.Maybe [ImageIdentifier]) (\s a -> s {imageIds = a} :: BatchDeleteImageResponse)
{-# DEPRECATED bdirsImageIds "Use generic-lens or generic-optics with 'imageIds' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdirsResponseStatus :: Lens.Lens' BatchDeleteImageResponse Lude.Int
bdirsResponseStatus = Lens.lens (responseStatus :: BatchDeleteImageResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: BatchDeleteImageResponse)
{-# DEPRECATED bdirsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
