{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECR.InitiateLayerUpload
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Notifies Amazon ECR that you intend to upload an image layer.
--
-- When an image is pushed, the InitiateLayerUpload API is called once per image layer that has not already been uploaded. Whether or not an image layer has been uploaded is determined by the BatchCheckLayerAvailability API action.
module Network.AWS.ECR.InitiateLayerUpload
  ( -- * Creating a request
    InitiateLayerUpload (..),
    mkInitiateLayerUpload,

    -- ** Request lenses
    iluRegistryId,
    iluRepositoryName,

    -- * Destructuring the response
    InitiateLayerUploadResponse (..),
    mkInitiateLayerUploadResponse,

    -- ** Response lenses
    ilursPartSize,
    ilursUploadId,
    ilursResponseStatus,
  )
where

import Network.AWS.ECR.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkInitiateLayerUpload' smart constructor.
data InitiateLayerUpload = InitiateLayerUpload'
  { -- | The AWS account ID associated with the registry to which you intend to upload layers. If you do not specify a registry, the default registry is assumed.
    registryId :: Lude.Maybe Lude.Text,
    -- | The name of the repository to which you intend to upload layers.
    repositoryName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'InitiateLayerUpload' with the minimum fields required to make a request.
--
-- * 'registryId' - The AWS account ID associated with the registry to which you intend to upload layers. If you do not specify a registry, the default registry is assumed.
-- * 'repositoryName' - The name of the repository to which you intend to upload layers.
mkInitiateLayerUpload ::
  -- | 'repositoryName'
  Lude.Text ->
  InitiateLayerUpload
mkInitiateLayerUpload pRepositoryName_ =
  InitiateLayerUpload'
    { registryId = Lude.Nothing,
      repositoryName = pRepositoryName_
    }

-- | The AWS account ID associated with the registry to which you intend to upload layers. If you do not specify a registry, the default registry is assumed.
--
-- /Note:/ Consider using 'registryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iluRegistryId :: Lens.Lens' InitiateLayerUpload (Lude.Maybe Lude.Text)
iluRegistryId = Lens.lens (registryId :: InitiateLayerUpload -> Lude.Maybe Lude.Text) (\s a -> s {registryId = a} :: InitiateLayerUpload)
{-# DEPRECATED iluRegistryId "Use generic-lens or generic-optics with 'registryId' instead." #-}

-- | The name of the repository to which you intend to upload layers.
--
-- /Note:/ Consider using 'repositoryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iluRepositoryName :: Lens.Lens' InitiateLayerUpload Lude.Text
iluRepositoryName = Lens.lens (repositoryName :: InitiateLayerUpload -> Lude.Text) (\s a -> s {repositoryName = a} :: InitiateLayerUpload)
{-# DEPRECATED iluRepositoryName "Use generic-lens or generic-optics with 'repositoryName' instead." #-}

instance Lude.AWSRequest InitiateLayerUpload where
  type Rs InitiateLayerUpload = InitiateLayerUploadResponse
  request = Req.postJSON ecrService
  response =
    Res.receiveJSON
      ( \s h x ->
          InitiateLayerUploadResponse'
            Lude.<$> (x Lude..?> "partSize")
            Lude.<*> (x Lude..?> "uploadId")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders InitiateLayerUpload where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AmazonEC2ContainerRegistry_V20150921.InitiateLayerUpload" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON InitiateLayerUpload where
  toJSON InitiateLayerUpload' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("registryId" Lude..=) Lude.<$> registryId,
            Lude.Just ("repositoryName" Lude..= repositoryName)
          ]
      )

instance Lude.ToPath InitiateLayerUpload where
  toPath = Lude.const "/"

instance Lude.ToQuery InitiateLayerUpload where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkInitiateLayerUploadResponse' smart constructor.
data InitiateLayerUploadResponse = InitiateLayerUploadResponse'
  { -- | The size, in bytes, that Amazon ECR expects future layer part uploads to be.
    partSize :: Lude.Maybe Lude.Natural,
    -- | The upload ID for the layer upload. This parameter is passed to further 'UploadLayerPart' and 'CompleteLayerUpload' operations.
    uploadId :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'InitiateLayerUploadResponse' with the minimum fields required to make a request.
--
-- * 'partSize' - The size, in bytes, that Amazon ECR expects future layer part uploads to be.
-- * 'uploadId' - The upload ID for the layer upload. This parameter is passed to further 'UploadLayerPart' and 'CompleteLayerUpload' operations.
-- * 'responseStatus' - The response status code.
mkInitiateLayerUploadResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  InitiateLayerUploadResponse
mkInitiateLayerUploadResponse pResponseStatus_ =
  InitiateLayerUploadResponse'
    { partSize = Lude.Nothing,
      uploadId = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The size, in bytes, that Amazon ECR expects future layer part uploads to be.
--
-- /Note:/ Consider using 'partSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ilursPartSize :: Lens.Lens' InitiateLayerUploadResponse (Lude.Maybe Lude.Natural)
ilursPartSize = Lens.lens (partSize :: InitiateLayerUploadResponse -> Lude.Maybe Lude.Natural) (\s a -> s {partSize = a} :: InitiateLayerUploadResponse)
{-# DEPRECATED ilursPartSize "Use generic-lens or generic-optics with 'partSize' instead." #-}

-- | The upload ID for the layer upload. This parameter is passed to further 'UploadLayerPart' and 'CompleteLayerUpload' operations.
--
-- /Note:/ Consider using 'uploadId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ilursUploadId :: Lens.Lens' InitiateLayerUploadResponse (Lude.Maybe Lude.Text)
ilursUploadId = Lens.lens (uploadId :: InitiateLayerUploadResponse -> Lude.Maybe Lude.Text) (\s a -> s {uploadId = a} :: InitiateLayerUploadResponse)
{-# DEPRECATED ilursUploadId "Use generic-lens or generic-optics with 'uploadId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ilursResponseStatus :: Lens.Lens' InitiateLayerUploadResponse Lude.Int
ilursResponseStatus = Lens.lens (responseStatus :: InitiateLayerUploadResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: InitiateLayerUploadResponse)
{-# DEPRECATED ilursResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
