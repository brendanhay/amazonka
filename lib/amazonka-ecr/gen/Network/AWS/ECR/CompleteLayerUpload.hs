{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECR.CompleteLayerUpload
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Informs Amazon ECR that the image layer upload has completed for a specified registry, repository name, and upload ID. You can optionally provide a @sha256@ digest of the image layer for data validation purposes.
--
-- When an image is pushed, the CompleteLayerUpload API is called once per each new image layer to verify that the upload has completed.
module Network.AWS.ECR.CompleteLayerUpload
  ( -- * Creating a request
    CompleteLayerUpload (..),
    mkCompleteLayerUpload,

    -- ** Request lenses
    cluRegistryId,
    cluRepositoryName,
    cluUploadId,
    cluLayerDigests,

    -- * Destructuring the response
    CompleteLayerUploadResponse (..),
    mkCompleteLayerUploadResponse,

    -- ** Response lenses
    clursRegistryId,
    clursLayerDigest,
    clursRepositoryName,
    clursUploadId,
    clursResponseStatus,
  )
where

import Network.AWS.ECR.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCompleteLayerUpload' smart constructor.
data CompleteLayerUpload = CompleteLayerUpload'
  { registryId ::
      Lude.Maybe Lude.Text,
    repositoryName :: Lude.Text,
    uploadId :: Lude.Text,
    layerDigests :: Lude.NonEmpty Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CompleteLayerUpload' with the minimum fields required to make a request.
--
-- * 'layerDigests' - The @sha256@ digest of the image layer.
-- * 'registryId' - The AWS account ID associated with the registry to which to upload layers. If you do not specify a registry, the default registry is assumed.
-- * 'repositoryName' - The name of the repository to associate with the image layer.
-- * 'uploadId' - The upload ID from a previous 'InitiateLayerUpload' operation to associate with the image layer.
mkCompleteLayerUpload ::
  -- | 'repositoryName'
  Lude.Text ->
  -- | 'uploadId'
  Lude.Text ->
  -- | 'layerDigests'
  Lude.NonEmpty Lude.Text ->
  CompleteLayerUpload
mkCompleteLayerUpload pRepositoryName_ pUploadId_ pLayerDigests_ =
  CompleteLayerUpload'
    { registryId = Lude.Nothing,
      repositoryName = pRepositoryName_,
      uploadId = pUploadId_,
      layerDigests = pLayerDigests_
    }

-- | The AWS account ID associated with the registry to which to upload layers. If you do not specify a registry, the default registry is assumed.
--
-- /Note:/ Consider using 'registryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cluRegistryId :: Lens.Lens' CompleteLayerUpload (Lude.Maybe Lude.Text)
cluRegistryId = Lens.lens (registryId :: CompleteLayerUpload -> Lude.Maybe Lude.Text) (\s a -> s {registryId = a} :: CompleteLayerUpload)
{-# DEPRECATED cluRegistryId "Use generic-lens or generic-optics with 'registryId' instead." #-}

-- | The name of the repository to associate with the image layer.
--
-- /Note:/ Consider using 'repositoryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cluRepositoryName :: Lens.Lens' CompleteLayerUpload Lude.Text
cluRepositoryName = Lens.lens (repositoryName :: CompleteLayerUpload -> Lude.Text) (\s a -> s {repositoryName = a} :: CompleteLayerUpload)
{-# DEPRECATED cluRepositoryName "Use generic-lens or generic-optics with 'repositoryName' instead." #-}

-- | The upload ID from a previous 'InitiateLayerUpload' operation to associate with the image layer.
--
-- /Note:/ Consider using 'uploadId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cluUploadId :: Lens.Lens' CompleteLayerUpload Lude.Text
cluUploadId = Lens.lens (uploadId :: CompleteLayerUpload -> Lude.Text) (\s a -> s {uploadId = a} :: CompleteLayerUpload)
{-# DEPRECATED cluUploadId "Use generic-lens or generic-optics with 'uploadId' instead." #-}

-- | The @sha256@ digest of the image layer.
--
-- /Note:/ Consider using 'layerDigests' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cluLayerDigests :: Lens.Lens' CompleteLayerUpload (Lude.NonEmpty Lude.Text)
cluLayerDigests = Lens.lens (layerDigests :: CompleteLayerUpload -> Lude.NonEmpty Lude.Text) (\s a -> s {layerDigests = a} :: CompleteLayerUpload)
{-# DEPRECATED cluLayerDigests "Use generic-lens or generic-optics with 'layerDigests' instead." #-}

instance Lude.AWSRequest CompleteLayerUpload where
  type Rs CompleteLayerUpload = CompleteLayerUploadResponse
  request = Req.postJSON ecrService
  response =
    Res.receiveJSON
      ( \s h x ->
          CompleteLayerUploadResponse'
            Lude.<$> (x Lude..?> "registryId")
            Lude.<*> (x Lude..?> "layerDigest")
            Lude.<*> (x Lude..?> "repositoryName")
            Lude.<*> (x Lude..?> "uploadId")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CompleteLayerUpload where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AmazonEC2ContainerRegistry_V20150921.CompleteLayerUpload" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CompleteLayerUpload where
  toJSON CompleteLayerUpload' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("registryId" Lude..=) Lude.<$> registryId,
            Lude.Just ("repositoryName" Lude..= repositoryName),
            Lude.Just ("uploadId" Lude..= uploadId),
            Lude.Just ("layerDigests" Lude..= layerDigests)
          ]
      )

instance Lude.ToPath CompleteLayerUpload where
  toPath = Lude.const "/"

instance Lude.ToQuery CompleteLayerUpload where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCompleteLayerUploadResponse' smart constructor.
data CompleteLayerUploadResponse = CompleteLayerUploadResponse'
  { registryId ::
      Lude.Maybe Lude.Text,
    layerDigest :: Lude.Maybe Lude.Text,
    repositoryName ::
      Lude.Maybe Lude.Text,
    uploadId :: Lude.Maybe Lude.Text,
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

-- | Creates a value of 'CompleteLayerUploadResponse' with the minimum fields required to make a request.
--
-- * 'layerDigest' - The @sha256@ digest of the image layer.
-- * 'registryId' - The registry ID associated with the request.
-- * 'repositoryName' - The repository name associated with the request.
-- * 'responseStatus' - The response status code.
-- * 'uploadId' - The upload ID associated with the layer.
mkCompleteLayerUploadResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CompleteLayerUploadResponse
mkCompleteLayerUploadResponse pResponseStatus_ =
  CompleteLayerUploadResponse'
    { registryId = Lude.Nothing,
      layerDigest = Lude.Nothing,
      repositoryName = Lude.Nothing,
      uploadId = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The registry ID associated with the request.
--
-- /Note:/ Consider using 'registryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clursRegistryId :: Lens.Lens' CompleteLayerUploadResponse (Lude.Maybe Lude.Text)
clursRegistryId = Lens.lens (registryId :: CompleteLayerUploadResponse -> Lude.Maybe Lude.Text) (\s a -> s {registryId = a} :: CompleteLayerUploadResponse)
{-# DEPRECATED clursRegistryId "Use generic-lens or generic-optics with 'registryId' instead." #-}

-- | The @sha256@ digest of the image layer.
--
-- /Note:/ Consider using 'layerDigest' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clursLayerDigest :: Lens.Lens' CompleteLayerUploadResponse (Lude.Maybe Lude.Text)
clursLayerDigest = Lens.lens (layerDigest :: CompleteLayerUploadResponse -> Lude.Maybe Lude.Text) (\s a -> s {layerDigest = a} :: CompleteLayerUploadResponse)
{-# DEPRECATED clursLayerDigest "Use generic-lens or generic-optics with 'layerDigest' instead." #-}

-- | The repository name associated with the request.
--
-- /Note:/ Consider using 'repositoryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clursRepositoryName :: Lens.Lens' CompleteLayerUploadResponse (Lude.Maybe Lude.Text)
clursRepositoryName = Lens.lens (repositoryName :: CompleteLayerUploadResponse -> Lude.Maybe Lude.Text) (\s a -> s {repositoryName = a} :: CompleteLayerUploadResponse)
{-# DEPRECATED clursRepositoryName "Use generic-lens or generic-optics with 'repositoryName' instead." #-}

-- | The upload ID associated with the layer.
--
-- /Note:/ Consider using 'uploadId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clursUploadId :: Lens.Lens' CompleteLayerUploadResponse (Lude.Maybe Lude.Text)
clursUploadId = Lens.lens (uploadId :: CompleteLayerUploadResponse -> Lude.Maybe Lude.Text) (\s a -> s {uploadId = a} :: CompleteLayerUploadResponse)
{-# DEPRECATED clursUploadId "Use generic-lens or generic-optics with 'uploadId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clursResponseStatus :: Lens.Lens' CompleteLayerUploadResponse Lude.Int
clursResponseStatus = Lens.lens (responseStatus :: CompleteLayerUploadResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CompleteLayerUploadResponse)
{-# DEPRECATED clursResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
