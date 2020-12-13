{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECR.UploadLayerPart
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Uploads an image layer part to Amazon ECR.
--
-- When an image is pushed, each new image layer is uploaded in parts. The maximum size of each image layer part can be 20971520 bytes (or about 20MB). The UploadLayerPart API is called once per each new image layer part.
module Network.AWS.ECR.UploadLayerPart
  ( -- * Creating a request
    UploadLayerPart (..),
    mkUploadLayerPart,

    -- ** Request lenses
    ulpRegistryId,
    ulpPartFirstByte,
    ulpLayerPartBlob,
    ulpPartLastByte,
    ulpRepositoryName,
    ulpUploadId,

    -- * Destructuring the response
    UploadLayerPartResponse (..),
    mkUploadLayerPartResponse,

    -- ** Response lenses
    ulprsRegistryId,
    ulprsLastByteReceived,
    ulprsRepositoryName,
    ulprsUploadId,
    ulprsResponseStatus,
  )
where

import Network.AWS.ECR.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUploadLayerPart' smart constructor.
data UploadLayerPart = UploadLayerPart'
  { -- | The AWS account ID associated with the registry to which you are uploading layer parts. If you do not specify a registry, the default registry is assumed.
    registryId :: Lude.Maybe Lude.Text,
    -- | The position of the first byte of the layer part witin the overall image layer.
    partFirstByte :: Lude.Natural,
    -- | The base64-encoded layer part payload.
    layerPartBlob :: Lude.Base64,
    -- | The position of the last byte of the layer part within the overall image layer.
    partLastByte :: Lude.Natural,
    -- | The name of the repository to which you are uploading layer parts.
    repositoryName :: Lude.Text,
    -- | The upload ID from a previous 'InitiateLayerUpload' operation to associate with the layer part upload.
    uploadId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UploadLayerPart' with the minimum fields required to make a request.
--
-- * 'registryId' - The AWS account ID associated with the registry to which you are uploading layer parts. If you do not specify a registry, the default registry is assumed.
-- * 'partFirstByte' - The position of the first byte of the layer part witin the overall image layer.
-- * 'layerPartBlob' - The base64-encoded layer part payload.
-- * 'partLastByte' - The position of the last byte of the layer part within the overall image layer.
-- * 'repositoryName' - The name of the repository to which you are uploading layer parts.
-- * 'uploadId' - The upload ID from a previous 'InitiateLayerUpload' operation to associate with the layer part upload.
mkUploadLayerPart ::
  -- | 'partFirstByte'
  Lude.Natural ->
  -- | 'layerPartBlob'
  Lude.Base64 ->
  -- | 'partLastByte'
  Lude.Natural ->
  -- | 'repositoryName'
  Lude.Text ->
  -- | 'uploadId'
  Lude.Text ->
  UploadLayerPart
mkUploadLayerPart
  pPartFirstByte_
  pLayerPartBlob_
  pPartLastByte_
  pRepositoryName_
  pUploadId_ =
    UploadLayerPart'
      { registryId = Lude.Nothing,
        partFirstByte = pPartFirstByte_,
        layerPartBlob = pLayerPartBlob_,
        partLastByte = pPartLastByte_,
        repositoryName = pRepositoryName_,
        uploadId = pUploadId_
      }

-- | The AWS account ID associated with the registry to which you are uploading layer parts. If you do not specify a registry, the default registry is assumed.
--
-- /Note:/ Consider using 'registryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ulpRegistryId :: Lens.Lens' UploadLayerPart (Lude.Maybe Lude.Text)
ulpRegistryId = Lens.lens (registryId :: UploadLayerPart -> Lude.Maybe Lude.Text) (\s a -> s {registryId = a} :: UploadLayerPart)
{-# DEPRECATED ulpRegistryId "Use generic-lens or generic-optics with 'registryId' instead." #-}

-- | The position of the first byte of the layer part witin the overall image layer.
--
-- /Note:/ Consider using 'partFirstByte' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ulpPartFirstByte :: Lens.Lens' UploadLayerPart Lude.Natural
ulpPartFirstByte = Lens.lens (partFirstByte :: UploadLayerPart -> Lude.Natural) (\s a -> s {partFirstByte = a} :: UploadLayerPart)
{-# DEPRECATED ulpPartFirstByte "Use generic-lens or generic-optics with 'partFirstByte' instead." #-}

-- | The base64-encoded layer part payload.--
-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- The underlying isomorphism will encode to Base64 representation during
-- serialisation, and decode from Base64 representation during deserialisation.
-- This 'Lens' accepts and returns only raw unencoded data.
--
-- /Note:/ Consider using 'layerPartBlob' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ulpLayerPartBlob :: Lens.Lens' UploadLayerPart Lude.Base64
ulpLayerPartBlob = Lens.lens (layerPartBlob :: UploadLayerPart -> Lude.Base64) (\s a -> s {layerPartBlob = a} :: UploadLayerPart)
{-# DEPRECATED ulpLayerPartBlob "Use generic-lens or generic-optics with 'layerPartBlob' instead." #-}

-- | The position of the last byte of the layer part within the overall image layer.
--
-- /Note:/ Consider using 'partLastByte' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ulpPartLastByte :: Lens.Lens' UploadLayerPart Lude.Natural
ulpPartLastByte = Lens.lens (partLastByte :: UploadLayerPart -> Lude.Natural) (\s a -> s {partLastByte = a} :: UploadLayerPart)
{-# DEPRECATED ulpPartLastByte "Use generic-lens or generic-optics with 'partLastByte' instead." #-}

-- | The name of the repository to which you are uploading layer parts.
--
-- /Note:/ Consider using 'repositoryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ulpRepositoryName :: Lens.Lens' UploadLayerPart Lude.Text
ulpRepositoryName = Lens.lens (repositoryName :: UploadLayerPart -> Lude.Text) (\s a -> s {repositoryName = a} :: UploadLayerPart)
{-# DEPRECATED ulpRepositoryName "Use generic-lens or generic-optics with 'repositoryName' instead." #-}

-- | The upload ID from a previous 'InitiateLayerUpload' operation to associate with the layer part upload.
--
-- /Note:/ Consider using 'uploadId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ulpUploadId :: Lens.Lens' UploadLayerPart Lude.Text
ulpUploadId = Lens.lens (uploadId :: UploadLayerPart -> Lude.Text) (\s a -> s {uploadId = a} :: UploadLayerPart)
{-# DEPRECATED ulpUploadId "Use generic-lens or generic-optics with 'uploadId' instead." #-}

instance Lude.AWSRequest UploadLayerPart where
  type Rs UploadLayerPart = UploadLayerPartResponse
  request = Req.postJSON ecrService
  response =
    Res.receiveJSON
      ( \s h x ->
          UploadLayerPartResponse'
            Lude.<$> (x Lude..?> "registryId")
            Lude.<*> (x Lude..?> "lastByteReceived")
            Lude.<*> (x Lude..?> "repositoryName")
            Lude.<*> (x Lude..?> "uploadId")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UploadLayerPart where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AmazonEC2ContainerRegistry_V20150921.UploadLayerPart" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UploadLayerPart where
  toJSON UploadLayerPart' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("registryId" Lude..=) Lude.<$> registryId,
            Lude.Just ("partFirstByte" Lude..= partFirstByte),
            Lude.Just ("layerPartBlob" Lude..= layerPartBlob),
            Lude.Just ("partLastByte" Lude..= partLastByte),
            Lude.Just ("repositoryName" Lude..= repositoryName),
            Lude.Just ("uploadId" Lude..= uploadId)
          ]
      )

instance Lude.ToPath UploadLayerPart where
  toPath = Lude.const "/"

instance Lude.ToQuery UploadLayerPart where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUploadLayerPartResponse' smart constructor.
data UploadLayerPartResponse = UploadLayerPartResponse'
  { -- | The registry ID associated with the request.
    registryId :: Lude.Maybe Lude.Text,
    -- | The integer value of the last byte received in the request.
    lastByteReceived :: Lude.Maybe Lude.Natural,
    -- | The repository name associated with the request.
    repositoryName :: Lude.Maybe Lude.Text,
    -- | The upload ID associated with the request.
    uploadId :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UploadLayerPartResponse' with the minimum fields required to make a request.
--
-- * 'registryId' - The registry ID associated with the request.
-- * 'lastByteReceived' - The integer value of the last byte received in the request.
-- * 'repositoryName' - The repository name associated with the request.
-- * 'uploadId' - The upload ID associated with the request.
-- * 'responseStatus' - The response status code.
mkUploadLayerPartResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UploadLayerPartResponse
mkUploadLayerPartResponse pResponseStatus_ =
  UploadLayerPartResponse'
    { registryId = Lude.Nothing,
      lastByteReceived = Lude.Nothing,
      repositoryName = Lude.Nothing,
      uploadId = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The registry ID associated with the request.
--
-- /Note:/ Consider using 'registryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ulprsRegistryId :: Lens.Lens' UploadLayerPartResponse (Lude.Maybe Lude.Text)
ulprsRegistryId = Lens.lens (registryId :: UploadLayerPartResponse -> Lude.Maybe Lude.Text) (\s a -> s {registryId = a} :: UploadLayerPartResponse)
{-# DEPRECATED ulprsRegistryId "Use generic-lens or generic-optics with 'registryId' instead." #-}

-- | The integer value of the last byte received in the request.
--
-- /Note:/ Consider using 'lastByteReceived' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ulprsLastByteReceived :: Lens.Lens' UploadLayerPartResponse (Lude.Maybe Lude.Natural)
ulprsLastByteReceived = Lens.lens (lastByteReceived :: UploadLayerPartResponse -> Lude.Maybe Lude.Natural) (\s a -> s {lastByteReceived = a} :: UploadLayerPartResponse)
{-# DEPRECATED ulprsLastByteReceived "Use generic-lens or generic-optics with 'lastByteReceived' instead." #-}

-- | The repository name associated with the request.
--
-- /Note:/ Consider using 'repositoryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ulprsRepositoryName :: Lens.Lens' UploadLayerPartResponse (Lude.Maybe Lude.Text)
ulprsRepositoryName = Lens.lens (repositoryName :: UploadLayerPartResponse -> Lude.Maybe Lude.Text) (\s a -> s {repositoryName = a} :: UploadLayerPartResponse)
{-# DEPRECATED ulprsRepositoryName "Use generic-lens or generic-optics with 'repositoryName' instead." #-}

-- | The upload ID associated with the request.
--
-- /Note:/ Consider using 'uploadId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ulprsUploadId :: Lens.Lens' UploadLayerPartResponse (Lude.Maybe Lude.Text)
ulprsUploadId = Lens.lens (uploadId :: UploadLayerPartResponse -> Lude.Maybe Lude.Text) (\s a -> s {uploadId = a} :: UploadLayerPartResponse)
{-# DEPRECATED ulprsUploadId "Use generic-lens or generic-optics with 'uploadId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ulprsResponseStatus :: Lens.Lens' UploadLayerPartResponse Lude.Int
ulprsResponseStatus = Lens.lens (responseStatus :: UploadLayerPartResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UploadLayerPartResponse)
{-# DEPRECATED ulprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
