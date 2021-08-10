{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECR.CompleteLayerUpload
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Informs Amazon ECR that the image layer upload has completed for a
-- specified registry, repository name, and upload ID. You can optionally
-- provide a @sha256@ digest of the image layer for data validation
-- purposes.
--
-- When an image is pushed, the CompleteLayerUpload API is called once per
-- each new image layer to verify that the upload has completed.
--
-- This operation is used by the Amazon ECR proxy and is not generally used
-- by customers for pulling and pushing images. In most cases, you should
-- use the @docker@ CLI to pull, tag, and push images.
module Network.AWS.ECR.CompleteLayerUpload
  ( -- * Creating a Request
    CompleteLayerUpload (..),
    newCompleteLayerUpload,

    -- * Request Lenses
    completeLayerUpload_registryId,
    completeLayerUpload_repositoryName,
    completeLayerUpload_uploadId,
    completeLayerUpload_layerDigests,

    -- * Destructuring the Response
    CompleteLayerUploadResponse (..),
    newCompleteLayerUploadResponse,

    -- * Response Lenses
    completeLayerUploadResponse_uploadId,
    completeLayerUploadResponse_registryId,
    completeLayerUploadResponse_repositoryName,
    completeLayerUploadResponse_layerDigest,
    completeLayerUploadResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.ECR.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCompleteLayerUpload' smart constructor.
data CompleteLayerUpload = CompleteLayerUpload'
  { -- | The AWS account ID associated with the registry to which to upload
    -- layers. If you do not specify a registry, the default registry is
    -- assumed.
    registryId :: Prelude.Maybe Prelude.Text,
    -- | The name of the repository to associate with the image layer.
    repositoryName :: Prelude.Text,
    -- | The upload ID from a previous InitiateLayerUpload operation to associate
    -- with the image layer.
    uploadId :: Prelude.Text,
    -- | The @sha256@ digest of the image layer.
    layerDigests :: Prelude.NonEmpty Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CompleteLayerUpload' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'registryId', 'completeLayerUpload_registryId' - The AWS account ID associated with the registry to which to upload
-- layers. If you do not specify a registry, the default registry is
-- assumed.
--
-- 'repositoryName', 'completeLayerUpload_repositoryName' - The name of the repository to associate with the image layer.
--
-- 'uploadId', 'completeLayerUpload_uploadId' - The upload ID from a previous InitiateLayerUpload operation to associate
-- with the image layer.
--
-- 'layerDigests', 'completeLayerUpload_layerDigests' - The @sha256@ digest of the image layer.
newCompleteLayerUpload ::
  -- | 'repositoryName'
  Prelude.Text ->
  -- | 'uploadId'
  Prelude.Text ->
  -- | 'layerDigests'
  Prelude.NonEmpty Prelude.Text ->
  CompleteLayerUpload
newCompleteLayerUpload
  pRepositoryName_
  pUploadId_
  pLayerDigests_ =
    CompleteLayerUpload'
      { registryId = Prelude.Nothing,
        repositoryName = pRepositoryName_,
        uploadId = pUploadId_,
        layerDigests = Lens._Coerce Lens.# pLayerDigests_
      }

-- | The AWS account ID associated with the registry to which to upload
-- layers. If you do not specify a registry, the default registry is
-- assumed.
completeLayerUpload_registryId :: Lens.Lens' CompleteLayerUpload (Prelude.Maybe Prelude.Text)
completeLayerUpload_registryId = Lens.lens (\CompleteLayerUpload' {registryId} -> registryId) (\s@CompleteLayerUpload' {} a -> s {registryId = a} :: CompleteLayerUpload)

-- | The name of the repository to associate with the image layer.
completeLayerUpload_repositoryName :: Lens.Lens' CompleteLayerUpload Prelude.Text
completeLayerUpload_repositoryName = Lens.lens (\CompleteLayerUpload' {repositoryName} -> repositoryName) (\s@CompleteLayerUpload' {} a -> s {repositoryName = a} :: CompleteLayerUpload)

-- | The upload ID from a previous InitiateLayerUpload operation to associate
-- with the image layer.
completeLayerUpload_uploadId :: Lens.Lens' CompleteLayerUpload Prelude.Text
completeLayerUpload_uploadId = Lens.lens (\CompleteLayerUpload' {uploadId} -> uploadId) (\s@CompleteLayerUpload' {} a -> s {uploadId = a} :: CompleteLayerUpload)

-- | The @sha256@ digest of the image layer.
completeLayerUpload_layerDigests :: Lens.Lens' CompleteLayerUpload (Prelude.NonEmpty Prelude.Text)
completeLayerUpload_layerDigests = Lens.lens (\CompleteLayerUpload' {layerDigests} -> layerDigests) (\s@CompleteLayerUpload' {} a -> s {layerDigests = a} :: CompleteLayerUpload) Prelude.. Lens._Coerce

instance Core.AWSRequest CompleteLayerUpload where
  type
    AWSResponse CompleteLayerUpload =
      CompleteLayerUploadResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CompleteLayerUploadResponse'
            Prelude.<$> (x Core..?> "uploadId")
            Prelude.<*> (x Core..?> "registryId")
            Prelude.<*> (x Core..?> "repositoryName")
            Prelude.<*> (x Core..?> "layerDigest")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CompleteLayerUpload

instance Prelude.NFData CompleteLayerUpload

instance Core.ToHeaders CompleteLayerUpload where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonEC2ContainerRegistry_V20150921.CompleteLayerUpload" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CompleteLayerUpload where
  toJSON CompleteLayerUpload' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("registryId" Core..=) Prelude.<$> registryId,
            Prelude.Just
              ("repositoryName" Core..= repositoryName),
            Prelude.Just ("uploadId" Core..= uploadId),
            Prelude.Just ("layerDigests" Core..= layerDigests)
          ]
      )

instance Core.ToPath CompleteLayerUpload where
  toPath = Prelude.const "/"

instance Core.ToQuery CompleteLayerUpload where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCompleteLayerUploadResponse' smart constructor.
data CompleteLayerUploadResponse = CompleteLayerUploadResponse'
  { -- | The upload ID associated with the layer.
    uploadId :: Prelude.Maybe Prelude.Text,
    -- | The registry ID associated with the request.
    registryId :: Prelude.Maybe Prelude.Text,
    -- | The repository name associated with the request.
    repositoryName :: Prelude.Maybe Prelude.Text,
    -- | The @sha256@ digest of the image layer.
    layerDigest :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CompleteLayerUploadResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'uploadId', 'completeLayerUploadResponse_uploadId' - The upload ID associated with the layer.
--
-- 'registryId', 'completeLayerUploadResponse_registryId' - The registry ID associated with the request.
--
-- 'repositoryName', 'completeLayerUploadResponse_repositoryName' - The repository name associated with the request.
--
-- 'layerDigest', 'completeLayerUploadResponse_layerDigest' - The @sha256@ digest of the image layer.
--
-- 'httpStatus', 'completeLayerUploadResponse_httpStatus' - The response's http status code.
newCompleteLayerUploadResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CompleteLayerUploadResponse
newCompleteLayerUploadResponse pHttpStatus_ =
  CompleteLayerUploadResponse'
    { uploadId =
        Prelude.Nothing,
      registryId = Prelude.Nothing,
      repositoryName = Prelude.Nothing,
      layerDigest = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The upload ID associated with the layer.
completeLayerUploadResponse_uploadId :: Lens.Lens' CompleteLayerUploadResponse (Prelude.Maybe Prelude.Text)
completeLayerUploadResponse_uploadId = Lens.lens (\CompleteLayerUploadResponse' {uploadId} -> uploadId) (\s@CompleteLayerUploadResponse' {} a -> s {uploadId = a} :: CompleteLayerUploadResponse)

-- | The registry ID associated with the request.
completeLayerUploadResponse_registryId :: Lens.Lens' CompleteLayerUploadResponse (Prelude.Maybe Prelude.Text)
completeLayerUploadResponse_registryId = Lens.lens (\CompleteLayerUploadResponse' {registryId} -> registryId) (\s@CompleteLayerUploadResponse' {} a -> s {registryId = a} :: CompleteLayerUploadResponse)

-- | The repository name associated with the request.
completeLayerUploadResponse_repositoryName :: Lens.Lens' CompleteLayerUploadResponse (Prelude.Maybe Prelude.Text)
completeLayerUploadResponse_repositoryName = Lens.lens (\CompleteLayerUploadResponse' {repositoryName} -> repositoryName) (\s@CompleteLayerUploadResponse' {} a -> s {repositoryName = a} :: CompleteLayerUploadResponse)

-- | The @sha256@ digest of the image layer.
completeLayerUploadResponse_layerDigest :: Lens.Lens' CompleteLayerUploadResponse (Prelude.Maybe Prelude.Text)
completeLayerUploadResponse_layerDigest = Lens.lens (\CompleteLayerUploadResponse' {layerDigest} -> layerDigest) (\s@CompleteLayerUploadResponse' {} a -> s {layerDigest = a} :: CompleteLayerUploadResponse)

-- | The response's http status code.
completeLayerUploadResponse_httpStatus :: Lens.Lens' CompleteLayerUploadResponse Prelude.Int
completeLayerUploadResponse_httpStatus = Lens.lens (\CompleteLayerUploadResponse' {httpStatus} -> httpStatus) (\s@CompleteLayerUploadResponse' {} a -> s {httpStatus = a} :: CompleteLayerUploadResponse)

instance Prelude.NFData CompleteLayerUploadResponse
