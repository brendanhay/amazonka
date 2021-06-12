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
-- Module      : Network.AWS.ECR.GetDownloadUrlForLayer
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the pre-signed Amazon S3 download URL corresponding to an
-- image layer. You can only get URLs for image layers that are referenced
-- in an image.
--
-- When an image is pulled, the GetDownloadUrlForLayer API is called once
-- per image layer that is not already cached.
--
-- This operation is used by the Amazon ECR proxy and is not generally used
-- by customers for pulling and pushing images. In most cases, you should
-- use the @docker@ CLI to pull, tag, and push images.
module Network.AWS.ECR.GetDownloadUrlForLayer
  ( -- * Creating a Request
    GetDownloadUrlForLayer (..),
    newGetDownloadUrlForLayer,

    -- * Request Lenses
    getDownloadUrlForLayer_registryId,
    getDownloadUrlForLayer_repositoryName,
    getDownloadUrlForLayer_layerDigest,

    -- * Destructuring the Response
    GetDownloadUrlForLayerResponse (..),
    newGetDownloadUrlForLayerResponse,

    -- * Response Lenses
    getDownloadUrlForLayerResponse_downloadUrl,
    getDownloadUrlForLayerResponse_layerDigest,
    getDownloadUrlForLayerResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.ECR.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetDownloadUrlForLayer' smart constructor.
data GetDownloadUrlForLayer = GetDownloadUrlForLayer'
  { -- | The AWS account ID associated with the registry that contains the image
    -- layer to download. If you do not specify a registry, the default
    -- registry is assumed.
    registryId :: Core.Maybe Core.Text,
    -- | The name of the repository that is associated with the image layer to
    -- download.
    repositoryName :: Core.Text,
    -- | The digest of the image layer to download.
    layerDigest :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetDownloadUrlForLayer' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'registryId', 'getDownloadUrlForLayer_registryId' - The AWS account ID associated with the registry that contains the image
-- layer to download. If you do not specify a registry, the default
-- registry is assumed.
--
-- 'repositoryName', 'getDownloadUrlForLayer_repositoryName' - The name of the repository that is associated with the image layer to
-- download.
--
-- 'layerDigest', 'getDownloadUrlForLayer_layerDigest' - The digest of the image layer to download.
newGetDownloadUrlForLayer ::
  -- | 'repositoryName'
  Core.Text ->
  -- | 'layerDigest'
  Core.Text ->
  GetDownloadUrlForLayer
newGetDownloadUrlForLayer
  pRepositoryName_
  pLayerDigest_ =
    GetDownloadUrlForLayer'
      { registryId = Core.Nothing,
        repositoryName = pRepositoryName_,
        layerDigest = pLayerDigest_
      }

-- | The AWS account ID associated with the registry that contains the image
-- layer to download. If you do not specify a registry, the default
-- registry is assumed.
getDownloadUrlForLayer_registryId :: Lens.Lens' GetDownloadUrlForLayer (Core.Maybe Core.Text)
getDownloadUrlForLayer_registryId = Lens.lens (\GetDownloadUrlForLayer' {registryId} -> registryId) (\s@GetDownloadUrlForLayer' {} a -> s {registryId = a} :: GetDownloadUrlForLayer)

-- | The name of the repository that is associated with the image layer to
-- download.
getDownloadUrlForLayer_repositoryName :: Lens.Lens' GetDownloadUrlForLayer Core.Text
getDownloadUrlForLayer_repositoryName = Lens.lens (\GetDownloadUrlForLayer' {repositoryName} -> repositoryName) (\s@GetDownloadUrlForLayer' {} a -> s {repositoryName = a} :: GetDownloadUrlForLayer)

-- | The digest of the image layer to download.
getDownloadUrlForLayer_layerDigest :: Lens.Lens' GetDownloadUrlForLayer Core.Text
getDownloadUrlForLayer_layerDigest = Lens.lens (\GetDownloadUrlForLayer' {layerDigest} -> layerDigest) (\s@GetDownloadUrlForLayer' {} a -> s {layerDigest = a} :: GetDownloadUrlForLayer)

instance Core.AWSRequest GetDownloadUrlForLayer where
  type
    AWSResponse GetDownloadUrlForLayer =
      GetDownloadUrlForLayerResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetDownloadUrlForLayerResponse'
            Core.<$> (x Core..?> "downloadUrl")
            Core.<*> (x Core..?> "layerDigest")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetDownloadUrlForLayer

instance Core.NFData GetDownloadUrlForLayer

instance Core.ToHeaders GetDownloadUrlForLayer where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonEC2ContainerRegistry_V20150921.GetDownloadUrlForLayer" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON GetDownloadUrlForLayer where
  toJSON GetDownloadUrlForLayer' {..} =
    Core.object
      ( Core.catMaybes
          [ ("registryId" Core..=) Core.<$> registryId,
            Core.Just ("repositoryName" Core..= repositoryName),
            Core.Just ("layerDigest" Core..= layerDigest)
          ]
      )

instance Core.ToPath GetDownloadUrlForLayer where
  toPath = Core.const "/"

instance Core.ToQuery GetDownloadUrlForLayer where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetDownloadUrlForLayerResponse' smart constructor.
data GetDownloadUrlForLayerResponse = GetDownloadUrlForLayerResponse'
  { -- | The pre-signed Amazon S3 download URL for the requested layer.
    downloadUrl :: Core.Maybe Core.Text,
    -- | The digest of the image layer to download.
    layerDigest :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetDownloadUrlForLayerResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'downloadUrl', 'getDownloadUrlForLayerResponse_downloadUrl' - The pre-signed Amazon S3 download URL for the requested layer.
--
-- 'layerDigest', 'getDownloadUrlForLayerResponse_layerDigest' - The digest of the image layer to download.
--
-- 'httpStatus', 'getDownloadUrlForLayerResponse_httpStatus' - The response's http status code.
newGetDownloadUrlForLayerResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetDownloadUrlForLayerResponse
newGetDownloadUrlForLayerResponse pHttpStatus_ =
  GetDownloadUrlForLayerResponse'
    { downloadUrl =
        Core.Nothing,
      layerDigest = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The pre-signed Amazon S3 download URL for the requested layer.
getDownloadUrlForLayerResponse_downloadUrl :: Lens.Lens' GetDownloadUrlForLayerResponse (Core.Maybe Core.Text)
getDownloadUrlForLayerResponse_downloadUrl = Lens.lens (\GetDownloadUrlForLayerResponse' {downloadUrl} -> downloadUrl) (\s@GetDownloadUrlForLayerResponse' {} a -> s {downloadUrl = a} :: GetDownloadUrlForLayerResponse)

-- | The digest of the image layer to download.
getDownloadUrlForLayerResponse_layerDigest :: Lens.Lens' GetDownloadUrlForLayerResponse (Core.Maybe Core.Text)
getDownloadUrlForLayerResponse_layerDigest = Lens.lens (\GetDownloadUrlForLayerResponse' {layerDigest} -> layerDigest) (\s@GetDownloadUrlForLayerResponse' {} a -> s {layerDigest = a} :: GetDownloadUrlForLayerResponse)

-- | The response's http status code.
getDownloadUrlForLayerResponse_httpStatus :: Lens.Lens' GetDownloadUrlForLayerResponse Core.Int
getDownloadUrlForLayerResponse_httpStatus = Lens.lens (\GetDownloadUrlForLayerResponse' {httpStatus} -> httpStatus) (\s@GetDownloadUrlForLayerResponse' {} a -> s {httpStatus = a} :: GetDownloadUrlForLayerResponse)

instance Core.NFData GetDownloadUrlForLayerResponse
