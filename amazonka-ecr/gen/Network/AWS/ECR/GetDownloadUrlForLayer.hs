{-# LANGUAGE DeriveDataTypeable #-}
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

import Network.AWS.ECR.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetDownloadUrlForLayer' smart constructor.
data GetDownloadUrlForLayer = GetDownloadUrlForLayer'
  { -- | The AWS account ID associated with the registry that contains the image
    -- layer to download. If you do not specify a registry, the default
    -- registry is assumed.
    registryId :: Prelude.Maybe Prelude.Text,
    -- | The name of the repository that is associated with the image layer to
    -- download.
    repositoryName :: Prelude.Text,
    -- | The digest of the image layer to download.
    layerDigest :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'layerDigest'
  Prelude.Text ->
  GetDownloadUrlForLayer
newGetDownloadUrlForLayer
  pRepositoryName_
  pLayerDigest_ =
    GetDownloadUrlForLayer'
      { registryId =
          Prelude.Nothing,
        repositoryName = pRepositoryName_,
        layerDigest = pLayerDigest_
      }

-- | The AWS account ID associated with the registry that contains the image
-- layer to download. If you do not specify a registry, the default
-- registry is assumed.
getDownloadUrlForLayer_registryId :: Lens.Lens' GetDownloadUrlForLayer (Prelude.Maybe Prelude.Text)
getDownloadUrlForLayer_registryId = Lens.lens (\GetDownloadUrlForLayer' {registryId} -> registryId) (\s@GetDownloadUrlForLayer' {} a -> s {registryId = a} :: GetDownloadUrlForLayer)

-- | The name of the repository that is associated with the image layer to
-- download.
getDownloadUrlForLayer_repositoryName :: Lens.Lens' GetDownloadUrlForLayer Prelude.Text
getDownloadUrlForLayer_repositoryName = Lens.lens (\GetDownloadUrlForLayer' {repositoryName} -> repositoryName) (\s@GetDownloadUrlForLayer' {} a -> s {repositoryName = a} :: GetDownloadUrlForLayer)

-- | The digest of the image layer to download.
getDownloadUrlForLayer_layerDigest :: Lens.Lens' GetDownloadUrlForLayer Prelude.Text
getDownloadUrlForLayer_layerDigest = Lens.lens (\GetDownloadUrlForLayer' {layerDigest} -> layerDigest) (\s@GetDownloadUrlForLayer' {} a -> s {layerDigest = a} :: GetDownloadUrlForLayer)

instance Prelude.AWSRequest GetDownloadUrlForLayer where
  type
    Rs GetDownloadUrlForLayer =
      GetDownloadUrlForLayerResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetDownloadUrlForLayerResponse'
            Prelude.<$> (x Prelude..?> "downloadUrl")
            Prelude.<*> (x Prelude..?> "layerDigest")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetDownloadUrlForLayer

instance Prelude.NFData GetDownloadUrlForLayer

instance Prelude.ToHeaders GetDownloadUrlForLayer where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AmazonEC2ContainerRegistry_V20150921.GetDownloadUrlForLayer" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON GetDownloadUrlForLayer where
  toJSON GetDownloadUrlForLayer' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("registryId" Prelude..=) Prelude.<$> registryId,
            Prelude.Just
              ("repositoryName" Prelude..= repositoryName),
            Prelude.Just ("layerDigest" Prelude..= layerDigest)
          ]
      )

instance Prelude.ToPath GetDownloadUrlForLayer where
  toPath = Prelude.const "/"

instance Prelude.ToQuery GetDownloadUrlForLayer where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetDownloadUrlForLayerResponse' smart constructor.
data GetDownloadUrlForLayerResponse = GetDownloadUrlForLayerResponse'
  { -- | The pre-signed Amazon S3 download URL for the requested layer.
    downloadUrl :: Prelude.Maybe Prelude.Text,
    -- | The digest of the image layer to download.
    layerDigest :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  GetDownloadUrlForLayerResponse
newGetDownloadUrlForLayerResponse pHttpStatus_ =
  GetDownloadUrlForLayerResponse'
    { downloadUrl =
        Prelude.Nothing,
      layerDigest = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The pre-signed Amazon S3 download URL for the requested layer.
getDownloadUrlForLayerResponse_downloadUrl :: Lens.Lens' GetDownloadUrlForLayerResponse (Prelude.Maybe Prelude.Text)
getDownloadUrlForLayerResponse_downloadUrl = Lens.lens (\GetDownloadUrlForLayerResponse' {downloadUrl} -> downloadUrl) (\s@GetDownloadUrlForLayerResponse' {} a -> s {downloadUrl = a} :: GetDownloadUrlForLayerResponse)

-- | The digest of the image layer to download.
getDownloadUrlForLayerResponse_layerDigest :: Lens.Lens' GetDownloadUrlForLayerResponse (Prelude.Maybe Prelude.Text)
getDownloadUrlForLayerResponse_layerDigest = Lens.lens (\GetDownloadUrlForLayerResponse' {layerDigest} -> layerDigest) (\s@GetDownloadUrlForLayerResponse' {} a -> s {layerDigest = a} :: GetDownloadUrlForLayerResponse)

-- | The response's http status code.
getDownloadUrlForLayerResponse_httpStatus :: Lens.Lens' GetDownloadUrlForLayerResponse Prelude.Int
getDownloadUrlForLayerResponse_httpStatus = Lens.lens (\GetDownloadUrlForLayerResponse' {httpStatus} -> httpStatus) (\s@GetDownloadUrlForLayerResponse' {} a -> s {httpStatus = a} :: GetDownloadUrlForLayerResponse)

instance
  Prelude.NFData
    GetDownloadUrlForLayerResponse
