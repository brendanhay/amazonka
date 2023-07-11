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
-- Module      : Amazonka.ECRPublic.InitiateLayerUpload
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Notifies Amazon ECR that you intend to upload an image layer.
--
-- When an image is pushed, the InitiateLayerUpload API is called once per
-- image layer that has not already been uploaded. Whether or not an image
-- layer has been uploaded is determined by the BatchCheckLayerAvailability
-- API action.
--
-- This operation is used by the Amazon ECR proxy and is not generally used
-- by customers for pulling and pushing images. In most cases, you should
-- use the @docker@ CLI to pull, tag, and push images.
module Amazonka.ECRPublic.InitiateLayerUpload
  ( -- * Creating a Request
    InitiateLayerUpload (..),
    newInitiateLayerUpload,

    -- * Request Lenses
    initiateLayerUpload_registryId,
    initiateLayerUpload_repositoryName,

    -- * Destructuring the Response
    InitiateLayerUploadResponse (..),
    newInitiateLayerUploadResponse,

    -- * Response Lenses
    initiateLayerUploadResponse_partSize,
    initiateLayerUploadResponse_uploadId,
    initiateLayerUploadResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ECRPublic.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newInitiateLayerUpload' smart constructor.
data InitiateLayerUpload = InitiateLayerUpload'
  { -- | The AWS account ID associated with the registry to which you intend to
    -- upload layers. If you do not specify a registry, the default public
    -- registry is assumed.
    registryId :: Prelude.Maybe Prelude.Text,
    -- | The name of the repository to which you intend to upload layers.
    repositoryName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InitiateLayerUpload' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'registryId', 'initiateLayerUpload_registryId' - The AWS account ID associated with the registry to which you intend to
-- upload layers. If you do not specify a registry, the default public
-- registry is assumed.
--
-- 'repositoryName', 'initiateLayerUpload_repositoryName' - The name of the repository to which you intend to upload layers.
newInitiateLayerUpload ::
  -- | 'repositoryName'
  Prelude.Text ->
  InitiateLayerUpload
newInitiateLayerUpload pRepositoryName_ =
  InitiateLayerUpload'
    { registryId = Prelude.Nothing,
      repositoryName = pRepositoryName_
    }

-- | The AWS account ID associated with the registry to which you intend to
-- upload layers. If you do not specify a registry, the default public
-- registry is assumed.
initiateLayerUpload_registryId :: Lens.Lens' InitiateLayerUpload (Prelude.Maybe Prelude.Text)
initiateLayerUpload_registryId = Lens.lens (\InitiateLayerUpload' {registryId} -> registryId) (\s@InitiateLayerUpload' {} a -> s {registryId = a} :: InitiateLayerUpload)

-- | The name of the repository to which you intend to upload layers.
initiateLayerUpload_repositoryName :: Lens.Lens' InitiateLayerUpload Prelude.Text
initiateLayerUpload_repositoryName = Lens.lens (\InitiateLayerUpload' {repositoryName} -> repositoryName) (\s@InitiateLayerUpload' {} a -> s {repositoryName = a} :: InitiateLayerUpload)

instance Core.AWSRequest InitiateLayerUpload where
  type
    AWSResponse InitiateLayerUpload =
      InitiateLayerUploadResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          InitiateLayerUploadResponse'
            Prelude.<$> (x Data..?> "partSize")
            Prelude.<*> (x Data..?> "uploadId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable InitiateLayerUpload where
  hashWithSalt _salt InitiateLayerUpload' {..} =
    _salt
      `Prelude.hashWithSalt` registryId
      `Prelude.hashWithSalt` repositoryName

instance Prelude.NFData InitiateLayerUpload where
  rnf InitiateLayerUpload' {..} =
    Prelude.rnf registryId
      `Prelude.seq` Prelude.rnf repositoryName

instance Data.ToHeaders InitiateLayerUpload where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "SpencerFrontendService.InitiateLayerUpload" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON InitiateLayerUpload where
  toJSON InitiateLayerUpload' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("registryId" Data..=) Prelude.<$> registryId,
            Prelude.Just
              ("repositoryName" Data..= repositoryName)
          ]
      )

instance Data.ToPath InitiateLayerUpload where
  toPath = Prelude.const "/"

instance Data.ToQuery InitiateLayerUpload where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newInitiateLayerUploadResponse' smart constructor.
data InitiateLayerUploadResponse = InitiateLayerUploadResponse'
  { -- | The size, in bytes, that Amazon ECR expects future layer part uploads to
    -- be.
    partSize :: Prelude.Maybe Prelude.Natural,
    -- | The upload ID for the layer upload. This parameter is passed to further
    -- UploadLayerPart and CompleteLayerUpload operations.
    uploadId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InitiateLayerUploadResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'partSize', 'initiateLayerUploadResponse_partSize' - The size, in bytes, that Amazon ECR expects future layer part uploads to
-- be.
--
-- 'uploadId', 'initiateLayerUploadResponse_uploadId' - The upload ID for the layer upload. This parameter is passed to further
-- UploadLayerPart and CompleteLayerUpload operations.
--
-- 'httpStatus', 'initiateLayerUploadResponse_httpStatus' - The response's http status code.
newInitiateLayerUploadResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  InitiateLayerUploadResponse
newInitiateLayerUploadResponse pHttpStatus_ =
  InitiateLayerUploadResponse'
    { partSize =
        Prelude.Nothing,
      uploadId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The size, in bytes, that Amazon ECR expects future layer part uploads to
-- be.
initiateLayerUploadResponse_partSize :: Lens.Lens' InitiateLayerUploadResponse (Prelude.Maybe Prelude.Natural)
initiateLayerUploadResponse_partSize = Lens.lens (\InitiateLayerUploadResponse' {partSize} -> partSize) (\s@InitiateLayerUploadResponse' {} a -> s {partSize = a} :: InitiateLayerUploadResponse)

-- | The upload ID for the layer upload. This parameter is passed to further
-- UploadLayerPart and CompleteLayerUpload operations.
initiateLayerUploadResponse_uploadId :: Lens.Lens' InitiateLayerUploadResponse (Prelude.Maybe Prelude.Text)
initiateLayerUploadResponse_uploadId = Lens.lens (\InitiateLayerUploadResponse' {uploadId} -> uploadId) (\s@InitiateLayerUploadResponse' {} a -> s {uploadId = a} :: InitiateLayerUploadResponse)

-- | The response's http status code.
initiateLayerUploadResponse_httpStatus :: Lens.Lens' InitiateLayerUploadResponse Prelude.Int
initiateLayerUploadResponse_httpStatus = Lens.lens (\InitiateLayerUploadResponse' {httpStatus} -> httpStatus) (\s@InitiateLayerUploadResponse' {} a -> s {httpStatus = a} :: InitiateLayerUploadResponse)

instance Prelude.NFData InitiateLayerUploadResponse where
  rnf InitiateLayerUploadResponse' {..} =
    Prelude.rnf partSize
      `Prelude.seq` Prelude.rnf uploadId
      `Prelude.seq` Prelude.rnf httpStatus
