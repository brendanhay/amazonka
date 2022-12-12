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
-- Module      : Amazonka.ECRPublic.UploadLayerPart
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Uploads an image layer part to Amazon ECR.
--
-- When an image is pushed, each new image layer is uploaded in parts. The
-- maximum size of each image layer part can be 20971520 bytes (or about
-- 20MB). The UploadLayerPart API is called once per each new image layer
-- part.
--
-- This operation is used by the Amazon ECR proxy and is not generally used
-- by customers for pulling and pushing images. In most cases, you should
-- use the @docker@ CLI to pull, tag, and push images.
module Amazonka.ECRPublic.UploadLayerPart
  ( -- * Creating a Request
    UploadLayerPart (..),
    newUploadLayerPart,

    -- * Request Lenses
    uploadLayerPart_registryId,
    uploadLayerPart_repositoryName,
    uploadLayerPart_uploadId,
    uploadLayerPart_partFirstByte,
    uploadLayerPart_partLastByte,
    uploadLayerPart_layerPartBlob,

    -- * Destructuring the Response
    UploadLayerPartResponse (..),
    newUploadLayerPartResponse,

    -- * Response Lenses
    uploadLayerPartResponse_lastByteReceived,
    uploadLayerPartResponse_registryId,
    uploadLayerPartResponse_repositoryName,
    uploadLayerPartResponse_uploadId,
    uploadLayerPartResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ECRPublic.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUploadLayerPart' smart constructor.
data UploadLayerPart = UploadLayerPart'
  { -- | The AWS account ID associated with the registry to which you are
    -- uploading layer parts. If you do not specify a registry, the default
    -- public registry is assumed.
    registryId :: Prelude.Maybe Prelude.Text,
    -- | The name of the repository to which you are uploading layer parts.
    repositoryName :: Prelude.Text,
    -- | The upload ID from a previous InitiateLayerUpload operation to associate
    -- with the layer part upload.
    uploadId :: Prelude.Text,
    -- | The position of the first byte of the layer part witin the overall image
    -- layer.
    partFirstByte :: Prelude.Natural,
    -- | The position of the last byte of the layer part within the overall image
    -- layer.
    partLastByte :: Prelude.Natural,
    -- | The base64-encoded layer part payload.
    layerPartBlob :: Data.Base64
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UploadLayerPart' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'registryId', 'uploadLayerPart_registryId' - The AWS account ID associated with the registry to which you are
-- uploading layer parts. If you do not specify a registry, the default
-- public registry is assumed.
--
-- 'repositoryName', 'uploadLayerPart_repositoryName' - The name of the repository to which you are uploading layer parts.
--
-- 'uploadId', 'uploadLayerPart_uploadId' - The upload ID from a previous InitiateLayerUpload operation to associate
-- with the layer part upload.
--
-- 'partFirstByte', 'uploadLayerPart_partFirstByte' - The position of the first byte of the layer part witin the overall image
-- layer.
--
-- 'partLastByte', 'uploadLayerPart_partLastByte' - The position of the last byte of the layer part within the overall image
-- layer.
--
-- 'layerPartBlob', 'uploadLayerPart_layerPartBlob' - The base64-encoded layer part payload.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
newUploadLayerPart ::
  -- | 'repositoryName'
  Prelude.Text ->
  -- | 'uploadId'
  Prelude.Text ->
  -- | 'partFirstByte'
  Prelude.Natural ->
  -- | 'partLastByte'
  Prelude.Natural ->
  -- | 'layerPartBlob'
  Prelude.ByteString ->
  UploadLayerPart
newUploadLayerPart
  pRepositoryName_
  pUploadId_
  pPartFirstByte_
  pPartLastByte_
  pLayerPartBlob_ =
    UploadLayerPart'
      { registryId = Prelude.Nothing,
        repositoryName = pRepositoryName_,
        uploadId = pUploadId_,
        partFirstByte = pPartFirstByte_,
        partLastByte = pPartLastByte_,
        layerPartBlob = Data._Base64 Lens.# pLayerPartBlob_
      }

-- | The AWS account ID associated with the registry to which you are
-- uploading layer parts. If you do not specify a registry, the default
-- public registry is assumed.
uploadLayerPart_registryId :: Lens.Lens' UploadLayerPart (Prelude.Maybe Prelude.Text)
uploadLayerPart_registryId = Lens.lens (\UploadLayerPart' {registryId} -> registryId) (\s@UploadLayerPart' {} a -> s {registryId = a} :: UploadLayerPart)

-- | The name of the repository to which you are uploading layer parts.
uploadLayerPart_repositoryName :: Lens.Lens' UploadLayerPart Prelude.Text
uploadLayerPart_repositoryName = Lens.lens (\UploadLayerPart' {repositoryName} -> repositoryName) (\s@UploadLayerPart' {} a -> s {repositoryName = a} :: UploadLayerPart)

-- | The upload ID from a previous InitiateLayerUpload operation to associate
-- with the layer part upload.
uploadLayerPart_uploadId :: Lens.Lens' UploadLayerPart Prelude.Text
uploadLayerPart_uploadId = Lens.lens (\UploadLayerPart' {uploadId} -> uploadId) (\s@UploadLayerPart' {} a -> s {uploadId = a} :: UploadLayerPart)

-- | The position of the first byte of the layer part witin the overall image
-- layer.
uploadLayerPart_partFirstByte :: Lens.Lens' UploadLayerPart Prelude.Natural
uploadLayerPart_partFirstByte = Lens.lens (\UploadLayerPart' {partFirstByte} -> partFirstByte) (\s@UploadLayerPart' {} a -> s {partFirstByte = a} :: UploadLayerPart)

-- | The position of the last byte of the layer part within the overall image
-- layer.
uploadLayerPart_partLastByte :: Lens.Lens' UploadLayerPart Prelude.Natural
uploadLayerPart_partLastByte = Lens.lens (\UploadLayerPart' {partLastByte} -> partLastByte) (\s@UploadLayerPart' {} a -> s {partLastByte = a} :: UploadLayerPart)

-- | The base64-encoded layer part payload.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
uploadLayerPart_layerPartBlob :: Lens.Lens' UploadLayerPart Prelude.ByteString
uploadLayerPart_layerPartBlob = Lens.lens (\UploadLayerPart' {layerPartBlob} -> layerPartBlob) (\s@UploadLayerPart' {} a -> s {layerPartBlob = a} :: UploadLayerPart) Prelude.. Data._Base64

instance Core.AWSRequest UploadLayerPart where
  type
    AWSResponse UploadLayerPart =
      UploadLayerPartResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UploadLayerPartResponse'
            Prelude.<$> (x Data..?> "lastByteReceived")
            Prelude.<*> (x Data..?> "registryId")
            Prelude.<*> (x Data..?> "repositoryName")
            Prelude.<*> (x Data..?> "uploadId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UploadLayerPart where
  hashWithSalt _salt UploadLayerPart' {..} =
    _salt `Prelude.hashWithSalt` registryId
      `Prelude.hashWithSalt` repositoryName
      `Prelude.hashWithSalt` uploadId
      `Prelude.hashWithSalt` partFirstByte
      `Prelude.hashWithSalt` partLastByte
      `Prelude.hashWithSalt` layerPartBlob

instance Prelude.NFData UploadLayerPart where
  rnf UploadLayerPart' {..} =
    Prelude.rnf registryId
      `Prelude.seq` Prelude.rnf repositoryName
      `Prelude.seq` Prelude.rnf uploadId
      `Prelude.seq` Prelude.rnf partFirstByte
      `Prelude.seq` Prelude.rnf partLastByte
      `Prelude.seq` Prelude.rnf layerPartBlob

instance Data.ToHeaders UploadLayerPart where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "SpencerFrontendService.UploadLayerPart" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UploadLayerPart where
  toJSON UploadLayerPart' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("registryId" Data..=) Prelude.<$> registryId,
            Prelude.Just
              ("repositoryName" Data..= repositoryName),
            Prelude.Just ("uploadId" Data..= uploadId),
            Prelude.Just ("partFirstByte" Data..= partFirstByte),
            Prelude.Just ("partLastByte" Data..= partLastByte),
            Prelude.Just
              ("layerPartBlob" Data..= layerPartBlob)
          ]
      )

instance Data.ToPath UploadLayerPart where
  toPath = Prelude.const "/"

instance Data.ToQuery UploadLayerPart where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUploadLayerPartResponse' smart constructor.
data UploadLayerPartResponse = UploadLayerPartResponse'
  { -- | The integer value of the last byte received in the request.
    lastByteReceived :: Prelude.Maybe Prelude.Natural,
    -- | The registry ID associated with the request.
    registryId :: Prelude.Maybe Prelude.Text,
    -- | The repository name associated with the request.
    repositoryName :: Prelude.Maybe Prelude.Text,
    -- | The upload ID associated with the request.
    uploadId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UploadLayerPartResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lastByteReceived', 'uploadLayerPartResponse_lastByteReceived' - The integer value of the last byte received in the request.
--
-- 'registryId', 'uploadLayerPartResponse_registryId' - The registry ID associated with the request.
--
-- 'repositoryName', 'uploadLayerPartResponse_repositoryName' - The repository name associated with the request.
--
-- 'uploadId', 'uploadLayerPartResponse_uploadId' - The upload ID associated with the request.
--
-- 'httpStatus', 'uploadLayerPartResponse_httpStatus' - The response's http status code.
newUploadLayerPartResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UploadLayerPartResponse
newUploadLayerPartResponse pHttpStatus_ =
  UploadLayerPartResponse'
    { lastByteReceived =
        Prelude.Nothing,
      registryId = Prelude.Nothing,
      repositoryName = Prelude.Nothing,
      uploadId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The integer value of the last byte received in the request.
uploadLayerPartResponse_lastByteReceived :: Lens.Lens' UploadLayerPartResponse (Prelude.Maybe Prelude.Natural)
uploadLayerPartResponse_lastByteReceived = Lens.lens (\UploadLayerPartResponse' {lastByteReceived} -> lastByteReceived) (\s@UploadLayerPartResponse' {} a -> s {lastByteReceived = a} :: UploadLayerPartResponse)

-- | The registry ID associated with the request.
uploadLayerPartResponse_registryId :: Lens.Lens' UploadLayerPartResponse (Prelude.Maybe Prelude.Text)
uploadLayerPartResponse_registryId = Lens.lens (\UploadLayerPartResponse' {registryId} -> registryId) (\s@UploadLayerPartResponse' {} a -> s {registryId = a} :: UploadLayerPartResponse)

-- | The repository name associated with the request.
uploadLayerPartResponse_repositoryName :: Lens.Lens' UploadLayerPartResponse (Prelude.Maybe Prelude.Text)
uploadLayerPartResponse_repositoryName = Lens.lens (\UploadLayerPartResponse' {repositoryName} -> repositoryName) (\s@UploadLayerPartResponse' {} a -> s {repositoryName = a} :: UploadLayerPartResponse)

-- | The upload ID associated with the request.
uploadLayerPartResponse_uploadId :: Lens.Lens' UploadLayerPartResponse (Prelude.Maybe Prelude.Text)
uploadLayerPartResponse_uploadId = Lens.lens (\UploadLayerPartResponse' {uploadId} -> uploadId) (\s@UploadLayerPartResponse' {} a -> s {uploadId = a} :: UploadLayerPartResponse)

-- | The response's http status code.
uploadLayerPartResponse_httpStatus :: Lens.Lens' UploadLayerPartResponse Prelude.Int
uploadLayerPartResponse_httpStatus = Lens.lens (\UploadLayerPartResponse' {httpStatus} -> httpStatus) (\s@UploadLayerPartResponse' {} a -> s {httpStatus = a} :: UploadLayerPartResponse)

instance Prelude.NFData UploadLayerPartResponse where
  rnf UploadLayerPartResponse' {..} =
    Prelude.rnf lastByteReceived
      `Prelude.seq` Prelude.rnf registryId
      `Prelude.seq` Prelude.rnf repositoryName
      `Prelude.seq` Prelude.rnf uploadId
      `Prelude.seq` Prelude.rnf httpStatus
