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
-- Module      : Amazonka.ECR.StartImageScan
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts an image vulnerability scan. An image scan can only be started
-- once per 24 hours on an individual image. This limit includes if an
-- image was scanned on initial push. For more information, see
-- <https://docs.aws.amazon.com/AmazonECR/latest/userguide/image-scanning.html Image scanning>
-- in the /Amazon Elastic Container Registry User Guide/.
module Amazonka.ECR.StartImageScan
  ( -- * Creating a Request
    StartImageScan (..),
    newStartImageScan,

    -- * Request Lenses
    startImageScan_registryId,
    startImageScan_repositoryName,
    startImageScan_imageId,

    -- * Destructuring the Response
    StartImageScanResponse (..),
    newStartImageScanResponse,

    -- * Response Lenses
    startImageScanResponse_imageId,
    startImageScanResponse_imageScanStatus,
    startImageScanResponse_registryId,
    startImageScanResponse_repositoryName,
    startImageScanResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ECR.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStartImageScan' smart constructor.
data StartImageScan = StartImageScan'
  { -- | The Amazon Web Services account ID associated with the registry that
    -- contains the repository in which to start an image scan request. If you
    -- do not specify a registry, the default registry is assumed.
    registryId :: Prelude.Maybe Prelude.Text,
    -- | The name of the repository that contains the images to scan.
    repositoryName :: Prelude.Text,
    imageId :: ImageIdentifier
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartImageScan' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'registryId', 'startImageScan_registryId' - The Amazon Web Services account ID associated with the registry that
-- contains the repository in which to start an image scan request. If you
-- do not specify a registry, the default registry is assumed.
--
-- 'repositoryName', 'startImageScan_repositoryName' - The name of the repository that contains the images to scan.
--
-- 'imageId', 'startImageScan_imageId' - Undocumented member.
newStartImageScan ::
  -- | 'repositoryName'
  Prelude.Text ->
  -- | 'imageId'
  ImageIdentifier ->
  StartImageScan
newStartImageScan pRepositoryName_ pImageId_ =
  StartImageScan'
    { registryId = Prelude.Nothing,
      repositoryName = pRepositoryName_,
      imageId = pImageId_
    }

-- | The Amazon Web Services account ID associated with the registry that
-- contains the repository in which to start an image scan request. If you
-- do not specify a registry, the default registry is assumed.
startImageScan_registryId :: Lens.Lens' StartImageScan (Prelude.Maybe Prelude.Text)
startImageScan_registryId = Lens.lens (\StartImageScan' {registryId} -> registryId) (\s@StartImageScan' {} a -> s {registryId = a} :: StartImageScan)

-- | The name of the repository that contains the images to scan.
startImageScan_repositoryName :: Lens.Lens' StartImageScan Prelude.Text
startImageScan_repositoryName = Lens.lens (\StartImageScan' {repositoryName} -> repositoryName) (\s@StartImageScan' {} a -> s {repositoryName = a} :: StartImageScan)

-- | Undocumented member.
startImageScan_imageId :: Lens.Lens' StartImageScan ImageIdentifier
startImageScan_imageId = Lens.lens (\StartImageScan' {imageId} -> imageId) (\s@StartImageScan' {} a -> s {imageId = a} :: StartImageScan)

instance Core.AWSRequest StartImageScan where
  type
    AWSResponse StartImageScan =
      StartImageScanResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          StartImageScanResponse'
            Prelude.<$> (x Data..?> "imageId")
            Prelude.<*> (x Data..?> "imageScanStatus")
            Prelude.<*> (x Data..?> "registryId")
            Prelude.<*> (x Data..?> "repositoryName")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StartImageScan where
  hashWithSalt _salt StartImageScan' {..} =
    _salt
      `Prelude.hashWithSalt` registryId
      `Prelude.hashWithSalt` repositoryName
      `Prelude.hashWithSalt` imageId

instance Prelude.NFData StartImageScan where
  rnf StartImageScan' {..} =
    Prelude.rnf registryId
      `Prelude.seq` Prelude.rnf repositoryName
      `Prelude.seq` Prelude.rnf imageId

instance Data.ToHeaders StartImageScan where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonEC2ContainerRegistry_V20150921.StartImageScan" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON StartImageScan where
  toJSON StartImageScan' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("registryId" Data..=) Prelude.<$> registryId,
            Prelude.Just
              ("repositoryName" Data..= repositoryName),
            Prelude.Just ("imageId" Data..= imageId)
          ]
      )

instance Data.ToPath StartImageScan where
  toPath = Prelude.const "/"

instance Data.ToQuery StartImageScan where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartImageScanResponse' smart constructor.
data StartImageScanResponse = StartImageScanResponse'
  { imageId :: Prelude.Maybe ImageIdentifier,
    -- | The current state of the scan.
    imageScanStatus :: Prelude.Maybe ImageScanStatus,
    -- | The registry ID associated with the request.
    registryId :: Prelude.Maybe Prelude.Text,
    -- | The repository name associated with the request.
    repositoryName :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartImageScanResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'imageId', 'startImageScanResponse_imageId' - Undocumented member.
--
-- 'imageScanStatus', 'startImageScanResponse_imageScanStatus' - The current state of the scan.
--
-- 'registryId', 'startImageScanResponse_registryId' - The registry ID associated with the request.
--
-- 'repositoryName', 'startImageScanResponse_repositoryName' - The repository name associated with the request.
--
-- 'httpStatus', 'startImageScanResponse_httpStatus' - The response's http status code.
newStartImageScanResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StartImageScanResponse
newStartImageScanResponse pHttpStatus_ =
  StartImageScanResponse'
    { imageId = Prelude.Nothing,
      imageScanStatus = Prelude.Nothing,
      registryId = Prelude.Nothing,
      repositoryName = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
startImageScanResponse_imageId :: Lens.Lens' StartImageScanResponse (Prelude.Maybe ImageIdentifier)
startImageScanResponse_imageId = Lens.lens (\StartImageScanResponse' {imageId} -> imageId) (\s@StartImageScanResponse' {} a -> s {imageId = a} :: StartImageScanResponse)

-- | The current state of the scan.
startImageScanResponse_imageScanStatus :: Lens.Lens' StartImageScanResponse (Prelude.Maybe ImageScanStatus)
startImageScanResponse_imageScanStatus = Lens.lens (\StartImageScanResponse' {imageScanStatus} -> imageScanStatus) (\s@StartImageScanResponse' {} a -> s {imageScanStatus = a} :: StartImageScanResponse)

-- | The registry ID associated with the request.
startImageScanResponse_registryId :: Lens.Lens' StartImageScanResponse (Prelude.Maybe Prelude.Text)
startImageScanResponse_registryId = Lens.lens (\StartImageScanResponse' {registryId} -> registryId) (\s@StartImageScanResponse' {} a -> s {registryId = a} :: StartImageScanResponse)

-- | The repository name associated with the request.
startImageScanResponse_repositoryName :: Lens.Lens' StartImageScanResponse (Prelude.Maybe Prelude.Text)
startImageScanResponse_repositoryName = Lens.lens (\StartImageScanResponse' {repositoryName} -> repositoryName) (\s@StartImageScanResponse' {} a -> s {repositoryName = a} :: StartImageScanResponse)

-- | The response's http status code.
startImageScanResponse_httpStatus :: Lens.Lens' StartImageScanResponse Prelude.Int
startImageScanResponse_httpStatus = Lens.lens (\StartImageScanResponse' {httpStatus} -> httpStatus) (\s@StartImageScanResponse' {} a -> s {httpStatus = a} :: StartImageScanResponse)

instance Prelude.NFData StartImageScanResponse where
  rnf StartImageScanResponse' {..} =
    Prelude.rnf imageId
      `Prelude.seq` Prelude.rnf imageScanStatus
      `Prelude.seq` Prelude.rnf registryId
      `Prelude.seq` Prelude.rnf repositoryName
      `Prelude.seq` Prelude.rnf httpStatus
