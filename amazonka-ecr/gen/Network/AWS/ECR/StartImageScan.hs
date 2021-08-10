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
-- Module      : Network.AWS.ECR.StartImageScan
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts an image vulnerability scan. An image scan can only be started
-- once per day on an individual image. This limit includes if an image was
-- scanned on initial push. For more information, see
-- <https://docs.aws.amazon.com/AmazonECR/latest/userguide/image-scanning.html Image Scanning>
-- in the /Amazon Elastic Container Registry User Guide/.
module Network.AWS.ECR.StartImageScan
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
    startImageScanResponse_imageScanStatus,
    startImageScanResponse_registryId,
    startImageScanResponse_repositoryName,
    startImageScanResponse_imageId,
    startImageScanResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.ECR.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newStartImageScan' smart constructor.
data StartImageScan = StartImageScan'
  { -- | The AWS account ID associated with the registry that contains the
    -- repository in which to start an image scan request. If you do not
    -- specify a registry, the default registry is assumed.
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
-- 'registryId', 'startImageScan_registryId' - The AWS account ID associated with the registry that contains the
-- repository in which to start an image scan request. If you do not
-- specify a registry, the default registry is assumed.
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

-- | The AWS account ID associated with the registry that contains the
-- repository in which to start an image scan request. If you do not
-- specify a registry, the default registry is assumed.
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
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          StartImageScanResponse'
            Prelude.<$> (x Core..?> "imageScanStatus")
            Prelude.<*> (x Core..?> "registryId")
            Prelude.<*> (x Core..?> "repositoryName")
            Prelude.<*> (x Core..?> "imageId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StartImageScan

instance Prelude.NFData StartImageScan

instance Core.ToHeaders StartImageScan where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonEC2ContainerRegistry_V20150921.StartImageScan" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON StartImageScan where
  toJSON StartImageScan' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("registryId" Core..=) Prelude.<$> registryId,
            Prelude.Just
              ("repositoryName" Core..= repositoryName),
            Prelude.Just ("imageId" Core..= imageId)
          ]
      )

instance Core.ToPath StartImageScan where
  toPath = Prelude.const "/"

instance Core.ToQuery StartImageScan where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartImageScanResponse' smart constructor.
data StartImageScanResponse = StartImageScanResponse'
  { -- | The current state of the scan.
    imageScanStatus :: Prelude.Maybe ImageScanStatus,
    -- | The registry ID associated with the request.
    registryId :: Prelude.Maybe Prelude.Text,
    -- | The repository name associated with the request.
    repositoryName :: Prelude.Maybe Prelude.Text,
    imageId :: Prelude.Maybe ImageIdentifier,
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
-- 'imageScanStatus', 'startImageScanResponse_imageScanStatus' - The current state of the scan.
--
-- 'registryId', 'startImageScanResponse_registryId' - The registry ID associated with the request.
--
-- 'repositoryName', 'startImageScanResponse_repositoryName' - The repository name associated with the request.
--
-- 'imageId', 'startImageScanResponse_imageId' - Undocumented member.
--
-- 'httpStatus', 'startImageScanResponse_httpStatus' - The response's http status code.
newStartImageScanResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StartImageScanResponse
newStartImageScanResponse pHttpStatus_ =
  StartImageScanResponse'
    { imageScanStatus =
        Prelude.Nothing,
      registryId = Prelude.Nothing,
      repositoryName = Prelude.Nothing,
      imageId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The current state of the scan.
startImageScanResponse_imageScanStatus :: Lens.Lens' StartImageScanResponse (Prelude.Maybe ImageScanStatus)
startImageScanResponse_imageScanStatus = Lens.lens (\StartImageScanResponse' {imageScanStatus} -> imageScanStatus) (\s@StartImageScanResponse' {} a -> s {imageScanStatus = a} :: StartImageScanResponse)

-- | The registry ID associated with the request.
startImageScanResponse_registryId :: Lens.Lens' StartImageScanResponse (Prelude.Maybe Prelude.Text)
startImageScanResponse_registryId = Lens.lens (\StartImageScanResponse' {registryId} -> registryId) (\s@StartImageScanResponse' {} a -> s {registryId = a} :: StartImageScanResponse)

-- | The repository name associated with the request.
startImageScanResponse_repositoryName :: Lens.Lens' StartImageScanResponse (Prelude.Maybe Prelude.Text)
startImageScanResponse_repositoryName = Lens.lens (\StartImageScanResponse' {repositoryName} -> repositoryName) (\s@StartImageScanResponse' {} a -> s {repositoryName = a} :: StartImageScanResponse)

-- | Undocumented member.
startImageScanResponse_imageId :: Lens.Lens' StartImageScanResponse (Prelude.Maybe ImageIdentifier)
startImageScanResponse_imageId = Lens.lens (\StartImageScanResponse' {imageId} -> imageId) (\s@StartImageScanResponse' {} a -> s {imageId = a} :: StartImageScanResponse)

-- | The response's http status code.
startImageScanResponse_httpStatus :: Lens.Lens' StartImageScanResponse Prelude.Int
startImageScanResponse_httpStatus = Lens.lens (\StartImageScanResponse' {httpStatus} -> httpStatus) (\s@StartImageScanResponse' {} a -> s {httpStatus = a} :: StartImageScanResponse)

instance Prelude.NFData StartImageScanResponse
