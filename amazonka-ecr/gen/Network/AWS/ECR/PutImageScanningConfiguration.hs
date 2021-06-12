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
-- Module      : Network.AWS.ECR.PutImageScanningConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the image scanning configuration for the specified repository.
module Network.AWS.ECR.PutImageScanningConfiguration
  ( -- * Creating a Request
    PutImageScanningConfiguration (..),
    newPutImageScanningConfiguration,

    -- * Request Lenses
    putImageScanningConfiguration_registryId,
    putImageScanningConfiguration_repositoryName,
    putImageScanningConfiguration_imageScanningConfiguration,

    -- * Destructuring the Response
    PutImageScanningConfigurationResponse (..),
    newPutImageScanningConfigurationResponse,

    -- * Response Lenses
    putImageScanningConfigurationResponse_registryId,
    putImageScanningConfigurationResponse_repositoryName,
    putImageScanningConfigurationResponse_imageScanningConfiguration,
    putImageScanningConfigurationResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.ECR.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newPutImageScanningConfiguration' smart constructor.
data PutImageScanningConfiguration = PutImageScanningConfiguration'
  { -- | The AWS account ID associated with the registry that contains the
    -- repository in which to update the image scanning configuration setting.
    -- If you do not specify a registry, the default registry is assumed.
    registryId :: Core.Maybe Core.Text,
    -- | The name of the repository in which to update the image scanning
    -- configuration setting.
    repositoryName :: Core.Text,
    -- | The image scanning configuration for the repository. This setting
    -- determines whether images are scanned for known vulnerabilities after
    -- being pushed to the repository.
    imageScanningConfiguration :: ImageScanningConfiguration
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'PutImageScanningConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'registryId', 'putImageScanningConfiguration_registryId' - The AWS account ID associated with the registry that contains the
-- repository in which to update the image scanning configuration setting.
-- If you do not specify a registry, the default registry is assumed.
--
-- 'repositoryName', 'putImageScanningConfiguration_repositoryName' - The name of the repository in which to update the image scanning
-- configuration setting.
--
-- 'imageScanningConfiguration', 'putImageScanningConfiguration_imageScanningConfiguration' - The image scanning configuration for the repository. This setting
-- determines whether images are scanned for known vulnerabilities after
-- being pushed to the repository.
newPutImageScanningConfiguration ::
  -- | 'repositoryName'
  Core.Text ->
  -- | 'imageScanningConfiguration'
  ImageScanningConfiguration ->
  PutImageScanningConfiguration
newPutImageScanningConfiguration
  pRepositoryName_
  pImageScanningConfiguration_ =
    PutImageScanningConfiguration'
      { registryId =
          Core.Nothing,
        repositoryName = pRepositoryName_,
        imageScanningConfiguration =
          pImageScanningConfiguration_
      }

-- | The AWS account ID associated with the registry that contains the
-- repository in which to update the image scanning configuration setting.
-- If you do not specify a registry, the default registry is assumed.
putImageScanningConfiguration_registryId :: Lens.Lens' PutImageScanningConfiguration (Core.Maybe Core.Text)
putImageScanningConfiguration_registryId = Lens.lens (\PutImageScanningConfiguration' {registryId} -> registryId) (\s@PutImageScanningConfiguration' {} a -> s {registryId = a} :: PutImageScanningConfiguration)

-- | The name of the repository in which to update the image scanning
-- configuration setting.
putImageScanningConfiguration_repositoryName :: Lens.Lens' PutImageScanningConfiguration Core.Text
putImageScanningConfiguration_repositoryName = Lens.lens (\PutImageScanningConfiguration' {repositoryName} -> repositoryName) (\s@PutImageScanningConfiguration' {} a -> s {repositoryName = a} :: PutImageScanningConfiguration)

-- | The image scanning configuration for the repository. This setting
-- determines whether images are scanned for known vulnerabilities after
-- being pushed to the repository.
putImageScanningConfiguration_imageScanningConfiguration :: Lens.Lens' PutImageScanningConfiguration ImageScanningConfiguration
putImageScanningConfiguration_imageScanningConfiguration = Lens.lens (\PutImageScanningConfiguration' {imageScanningConfiguration} -> imageScanningConfiguration) (\s@PutImageScanningConfiguration' {} a -> s {imageScanningConfiguration = a} :: PutImageScanningConfiguration)

instance
  Core.AWSRequest
    PutImageScanningConfiguration
  where
  type
    AWSResponse PutImageScanningConfiguration =
      PutImageScanningConfigurationResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          PutImageScanningConfigurationResponse'
            Core.<$> (x Core..?> "registryId")
            Core.<*> (x Core..?> "repositoryName")
            Core.<*> (x Core..?> "imageScanningConfiguration")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable PutImageScanningConfiguration

instance Core.NFData PutImageScanningConfiguration

instance Core.ToHeaders PutImageScanningConfiguration where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonEC2ContainerRegistry_V20150921.PutImageScanningConfiguration" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON PutImageScanningConfiguration where
  toJSON PutImageScanningConfiguration' {..} =
    Core.object
      ( Core.catMaybes
          [ ("registryId" Core..=) Core.<$> registryId,
            Core.Just ("repositoryName" Core..= repositoryName),
            Core.Just
              ( "imageScanningConfiguration"
                  Core..= imageScanningConfiguration
              )
          ]
      )

instance Core.ToPath PutImageScanningConfiguration where
  toPath = Core.const "/"

instance Core.ToQuery PutImageScanningConfiguration where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newPutImageScanningConfigurationResponse' smart constructor.
data PutImageScanningConfigurationResponse = PutImageScanningConfigurationResponse'
  { -- | The registry ID associated with the request.
    registryId :: Core.Maybe Core.Text,
    -- | The repository name associated with the request.
    repositoryName :: Core.Maybe Core.Text,
    -- | The image scanning configuration setting for the repository.
    imageScanningConfiguration :: Core.Maybe ImageScanningConfiguration,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'PutImageScanningConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'registryId', 'putImageScanningConfigurationResponse_registryId' - The registry ID associated with the request.
--
-- 'repositoryName', 'putImageScanningConfigurationResponse_repositoryName' - The repository name associated with the request.
--
-- 'imageScanningConfiguration', 'putImageScanningConfigurationResponse_imageScanningConfiguration' - The image scanning configuration setting for the repository.
--
-- 'httpStatus', 'putImageScanningConfigurationResponse_httpStatus' - The response's http status code.
newPutImageScanningConfigurationResponse ::
  -- | 'httpStatus'
  Core.Int ->
  PutImageScanningConfigurationResponse
newPutImageScanningConfigurationResponse pHttpStatus_ =
  PutImageScanningConfigurationResponse'
    { registryId =
        Core.Nothing,
      repositoryName = Core.Nothing,
      imageScanningConfiguration =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The registry ID associated with the request.
putImageScanningConfigurationResponse_registryId :: Lens.Lens' PutImageScanningConfigurationResponse (Core.Maybe Core.Text)
putImageScanningConfigurationResponse_registryId = Lens.lens (\PutImageScanningConfigurationResponse' {registryId} -> registryId) (\s@PutImageScanningConfigurationResponse' {} a -> s {registryId = a} :: PutImageScanningConfigurationResponse)

-- | The repository name associated with the request.
putImageScanningConfigurationResponse_repositoryName :: Lens.Lens' PutImageScanningConfigurationResponse (Core.Maybe Core.Text)
putImageScanningConfigurationResponse_repositoryName = Lens.lens (\PutImageScanningConfigurationResponse' {repositoryName} -> repositoryName) (\s@PutImageScanningConfigurationResponse' {} a -> s {repositoryName = a} :: PutImageScanningConfigurationResponse)

-- | The image scanning configuration setting for the repository.
putImageScanningConfigurationResponse_imageScanningConfiguration :: Lens.Lens' PutImageScanningConfigurationResponse (Core.Maybe ImageScanningConfiguration)
putImageScanningConfigurationResponse_imageScanningConfiguration = Lens.lens (\PutImageScanningConfigurationResponse' {imageScanningConfiguration} -> imageScanningConfiguration) (\s@PutImageScanningConfigurationResponse' {} a -> s {imageScanningConfiguration = a} :: PutImageScanningConfigurationResponse)

-- | The response's http status code.
putImageScanningConfigurationResponse_httpStatus :: Lens.Lens' PutImageScanningConfigurationResponse Core.Int
putImageScanningConfigurationResponse_httpStatus = Lens.lens (\PutImageScanningConfigurationResponse' {httpStatus} -> httpStatus) (\s@PutImageScanningConfigurationResponse' {} a -> s {httpStatus = a} :: PutImageScanningConfigurationResponse)

instance
  Core.NFData
    PutImageScanningConfigurationResponse
