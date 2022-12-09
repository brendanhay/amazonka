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
-- Module      : Amazonka.ECR.PutImageScanningConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The @PutImageScanningConfiguration@ API is being deprecated, in favor of
-- specifying the image scanning configuration at the registry level. For
-- more information, see PutRegistryScanningConfiguration.
--
-- Updates the image scanning configuration for the specified repository.
module Amazonka.ECR.PutImageScanningConfiguration
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
    putImageScanningConfigurationResponse_imageScanningConfiguration,
    putImageScanningConfigurationResponse_registryId,
    putImageScanningConfigurationResponse_repositoryName,
    putImageScanningConfigurationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ECR.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newPutImageScanningConfiguration' smart constructor.
data PutImageScanningConfiguration = PutImageScanningConfiguration'
  { -- | The Amazon Web Services account ID associated with the registry that
    -- contains the repository in which to update the image scanning
    -- configuration setting. If you do not specify a registry, the default
    -- registry is assumed.
    registryId :: Prelude.Maybe Prelude.Text,
    -- | The name of the repository in which to update the image scanning
    -- configuration setting.
    repositoryName :: Prelude.Text,
    -- | The image scanning configuration for the repository. This setting
    -- determines whether images are scanned for known vulnerabilities after
    -- being pushed to the repository.
    imageScanningConfiguration :: ImageScanningConfiguration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutImageScanningConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'registryId', 'putImageScanningConfiguration_registryId' - The Amazon Web Services account ID associated with the registry that
-- contains the repository in which to update the image scanning
-- configuration setting. If you do not specify a registry, the default
-- registry is assumed.
--
-- 'repositoryName', 'putImageScanningConfiguration_repositoryName' - The name of the repository in which to update the image scanning
-- configuration setting.
--
-- 'imageScanningConfiguration', 'putImageScanningConfiguration_imageScanningConfiguration' - The image scanning configuration for the repository. This setting
-- determines whether images are scanned for known vulnerabilities after
-- being pushed to the repository.
newPutImageScanningConfiguration ::
  -- | 'repositoryName'
  Prelude.Text ->
  -- | 'imageScanningConfiguration'
  ImageScanningConfiguration ->
  PutImageScanningConfiguration
newPutImageScanningConfiguration
  pRepositoryName_
  pImageScanningConfiguration_ =
    PutImageScanningConfiguration'
      { registryId =
          Prelude.Nothing,
        repositoryName = pRepositoryName_,
        imageScanningConfiguration =
          pImageScanningConfiguration_
      }

-- | The Amazon Web Services account ID associated with the registry that
-- contains the repository in which to update the image scanning
-- configuration setting. If you do not specify a registry, the default
-- registry is assumed.
putImageScanningConfiguration_registryId :: Lens.Lens' PutImageScanningConfiguration (Prelude.Maybe Prelude.Text)
putImageScanningConfiguration_registryId = Lens.lens (\PutImageScanningConfiguration' {registryId} -> registryId) (\s@PutImageScanningConfiguration' {} a -> s {registryId = a} :: PutImageScanningConfiguration)

-- | The name of the repository in which to update the image scanning
-- configuration setting.
putImageScanningConfiguration_repositoryName :: Lens.Lens' PutImageScanningConfiguration Prelude.Text
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
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          PutImageScanningConfigurationResponse'
            Prelude.<$> (x Data..?> "imageScanningConfiguration")
            Prelude.<*> (x Data..?> "registryId")
            Prelude.<*> (x Data..?> "repositoryName")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    PutImageScanningConfiguration
  where
  hashWithSalt _salt PutImageScanningConfiguration' {..} =
    _salt `Prelude.hashWithSalt` registryId
      `Prelude.hashWithSalt` repositoryName
      `Prelude.hashWithSalt` imageScanningConfiguration

instance Prelude.NFData PutImageScanningConfiguration where
  rnf PutImageScanningConfiguration' {..} =
    Prelude.rnf registryId
      `Prelude.seq` Prelude.rnf repositoryName
      `Prelude.seq` Prelude.rnf imageScanningConfiguration

instance Data.ToHeaders PutImageScanningConfiguration where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonEC2ContainerRegistry_V20150921.PutImageScanningConfiguration" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON PutImageScanningConfiguration where
  toJSON PutImageScanningConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("registryId" Data..=) Prelude.<$> registryId,
            Prelude.Just
              ("repositoryName" Data..= repositoryName),
            Prelude.Just
              ( "imageScanningConfiguration"
                  Data..= imageScanningConfiguration
              )
          ]
      )

instance Data.ToPath PutImageScanningConfiguration where
  toPath = Prelude.const "/"

instance Data.ToQuery PutImageScanningConfiguration where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPutImageScanningConfigurationResponse' smart constructor.
data PutImageScanningConfigurationResponse = PutImageScanningConfigurationResponse'
  { -- | The image scanning configuration setting for the repository.
    imageScanningConfiguration :: Prelude.Maybe ImageScanningConfiguration,
    -- | The registry ID associated with the request.
    registryId :: Prelude.Maybe Prelude.Text,
    -- | The repository name associated with the request.
    repositoryName :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutImageScanningConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'imageScanningConfiguration', 'putImageScanningConfigurationResponse_imageScanningConfiguration' - The image scanning configuration setting for the repository.
--
-- 'registryId', 'putImageScanningConfigurationResponse_registryId' - The registry ID associated with the request.
--
-- 'repositoryName', 'putImageScanningConfigurationResponse_repositoryName' - The repository name associated with the request.
--
-- 'httpStatus', 'putImageScanningConfigurationResponse_httpStatus' - The response's http status code.
newPutImageScanningConfigurationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  PutImageScanningConfigurationResponse
newPutImageScanningConfigurationResponse pHttpStatus_ =
  PutImageScanningConfigurationResponse'
    { imageScanningConfiguration =
        Prelude.Nothing,
      registryId = Prelude.Nothing,
      repositoryName = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The image scanning configuration setting for the repository.
putImageScanningConfigurationResponse_imageScanningConfiguration :: Lens.Lens' PutImageScanningConfigurationResponse (Prelude.Maybe ImageScanningConfiguration)
putImageScanningConfigurationResponse_imageScanningConfiguration = Lens.lens (\PutImageScanningConfigurationResponse' {imageScanningConfiguration} -> imageScanningConfiguration) (\s@PutImageScanningConfigurationResponse' {} a -> s {imageScanningConfiguration = a} :: PutImageScanningConfigurationResponse)

-- | The registry ID associated with the request.
putImageScanningConfigurationResponse_registryId :: Lens.Lens' PutImageScanningConfigurationResponse (Prelude.Maybe Prelude.Text)
putImageScanningConfigurationResponse_registryId = Lens.lens (\PutImageScanningConfigurationResponse' {registryId} -> registryId) (\s@PutImageScanningConfigurationResponse' {} a -> s {registryId = a} :: PutImageScanningConfigurationResponse)

-- | The repository name associated with the request.
putImageScanningConfigurationResponse_repositoryName :: Lens.Lens' PutImageScanningConfigurationResponse (Prelude.Maybe Prelude.Text)
putImageScanningConfigurationResponse_repositoryName = Lens.lens (\PutImageScanningConfigurationResponse' {repositoryName} -> repositoryName) (\s@PutImageScanningConfigurationResponse' {} a -> s {repositoryName = a} :: PutImageScanningConfigurationResponse)

-- | The response's http status code.
putImageScanningConfigurationResponse_httpStatus :: Lens.Lens' PutImageScanningConfigurationResponse Prelude.Int
putImageScanningConfigurationResponse_httpStatus = Lens.lens (\PutImageScanningConfigurationResponse' {httpStatus} -> httpStatus) (\s@PutImageScanningConfigurationResponse' {} a -> s {httpStatus = a} :: PutImageScanningConfigurationResponse)

instance
  Prelude.NFData
    PutImageScanningConfigurationResponse
  where
  rnf PutImageScanningConfigurationResponse' {..} =
    Prelude.rnf imageScanningConfiguration
      `Prelude.seq` Prelude.rnf registryId
      `Prelude.seq` Prelude.rnf repositoryName
      `Prelude.seq` Prelude.rnf httpStatus
