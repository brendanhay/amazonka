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
-- Module      : Amazonka.ECR.GetRegistryScanningConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the scanning configuration for a registry.
module Amazonka.ECR.GetRegistryScanningConfiguration
  ( -- * Creating a Request
    GetRegistryScanningConfiguration (..),
    newGetRegistryScanningConfiguration,

    -- * Destructuring the Response
    GetRegistryScanningConfigurationResponse (..),
    newGetRegistryScanningConfigurationResponse,

    -- * Response Lenses
    getRegistryScanningConfigurationResponse_scanningConfiguration,
    getRegistryScanningConfigurationResponse_registryId,
    getRegistryScanningConfigurationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.ECR.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetRegistryScanningConfiguration' smart constructor.
data GetRegistryScanningConfiguration = GetRegistryScanningConfiguration'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetRegistryScanningConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newGetRegistryScanningConfiguration ::
  GetRegistryScanningConfiguration
newGetRegistryScanningConfiguration =
  GetRegistryScanningConfiguration'

instance
  Core.AWSRequest
    GetRegistryScanningConfiguration
  where
  type
    AWSResponse GetRegistryScanningConfiguration =
      GetRegistryScanningConfigurationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetRegistryScanningConfigurationResponse'
            Prelude.<$> (x Core..?> "scanningConfiguration")
            Prelude.<*> (x Core..?> "registryId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    GetRegistryScanningConfiguration
  where
  hashWithSalt _salt _ =
    _salt `Prelude.hashWithSalt` ()

instance
  Prelude.NFData
    GetRegistryScanningConfiguration
  where
  rnf _ = ()

instance
  Core.ToHeaders
    GetRegistryScanningConfiguration
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonEC2ContainerRegistry_V20150921.GetRegistryScanningConfiguration" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON GetRegistryScanningConfiguration where
  toJSON = Prelude.const (Core.Object Prelude.mempty)

instance Core.ToPath GetRegistryScanningConfiguration where
  toPath = Prelude.const "/"

instance
  Core.ToQuery
    GetRegistryScanningConfiguration
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetRegistryScanningConfigurationResponse' smart constructor.
data GetRegistryScanningConfigurationResponse = GetRegistryScanningConfigurationResponse'
  { -- | The scanning configuration for the registry.
    scanningConfiguration :: Prelude.Maybe RegistryScanningConfiguration,
    -- | The ID of the registry.
    registryId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetRegistryScanningConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'scanningConfiguration', 'getRegistryScanningConfigurationResponse_scanningConfiguration' - The scanning configuration for the registry.
--
-- 'registryId', 'getRegistryScanningConfigurationResponse_registryId' - The ID of the registry.
--
-- 'httpStatus', 'getRegistryScanningConfigurationResponse_httpStatus' - The response's http status code.
newGetRegistryScanningConfigurationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetRegistryScanningConfigurationResponse
newGetRegistryScanningConfigurationResponse
  pHttpStatus_ =
    GetRegistryScanningConfigurationResponse'
      { scanningConfiguration =
          Prelude.Nothing,
        registryId = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The scanning configuration for the registry.
getRegistryScanningConfigurationResponse_scanningConfiguration :: Lens.Lens' GetRegistryScanningConfigurationResponse (Prelude.Maybe RegistryScanningConfiguration)
getRegistryScanningConfigurationResponse_scanningConfiguration = Lens.lens (\GetRegistryScanningConfigurationResponse' {scanningConfiguration} -> scanningConfiguration) (\s@GetRegistryScanningConfigurationResponse' {} a -> s {scanningConfiguration = a} :: GetRegistryScanningConfigurationResponse)

-- | The ID of the registry.
getRegistryScanningConfigurationResponse_registryId :: Lens.Lens' GetRegistryScanningConfigurationResponse (Prelude.Maybe Prelude.Text)
getRegistryScanningConfigurationResponse_registryId = Lens.lens (\GetRegistryScanningConfigurationResponse' {registryId} -> registryId) (\s@GetRegistryScanningConfigurationResponse' {} a -> s {registryId = a} :: GetRegistryScanningConfigurationResponse)

-- | The response's http status code.
getRegistryScanningConfigurationResponse_httpStatus :: Lens.Lens' GetRegistryScanningConfigurationResponse Prelude.Int
getRegistryScanningConfigurationResponse_httpStatus = Lens.lens (\GetRegistryScanningConfigurationResponse' {httpStatus} -> httpStatus) (\s@GetRegistryScanningConfigurationResponse' {} a -> s {httpStatus = a} :: GetRegistryScanningConfigurationResponse)

instance
  Prelude.NFData
    GetRegistryScanningConfigurationResponse
  where
  rnf GetRegistryScanningConfigurationResponse' {..} =
    Prelude.rnf scanningConfiguration
      `Prelude.seq` Prelude.rnf registryId
      `Prelude.seq` Prelude.rnf httpStatus
