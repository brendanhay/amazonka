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
-- Module      : Amazonka.ECR.PutRegistryScanningConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates or updates the scanning configuration for your private registry.
module Amazonka.ECR.PutRegistryScanningConfiguration
  ( -- * Creating a Request
    PutRegistryScanningConfiguration (..),
    newPutRegistryScanningConfiguration,

    -- * Request Lenses
    putRegistryScanningConfiguration_rules,
    putRegistryScanningConfiguration_scanType,

    -- * Destructuring the Response
    PutRegistryScanningConfigurationResponse (..),
    newPutRegistryScanningConfigurationResponse,

    -- * Response Lenses
    putRegistryScanningConfigurationResponse_registryScanningConfiguration,
    putRegistryScanningConfigurationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.ECR.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newPutRegistryScanningConfiguration' smart constructor.
data PutRegistryScanningConfiguration = PutRegistryScanningConfiguration'
  { -- | The scanning rules to use for the registry. A scanning rule is used to
    -- determine which repository filters are used and at what frequency
    -- scanning will occur.
    rules :: Prelude.Maybe [RegistryScanningRule],
    -- | The scanning type to set for the registry.
    --
    -- When a registry scanning configuration is not defined, by default the
    -- @BASIC@ scan type is used. When basic scanning is used, you may specify
    -- filters to determine which individual repositories, or all repositories,
    -- are scanned when new images are pushed to those repositories.
    -- Alternatively, you can do manual scans of images with basic scanning.
    --
    -- When the @ENHANCED@ scan type is set, Amazon Inspector provides
    -- automated vulnerability scanning. You may choose between continuous
    -- scanning or scan on push and you may specify filters to determine which
    -- individual repositories, or all repositories, are scanned.
    scanType :: Prelude.Maybe ScanType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutRegistryScanningConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'rules', 'putRegistryScanningConfiguration_rules' - The scanning rules to use for the registry. A scanning rule is used to
-- determine which repository filters are used and at what frequency
-- scanning will occur.
--
-- 'scanType', 'putRegistryScanningConfiguration_scanType' - The scanning type to set for the registry.
--
-- When a registry scanning configuration is not defined, by default the
-- @BASIC@ scan type is used. When basic scanning is used, you may specify
-- filters to determine which individual repositories, or all repositories,
-- are scanned when new images are pushed to those repositories.
-- Alternatively, you can do manual scans of images with basic scanning.
--
-- When the @ENHANCED@ scan type is set, Amazon Inspector provides
-- automated vulnerability scanning. You may choose between continuous
-- scanning or scan on push and you may specify filters to determine which
-- individual repositories, or all repositories, are scanned.
newPutRegistryScanningConfiguration ::
  PutRegistryScanningConfiguration
newPutRegistryScanningConfiguration =
  PutRegistryScanningConfiguration'
    { rules =
        Prelude.Nothing,
      scanType = Prelude.Nothing
    }

-- | The scanning rules to use for the registry. A scanning rule is used to
-- determine which repository filters are used and at what frequency
-- scanning will occur.
putRegistryScanningConfiguration_rules :: Lens.Lens' PutRegistryScanningConfiguration (Prelude.Maybe [RegistryScanningRule])
putRegistryScanningConfiguration_rules = Lens.lens (\PutRegistryScanningConfiguration' {rules} -> rules) (\s@PutRegistryScanningConfiguration' {} a -> s {rules = a} :: PutRegistryScanningConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | The scanning type to set for the registry.
--
-- When a registry scanning configuration is not defined, by default the
-- @BASIC@ scan type is used. When basic scanning is used, you may specify
-- filters to determine which individual repositories, or all repositories,
-- are scanned when new images are pushed to those repositories.
-- Alternatively, you can do manual scans of images with basic scanning.
--
-- When the @ENHANCED@ scan type is set, Amazon Inspector provides
-- automated vulnerability scanning. You may choose between continuous
-- scanning or scan on push and you may specify filters to determine which
-- individual repositories, or all repositories, are scanned.
putRegistryScanningConfiguration_scanType :: Lens.Lens' PutRegistryScanningConfiguration (Prelude.Maybe ScanType)
putRegistryScanningConfiguration_scanType = Lens.lens (\PutRegistryScanningConfiguration' {scanType} -> scanType) (\s@PutRegistryScanningConfiguration' {} a -> s {scanType = a} :: PutRegistryScanningConfiguration)

instance
  Core.AWSRequest
    PutRegistryScanningConfiguration
  where
  type
    AWSResponse PutRegistryScanningConfiguration =
      PutRegistryScanningConfigurationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          PutRegistryScanningConfigurationResponse'
            Prelude.<$> (x Core..?> "registryScanningConfiguration")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    PutRegistryScanningConfiguration
  where
  hashWithSalt
    _salt
    PutRegistryScanningConfiguration' {..} =
      _salt `Prelude.hashWithSalt` rules
        `Prelude.hashWithSalt` scanType

instance
  Prelude.NFData
    PutRegistryScanningConfiguration
  where
  rnf PutRegistryScanningConfiguration' {..} =
    Prelude.rnf rules
      `Prelude.seq` Prelude.rnf scanType

instance
  Core.ToHeaders
    PutRegistryScanningConfiguration
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonEC2ContainerRegistry_V20150921.PutRegistryScanningConfiguration" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON PutRegistryScanningConfiguration where
  toJSON PutRegistryScanningConfiguration' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("rules" Core..=) Prelude.<$> rules,
            ("scanType" Core..=) Prelude.<$> scanType
          ]
      )

instance Core.ToPath PutRegistryScanningConfiguration where
  toPath = Prelude.const "/"

instance
  Core.ToQuery
    PutRegistryScanningConfiguration
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPutRegistryScanningConfigurationResponse' smart constructor.
data PutRegistryScanningConfigurationResponse = PutRegistryScanningConfigurationResponse'
  { -- | The scanning configuration for your registry.
    registryScanningConfiguration :: Prelude.Maybe RegistryScanningConfiguration,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutRegistryScanningConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'registryScanningConfiguration', 'putRegistryScanningConfigurationResponse_registryScanningConfiguration' - The scanning configuration for your registry.
--
-- 'httpStatus', 'putRegistryScanningConfigurationResponse_httpStatus' - The response's http status code.
newPutRegistryScanningConfigurationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  PutRegistryScanningConfigurationResponse
newPutRegistryScanningConfigurationResponse
  pHttpStatus_ =
    PutRegistryScanningConfigurationResponse'
      { registryScanningConfiguration =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The scanning configuration for your registry.
putRegistryScanningConfigurationResponse_registryScanningConfiguration :: Lens.Lens' PutRegistryScanningConfigurationResponse (Prelude.Maybe RegistryScanningConfiguration)
putRegistryScanningConfigurationResponse_registryScanningConfiguration = Lens.lens (\PutRegistryScanningConfigurationResponse' {registryScanningConfiguration} -> registryScanningConfiguration) (\s@PutRegistryScanningConfigurationResponse' {} a -> s {registryScanningConfiguration = a} :: PutRegistryScanningConfigurationResponse)

-- | The response's http status code.
putRegistryScanningConfigurationResponse_httpStatus :: Lens.Lens' PutRegistryScanningConfigurationResponse Prelude.Int
putRegistryScanningConfigurationResponse_httpStatus = Lens.lens (\PutRegistryScanningConfigurationResponse' {httpStatus} -> httpStatus) (\s@PutRegistryScanningConfigurationResponse' {} a -> s {httpStatus = a} :: PutRegistryScanningConfigurationResponse)

instance
  Prelude.NFData
    PutRegistryScanningConfigurationResponse
  where
  rnf PutRegistryScanningConfigurationResponse' {..} =
    Prelude.rnf registryScanningConfiguration
      `Prelude.seq` Prelude.rnf httpStatus
