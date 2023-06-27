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
-- Module      : Amazonka.IoT.GetPackageConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about the specified software package\'s configuration.
--
-- Requires permission to access the
-- <https://docs.aws.amazon.com/service-authorization/latest/reference/list_awsiot.html#awsiot-actions-as-permissions GetPackageConfiguration>
-- action.
module Amazonka.IoT.GetPackageConfiguration
  ( -- * Creating a Request
    GetPackageConfiguration (..),
    newGetPackageConfiguration,

    -- * Destructuring the Response
    GetPackageConfigurationResponse (..),
    newGetPackageConfigurationResponse,

    -- * Response Lenses
    getPackageConfigurationResponse_versionUpdateByJobsConfig,
    getPackageConfigurationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoT.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetPackageConfiguration' smart constructor.
data GetPackageConfiguration = GetPackageConfiguration'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetPackageConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newGetPackageConfiguration ::
  GetPackageConfiguration
newGetPackageConfiguration = GetPackageConfiguration'

instance Core.AWSRequest GetPackageConfiguration where
  type
    AWSResponse GetPackageConfiguration =
      GetPackageConfigurationResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetPackageConfigurationResponse'
            Prelude.<$> (x Data..?> "versionUpdateByJobsConfig")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetPackageConfiguration where
  hashWithSalt _salt _ =
    _salt `Prelude.hashWithSalt` ()

instance Prelude.NFData GetPackageConfiguration where
  rnf _ = ()

instance Data.ToHeaders GetPackageConfiguration where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath GetPackageConfiguration where
  toPath = Prelude.const "/package-configuration"

instance Data.ToQuery GetPackageConfiguration where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetPackageConfigurationResponse' smart constructor.
data GetPackageConfigurationResponse = GetPackageConfigurationResponse'
  { -- | The version that is associated to a specific job.
    versionUpdateByJobsConfig :: Prelude.Maybe VersionUpdateByJobsConfig,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetPackageConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'versionUpdateByJobsConfig', 'getPackageConfigurationResponse_versionUpdateByJobsConfig' - The version that is associated to a specific job.
--
-- 'httpStatus', 'getPackageConfigurationResponse_httpStatus' - The response's http status code.
newGetPackageConfigurationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetPackageConfigurationResponse
newGetPackageConfigurationResponse pHttpStatus_ =
  GetPackageConfigurationResponse'
    { versionUpdateByJobsConfig =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The version that is associated to a specific job.
getPackageConfigurationResponse_versionUpdateByJobsConfig :: Lens.Lens' GetPackageConfigurationResponse (Prelude.Maybe VersionUpdateByJobsConfig)
getPackageConfigurationResponse_versionUpdateByJobsConfig = Lens.lens (\GetPackageConfigurationResponse' {versionUpdateByJobsConfig} -> versionUpdateByJobsConfig) (\s@GetPackageConfigurationResponse' {} a -> s {versionUpdateByJobsConfig = a} :: GetPackageConfigurationResponse)

-- | The response's http status code.
getPackageConfigurationResponse_httpStatus :: Lens.Lens' GetPackageConfigurationResponse Prelude.Int
getPackageConfigurationResponse_httpStatus = Lens.lens (\GetPackageConfigurationResponse' {httpStatus} -> httpStatus) (\s@GetPackageConfigurationResponse' {} a -> s {httpStatus = a} :: GetPackageConfigurationResponse)

instance
  Prelude.NFData
    GetPackageConfigurationResponse
  where
  rnf GetPackageConfigurationResponse' {..} =
    Prelude.rnf versionUpdateByJobsConfig
      `Prelude.seq` Prelude.rnf httpStatus
