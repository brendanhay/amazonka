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
-- Module      : Amazonka.Inspector2.GetEc2DeepInspectionConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the activation status of Amazon Inspector deep inspection and
-- custom paths associated with your account.
module Amazonka.Inspector2.GetEc2DeepInspectionConfiguration
  ( -- * Creating a Request
    GetEc2DeepInspectionConfiguration (..),
    newGetEc2DeepInspectionConfiguration,

    -- * Destructuring the Response
    GetEc2DeepInspectionConfigurationResponse (..),
    newGetEc2DeepInspectionConfigurationResponse,

    -- * Response Lenses
    getEc2DeepInspectionConfigurationResponse_errorMessage,
    getEc2DeepInspectionConfigurationResponse_orgPackagePaths,
    getEc2DeepInspectionConfigurationResponse_packagePaths,
    getEc2DeepInspectionConfigurationResponse_status,
    getEc2DeepInspectionConfigurationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Inspector2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetEc2DeepInspectionConfiguration' smart constructor.
data GetEc2DeepInspectionConfiguration = GetEc2DeepInspectionConfiguration'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetEc2DeepInspectionConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newGetEc2DeepInspectionConfiguration ::
  GetEc2DeepInspectionConfiguration
newGetEc2DeepInspectionConfiguration =
  GetEc2DeepInspectionConfiguration'

instance
  Core.AWSRequest
    GetEc2DeepInspectionConfiguration
  where
  type
    AWSResponse GetEc2DeepInspectionConfiguration =
      GetEc2DeepInspectionConfigurationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetEc2DeepInspectionConfigurationResponse'
            Prelude.<$> (x Data..?> "errorMessage")
            Prelude.<*> ( x
                            Data..?> "orgPackagePaths"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "packagePaths" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "status")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    GetEc2DeepInspectionConfiguration
  where
  hashWithSalt _salt _ =
    _salt `Prelude.hashWithSalt` ()

instance
  Prelude.NFData
    GetEc2DeepInspectionConfiguration
  where
  rnf _ = ()

instance
  Data.ToHeaders
    GetEc2DeepInspectionConfiguration
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Data.ToJSON
    GetEc2DeepInspectionConfiguration
  where
  toJSON = Prelude.const (Data.Object Prelude.mempty)

instance
  Data.ToPath
    GetEc2DeepInspectionConfiguration
  where
  toPath =
    Prelude.const "/ec2deepinspectionconfiguration/get"

instance
  Data.ToQuery
    GetEc2DeepInspectionConfiguration
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetEc2DeepInspectionConfigurationResponse' smart constructor.
data GetEc2DeepInspectionConfigurationResponse = GetEc2DeepInspectionConfigurationResponse'
  { -- | An error message explaining why Amazon Inspector deep inspection
    -- configurations could not be retrieved for your account.
    errorMessage :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Inspector deep inspection custom paths for your organization.
    orgPackagePaths :: Prelude.Maybe [Prelude.Text],
    -- | The Amazon Inspector deep inspection custom paths for your account.
    packagePaths :: Prelude.Maybe [Prelude.Text],
    -- | The activation status of Amazon Inspector deep inspection in your
    -- account.
    status :: Prelude.Maybe Ec2DeepInspectionStatus,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetEc2DeepInspectionConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'errorMessage', 'getEc2DeepInspectionConfigurationResponse_errorMessage' - An error message explaining why Amazon Inspector deep inspection
-- configurations could not be retrieved for your account.
--
-- 'orgPackagePaths', 'getEc2DeepInspectionConfigurationResponse_orgPackagePaths' - The Amazon Inspector deep inspection custom paths for your organization.
--
-- 'packagePaths', 'getEc2DeepInspectionConfigurationResponse_packagePaths' - The Amazon Inspector deep inspection custom paths for your account.
--
-- 'status', 'getEc2DeepInspectionConfigurationResponse_status' - The activation status of Amazon Inspector deep inspection in your
-- account.
--
-- 'httpStatus', 'getEc2DeepInspectionConfigurationResponse_httpStatus' - The response's http status code.
newGetEc2DeepInspectionConfigurationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetEc2DeepInspectionConfigurationResponse
newGetEc2DeepInspectionConfigurationResponse
  pHttpStatus_ =
    GetEc2DeepInspectionConfigurationResponse'
      { errorMessage =
          Prelude.Nothing,
        orgPackagePaths =
          Prelude.Nothing,
        packagePaths = Prelude.Nothing,
        status = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | An error message explaining why Amazon Inspector deep inspection
-- configurations could not be retrieved for your account.
getEc2DeepInspectionConfigurationResponse_errorMessage :: Lens.Lens' GetEc2DeepInspectionConfigurationResponse (Prelude.Maybe Prelude.Text)
getEc2DeepInspectionConfigurationResponse_errorMessage = Lens.lens (\GetEc2DeepInspectionConfigurationResponse' {errorMessage} -> errorMessage) (\s@GetEc2DeepInspectionConfigurationResponse' {} a -> s {errorMessage = a} :: GetEc2DeepInspectionConfigurationResponse)

-- | The Amazon Inspector deep inspection custom paths for your organization.
getEc2DeepInspectionConfigurationResponse_orgPackagePaths :: Lens.Lens' GetEc2DeepInspectionConfigurationResponse (Prelude.Maybe [Prelude.Text])
getEc2DeepInspectionConfigurationResponse_orgPackagePaths = Lens.lens (\GetEc2DeepInspectionConfigurationResponse' {orgPackagePaths} -> orgPackagePaths) (\s@GetEc2DeepInspectionConfigurationResponse' {} a -> s {orgPackagePaths = a} :: GetEc2DeepInspectionConfigurationResponse) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Inspector deep inspection custom paths for your account.
getEc2DeepInspectionConfigurationResponse_packagePaths :: Lens.Lens' GetEc2DeepInspectionConfigurationResponse (Prelude.Maybe [Prelude.Text])
getEc2DeepInspectionConfigurationResponse_packagePaths = Lens.lens (\GetEc2DeepInspectionConfigurationResponse' {packagePaths} -> packagePaths) (\s@GetEc2DeepInspectionConfigurationResponse' {} a -> s {packagePaths = a} :: GetEc2DeepInspectionConfigurationResponse) Prelude.. Lens.mapping Lens.coerced

-- | The activation status of Amazon Inspector deep inspection in your
-- account.
getEc2DeepInspectionConfigurationResponse_status :: Lens.Lens' GetEc2DeepInspectionConfigurationResponse (Prelude.Maybe Ec2DeepInspectionStatus)
getEc2DeepInspectionConfigurationResponse_status = Lens.lens (\GetEc2DeepInspectionConfigurationResponse' {status} -> status) (\s@GetEc2DeepInspectionConfigurationResponse' {} a -> s {status = a} :: GetEc2DeepInspectionConfigurationResponse)

-- | The response's http status code.
getEc2DeepInspectionConfigurationResponse_httpStatus :: Lens.Lens' GetEc2DeepInspectionConfigurationResponse Prelude.Int
getEc2DeepInspectionConfigurationResponse_httpStatus = Lens.lens (\GetEc2DeepInspectionConfigurationResponse' {httpStatus} -> httpStatus) (\s@GetEc2DeepInspectionConfigurationResponse' {} a -> s {httpStatus = a} :: GetEc2DeepInspectionConfigurationResponse)

instance
  Prelude.NFData
    GetEc2DeepInspectionConfigurationResponse
  where
  rnf GetEc2DeepInspectionConfigurationResponse' {..} =
    Prelude.rnf errorMessage
      `Prelude.seq` Prelude.rnf orgPackagePaths
      `Prelude.seq` Prelude.rnf packagePaths
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf httpStatus
