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
-- Module      : Amazonka.SecurityLake.GetDataLakeOrganizationConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the configuration that will be automatically set up for
-- accounts added to the organization after the organization has onboarded
-- to Amazon Security Lake. This API does not take input parameters.
module Amazonka.SecurityLake.GetDataLakeOrganizationConfiguration
  ( -- * Creating a Request
    GetDataLakeOrganizationConfiguration (..),
    newGetDataLakeOrganizationConfiguration,

    -- * Destructuring the Response
    GetDataLakeOrganizationConfigurationResponse (..),
    newGetDataLakeOrganizationConfigurationResponse,

    -- * Response Lenses
    getDataLakeOrganizationConfigurationResponse_autoEnableNewAccount,
    getDataLakeOrganizationConfigurationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SecurityLake.Types

-- | /See:/ 'newGetDataLakeOrganizationConfiguration' smart constructor.
data GetDataLakeOrganizationConfiguration = GetDataLakeOrganizationConfiguration'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetDataLakeOrganizationConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newGetDataLakeOrganizationConfiguration ::
  GetDataLakeOrganizationConfiguration
newGetDataLakeOrganizationConfiguration =
  GetDataLakeOrganizationConfiguration'

instance
  Core.AWSRequest
    GetDataLakeOrganizationConfiguration
  where
  type
    AWSResponse GetDataLakeOrganizationConfiguration =
      GetDataLakeOrganizationConfigurationResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetDataLakeOrganizationConfigurationResponse'
            Prelude.<$> ( x
                            Data..?> "autoEnableNewAccount"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    GetDataLakeOrganizationConfiguration
  where
  hashWithSalt _salt _ =
    _salt `Prelude.hashWithSalt` ()

instance
  Prelude.NFData
    GetDataLakeOrganizationConfiguration
  where
  rnf _ = ()

instance
  Data.ToHeaders
    GetDataLakeOrganizationConfiguration
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
  Data.ToPath
    GetDataLakeOrganizationConfiguration
  where
  toPath =
    Prelude.const
      "/v1/datalake/organization/configuration"

instance
  Data.ToQuery
    GetDataLakeOrganizationConfiguration
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetDataLakeOrganizationConfigurationResponse' smart constructor.
data GetDataLakeOrganizationConfigurationResponse = GetDataLakeOrganizationConfigurationResponse'
  { -- | The configuration for new accounts.
    autoEnableNewAccount :: Prelude.Maybe [DataLakeAutoEnableNewAccountConfiguration],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetDataLakeOrganizationConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'autoEnableNewAccount', 'getDataLakeOrganizationConfigurationResponse_autoEnableNewAccount' - The configuration for new accounts.
--
-- 'httpStatus', 'getDataLakeOrganizationConfigurationResponse_httpStatus' - The response's http status code.
newGetDataLakeOrganizationConfigurationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetDataLakeOrganizationConfigurationResponse
newGetDataLakeOrganizationConfigurationResponse
  pHttpStatus_ =
    GetDataLakeOrganizationConfigurationResponse'
      { autoEnableNewAccount =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The configuration for new accounts.
getDataLakeOrganizationConfigurationResponse_autoEnableNewAccount :: Lens.Lens' GetDataLakeOrganizationConfigurationResponse (Prelude.Maybe [DataLakeAutoEnableNewAccountConfiguration])
getDataLakeOrganizationConfigurationResponse_autoEnableNewAccount = Lens.lens (\GetDataLakeOrganizationConfigurationResponse' {autoEnableNewAccount} -> autoEnableNewAccount) (\s@GetDataLakeOrganizationConfigurationResponse' {} a -> s {autoEnableNewAccount = a} :: GetDataLakeOrganizationConfigurationResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getDataLakeOrganizationConfigurationResponse_httpStatus :: Lens.Lens' GetDataLakeOrganizationConfigurationResponse Prelude.Int
getDataLakeOrganizationConfigurationResponse_httpStatus = Lens.lens (\GetDataLakeOrganizationConfigurationResponse' {httpStatus} -> httpStatus) (\s@GetDataLakeOrganizationConfigurationResponse' {} a -> s {httpStatus = a} :: GetDataLakeOrganizationConfigurationResponse)

instance
  Prelude.NFData
    GetDataLakeOrganizationConfigurationResponse
  where
  rnf GetDataLakeOrganizationConfigurationResponse' {..} =
    Prelude.rnf autoEnableNewAccount
      `Prelude.seq` Prelude.rnf httpStatus
