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
-- Module      : Amazonka.SecurityLake.CreateDataLakeOrganizationConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Automatically enables Amazon Security Lake for new member accounts in
-- your organization. Security Lake is not automatically enabled for any
-- existing member accounts in your organization.
module Amazonka.SecurityLake.CreateDataLakeOrganizationConfiguration
  ( -- * Creating a Request
    CreateDataLakeOrganizationConfiguration (..),
    newCreateDataLakeOrganizationConfiguration,

    -- * Request Lenses
    createDataLakeOrganizationConfiguration_autoEnableNewAccount,

    -- * Destructuring the Response
    CreateDataLakeOrganizationConfigurationResponse (..),
    newCreateDataLakeOrganizationConfigurationResponse,

    -- * Response Lenses
    createDataLakeOrganizationConfigurationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SecurityLake.Types

-- | /See:/ 'newCreateDataLakeOrganizationConfiguration' smart constructor.
data CreateDataLakeOrganizationConfiguration = CreateDataLakeOrganizationConfiguration'
  { -- | Enable Security Lake with the specified configuration settings, to begin
    -- collecting security data for new accounts in your organization.
    autoEnableNewAccount :: [DataLakeAutoEnableNewAccountConfiguration]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateDataLakeOrganizationConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'autoEnableNewAccount', 'createDataLakeOrganizationConfiguration_autoEnableNewAccount' - Enable Security Lake with the specified configuration settings, to begin
-- collecting security data for new accounts in your organization.
newCreateDataLakeOrganizationConfiguration ::
  CreateDataLakeOrganizationConfiguration
newCreateDataLakeOrganizationConfiguration =
  CreateDataLakeOrganizationConfiguration'
    { autoEnableNewAccount =
        Prelude.mempty
    }

-- | Enable Security Lake with the specified configuration settings, to begin
-- collecting security data for new accounts in your organization.
createDataLakeOrganizationConfiguration_autoEnableNewAccount :: Lens.Lens' CreateDataLakeOrganizationConfiguration [DataLakeAutoEnableNewAccountConfiguration]
createDataLakeOrganizationConfiguration_autoEnableNewAccount = Lens.lens (\CreateDataLakeOrganizationConfiguration' {autoEnableNewAccount} -> autoEnableNewAccount) (\s@CreateDataLakeOrganizationConfiguration' {} a -> s {autoEnableNewAccount = a} :: CreateDataLakeOrganizationConfiguration) Prelude.. Lens.coerced

instance
  Core.AWSRequest
    CreateDataLakeOrganizationConfiguration
  where
  type
    AWSResponse
      CreateDataLakeOrganizationConfiguration =
      CreateDataLakeOrganizationConfigurationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          CreateDataLakeOrganizationConfigurationResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    CreateDataLakeOrganizationConfiguration
  where
  hashWithSalt
    _salt
    CreateDataLakeOrganizationConfiguration' {..} =
      _salt `Prelude.hashWithSalt` autoEnableNewAccount

instance
  Prelude.NFData
    CreateDataLakeOrganizationConfiguration
  where
  rnf CreateDataLakeOrganizationConfiguration' {..} =
    Prelude.rnf autoEnableNewAccount

instance
  Data.ToHeaders
    CreateDataLakeOrganizationConfiguration
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
    CreateDataLakeOrganizationConfiguration
  where
  toJSON CreateDataLakeOrganizationConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "autoEnableNewAccount"
                  Data..= autoEnableNewAccount
              )
          ]
      )

instance
  Data.ToPath
    CreateDataLakeOrganizationConfiguration
  where
  toPath =
    Prelude.const
      "/v1/datalake/organization/configuration"

instance
  Data.ToQuery
    CreateDataLakeOrganizationConfiguration
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateDataLakeOrganizationConfigurationResponse' smart constructor.
data CreateDataLakeOrganizationConfigurationResponse = CreateDataLakeOrganizationConfigurationResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateDataLakeOrganizationConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createDataLakeOrganizationConfigurationResponse_httpStatus' - The response's http status code.
newCreateDataLakeOrganizationConfigurationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateDataLakeOrganizationConfigurationResponse
newCreateDataLakeOrganizationConfigurationResponse
  pHttpStatus_ =
    CreateDataLakeOrganizationConfigurationResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
createDataLakeOrganizationConfigurationResponse_httpStatus :: Lens.Lens' CreateDataLakeOrganizationConfigurationResponse Prelude.Int
createDataLakeOrganizationConfigurationResponse_httpStatus = Lens.lens (\CreateDataLakeOrganizationConfigurationResponse' {httpStatus} -> httpStatus) (\s@CreateDataLakeOrganizationConfigurationResponse' {} a -> s {httpStatus = a} :: CreateDataLakeOrganizationConfigurationResponse)

instance
  Prelude.NFData
    CreateDataLakeOrganizationConfigurationResponse
  where
  rnf
    CreateDataLakeOrganizationConfigurationResponse' {..} =
      Prelude.rnf httpStatus
