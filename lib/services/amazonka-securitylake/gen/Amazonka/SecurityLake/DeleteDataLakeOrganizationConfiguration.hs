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
-- Module      : Amazonka.SecurityLake.DeleteDataLakeOrganizationConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes automatic the enablement of configuration settings for new
-- member accounts (but retains the settings for the delegated
-- administrator) from Amazon Security Lake. You must run this API using
-- the credentials of the delegated administrator. When you run this API,
-- new member accounts that are added after the organization enables
-- Security Lake won\'t contribute to the data lake.
module Amazonka.SecurityLake.DeleteDataLakeOrganizationConfiguration
  ( -- * Creating a Request
    DeleteDataLakeOrganizationConfiguration (..),
    newDeleteDataLakeOrganizationConfiguration,

    -- * Request Lenses
    deleteDataLakeOrganizationConfiguration_autoEnableNewAccount,

    -- * Destructuring the Response
    DeleteDataLakeOrganizationConfigurationResponse (..),
    newDeleteDataLakeOrganizationConfigurationResponse,

    -- * Response Lenses
    deleteDataLakeOrganizationConfigurationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SecurityLake.Types

-- | /See:/ 'newDeleteDataLakeOrganizationConfiguration' smart constructor.
data DeleteDataLakeOrganizationConfiguration = DeleteDataLakeOrganizationConfiguration'
  { -- | Removes the automatic enablement of configuration settings for new
    -- member accounts in Security Lake.
    autoEnableNewAccount :: [DataLakeAutoEnableNewAccountConfiguration]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteDataLakeOrganizationConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'autoEnableNewAccount', 'deleteDataLakeOrganizationConfiguration_autoEnableNewAccount' - Removes the automatic enablement of configuration settings for new
-- member accounts in Security Lake.
newDeleteDataLakeOrganizationConfiguration ::
  DeleteDataLakeOrganizationConfiguration
newDeleteDataLakeOrganizationConfiguration =
  DeleteDataLakeOrganizationConfiguration'
    { autoEnableNewAccount =
        Prelude.mempty
    }

-- | Removes the automatic enablement of configuration settings for new
-- member accounts in Security Lake.
deleteDataLakeOrganizationConfiguration_autoEnableNewAccount :: Lens.Lens' DeleteDataLakeOrganizationConfiguration [DataLakeAutoEnableNewAccountConfiguration]
deleteDataLakeOrganizationConfiguration_autoEnableNewAccount = Lens.lens (\DeleteDataLakeOrganizationConfiguration' {autoEnableNewAccount} -> autoEnableNewAccount) (\s@DeleteDataLakeOrganizationConfiguration' {} a -> s {autoEnableNewAccount = a} :: DeleteDataLakeOrganizationConfiguration) Prelude.. Lens.coerced

instance
  Core.AWSRequest
    DeleteDataLakeOrganizationConfiguration
  where
  type
    AWSResponse
      DeleteDataLakeOrganizationConfiguration =
      DeleteDataLakeOrganizationConfigurationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteDataLakeOrganizationConfigurationResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DeleteDataLakeOrganizationConfiguration
  where
  hashWithSalt
    _salt
    DeleteDataLakeOrganizationConfiguration' {..} =
      _salt `Prelude.hashWithSalt` autoEnableNewAccount

instance
  Prelude.NFData
    DeleteDataLakeOrganizationConfiguration
  where
  rnf DeleteDataLakeOrganizationConfiguration' {..} =
    Prelude.rnf autoEnableNewAccount

instance
  Data.ToHeaders
    DeleteDataLakeOrganizationConfiguration
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
    DeleteDataLakeOrganizationConfiguration
  where
  toJSON DeleteDataLakeOrganizationConfiguration' {..} =
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
    DeleteDataLakeOrganizationConfiguration
  where
  toPath =
    Prelude.const
      "/v1/datalake/organization/configuration/delete"

instance
  Data.ToQuery
    DeleteDataLakeOrganizationConfiguration
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteDataLakeOrganizationConfigurationResponse' smart constructor.
data DeleteDataLakeOrganizationConfigurationResponse = DeleteDataLakeOrganizationConfigurationResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteDataLakeOrganizationConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteDataLakeOrganizationConfigurationResponse_httpStatus' - The response's http status code.
newDeleteDataLakeOrganizationConfigurationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteDataLakeOrganizationConfigurationResponse
newDeleteDataLakeOrganizationConfigurationResponse
  pHttpStatus_ =
    DeleteDataLakeOrganizationConfigurationResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
deleteDataLakeOrganizationConfigurationResponse_httpStatus :: Lens.Lens' DeleteDataLakeOrganizationConfigurationResponse Prelude.Int
deleteDataLakeOrganizationConfigurationResponse_httpStatus = Lens.lens (\DeleteDataLakeOrganizationConfigurationResponse' {httpStatus} -> httpStatus) (\s@DeleteDataLakeOrganizationConfigurationResponse' {} a -> s {httpStatus = a} :: DeleteDataLakeOrganizationConfigurationResponse)

instance
  Prelude.NFData
    DeleteDataLakeOrganizationConfigurationResponse
  where
  rnf
    DeleteDataLakeOrganizationConfigurationResponse' {..} =
      Prelude.rnf httpStatus
