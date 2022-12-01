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
-- Module      : Amazonka.SMS.DeleteAppValidationConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the validation configuration for the specified application.
module Amazonka.SMS.DeleteAppValidationConfiguration
  ( -- * Creating a Request
    DeleteAppValidationConfiguration (..),
    newDeleteAppValidationConfiguration,

    -- * Request Lenses
    deleteAppValidationConfiguration_appId,

    -- * Destructuring the Response
    DeleteAppValidationConfigurationResponse (..),
    newDeleteAppValidationConfigurationResponse,

    -- * Response Lenses
    deleteAppValidationConfigurationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SMS.Types

-- | /See:/ 'newDeleteAppValidationConfiguration' smart constructor.
data DeleteAppValidationConfiguration = DeleteAppValidationConfiguration'
  { -- | The ID of the application.
    appId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteAppValidationConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'appId', 'deleteAppValidationConfiguration_appId' - The ID of the application.
newDeleteAppValidationConfiguration ::
  -- | 'appId'
  Prelude.Text ->
  DeleteAppValidationConfiguration
newDeleteAppValidationConfiguration pAppId_ =
  DeleteAppValidationConfiguration' {appId = pAppId_}

-- | The ID of the application.
deleteAppValidationConfiguration_appId :: Lens.Lens' DeleteAppValidationConfiguration Prelude.Text
deleteAppValidationConfiguration_appId = Lens.lens (\DeleteAppValidationConfiguration' {appId} -> appId) (\s@DeleteAppValidationConfiguration' {} a -> s {appId = a} :: DeleteAppValidationConfiguration)

instance
  Core.AWSRequest
    DeleteAppValidationConfiguration
  where
  type
    AWSResponse DeleteAppValidationConfiguration =
      DeleteAppValidationConfigurationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteAppValidationConfigurationResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DeleteAppValidationConfiguration
  where
  hashWithSalt
    _salt
    DeleteAppValidationConfiguration' {..} =
      _salt `Prelude.hashWithSalt` appId

instance
  Prelude.NFData
    DeleteAppValidationConfiguration
  where
  rnf DeleteAppValidationConfiguration' {..} =
    Prelude.rnf appId

instance
  Core.ToHeaders
    DeleteAppValidationConfiguration
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSServerMigrationService_V2016_10_24.DeleteAppValidationConfiguration" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DeleteAppValidationConfiguration where
  toJSON DeleteAppValidationConfiguration' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("appId" Core..= appId)]
      )

instance Core.ToPath DeleteAppValidationConfiguration where
  toPath = Prelude.const "/"

instance
  Core.ToQuery
    DeleteAppValidationConfiguration
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteAppValidationConfigurationResponse' smart constructor.
data DeleteAppValidationConfigurationResponse = DeleteAppValidationConfigurationResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteAppValidationConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteAppValidationConfigurationResponse_httpStatus' - The response's http status code.
newDeleteAppValidationConfigurationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteAppValidationConfigurationResponse
newDeleteAppValidationConfigurationResponse
  pHttpStatus_ =
    DeleteAppValidationConfigurationResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
deleteAppValidationConfigurationResponse_httpStatus :: Lens.Lens' DeleteAppValidationConfigurationResponse Prelude.Int
deleteAppValidationConfigurationResponse_httpStatus = Lens.lens (\DeleteAppValidationConfigurationResponse' {httpStatus} -> httpStatus) (\s@DeleteAppValidationConfigurationResponse' {} a -> s {httpStatus = a} :: DeleteAppValidationConfigurationResponse)

instance
  Prelude.NFData
    DeleteAppValidationConfigurationResponse
  where
  rnf DeleteAppValidationConfigurationResponse' {..} =
    Prelude.rnf httpStatus
