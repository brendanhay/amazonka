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
-- Module      : Amazonka.SMS.DeleteAppLaunchConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the launch configuration for the specified application.
module Amazonka.SMS.DeleteAppLaunchConfiguration
  ( -- * Creating a Request
    DeleteAppLaunchConfiguration (..),
    newDeleteAppLaunchConfiguration,

    -- * Request Lenses
    deleteAppLaunchConfiguration_appId,

    -- * Destructuring the Response
    DeleteAppLaunchConfigurationResponse (..),
    newDeleteAppLaunchConfigurationResponse,

    -- * Response Lenses
    deleteAppLaunchConfigurationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SMS.Types

-- | /See:/ 'newDeleteAppLaunchConfiguration' smart constructor.
data DeleteAppLaunchConfiguration = DeleteAppLaunchConfiguration'
  { -- | The ID of the application.
    appId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteAppLaunchConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'appId', 'deleteAppLaunchConfiguration_appId' - The ID of the application.
newDeleteAppLaunchConfiguration ::
  DeleteAppLaunchConfiguration
newDeleteAppLaunchConfiguration =
  DeleteAppLaunchConfiguration'
    { appId =
        Prelude.Nothing
    }

-- | The ID of the application.
deleteAppLaunchConfiguration_appId :: Lens.Lens' DeleteAppLaunchConfiguration (Prelude.Maybe Prelude.Text)
deleteAppLaunchConfiguration_appId = Lens.lens (\DeleteAppLaunchConfiguration' {appId} -> appId) (\s@DeleteAppLaunchConfiguration' {} a -> s {appId = a} :: DeleteAppLaunchConfiguration)

instance Core.AWSRequest DeleteAppLaunchConfiguration where
  type
    AWSResponse DeleteAppLaunchConfiguration =
      DeleteAppLaunchConfigurationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteAppLaunchConfigurationResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DeleteAppLaunchConfiguration
  where
  hashWithSalt _salt DeleteAppLaunchConfiguration' {..} =
    _salt `Prelude.hashWithSalt` appId

instance Prelude.NFData DeleteAppLaunchConfiguration where
  rnf DeleteAppLaunchConfiguration' {..} =
    Prelude.rnf appId

instance Data.ToHeaders DeleteAppLaunchConfiguration where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSServerMigrationService_V2016_10_24.DeleteAppLaunchConfiguration" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteAppLaunchConfiguration where
  toJSON DeleteAppLaunchConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [("appId" Data..=) Prelude.<$> appId]
      )

instance Data.ToPath DeleteAppLaunchConfiguration where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteAppLaunchConfiguration where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteAppLaunchConfigurationResponse' smart constructor.
data DeleteAppLaunchConfigurationResponse = DeleteAppLaunchConfigurationResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteAppLaunchConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteAppLaunchConfigurationResponse_httpStatus' - The response's http status code.
newDeleteAppLaunchConfigurationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteAppLaunchConfigurationResponse
newDeleteAppLaunchConfigurationResponse pHttpStatus_ =
  DeleteAppLaunchConfigurationResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteAppLaunchConfigurationResponse_httpStatus :: Lens.Lens' DeleteAppLaunchConfigurationResponse Prelude.Int
deleteAppLaunchConfigurationResponse_httpStatus = Lens.lens (\DeleteAppLaunchConfigurationResponse' {httpStatus} -> httpStatus) (\s@DeleteAppLaunchConfigurationResponse' {} a -> s {httpStatus = a} :: DeleteAppLaunchConfigurationResponse)

instance
  Prelude.NFData
    DeleteAppLaunchConfigurationResponse
  where
  rnf DeleteAppLaunchConfigurationResponse' {..} =
    Prelude.rnf httpStatus
