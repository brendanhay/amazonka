{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.SMS.DeleteAppLaunchConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the launch configuration for the specified application.
module Network.AWS.SMS.DeleteAppLaunchConfiguration
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SMS.Types

-- | /See:/ 'newDeleteAppLaunchConfiguration' smart constructor.
data DeleteAppLaunchConfiguration = DeleteAppLaunchConfiguration'
  { -- | The ID of the application.
    appId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance
  Prelude.AWSRequest
    DeleteAppLaunchConfiguration
  where
  type
    Rs DeleteAppLaunchConfiguration =
      DeleteAppLaunchConfigurationResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteAppLaunchConfigurationResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DeleteAppLaunchConfiguration

instance Prelude.NFData DeleteAppLaunchConfiguration

instance
  Prelude.ToHeaders
    DeleteAppLaunchConfiguration
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AWSServerMigrationService_V2016_10_24.DeleteAppLaunchConfiguration" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DeleteAppLaunchConfiguration where
  toJSON DeleteAppLaunchConfiguration' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [("appId" Prelude..=) Prelude.<$> appId]
      )

instance Prelude.ToPath DeleteAppLaunchConfiguration where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DeleteAppLaunchConfiguration where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteAppLaunchConfigurationResponse' smart constructor.
data DeleteAppLaunchConfigurationResponse = DeleteAppLaunchConfigurationResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
