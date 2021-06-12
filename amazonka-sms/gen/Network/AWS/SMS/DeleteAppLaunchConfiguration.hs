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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SMS.Types

-- | /See:/ 'newDeleteAppLaunchConfiguration' smart constructor.
data DeleteAppLaunchConfiguration = DeleteAppLaunchConfiguration'
  { -- | The ID of the application.
    appId :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  DeleteAppLaunchConfiguration' {appId = Core.Nothing}

-- | The ID of the application.
deleteAppLaunchConfiguration_appId :: Lens.Lens' DeleteAppLaunchConfiguration (Core.Maybe Core.Text)
deleteAppLaunchConfiguration_appId = Lens.lens (\DeleteAppLaunchConfiguration' {appId} -> appId) (\s@DeleteAppLaunchConfiguration' {} a -> s {appId = a} :: DeleteAppLaunchConfiguration)

instance Core.AWSRequest DeleteAppLaunchConfiguration where
  type
    AWSResponse DeleteAppLaunchConfiguration =
      DeleteAppLaunchConfigurationResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteAppLaunchConfigurationResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DeleteAppLaunchConfiguration

instance Core.NFData DeleteAppLaunchConfiguration

instance Core.ToHeaders DeleteAppLaunchConfiguration where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSServerMigrationService_V2016_10_24.DeleteAppLaunchConfiguration" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DeleteAppLaunchConfiguration where
  toJSON DeleteAppLaunchConfiguration' {..} =
    Core.object
      (Core.catMaybes [("appId" Core..=) Core.<$> appId])

instance Core.ToPath DeleteAppLaunchConfiguration where
  toPath = Core.const "/"

instance Core.ToQuery DeleteAppLaunchConfiguration where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeleteAppLaunchConfigurationResponse' smart constructor.
data DeleteAppLaunchConfigurationResponse = DeleteAppLaunchConfigurationResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  DeleteAppLaunchConfigurationResponse
newDeleteAppLaunchConfigurationResponse pHttpStatus_ =
  DeleteAppLaunchConfigurationResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteAppLaunchConfigurationResponse_httpStatus :: Lens.Lens' DeleteAppLaunchConfigurationResponse Core.Int
deleteAppLaunchConfigurationResponse_httpStatus = Lens.lens (\DeleteAppLaunchConfigurationResponse' {httpStatus} -> httpStatus) (\s@DeleteAppLaunchConfigurationResponse' {} a -> s {httpStatus = a} :: DeleteAppLaunchConfigurationResponse)

instance
  Core.NFData
    DeleteAppLaunchConfigurationResponse
