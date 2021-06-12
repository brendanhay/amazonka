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
-- Module      : Network.AWS.SMS.PutAppLaunchConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates or updates the launch configuration for the specified
-- application.
module Network.AWS.SMS.PutAppLaunchConfiguration
  ( -- * Creating a Request
    PutAppLaunchConfiguration (..),
    newPutAppLaunchConfiguration,

    -- * Request Lenses
    putAppLaunchConfiguration_appId,
    putAppLaunchConfiguration_roleName,
    putAppLaunchConfiguration_autoLaunch,
    putAppLaunchConfiguration_serverGroupLaunchConfigurations,

    -- * Destructuring the Response
    PutAppLaunchConfigurationResponse (..),
    newPutAppLaunchConfigurationResponse,

    -- * Response Lenses
    putAppLaunchConfigurationResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SMS.Types

-- | /See:/ 'newPutAppLaunchConfiguration' smart constructor.
data PutAppLaunchConfiguration = PutAppLaunchConfiguration'
  { -- | The ID of the application.
    appId :: Core.Maybe Core.Text,
    -- | The name of service role in the customer\'s account that AWS
    -- CloudFormation uses to launch the application.
    roleName :: Core.Maybe Core.Text,
    -- | Indicates whether the application is configured to launch automatically
    -- after replication is complete.
    autoLaunch :: Core.Maybe Core.Bool,
    -- | Information about the launch configurations for server groups in the
    -- application.
    serverGroupLaunchConfigurations :: Core.Maybe [ServerGroupLaunchConfiguration]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'PutAppLaunchConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'appId', 'putAppLaunchConfiguration_appId' - The ID of the application.
--
-- 'roleName', 'putAppLaunchConfiguration_roleName' - The name of service role in the customer\'s account that AWS
-- CloudFormation uses to launch the application.
--
-- 'autoLaunch', 'putAppLaunchConfiguration_autoLaunch' - Indicates whether the application is configured to launch automatically
-- after replication is complete.
--
-- 'serverGroupLaunchConfigurations', 'putAppLaunchConfiguration_serverGroupLaunchConfigurations' - Information about the launch configurations for server groups in the
-- application.
newPutAppLaunchConfiguration ::
  PutAppLaunchConfiguration
newPutAppLaunchConfiguration =
  PutAppLaunchConfiguration'
    { appId = Core.Nothing,
      roleName = Core.Nothing,
      autoLaunch = Core.Nothing,
      serverGroupLaunchConfigurations = Core.Nothing
    }

-- | The ID of the application.
putAppLaunchConfiguration_appId :: Lens.Lens' PutAppLaunchConfiguration (Core.Maybe Core.Text)
putAppLaunchConfiguration_appId = Lens.lens (\PutAppLaunchConfiguration' {appId} -> appId) (\s@PutAppLaunchConfiguration' {} a -> s {appId = a} :: PutAppLaunchConfiguration)

-- | The name of service role in the customer\'s account that AWS
-- CloudFormation uses to launch the application.
putAppLaunchConfiguration_roleName :: Lens.Lens' PutAppLaunchConfiguration (Core.Maybe Core.Text)
putAppLaunchConfiguration_roleName = Lens.lens (\PutAppLaunchConfiguration' {roleName} -> roleName) (\s@PutAppLaunchConfiguration' {} a -> s {roleName = a} :: PutAppLaunchConfiguration)

-- | Indicates whether the application is configured to launch automatically
-- after replication is complete.
putAppLaunchConfiguration_autoLaunch :: Lens.Lens' PutAppLaunchConfiguration (Core.Maybe Core.Bool)
putAppLaunchConfiguration_autoLaunch = Lens.lens (\PutAppLaunchConfiguration' {autoLaunch} -> autoLaunch) (\s@PutAppLaunchConfiguration' {} a -> s {autoLaunch = a} :: PutAppLaunchConfiguration)

-- | Information about the launch configurations for server groups in the
-- application.
putAppLaunchConfiguration_serverGroupLaunchConfigurations :: Lens.Lens' PutAppLaunchConfiguration (Core.Maybe [ServerGroupLaunchConfiguration])
putAppLaunchConfiguration_serverGroupLaunchConfigurations = Lens.lens (\PutAppLaunchConfiguration' {serverGroupLaunchConfigurations} -> serverGroupLaunchConfigurations) (\s@PutAppLaunchConfiguration' {} a -> s {serverGroupLaunchConfigurations = a} :: PutAppLaunchConfiguration) Core.. Lens.mapping Lens._Coerce

instance Core.AWSRequest PutAppLaunchConfiguration where
  type
    AWSResponse PutAppLaunchConfiguration =
      PutAppLaunchConfigurationResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          PutAppLaunchConfigurationResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable PutAppLaunchConfiguration

instance Core.NFData PutAppLaunchConfiguration

instance Core.ToHeaders PutAppLaunchConfiguration where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSServerMigrationService_V2016_10_24.PutAppLaunchConfiguration" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON PutAppLaunchConfiguration where
  toJSON PutAppLaunchConfiguration' {..} =
    Core.object
      ( Core.catMaybes
          [ ("appId" Core..=) Core.<$> appId,
            ("roleName" Core..=) Core.<$> roleName,
            ("autoLaunch" Core..=) Core.<$> autoLaunch,
            ("serverGroupLaunchConfigurations" Core..=)
              Core.<$> serverGroupLaunchConfigurations
          ]
      )

instance Core.ToPath PutAppLaunchConfiguration where
  toPath = Core.const "/"

instance Core.ToQuery PutAppLaunchConfiguration where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newPutAppLaunchConfigurationResponse' smart constructor.
data PutAppLaunchConfigurationResponse = PutAppLaunchConfigurationResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'PutAppLaunchConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'putAppLaunchConfigurationResponse_httpStatus' - The response's http status code.
newPutAppLaunchConfigurationResponse ::
  -- | 'httpStatus'
  Core.Int ->
  PutAppLaunchConfigurationResponse
newPutAppLaunchConfigurationResponse pHttpStatus_ =
  PutAppLaunchConfigurationResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
putAppLaunchConfigurationResponse_httpStatus :: Lens.Lens' PutAppLaunchConfigurationResponse Core.Int
putAppLaunchConfigurationResponse_httpStatus = Lens.lens (\PutAppLaunchConfigurationResponse' {httpStatus} -> httpStatus) (\s@PutAppLaunchConfigurationResponse' {} a -> s {httpStatus = a} :: PutAppLaunchConfigurationResponse)

instance
  Core.NFData
    PutAppLaunchConfigurationResponse
