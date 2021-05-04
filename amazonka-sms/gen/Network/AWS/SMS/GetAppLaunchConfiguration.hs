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
-- Module      : Network.AWS.SMS.GetAppLaunchConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the application launch configuration associated with the
-- specified application.
module Network.AWS.SMS.GetAppLaunchConfiguration
  ( -- * Creating a Request
    GetAppLaunchConfiguration (..),
    newGetAppLaunchConfiguration,

    -- * Request Lenses
    getAppLaunchConfiguration_appId,

    -- * Destructuring the Response
    GetAppLaunchConfigurationResponse (..),
    newGetAppLaunchConfigurationResponse,

    -- * Response Lenses
    getAppLaunchConfigurationResponse_appId,
    getAppLaunchConfigurationResponse_roleName,
    getAppLaunchConfigurationResponse_autoLaunch,
    getAppLaunchConfigurationResponse_serverGroupLaunchConfigurations,
    getAppLaunchConfigurationResponse_httpStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SMS.Types

-- | /See:/ 'newGetAppLaunchConfiguration' smart constructor.
data GetAppLaunchConfiguration = GetAppLaunchConfiguration'
  { -- | The ID of the application.
    appId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'GetAppLaunchConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'appId', 'getAppLaunchConfiguration_appId' - The ID of the application.
newGetAppLaunchConfiguration ::
  GetAppLaunchConfiguration
newGetAppLaunchConfiguration =
  GetAppLaunchConfiguration' {appId = Prelude.Nothing}

-- | The ID of the application.
getAppLaunchConfiguration_appId :: Lens.Lens' GetAppLaunchConfiguration (Prelude.Maybe Prelude.Text)
getAppLaunchConfiguration_appId = Lens.lens (\GetAppLaunchConfiguration' {appId} -> appId) (\s@GetAppLaunchConfiguration' {} a -> s {appId = a} :: GetAppLaunchConfiguration)

instance Prelude.AWSRequest GetAppLaunchConfiguration where
  type
    Rs GetAppLaunchConfiguration =
      GetAppLaunchConfigurationResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetAppLaunchConfigurationResponse'
            Prelude.<$> (x Prelude..?> "appId")
            Prelude.<*> (x Prelude..?> "roleName")
            Prelude.<*> (x Prelude..?> "autoLaunch")
            Prelude.<*> ( x Prelude..?> "serverGroupLaunchConfigurations"
                            Prelude..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetAppLaunchConfiguration

instance Prelude.NFData GetAppLaunchConfiguration

instance Prelude.ToHeaders GetAppLaunchConfiguration where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AWSServerMigrationService_V2016_10_24.GetAppLaunchConfiguration" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON GetAppLaunchConfiguration where
  toJSON GetAppLaunchConfiguration' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [("appId" Prelude..=) Prelude.<$> appId]
      )

instance Prelude.ToPath GetAppLaunchConfiguration where
  toPath = Prelude.const "/"

instance Prelude.ToQuery GetAppLaunchConfiguration where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetAppLaunchConfigurationResponse' smart constructor.
data GetAppLaunchConfigurationResponse = GetAppLaunchConfigurationResponse'
  { -- | The ID of the application.
    appId :: Prelude.Maybe Prelude.Text,
    -- | The name of the service role in the customer\'s account that AWS
    -- CloudFormation uses to launch the application.
    roleName :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether the application is configured to launch automatically
    -- after replication is complete.
    autoLaunch :: Prelude.Maybe Prelude.Bool,
    -- | The launch configurations for server groups in this application.
    serverGroupLaunchConfigurations :: Prelude.Maybe [ServerGroupLaunchConfiguration],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'GetAppLaunchConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'appId', 'getAppLaunchConfigurationResponse_appId' - The ID of the application.
--
-- 'roleName', 'getAppLaunchConfigurationResponse_roleName' - The name of the service role in the customer\'s account that AWS
-- CloudFormation uses to launch the application.
--
-- 'autoLaunch', 'getAppLaunchConfigurationResponse_autoLaunch' - Indicates whether the application is configured to launch automatically
-- after replication is complete.
--
-- 'serverGroupLaunchConfigurations', 'getAppLaunchConfigurationResponse_serverGroupLaunchConfigurations' - The launch configurations for server groups in this application.
--
-- 'httpStatus', 'getAppLaunchConfigurationResponse_httpStatus' - The response's http status code.
newGetAppLaunchConfigurationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetAppLaunchConfigurationResponse
newGetAppLaunchConfigurationResponse pHttpStatus_ =
  GetAppLaunchConfigurationResponse'
    { appId =
        Prelude.Nothing,
      roleName = Prelude.Nothing,
      autoLaunch = Prelude.Nothing,
      serverGroupLaunchConfigurations =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ID of the application.
getAppLaunchConfigurationResponse_appId :: Lens.Lens' GetAppLaunchConfigurationResponse (Prelude.Maybe Prelude.Text)
getAppLaunchConfigurationResponse_appId = Lens.lens (\GetAppLaunchConfigurationResponse' {appId} -> appId) (\s@GetAppLaunchConfigurationResponse' {} a -> s {appId = a} :: GetAppLaunchConfigurationResponse)

-- | The name of the service role in the customer\'s account that AWS
-- CloudFormation uses to launch the application.
getAppLaunchConfigurationResponse_roleName :: Lens.Lens' GetAppLaunchConfigurationResponse (Prelude.Maybe Prelude.Text)
getAppLaunchConfigurationResponse_roleName = Lens.lens (\GetAppLaunchConfigurationResponse' {roleName} -> roleName) (\s@GetAppLaunchConfigurationResponse' {} a -> s {roleName = a} :: GetAppLaunchConfigurationResponse)

-- | Indicates whether the application is configured to launch automatically
-- after replication is complete.
getAppLaunchConfigurationResponse_autoLaunch :: Lens.Lens' GetAppLaunchConfigurationResponse (Prelude.Maybe Prelude.Bool)
getAppLaunchConfigurationResponse_autoLaunch = Lens.lens (\GetAppLaunchConfigurationResponse' {autoLaunch} -> autoLaunch) (\s@GetAppLaunchConfigurationResponse' {} a -> s {autoLaunch = a} :: GetAppLaunchConfigurationResponse)

-- | The launch configurations for server groups in this application.
getAppLaunchConfigurationResponse_serverGroupLaunchConfigurations :: Lens.Lens' GetAppLaunchConfigurationResponse (Prelude.Maybe [ServerGroupLaunchConfiguration])
getAppLaunchConfigurationResponse_serverGroupLaunchConfigurations = Lens.lens (\GetAppLaunchConfigurationResponse' {serverGroupLaunchConfigurations} -> serverGroupLaunchConfigurations) (\s@GetAppLaunchConfigurationResponse' {} a -> s {serverGroupLaunchConfigurations = a} :: GetAppLaunchConfigurationResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The response's http status code.
getAppLaunchConfigurationResponse_httpStatus :: Lens.Lens' GetAppLaunchConfigurationResponse Prelude.Int
getAppLaunchConfigurationResponse_httpStatus = Lens.lens (\GetAppLaunchConfigurationResponse' {httpStatus} -> httpStatus) (\s@GetAppLaunchConfigurationResponse' {} a -> s {httpStatus = a} :: GetAppLaunchConfigurationResponse)

instance
  Prelude.NFData
    GetAppLaunchConfigurationResponse
