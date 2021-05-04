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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SMS.Types

-- | /See:/ 'newPutAppLaunchConfiguration' smart constructor.
data PutAppLaunchConfiguration = PutAppLaunchConfiguration'
  { -- | The ID of the application.
    appId :: Prelude.Maybe Prelude.Text,
    -- | The name of service role in the customer\'s account that AWS
    -- CloudFormation uses to launch the application.
    roleName :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether the application is configured to launch automatically
    -- after replication is complete.
    autoLaunch :: Prelude.Maybe Prelude.Bool,
    -- | Information about the launch configurations for server groups in the
    -- application.
    serverGroupLaunchConfigurations :: Prelude.Maybe [ServerGroupLaunchConfiguration]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { appId = Prelude.Nothing,
      roleName = Prelude.Nothing,
      autoLaunch = Prelude.Nothing,
      serverGroupLaunchConfigurations =
        Prelude.Nothing
    }

-- | The ID of the application.
putAppLaunchConfiguration_appId :: Lens.Lens' PutAppLaunchConfiguration (Prelude.Maybe Prelude.Text)
putAppLaunchConfiguration_appId = Lens.lens (\PutAppLaunchConfiguration' {appId} -> appId) (\s@PutAppLaunchConfiguration' {} a -> s {appId = a} :: PutAppLaunchConfiguration)

-- | The name of service role in the customer\'s account that AWS
-- CloudFormation uses to launch the application.
putAppLaunchConfiguration_roleName :: Lens.Lens' PutAppLaunchConfiguration (Prelude.Maybe Prelude.Text)
putAppLaunchConfiguration_roleName = Lens.lens (\PutAppLaunchConfiguration' {roleName} -> roleName) (\s@PutAppLaunchConfiguration' {} a -> s {roleName = a} :: PutAppLaunchConfiguration)

-- | Indicates whether the application is configured to launch automatically
-- after replication is complete.
putAppLaunchConfiguration_autoLaunch :: Lens.Lens' PutAppLaunchConfiguration (Prelude.Maybe Prelude.Bool)
putAppLaunchConfiguration_autoLaunch = Lens.lens (\PutAppLaunchConfiguration' {autoLaunch} -> autoLaunch) (\s@PutAppLaunchConfiguration' {} a -> s {autoLaunch = a} :: PutAppLaunchConfiguration)

-- | Information about the launch configurations for server groups in the
-- application.
putAppLaunchConfiguration_serverGroupLaunchConfigurations :: Lens.Lens' PutAppLaunchConfiguration (Prelude.Maybe [ServerGroupLaunchConfiguration])
putAppLaunchConfiguration_serverGroupLaunchConfigurations = Lens.lens (\PutAppLaunchConfiguration' {serverGroupLaunchConfigurations} -> serverGroupLaunchConfigurations) (\s@PutAppLaunchConfiguration' {} a -> s {serverGroupLaunchConfigurations = a} :: PutAppLaunchConfiguration) Prelude.. Lens.mapping Prelude._Coerce

instance Prelude.AWSRequest PutAppLaunchConfiguration where
  type
    Rs PutAppLaunchConfiguration =
      PutAppLaunchConfigurationResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          PutAppLaunchConfigurationResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable PutAppLaunchConfiguration

instance Prelude.NFData PutAppLaunchConfiguration

instance Prelude.ToHeaders PutAppLaunchConfiguration where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AWSServerMigrationService_V2016_10_24.PutAppLaunchConfiguration" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON PutAppLaunchConfiguration where
  toJSON PutAppLaunchConfiguration' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("appId" Prelude..=) Prelude.<$> appId,
            ("roleName" Prelude..=) Prelude.<$> roleName,
            ("autoLaunch" Prelude..=) Prelude.<$> autoLaunch,
            ("serverGroupLaunchConfigurations" Prelude..=)
              Prelude.<$> serverGroupLaunchConfigurations
          ]
      )

instance Prelude.ToPath PutAppLaunchConfiguration where
  toPath = Prelude.const "/"

instance Prelude.ToQuery PutAppLaunchConfiguration where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPutAppLaunchConfigurationResponse' smart constructor.
data PutAppLaunchConfigurationResponse = PutAppLaunchConfigurationResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  PutAppLaunchConfigurationResponse
newPutAppLaunchConfigurationResponse pHttpStatus_ =
  PutAppLaunchConfigurationResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
putAppLaunchConfigurationResponse_httpStatus :: Lens.Lens' PutAppLaunchConfigurationResponse Prelude.Int
putAppLaunchConfigurationResponse_httpStatus = Lens.lens (\PutAppLaunchConfigurationResponse' {httpStatus} -> httpStatus) (\s@PutAppLaunchConfigurationResponse' {} a -> s {httpStatus = a} :: PutAppLaunchConfigurationResponse)

instance
  Prelude.NFData
    PutAppLaunchConfigurationResponse
