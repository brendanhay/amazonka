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
-- Module      : Amazonka.SMS.PutAppLaunchConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates or updates the launch configuration for the specified
-- application.
module Amazonka.SMS.PutAppLaunchConfiguration
  ( -- * Creating a Request
    PutAppLaunchConfiguration (..),
    newPutAppLaunchConfiguration,

    -- * Request Lenses
    putAppLaunchConfiguration_roleName,
    putAppLaunchConfiguration_serverGroupLaunchConfigurations,
    putAppLaunchConfiguration_autoLaunch,
    putAppLaunchConfiguration_appId,

    -- * Destructuring the Response
    PutAppLaunchConfigurationResponse (..),
    newPutAppLaunchConfigurationResponse,

    -- * Response Lenses
    putAppLaunchConfigurationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SMS.Types

-- | /See:/ 'newPutAppLaunchConfiguration' smart constructor.
data PutAppLaunchConfiguration = PutAppLaunchConfiguration'
  { -- | The name of service role in the customer\'s account that CloudFormation
    -- uses to launch the application.
    roleName :: Prelude.Maybe Prelude.Text,
    -- | Information about the launch configurations for server groups in the
    -- application.
    serverGroupLaunchConfigurations :: Prelude.Maybe [ServerGroupLaunchConfiguration],
    -- | Indicates whether the application is configured to launch automatically
    -- after replication is complete.
    autoLaunch :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the application.
    appId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutAppLaunchConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'roleName', 'putAppLaunchConfiguration_roleName' - The name of service role in the customer\'s account that CloudFormation
-- uses to launch the application.
--
-- 'serverGroupLaunchConfigurations', 'putAppLaunchConfiguration_serverGroupLaunchConfigurations' - Information about the launch configurations for server groups in the
-- application.
--
-- 'autoLaunch', 'putAppLaunchConfiguration_autoLaunch' - Indicates whether the application is configured to launch automatically
-- after replication is complete.
--
-- 'appId', 'putAppLaunchConfiguration_appId' - The ID of the application.
newPutAppLaunchConfiguration ::
  PutAppLaunchConfiguration
newPutAppLaunchConfiguration =
  PutAppLaunchConfiguration'
    { roleName =
        Prelude.Nothing,
      serverGroupLaunchConfigurations =
        Prelude.Nothing,
      autoLaunch = Prelude.Nothing,
      appId = Prelude.Nothing
    }

-- | The name of service role in the customer\'s account that CloudFormation
-- uses to launch the application.
putAppLaunchConfiguration_roleName :: Lens.Lens' PutAppLaunchConfiguration (Prelude.Maybe Prelude.Text)
putAppLaunchConfiguration_roleName = Lens.lens (\PutAppLaunchConfiguration' {roleName} -> roleName) (\s@PutAppLaunchConfiguration' {} a -> s {roleName = a} :: PutAppLaunchConfiguration)

-- | Information about the launch configurations for server groups in the
-- application.
putAppLaunchConfiguration_serverGroupLaunchConfigurations :: Lens.Lens' PutAppLaunchConfiguration (Prelude.Maybe [ServerGroupLaunchConfiguration])
putAppLaunchConfiguration_serverGroupLaunchConfigurations = Lens.lens (\PutAppLaunchConfiguration' {serverGroupLaunchConfigurations} -> serverGroupLaunchConfigurations) (\s@PutAppLaunchConfiguration' {} a -> s {serverGroupLaunchConfigurations = a} :: PutAppLaunchConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | Indicates whether the application is configured to launch automatically
-- after replication is complete.
putAppLaunchConfiguration_autoLaunch :: Lens.Lens' PutAppLaunchConfiguration (Prelude.Maybe Prelude.Bool)
putAppLaunchConfiguration_autoLaunch = Lens.lens (\PutAppLaunchConfiguration' {autoLaunch} -> autoLaunch) (\s@PutAppLaunchConfiguration' {} a -> s {autoLaunch = a} :: PutAppLaunchConfiguration)

-- | The ID of the application.
putAppLaunchConfiguration_appId :: Lens.Lens' PutAppLaunchConfiguration (Prelude.Maybe Prelude.Text)
putAppLaunchConfiguration_appId = Lens.lens (\PutAppLaunchConfiguration' {appId} -> appId) (\s@PutAppLaunchConfiguration' {} a -> s {appId = a} :: PutAppLaunchConfiguration)

instance Core.AWSRequest PutAppLaunchConfiguration where
  type
    AWSResponse PutAppLaunchConfiguration =
      PutAppLaunchConfigurationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          PutAppLaunchConfigurationResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable PutAppLaunchConfiguration where
  hashWithSalt _salt PutAppLaunchConfiguration' {..} =
    _salt `Prelude.hashWithSalt` roleName
      `Prelude.hashWithSalt` serverGroupLaunchConfigurations
      `Prelude.hashWithSalt` autoLaunch
      `Prelude.hashWithSalt` appId

instance Prelude.NFData PutAppLaunchConfiguration where
  rnf PutAppLaunchConfiguration' {..} =
    Prelude.rnf roleName
      `Prelude.seq` Prelude.rnf serverGroupLaunchConfigurations
      `Prelude.seq` Prelude.rnf autoLaunch
      `Prelude.seq` Prelude.rnf appId

instance Core.ToHeaders PutAppLaunchConfiguration where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSServerMigrationService_V2016_10_24.PutAppLaunchConfiguration" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON PutAppLaunchConfiguration where
  toJSON PutAppLaunchConfiguration' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("roleName" Core..=) Prelude.<$> roleName,
            ("serverGroupLaunchConfigurations" Core..=)
              Prelude.<$> serverGroupLaunchConfigurations,
            ("autoLaunch" Core..=) Prelude.<$> autoLaunch,
            ("appId" Core..=) Prelude.<$> appId
          ]
      )

instance Core.ToPath PutAppLaunchConfiguration where
  toPath = Prelude.const "/"

instance Core.ToQuery PutAppLaunchConfiguration where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPutAppLaunchConfigurationResponse' smart constructor.
data PutAppLaunchConfigurationResponse = PutAppLaunchConfigurationResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  where
  rnf PutAppLaunchConfigurationResponse' {..} =
    Prelude.rnf httpStatus
