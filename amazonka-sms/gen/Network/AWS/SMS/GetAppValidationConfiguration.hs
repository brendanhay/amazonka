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
-- Module      : Network.AWS.SMS.GetAppValidationConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about a configuration for validating an
-- application.
module Network.AWS.SMS.GetAppValidationConfiguration
  ( -- * Creating a Request
    GetAppValidationConfiguration (..),
    newGetAppValidationConfiguration,

    -- * Request Lenses
    getAppValidationConfiguration_appId,

    -- * Destructuring the Response
    GetAppValidationConfigurationResponse (..),
    newGetAppValidationConfigurationResponse,

    -- * Response Lenses
    getAppValidationConfigurationResponse_appValidationConfigurations,
    getAppValidationConfigurationResponse_serverGroupValidationConfigurations,
    getAppValidationConfigurationResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SMS.Types

-- | /See:/ 'newGetAppValidationConfiguration' smart constructor.
data GetAppValidationConfiguration = GetAppValidationConfiguration'
  { -- | The ID of the application.
    appId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetAppValidationConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'appId', 'getAppValidationConfiguration_appId' - The ID of the application.
newGetAppValidationConfiguration ::
  -- | 'appId'
  Core.Text ->
  GetAppValidationConfiguration
newGetAppValidationConfiguration pAppId_ =
  GetAppValidationConfiguration' {appId = pAppId_}

-- | The ID of the application.
getAppValidationConfiguration_appId :: Lens.Lens' GetAppValidationConfiguration Core.Text
getAppValidationConfiguration_appId = Lens.lens (\GetAppValidationConfiguration' {appId} -> appId) (\s@GetAppValidationConfiguration' {} a -> s {appId = a} :: GetAppValidationConfiguration)

instance
  Core.AWSRequest
    GetAppValidationConfiguration
  where
  type
    AWSResponse GetAppValidationConfiguration =
      GetAppValidationConfigurationResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetAppValidationConfigurationResponse'
            Core.<$> ( x Core..?> "appValidationConfigurations"
                         Core..!@ Core.mempty
                     )
            Core.<*> ( x Core..?> "serverGroupValidationConfigurations"
                         Core..!@ Core.mempty
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetAppValidationConfiguration

instance Core.NFData GetAppValidationConfiguration

instance Core.ToHeaders GetAppValidationConfiguration where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSServerMigrationService_V2016_10_24.GetAppValidationConfiguration" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON GetAppValidationConfiguration where
  toJSON GetAppValidationConfiguration' {..} =
    Core.object
      (Core.catMaybes [Core.Just ("appId" Core..= appId)])

instance Core.ToPath GetAppValidationConfiguration where
  toPath = Core.const "/"

instance Core.ToQuery GetAppValidationConfiguration where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetAppValidationConfigurationResponse' smart constructor.
data GetAppValidationConfigurationResponse = GetAppValidationConfigurationResponse'
  { -- | The configuration for application validation.
    appValidationConfigurations :: Core.Maybe [AppValidationConfiguration],
    -- | The configuration for instance validation.
    serverGroupValidationConfigurations :: Core.Maybe [ServerGroupValidationConfiguration],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetAppValidationConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'appValidationConfigurations', 'getAppValidationConfigurationResponse_appValidationConfigurations' - The configuration for application validation.
--
-- 'serverGroupValidationConfigurations', 'getAppValidationConfigurationResponse_serverGroupValidationConfigurations' - The configuration for instance validation.
--
-- 'httpStatus', 'getAppValidationConfigurationResponse_httpStatus' - The response's http status code.
newGetAppValidationConfigurationResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetAppValidationConfigurationResponse
newGetAppValidationConfigurationResponse pHttpStatus_ =
  GetAppValidationConfigurationResponse'
    { appValidationConfigurations =
        Core.Nothing,
      serverGroupValidationConfigurations =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The configuration for application validation.
getAppValidationConfigurationResponse_appValidationConfigurations :: Lens.Lens' GetAppValidationConfigurationResponse (Core.Maybe [AppValidationConfiguration])
getAppValidationConfigurationResponse_appValidationConfigurations = Lens.lens (\GetAppValidationConfigurationResponse' {appValidationConfigurations} -> appValidationConfigurations) (\s@GetAppValidationConfigurationResponse' {} a -> s {appValidationConfigurations = a} :: GetAppValidationConfigurationResponse) Core.. Lens.mapping Lens._Coerce

-- | The configuration for instance validation.
getAppValidationConfigurationResponse_serverGroupValidationConfigurations :: Lens.Lens' GetAppValidationConfigurationResponse (Core.Maybe [ServerGroupValidationConfiguration])
getAppValidationConfigurationResponse_serverGroupValidationConfigurations = Lens.lens (\GetAppValidationConfigurationResponse' {serverGroupValidationConfigurations} -> serverGroupValidationConfigurations) (\s@GetAppValidationConfigurationResponse' {} a -> s {serverGroupValidationConfigurations = a} :: GetAppValidationConfigurationResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
getAppValidationConfigurationResponse_httpStatus :: Lens.Lens' GetAppValidationConfigurationResponse Core.Int
getAppValidationConfigurationResponse_httpStatus = Lens.lens (\GetAppValidationConfigurationResponse' {httpStatus} -> httpStatus) (\s@GetAppValidationConfigurationResponse' {} a -> s {httpStatus = a} :: GetAppValidationConfigurationResponse)

instance
  Core.NFData
    GetAppValidationConfigurationResponse
