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
-- Module      : Network.AWS.SMS.GetAppReplicationConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the application replication configuration associated with the
-- specified application.
module Network.AWS.SMS.GetAppReplicationConfiguration
  ( -- * Creating a Request
    GetAppReplicationConfiguration (..),
    newGetAppReplicationConfiguration,

    -- * Request Lenses
    getAppReplicationConfiguration_appId,

    -- * Destructuring the Response
    GetAppReplicationConfigurationResponse (..),
    newGetAppReplicationConfigurationResponse,

    -- * Response Lenses
    getAppReplicationConfigurationResponse_serverGroupReplicationConfigurations,
    getAppReplicationConfigurationResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SMS.Types

-- | /See:/ 'newGetAppReplicationConfiguration' smart constructor.
data GetAppReplicationConfiguration = GetAppReplicationConfiguration'
  { -- | The ID of the application.
    appId :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetAppReplicationConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'appId', 'getAppReplicationConfiguration_appId' - The ID of the application.
newGetAppReplicationConfiguration ::
  GetAppReplicationConfiguration
newGetAppReplicationConfiguration =
  GetAppReplicationConfiguration'
    { appId =
        Core.Nothing
    }

-- | The ID of the application.
getAppReplicationConfiguration_appId :: Lens.Lens' GetAppReplicationConfiguration (Core.Maybe Core.Text)
getAppReplicationConfiguration_appId = Lens.lens (\GetAppReplicationConfiguration' {appId} -> appId) (\s@GetAppReplicationConfiguration' {} a -> s {appId = a} :: GetAppReplicationConfiguration)

instance
  Core.AWSRequest
    GetAppReplicationConfiguration
  where
  type
    AWSResponse GetAppReplicationConfiguration =
      GetAppReplicationConfigurationResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetAppReplicationConfigurationResponse'
            Core.<$> ( x Core..?> "serverGroupReplicationConfigurations"
                         Core..!@ Core.mempty
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetAppReplicationConfiguration

instance Core.NFData GetAppReplicationConfiguration

instance
  Core.ToHeaders
    GetAppReplicationConfiguration
  where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSServerMigrationService_V2016_10_24.GetAppReplicationConfiguration" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON GetAppReplicationConfiguration where
  toJSON GetAppReplicationConfiguration' {..} =
    Core.object
      (Core.catMaybes [("appId" Core..=) Core.<$> appId])

instance Core.ToPath GetAppReplicationConfiguration where
  toPath = Core.const "/"

instance Core.ToQuery GetAppReplicationConfiguration where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetAppReplicationConfigurationResponse' smart constructor.
data GetAppReplicationConfigurationResponse = GetAppReplicationConfigurationResponse'
  { -- | The replication configurations associated with server groups in this
    -- application.
    serverGroupReplicationConfigurations :: Core.Maybe [ServerGroupReplicationConfiguration],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetAppReplicationConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'serverGroupReplicationConfigurations', 'getAppReplicationConfigurationResponse_serverGroupReplicationConfigurations' - The replication configurations associated with server groups in this
-- application.
--
-- 'httpStatus', 'getAppReplicationConfigurationResponse_httpStatus' - The response's http status code.
newGetAppReplicationConfigurationResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetAppReplicationConfigurationResponse
newGetAppReplicationConfigurationResponse
  pHttpStatus_ =
    GetAppReplicationConfigurationResponse'
      { serverGroupReplicationConfigurations =
          Core.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The replication configurations associated with server groups in this
-- application.
getAppReplicationConfigurationResponse_serverGroupReplicationConfigurations :: Lens.Lens' GetAppReplicationConfigurationResponse (Core.Maybe [ServerGroupReplicationConfiguration])
getAppReplicationConfigurationResponse_serverGroupReplicationConfigurations = Lens.lens (\GetAppReplicationConfigurationResponse' {serverGroupReplicationConfigurations} -> serverGroupReplicationConfigurations) (\s@GetAppReplicationConfigurationResponse' {} a -> s {serverGroupReplicationConfigurations = a} :: GetAppReplicationConfigurationResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
getAppReplicationConfigurationResponse_httpStatus :: Lens.Lens' GetAppReplicationConfigurationResponse Core.Int
getAppReplicationConfigurationResponse_httpStatus = Lens.lens (\GetAppReplicationConfigurationResponse' {httpStatus} -> httpStatus) (\s@GetAppReplicationConfigurationResponse' {} a -> s {httpStatus = a} :: GetAppReplicationConfigurationResponse)

instance
  Core.NFData
    GetAppReplicationConfigurationResponse
