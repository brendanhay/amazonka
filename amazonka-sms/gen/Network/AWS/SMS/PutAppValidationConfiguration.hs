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
-- Module      : Network.AWS.SMS.PutAppValidationConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates or updates a validation configuration for the specified
-- application.
module Network.AWS.SMS.PutAppValidationConfiguration
  ( -- * Creating a Request
    PutAppValidationConfiguration (..),
    newPutAppValidationConfiguration,

    -- * Request Lenses
    putAppValidationConfiguration_appValidationConfigurations,
    putAppValidationConfiguration_serverGroupValidationConfigurations,
    putAppValidationConfiguration_appId,

    -- * Destructuring the Response
    PutAppValidationConfigurationResponse (..),
    newPutAppValidationConfigurationResponse,

    -- * Response Lenses
    putAppValidationConfigurationResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SMS.Types

-- | /See:/ 'newPutAppValidationConfiguration' smart constructor.
data PutAppValidationConfiguration = PutAppValidationConfiguration'
  { -- | The configuration for application validation.
    appValidationConfigurations :: Core.Maybe [AppValidationConfiguration],
    -- | The configuration for instance validation.
    serverGroupValidationConfigurations :: Core.Maybe [ServerGroupValidationConfiguration],
    -- | The ID of the application.
    appId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'PutAppValidationConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'appValidationConfigurations', 'putAppValidationConfiguration_appValidationConfigurations' - The configuration for application validation.
--
-- 'serverGroupValidationConfigurations', 'putAppValidationConfiguration_serverGroupValidationConfigurations' - The configuration for instance validation.
--
-- 'appId', 'putAppValidationConfiguration_appId' - The ID of the application.
newPutAppValidationConfiguration ::
  -- | 'appId'
  Core.Text ->
  PutAppValidationConfiguration
newPutAppValidationConfiguration pAppId_ =
  PutAppValidationConfiguration'
    { appValidationConfigurations =
        Core.Nothing,
      serverGroupValidationConfigurations =
        Core.Nothing,
      appId = pAppId_
    }

-- | The configuration for application validation.
putAppValidationConfiguration_appValidationConfigurations :: Lens.Lens' PutAppValidationConfiguration (Core.Maybe [AppValidationConfiguration])
putAppValidationConfiguration_appValidationConfigurations = Lens.lens (\PutAppValidationConfiguration' {appValidationConfigurations} -> appValidationConfigurations) (\s@PutAppValidationConfiguration' {} a -> s {appValidationConfigurations = a} :: PutAppValidationConfiguration) Core.. Lens.mapping Lens._Coerce

-- | The configuration for instance validation.
putAppValidationConfiguration_serverGroupValidationConfigurations :: Lens.Lens' PutAppValidationConfiguration (Core.Maybe [ServerGroupValidationConfiguration])
putAppValidationConfiguration_serverGroupValidationConfigurations = Lens.lens (\PutAppValidationConfiguration' {serverGroupValidationConfigurations} -> serverGroupValidationConfigurations) (\s@PutAppValidationConfiguration' {} a -> s {serverGroupValidationConfigurations = a} :: PutAppValidationConfiguration) Core.. Lens.mapping Lens._Coerce

-- | The ID of the application.
putAppValidationConfiguration_appId :: Lens.Lens' PutAppValidationConfiguration Core.Text
putAppValidationConfiguration_appId = Lens.lens (\PutAppValidationConfiguration' {appId} -> appId) (\s@PutAppValidationConfiguration' {} a -> s {appId = a} :: PutAppValidationConfiguration)

instance
  Core.AWSRequest
    PutAppValidationConfiguration
  where
  type
    AWSResponse PutAppValidationConfiguration =
      PutAppValidationConfigurationResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          PutAppValidationConfigurationResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable PutAppValidationConfiguration

instance Core.NFData PutAppValidationConfiguration

instance Core.ToHeaders PutAppValidationConfiguration where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSServerMigrationService_V2016_10_24.PutAppValidationConfiguration" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON PutAppValidationConfiguration where
  toJSON PutAppValidationConfiguration' {..} =
    Core.object
      ( Core.catMaybes
          [ ("appValidationConfigurations" Core..=)
              Core.<$> appValidationConfigurations,
            ("serverGroupValidationConfigurations" Core..=)
              Core.<$> serverGroupValidationConfigurations,
            Core.Just ("appId" Core..= appId)
          ]
      )

instance Core.ToPath PutAppValidationConfiguration where
  toPath = Core.const "/"

instance Core.ToQuery PutAppValidationConfiguration where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newPutAppValidationConfigurationResponse' smart constructor.
data PutAppValidationConfigurationResponse = PutAppValidationConfigurationResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'PutAppValidationConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'putAppValidationConfigurationResponse_httpStatus' - The response's http status code.
newPutAppValidationConfigurationResponse ::
  -- | 'httpStatus'
  Core.Int ->
  PutAppValidationConfigurationResponse
newPutAppValidationConfigurationResponse pHttpStatus_ =
  PutAppValidationConfigurationResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
putAppValidationConfigurationResponse_httpStatus :: Lens.Lens' PutAppValidationConfigurationResponse Core.Int
putAppValidationConfigurationResponse_httpStatus = Lens.lens (\PutAppValidationConfigurationResponse' {httpStatus} -> httpStatus) (\s@PutAppValidationConfigurationResponse' {} a -> s {httpStatus = a} :: PutAppValidationConfigurationResponse)

instance
  Core.NFData
    PutAppValidationConfigurationResponse
