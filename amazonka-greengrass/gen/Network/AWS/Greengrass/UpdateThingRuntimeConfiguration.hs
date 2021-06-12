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
-- Module      : Network.AWS.Greengrass.UpdateThingRuntimeConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the runtime configuration of a thing.
module Network.AWS.Greengrass.UpdateThingRuntimeConfiguration
  ( -- * Creating a Request
    UpdateThingRuntimeConfiguration (..),
    newUpdateThingRuntimeConfiguration,

    -- * Request Lenses
    updateThingRuntimeConfiguration_telemetryConfiguration,
    updateThingRuntimeConfiguration_thingName,

    -- * Destructuring the Response
    UpdateThingRuntimeConfigurationResponse (..),
    newUpdateThingRuntimeConfigurationResponse,

    -- * Response Lenses
    updateThingRuntimeConfigurationResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Greengrass.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateThingRuntimeConfiguration' smart constructor.
data UpdateThingRuntimeConfiguration = UpdateThingRuntimeConfiguration'
  { -- | Configuration for telemetry service.
    telemetryConfiguration :: Core.Maybe TelemetryConfigurationUpdate,
    -- | The thing name.
    thingName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateThingRuntimeConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'telemetryConfiguration', 'updateThingRuntimeConfiguration_telemetryConfiguration' - Configuration for telemetry service.
--
-- 'thingName', 'updateThingRuntimeConfiguration_thingName' - The thing name.
newUpdateThingRuntimeConfiguration ::
  -- | 'thingName'
  Core.Text ->
  UpdateThingRuntimeConfiguration
newUpdateThingRuntimeConfiguration pThingName_ =
  UpdateThingRuntimeConfiguration'
    { telemetryConfiguration =
        Core.Nothing,
      thingName = pThingName_
    }

-- | Configuration for telemetry service.
updateThingRuntimeConfiguration_telemetryConfiguration :: Lens.Lens' UpdateThingRuntimeConfiguration (Core.Maybe TelemetryConfigurationUpdate)
updateThingRuntimeConfiguration_telemetryConfiguration = Lens.lens (\UpdateThingRuntimeConfiguration' {telemetryConfiguration} -> telemetryConfiguration) (\s@UpdateThingRuntimeConfiguration' {} a -> s {telemetryConfiguration = a} :: UpdateThingRuntimeConfiguration)

-- | The thing name.
updateThingRuntimeConfiguration_thingName :: Lens.Lens' UpdateThingRuntimeConfiguration Core.Text
updateThingRuntimeConfiguration_thingName = Lens.lens (\UpdateThingRuntimeConfiguration' {thingName} -> thingName) (\s@UpdateThingRuntimeConfiguration' {} a -> s {thingName = a} :: UpdateThingRuntimeConfiguration)

instance
  Core.AWSRequest
    UpdateThingRuntimeConfiguration
  where
  type
    AWSResponse UpdateThingRuntimeConfiguration =
      UpdateThingRuntimeConfigurationResponse
  request = Request.putJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateThingRuntimeConfigurationResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance
  Core.Hashable
    UpdateThingRuntimeConfiguration

instance Core.NFData UpdateThingRuntimeConfiguration

instance
  Core.ToHeaders
    UpdateThingRuntimeConfiguration
  where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON UpdateThingRuntimeConfiguration where
  toJSON UpdateThingRuntimeConfiguration' {..} =
    Core.object
      ( Core.catMaybes
          [ ("TelemetryConfiguration" Core..=)
              Core.<$> telemetryConfiguration
          ]
      )

instance Core.ToPath UpdateThingRuntimeConfiguration where
  toPath UpdateThingRuntimeConfiguration' {..} =
    Core.mconcat
      [ "/greengrass/things/",
        Core.toBS thingName,
        "/runtimeconfig"
      ]

instance Core.ToQuery UpdateThingRuntimeConfiguration where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newUpdateThingRuntimeConfigurationResponse' smart constructor.
data UpdateThingRuntimeConfigurationResponse = UpdateThingRuntimeConfigurationResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateThingRuntimeConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateThingRuntimeConfigurationResponse_httpStatus' - The response's http status code.
newUpdateThingRuntimeConfigurationResponse ::
  -- | 'httpStatus'
  Core.Int ->
  UpdateThingRuntimeConfigurationResponse
newUpdateThingRuntimeConfigurationResponse
  pHttpStatus_ =
    UpdateThingRuntimeConfigurationResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
updateThingRuntimeConfigurationResponse_httpStatus :: Lens.Lens' UpdateThingRuntimeConfigurationResponse Core.Int
updateThingRuntimeConfigurationResponse_httpStatus = Lens.lens (\UpdateThingRuntimeConfigurationResponse' {httpStatus} -> httpStatus) (\s@UpdateThingRuntimeConfigurationResponse' {} a -> s {httpStatus = a} :: UpdateThingRuntimeConfigurationResponse)

instance
  Core.NFData
    UpdateThingRuntimeConfigurationResponse
