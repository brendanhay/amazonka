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
-- Module      : Amazonka.Greengrass.UpdateThingRuntimeConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the runtime configuration of a thing.
module Amazonka.Greengrass.UpdateThingRuntimeConfiguration
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Greengrass.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateThingRuntimeConfiguration' smart constructor.
data UpdateThingRuntimeConfiguration = UpdateThingRuntimeConfiguration'
  { -- | Configuration for telemetry service.
    telemetryConfiguration :: Prelude.Maybe TelemetryConfigurationUpdate,
    -- | The thing name.
    thingName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  UpdateThingRuntimeConfiguration
newUpdateThingRuntimeConfiguration pThingName_ =
  UpdateThingRuntimeConfiguration'
    { telemetryConfiguration =
        Prelude.Nothing,
      thingName = pThingName_
    }

-- | Configuration for telemetry service.
updateThingRuntimeConfiguration_telemetryConfiguration :: Lens.Lens' UpdateThingRuntimeConfiguration (Prelude.Maybe TelemetryConfigurationUpdate)
updateThingRuntimeConfiguration_telemetryConfiguration = Lens.lens (\UpdateThingRuntimeConfiguration' {telemetryConfiguration} -> telemetryConfiguration) (\s@UpdateThingRuntimeConfiguration' {} a -> s {telemetryConfiguration = a} :: UpdateThingRuntimeConfiguration)

-- | The thing name.
updateThingRuntimeConfiguration_thingName :: Lens.Lens' UpdateThingRuntimeConfiguration Prelude.Text
updateThingRuntimeConfiguration_thingName = Lens.lens (\UpdateThingRuntimeConfiguration' {thingName} -> thingName) (\s@UpdateThingRuntimeConfiguration' {} a -> s {thingName = a} :: UpdateThingRuntimeConfiguration)

instance
  Core.AWSRequest
    UpdateThingRuntimeConfiguration
  where
  type
    AWSResponse UpdateThingRuntimeConfiguration =
      UpdateThingRuntimeConfigurationResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateThingRuntimeConfigurationResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    UpdateThingRuntimeConfiguration
  where
  hashWithSalt
    _salt
    UpdateThingRuntimeConfiguration' {..} =
      _salt
        `Prelude.hashWithSalt` telemetryConfiguration
        `Prelude.hashWithSalt` thingName

instance
  Prelude.NFData
    UpdateThingRuntimeConfiguration
  where
  rnf UpdateThingRuntimeConfiguration' {..} =
    Prelude.rnf telemetryConfiguration `Prelude.seq`
      Prelude.rnf thingName

instance
  Data.ToHeaders
    UpdateThingRuntimeConfiguration
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateThingRuntimeConfiguration where
  toJSON UpdateThingRuntimeConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("TelemetryConfiguration" Data..=)
              Prelude.<$> telemetryConfiguration
          ]
      )

instance Data.ToPath UpdateThingRuntimeConfiguration where
  toPath UpdateThingRuntimeConfiguration' {..} =
    Prelude.mconcat
      [ "/greengrass/things/",
        Data.toBS thingName,
        "/runtimeconfig"
      ]

instance Data.ToQuery UpdateThingRuntimeConfiguration where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateThingRuntimeConfigurationResponse' smart constructor.
data UpdateThingRuntimeConfigurationResponse = UpdateThingRuntimeConfigurationResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  UpdateThingRuntimeConfigurationResponse
newUpdateThingRuntimeConfigurationResponse
  pHttpStatus_ =
    UpdateThingRuntimeConfigurationResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
updateThingRuntimeConfigurationResponse_httpStatus :: Lens.Lens' UpdateThingRuntimeConfigurationResponse Prelude.Int
updateThingRuntimeConfigurationResponse_httpStatus = Lens.lens (\UpdateThingRuntimeConfigurationResponse' {httpStatus} -> httpStatus) (\s@UpdateThingRuntimeConfigurationResponse' {} a -> s {httpStatus = a} :: UpdateThingRuntimeConfigurationResponse)

instance
  Prelude.NFData
    UpdateThingRuntimeConfigurationResponse
  where
  rnf UpdateThingRuntimeConfigurationResponse' {..} =
    Prelude.rnf httpStatus
