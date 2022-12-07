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
-- Module      : Amazonka.IoTSiteWise.UpdateGatewayCapabilityConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a gateway capability configuration or defines a new capability
-- configuration. Each gateway capability defines data sources for a
-- gateway. A capability configuration can contain multiple data source
-- configurations. If you define OPC-UA sources for a gateway in the IoT
-- SiteWise console, all of your OPC-UA sources are stored in one
-- capability configuration. To list all capability configurations for a
-- gateway, use
-- <https://docs.aws.amazon.com/iot-sitewise/latest/APIReference/API_DescribeGateway.html DescribeGateway>.
module Amazonka.IoTSiteWise.UpdateGatewayCapabilityConfiguration
  ( -- * Creating a Request
    UpdateGatewayCapabilityConfiguration (..),
    newUpdateGatewayCapabilityConfiguration,

    -- * Request Lenses
    updateGatewayCapabilityConfiguration_gatewayId,
    updateGatewayCapabilityConfiguration_capabilityNamespace,
    updateGatewayCapabilityConfiguration_capabilityConfiguration,

    -- * Destructuring the Response
    UpdateGatewayCapabilityConfigurationResponse (..),
    newUpdateGatewayCapabilityConfigurationResponse,

    -- * Response Lenses
    updateGatewayCapabilityConfigurationResponse_httpStatus,
    updateGatewayCapabilityConfigurationResponse_capabilityNamespace,
    updateGatewayCapabilityConfigurationResponse_capabilitySyncStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTSiteWise.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateGatewayCapabilityConfiguration' smart constructor.
data UpdateGatewayCapabilityConfiguration = UpdateGatewayCapabilityConfiguration'
  { -- | The ID of the gateway to be updated.
    gatewayId :: Prelude.Text,
    -- | The namespace of the gateway capability configuration to be updated. For
    -- example, if you configure OPC-UA sources from the IoT SiteWise console,
    -- your OPC-UA capability configuration has the namespace
    -- @iotsitewise:opcuacollector:version@, where @version@ is a number such
    -- as @1@.
    capabilityNamespace :: Prelude.Text,
    -- | The JSON document that defines the configuration for the gateway
    -- capability. For more information, see
    -- <https://docs.aws.amazon.com/iot-sitewise/latest/userguide/configure-sources.html#configure-source-cli Configuring data sources (CLI)>
    -- in the /IoT SiteWise User Guide/.
    capabilityConfiguration :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateGatewayCapabilityConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'gatewayId', 'updateGatewayCapabilityConfiguration_gatewayId' - The ID of the gateway to be updated.
--
-- 'capabilityNamespace', 'updateGatewayCapabilityConfiguration_capabilityNamespace' - The namespace of the gateway capability configuration to be updated. For
-- example, if you configure OPC-UA sources from the IoT SiteWise console,
-- your OPC-UA capability configuration has the namespace
-- @iotsitewise:opcuacollector:version@, where @version@ is a number such
-- as @1@.
--
-- 'capabilityConfiguration', 'updateGatewayCapabilityConfiguration_capabilityConfiguration' - The JSON document that defines the configuration for the gateway
-- capability. For more information, see
-- <https://docs.aws.amazon.com/iot-sitewise/latest/userguide/configure-sources.html#configure-source-cli Configuring data sources (CLI)>
-- in the /IoT SiteWise User Guide/.
newUpdateGatewayCapabilityConfiguration ::
  -- | 'gatewayId'
  Prelude.Text ->
  -- | 'capabilityNamespace'
  Prelude.Text ->
  -- | 'capabilityConfiguration'
  Prelude.Text ->
  UpdateGatewayCapabilityConfiguration
newUpdateGatewayCapabilityConfiguration
  pGatewayId_
  pCapabilityNamespace_
  pCapabilityConfiguration_ =
    UpdateGatewayCapabilityConfiguration'
      { gatewayId =
          pGatewayId_,
        capabilityNamespace =
          pCapabilityNamespace_,
        capabilityConfiguration =
          pCapabilityConfiguration_
      }

-- | The ID of the gateway to be updated.
updateGatewayCapabilityConfiguration_gatewayId :: Lens.Lens' UpdateGatewayCapabilityConfiguration Prelude.Text
updateGatewayCapabilityConfiguration_gatewayId = Lens.lens (\UpdateGatewayCapabilityConfiguration' {gatewayId} -> gatewayId) (\s@UpdateGatewayCapabilityConfiguration' {} a -> s {gatewayId = a} :: UpdateGatewayCapabilityConfiguration)

-- | The namespace of the gateway capability configuration to be updated. For
-- example, if you configure OPC-UA sources from the IoT SiteWise console,
-- your OPC-UA capability configuration has the namespace
-- @iotsitewise:opcuacollector:version@, where @version@ is a number such
-- as @1@.
updateGatewayCapabilityConfiguration_capabilityNamespace :: Lens.Lens' UpdateGatewayCapabilityConfiguration Prelude.Text
updateGatewayCapabilityConfiguration_capabilityNamespace = Lens.lens (\UpdateGatewayCapabilityConfiguration' {capabilityNamespace} -> capabilityNamespace) (\s@UpdateGatewayCapabilityConfiguration' {} a -> s {capabilityNamespace = a} :: UpdateGatewayCapabilityConfiguration)

-- | The JSON document that defines the configuration for the gateway
-- capability. For more information, see
-- <https://docs.aws.amazon.com/iot-sitewise/latest/userguide/configure-sources.html#configure-source-cli Configuring data sources (CLI)>
-- in the /IoT SiteWise User Guide/.
updateGatewayCapabilityConfiguration_capabilityConfiguration :: Lens.Lens' UpdateGatewayCapabilityConfiguration Prelude.Text
updateGatewayCapabilityConfiguration_capabilityConfiguration = Lens.lens (\UpdateGatewayCapabilityConfiguration' {capabilityConfiguration} -> capabilityConfiguration) (\s@UpdateGatewayCapabilityConfiguration' {} a -> s {capabilityConfiguration = a} :: UpdateGatewayCapabilityConfiguration)

instance
  Core.AWSRequest
    UpdateGatewayCapabilityConfiguration
  where
  type
    AWSResponse UpdateGatewayCapabilityConfiguration =
      UpdateGatewayCapabilityConfigurationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateGatewayCapabilityConfigurationResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
              Prelude.<*> (x Data..:> "capabilityNamespace")
              Prelude.<*> (x Data..:> "capabilitySyncStatus")
      )

instance
  Prelude.Hashable
    UpdateGatewayCapabilityConfiguration
  where
  hashWithSalt
    _salt
    UpdateGatewayCapabilityConfiguration' {..} =
      _salt `Prelude.hashWithSalt` gatewayId
        `Prelude.hashWithSalt` capabilityNamespace
        `Prelude.hashWithSalt` capabilityConfiguration

instance
  Prelude.NFData
    UpdateGatewayCapabilityConfiguration
  where
  rnf UpdateGatewayCapabilityConfiguration' {..} =
    Prelude.rnf gatewayId
      `Prelude.seq` Prelude.rnf capabilityNamespace
      `Prelude.seq` Prelude.rnf capabilityConfiguration

instance
  Data.ToHeaders
    UpdateGatewayCapabilityConfiguration
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

instance
  Data.ToJSON
    UpdateGatewayCapabilityConfiguration
  where
  toJSON UpdateGatewayCapabilityConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("capabilityNamespace" Data..= capabilityNamespace),
            Prelude.Just
              ( "capabilityConfiguration"
                  Data..= capabilityConfiguration
              )
          ]
      )

instance
  Data.ToPath
    UpdateGatewayCapabilityConfiguration
  where
  toPath UpdateGatewayCapabilityConfiguration' {..} =
    Prelude.mconcat
      [ "/20200301/gateways/",
        Data.toBS gatewayId,
        "/capability"
      ]

instance
  Data.ToQuery
    UpdateGatewayCapabilityConfiguration
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateGatewayCapabilityConfigurationResponse' smart constructor.
data UpdateGatewayCapabilityConfigurationResponse = UpdateGatewayCapabilityConfigurationResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The namespace of the gateway capability.
    capabilityNamespace :: Prelude.Text,
    -- | The synchronization status of the capability configuration. The sync
    -- status can be one of the following:
    --
    -- -   @IN_SYNC@ – The gateway is running the capability configuration.
    --
    -- -   @OUT_OF_SYNC@ – The gateway hasn\'t received the capability
    --     configuration.
    --
    -- -   @SYNC_FAILED@ – The gateway rejected the capability configuration.
    --
    -- After you update a capability configuration, its sync status is
    -- @OUT_OF_SYNC@ until the gateway receives and applies or rejects the
    -- updated configuration.
    capabilitySyncStatus :: CapabilitySyncStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateGatewayCapabilityConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateGatewayCapabilityConfigurationResponse_httpStatus' - The response's http status code.
--
-- 'capabilityNamespace', 'updateGatewayCapabilityConfigurationResponse_capabilityNamespace' - The namespace of the gateway capability.
--
-- 'capabilitySyncStatus', 'updateGatewayCapabilityConfigurationResponse_capabilitySyncStatus' - The synchronization status of the capability configuration. The sync
-- status can be one of the following:
--
-- -   @IN_SYNC@ – The gateway is running the capability configuration.
--
-- -   @OUT_OF_SYNC@ – The gateway hasn\'t received the capability
--     configuration.
--
-- -   @SYNC_FAILED@ – The gateway rejected the capability configuration.
--
-- After you update a capability configuration, its sync status is
-- @OUT_OF_SYNC@ until the gateway receives and applies or rejects the
-- updated configuration.
newUpdateGatewayCapabilityConfigurationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'capabilityNamespace'
  Prelude.Text ->
  -- | 'capabilitySyncStatus'
  CapabilitySyncStatus ->
  UpdateGatewayCapabilityConfigurationResponse
newUpdateGatewayCapabilityConfigurationResponse
  pHttpStatus_
  pCapabilityNamespace_
  pCapabilitySyncStatus_ =
    UpdateGatewayCapabilityConfigurationResponse'
      { httpStatus =
          pHttpStatus_,
        capabilityNamespace =
          pCapabilityNamespace_,
        capabilitySyncStatus =
          pCapabilitySyncStatus_
      }

-- | The response's http status code.
updateGatewayCapabilityConfigurationResponse_httpStatus :: Lens.Lens' UpdateGatewayCapabilityConfigurationResponse Prelude.Int
updateGatewayCapabilityConfigurationResponse_httpStatus = Lens.lens (\UpdateGatewayCapabilityConfigurationResponse' {httpStatus} -> httpStatus) (\s@UpdateGatewayCapabilityConfigurationResponse' {} a -> s {httpStatus = a} :: UpdateGatewayCapabilityConfigurationResponse)

-- | The namespace of the gateway capability.
updateGatewayCapabilityConfigurationResponse_capabilityNamespace :: Lens.Lens' UpdateGatewayCapabilityConfigurationResponse Prelude.Text
updateGatewayCapabilityConfigurationResponse_capabilityNamespace = Lens.lens (\UpdateGatewayCapabilityConfigurationResponse' {capabilityNamespace} -> capabilityNamespace) (\s@UpdateGatewayCapabilityConfigurationResponse' {} a -> s {capabilityNamespace = a} :: UpdateGatewayCapabilityConfigurationResponse)

-- | The synchronization status of the capability configuration. The sync
-- status can be one of the following:
--
-- -   @IN_SYNC@ – The gateway is running the capability configuration.
--
-- -   @OUT_OF_SYNC@ – The gateway hasn\'t received the capability
--     configuration.
--
-- -   @SYNC_FAILED@ – The gateway rejected the capability configuration.
--
-- After you update a capability configuration, its sync status is
-- @OUT_OF_SYNC@ until the gateway receives and applies or rejects the
-- updated configuration.
updateGatewayCapabilityConfigurationResponse_capabilitySyncStatus :: Lens.Lens' UpdateGatewayCapabilityConfigurationResponse CapabilitySyncStatus
updateGatewayCapabilityConfigurationResponse_capabilitySyncStatus = Lens.lens (\UpdateGatewayCapabilityConfigurationResponse' {capabilitySyncStatus} -> capabilitySyncStatus) (\s@UpdateGatewayCapabilityConfigurationResponse' {} a -> s {capabilitySyncStatus = a} :: UpdateGatewayCapabilityConfigurationResponse)

instance
  Prelude.NFData
    UpdateGatewayCapabilityConfigurationResponse
  where
  rnf UpdateGatewayCapabilityConfigurationResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf capabilityNamespace
      `Prelude.seq` Prelude.rnf capabilitySyncStatus
